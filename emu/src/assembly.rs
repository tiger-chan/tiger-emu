use std::ops::Range;

use nes::{
    prelude::cpu::{AddrMode, ADDR_MODE, INSTRUCTION_TYPE},
    Byte, Word,
};

pub struct Assembly {
    lines: Vec<String>,
    mapping: Vec<usize>,
    offset: usize,
}

impl Assembly {
    pub fn with_capacity(size: usize) -> Self {
        Self {
            lines: Vec::with_capacity(size / 2),
            mapping: Vec::with_capacity(size),
            offset: 0,
        }
    }

    pub fn with_offset(mut self, offset: usize) -> Self {
        self.offset = offset;
        self
    }

    pub fn push(&mut self, line: String, addr_range: Range<u32>) {
        let idx = self.lines.len();
        self.lines.push(line);

        for x in addr_range {
            self.mapping.insert(x as usize, idx);
        }
    }

    pub fn get(&self, addr: u16) -> Option<&str> {
        if (addr as usize) < self.offset {
            None
        } else if (addr as usize) - self.offset < self.mapping.len() {
            let idx = self.mapping[addr as usize - self.offset];
            Some(&self.lines[idx])
        } else {
            None
        }
    }

    pub fn get_range(&self, addr: u16, dist: i8) -> Vec<&str> {
        if (addr as usize) < self.offset {
            vec![]
        } else if (addr as usize) - self.offset < self.mapping.len() + self.offset {
            let origin = self.mapping[addr as usize - self.offset];
            let (start, end) = if dist.is_negative() {
                (
                    origin.saturating_sub(-dist as usize).min(self.offset),
                    origin,
                )
            } else {
                (
                    origin,
                    origin.saturating_add(dist as usize).min(self.offset),
                )
            };
            let vec: Vec<&str> = self
                .lines
                .iter()
                .skip(start)
                .take(end - start)
                .map(|x| x.as_str())
                .collect();
            vec
        } else {
            vec![]
        }
    }
}

impl From<(&[Byte], usize)> for Assembly {
    fn from(value: (&[Byte], usize)) -> Self {
        use AddrMode::*;
        let start = 0;
        let end = value.0.len();
        let mut addr = start as u32;

        let mut asm = Assembly::with_capacity(end);

        while addr < end as u32 {
            let ln_addr = addr as Word;

            let opcode = value.0[ln_addr as usize] as usize;
            addr += 1;
            let op = INSTRUCTION_TYPE[opcode];
            let am = ADDR_MODE[opcode];

            let prefix = format!(
                "${:>04X}: {} ",
                ln_addr + value.1 as Word,
                &format!("{:>4}", op)
            );

            let line = match am {
                A => String::from("A"),
                IMP => String::from(""),
                IMM => {
                    let lo = value.0[addr as usize];
                    addr += 1;
                    format!("#${:>02X}", lo)
                }
                ZPG => {
                    let lo = value.0[addr as usize];
                    addr += 1;
                    format!("${:>02X}", lo)
                }
                ZPX => {
                    let lo = value.0[addr as usize];
                    addr += 1;
                    format!("${:>02X},X", lo)
                }
                ZPY => {
                    let lo = value.0[addr as usize];
                    addr += 1;
                    format!("${:>02X},Y", lo)
                }
                IZX => {
                    let lo = value.0[addr as usize];
                    addr += 1;
                    format!("(${:>02X},X)", lo)
                }
                IZY => {
                    let lo = value.0[addr as usize];
                    addr += 1;
                    format!("(${:>02X}),Y", lo)
                }
                ABS => {
                    let lo = value.0[addr as usize] as Word;
                    addr += 1;
                    let hi = (value.0[addr as usize] as Word) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!("${:>04X}", val)
                }
                ABX => {
                    let lo = value.0[addr as usize] as Word;
                    addr += 1;
                    let hi = (value.0[addr as usize] as Word) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!("${:>04X},X", val)
                }
                ABY => {
                    let lo = value.0[addr as usize] as Word;
                    addr += 1;
                    let hi = (value.0[addr as usize] as Word) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!("${:>04X},Y", val)
                }
                IND => {
                    let lo = value.0[addr as usize] as Word;
                    addr += 1;
                    let hi = (value.0[addr as usize] as Word) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!("(${:>04X})", val)
                }
                REL => {
                    let lo = value.0[addr as usize] as Word;
                    let lo = if lo & 0x80 == 0x80 { lo | 0xFF00 } else { lo };
                    addr += 1;
                    let rel = (addr as Word).wrapping_add(lo);
                    format!("${:>04X} [${:>02X}]", rel, lo as i8)
                }
            };

            let line = format!("{}{:<20}{{{:?}}}", prefix, line, am);

            asm.push(line, (ln_addr as u32)..addr);
        }

        asm.with_offset(value.1)
    }
}

impl From<&[Byte]> for Assembly {
    fn from(value: &[Byte]) -> Self {
        Assembly::from((value, 0))
    }
}
