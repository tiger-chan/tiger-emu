use std::ops::Range;

use nes::{
    prelude::cpu::{AddrMode, ADDR_MODE, INSTRUCTION_TYPE},
    Byte, Word,
};

pub struct Assembly {
    lines: Vec<String>,
    mapping: Vec<usize>,
}

impl Assembly {
    pub fn with_capacity(size: usize) -> Self {
        Self {
            lines: Vec::with_capacity(size / 2),
            mapping: Vec::with_capacity(size),
        }
    }

    pub fn push(&mut self, line: String, addr_range: Range<u32>) {
        let idx = self.lines.len();
        self.lines.push(line);

        for x in addr_range {
            self.mapping.insert(x as usize, idx);
        }
    }

    pub fn get(&self, addr: u16) -> Option<&str> {
        if (addr as usize) < self.mapping.len() {
            let idx = self.mapping[addr as usize];
            Some(&self.lines[idx])
        } else {
            None
        }
    }

    pub fn get_range(&self, addr: u16, dist: i8) -> Vec<&str> {
        if (addr as usize) < self.mapping.len() {
            let origin = self.mapping[addr as usize];
            let (start, end) = if dist.is_negative() {
                (origin.saturating_sub(-dist as usize), origin)
            } else {
                (origin, origin.saturating_add(dist as usize))
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

impl From<&[Byte]> for Assembly {
    fn from(value: &[Byte]) -> Self {
        use AddrMode::*;
        let start = 0;
        let end = value.len();
        let mut addr = start as u32;

        let mut asm = Assembly::with_capacity(end);

        while addr < end as u32 {
            let ln_addr = addr as Word;

            let opcode = value[ln_addr as usize] as usize;
            addr += 1;
            let op = INSTRUCTION_TYPE[opcode];
            let am = ADDR_MODE[opcode];

            let line = match am {
                A => {
                    format!("${:>04X}: {:?} A            {{{:?}}}", ln_addr, op, am)
                }
                IMP => {
                    format!("${:>04X}: {:?}              {{{:?}}}", ln_addr, op, am)
                }
                IMM => {
                    let lo = value[addr as usize];
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} #${:>02X}         {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                ZPG => {
                    let lo = value[addr as usize];
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} ${:>02X}          {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                ZPX => {
                    let lo = value[addr as usize];
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} ${:>02X},X        {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                ZPY => {
                    let lo = value[addr as usize];
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} ${:>02X},Y         {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                IZX => {
                    let lo = value[addr as usize];
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} (${:>02X},X)     {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                IZY => {
                    let lo = value[addr as usize];
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} (${:>02X}),Y      {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                ABS => {
                    let lo = value[addr as usize] as Word;
                    addr += 1;
                    let hi = (value[addr as usize] as Word) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} ${:>04X}        {{{:?}}}",
                        ln_addr, op, val, am
                    )
                }
                ABX => {
                    let lo = value[addr as usize] as Word;
                    addr += 1;
                    let hi = (value[addr as usize] as Word) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} ${:>04X},X      {{{:?}}}",
                        ln_addr, op, val, am
                    )
                }
                ABY => {
                    let lo = value[addr as usize] as Word;
                    addr += 1;
                    let hi = (value[addr as usize] as Word) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} ${:>04X},Y      {{{:?}}}",
                        ln_addr, op, val, am
                    )
                }
                IND => {
                    let lo = value[addr as usize] as Word;
                    addr += 1;
                    let hi = (value[addr as usize] as Word) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} (${:>04X})       {{{:?}}}",
                        ln_addr, op, val, am
                    )
                }
                REL => {
                    let lo = value[addr as usize] as Word;
                    let lo = if lo & 0x80 == 0x80 { lo | 0xFF00 } else { lo };
                    addr += 1;
                    let rel = (addr as Word).wrapping_add(lo);
                    format!(
                        "${:>04X}: {:?} ${:>04X} [${:>02X}]  {{{:?}}}",
                        ln_addr, op, rel, lo as i8, am
                    )
                }
            };

            asm.push(line, (ln_addr as u32)..addr);
        }

        asm
    }
}
