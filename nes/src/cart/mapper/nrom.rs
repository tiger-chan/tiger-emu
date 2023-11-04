// https://www.nesdev.org/wiki/NROM

use std::{cell::RefCell, rc::Rc};

use crate::{
    cart::{Cartridge, CharacterMemory, ProgramMemory},
    io::{ReadDevice, RwDevice, WriteDevice},
    mem_map::{Access, CpuMemoryMapper, MemoryMap},
    ppu::NameTable,
    Byte, Word,
};

use super::{Mirror, PpuMemoryMapper};

// CPU $6000-$7FFF: Family Basic only: PRG RAM, mirrored as
// necessary to fill entire 8 KiB window, write protectable with an
// external switch
// const RAM_LO: Word = 0x6000;
// const RAM_HI: Word = 0x7FFF;
// const RAM_MASK: Word = 0x1FFF;
// const RAM_SIZE: usize = 0x2000;

// CPU $8000-$BFFF: First 16 KB of ROM.
// CPU $C000-$FFFF: Last 16 KB of ROM (NROM-256) or mirror of
// $8000-$BFFF (NROM-128).
const PRG_LO: Word = 0x8000;
const PRG_HI: Word = 0xFFFF;
const PRG_MASK: Word = 0x7FFF;
const PRG_MIRROR_MASK: Word = 0x3FFF;

const CHR_LO: Word = 0x0000;
const CHR_HI: Word = 0x1FFF;
const CHR_SIZE: usize = 0x2000;

#[derive(Debug, Default, Clone, PartialEq, PartialOrd)]
struct ChrMem(pub CharacterMemory, pub usize);

impl ChrMem {
    fn with_capacity(capacity: usize) -> Self {
        Self(vec![0; capacity], 0)
    }

    fn new(mem: CharacterMemory, bnks: usize) -> Self {
        Self(mem, bnks)
    }
}

impl RwDevice for ChrMem {}

impl ReadDevice for ChrMem {
    fn read(&self, addr: Word) -> Byte {
        let masked = (addr & CHR_HI) as usize;
        self.0[masked]
    }
}

impl WriteDevice for ChrMem {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        let masked = (addr & CHR_HI) as usize;
        if self.1 == 0 {
            let tmp = self.0[masked];
            self.0[masked] = data;
            tmp
        } else {
            0
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, PartialOrd)]
struct PrgMem(pub ProgramMemory, pub usize);

impl PrgMem {
    fn new(mem: ProgramMemory, bnks: usize) -> Self {
        Self(mem, bnks)
    }

    #[allow(unused)]
    fn with_capacity(capacity: usize) -> Self {
        Self(vec![0; capacity], 0)
    }
}

impl RwDevice for PrgMem {}

impl ReadDevice for PrgMem {
    fn read(&self, addr: Word) -> Byte {
        let masked = (addr
            & if self.1 > 1 {
                PRG_MASK
            } else {
                PRG_MIRROR_MASK
            }) as usize;
        self.0[masked]
    }
}

impl WriteDevice for PrgMem {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        let masked = (addr
            & if self.1 > 1 {
                PRG_MASK
            } else {
                PRG_MIRROR_MASK
            }) as usize;
        let tmp = self.0[masked];
        self.0[masked] = data;
        tmp
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Nrom {
    //ram: ProgramMemory,
    prg: Rc<RefCell<PrgMem>>,
    chr: Rc<RefCell<ChrMem>>,
    prg_bnk: u8,
    chr_bnk: u8,
    mirror: Mirror,
}

impl Default for Nrom {
    fn default() -> Self {
        Self {
            //ram: vec![0; RAM_SIZE],
            prg: Rc::default(),
            chr: Rc::new(RefCell::new(ChrMem::with_capacity(CHR_SIZE))),
            prg_bnk: 0,
            chr_bnk: 0,
            mirror: Mirror::Horizontal,
        }
    }
}

impl From<Cartridge> for Nrom {
    fn from(value: Cartridge) -> Self {
        let mirror = if (value.header.mpr1 & 0x01) == 0x01 {
            Mirror::Vertical
        } else {
            Mirror::Horizontal
        };

        Self {
            chr: Rc::new(RefCell::new(ChrMem::new(value.chr, value.chr_bnk as usize))),
            chr_bnk: value.chr_bnk,
            prg: Rc::new(RefCell::new(PrgMem::new(value.prg, value.prg_bnk as usize))),
            prg_bnk: value.prg_bnk,
            //ram: vec![0; RAM_SIZE],
            mirror,
        }
    }
}

impl PpuMemoryMapper for Nrom {
    #[rustfmt::skip]
    fn map_ppu(&self, mem_map: &mut MemoryMap, nt: &[Rc<RefCell<NameTable>>; 2]) {
        use super::name_tbl::*;
        use Access::*;

        mem_map.register(CHR_LO, CHR_HI, self.chr.clone(), Read);
        match self.mirror {
            Mirror::Horizontal => {
                mem_map.register(NAMETABLE_0_LO, NAMETABLE_0_HI, nt[0].clone(), ReadWrite);
                mem_map.register(NAMETABLE_1_LO, NAMETABLE_1_HI, nt[0].clone(), ReadWrite);

                mem_map.register(NAMETABLE_2_LO, NAMETABLE_2_HI, nt[1].clone(), ReadWrite);
                mem_map.register(NAMETABLE_3_LO, NAMETABLE_3_HI, nt[1].clone(), ReadWrite);
            }
            Mirror::Vertical => {
                mem_map.register(NAMETABLE_0_LO, NAMETABLE_0_HI, nt[0].clone(), ReadWrite);
                mem_map.register(NAMETABLE_1_LO, NAMETABLE_1_HI, nt[1].clone(), ReadWrite);

                mem_map.register(NAMETABLE_2_LO, NAMETABLE_2_HI, nt[0].clone(), ReadWrite);
                mem_map.register(NAMETABLE_3_LO, NAMETABLE_3_HI, nt[1].clone(), ReadWrite);
            }
            _ => {
                unimplemented!("NROM must use vertical or horizontal mirroring");
            }
        }
    }
}

impl CpuMemoryMapper for Nrom {
    fn map_cpu(&self, mem_map: &mut MemoryMap) {
        use Access::*;
        //mem_map.register(RAM_LO, RAM_HI, self.ram.clone(), ReadWrite);
        mem_map.register(PRG_LO, PRG_HI, self.prg.clone(), Read);
    }
}
