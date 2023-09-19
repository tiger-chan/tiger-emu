use std::cell::RefCell;

use crate::nes::{Addr, RWPpuBus};

use super::{create_palette, Color, X2C02, X2C07, Registers};

const INTERN_PPU_MASK: Addr = 0x3FFF;
const TBL_NAME: usize = 0x0400;
const TBL_NAME_COUNT: usize = 2;
const TBL_PATTERN: usize = 0x1000;
const TBL_PATTERN_COUNT: usize = 2;
const TBL_PALETTE: usize = 0x0020;

type NameTable = [[u8; TBL_NAME]; TBL_NAME_COUNT];
type PatternTable = [[u8; TBL_PATTERN]; TBL_PATTERN_COUNT];
type PaletteTable = [u8; TBL_PALETTE];

macro_rules! name_arr {
    ($value:literal) => {
        [[$value; TBL_NAME]; TBL_NAME_COUNT]
    };
}

macro_rules! pattern_arr {
    ($value:literal) => {
        [[$value; TBL_PATTERN]; TBL_PATTERN_COUNT]
    };
}

pub struct PpuMemory {
    pub reg: RefCell<Registers>,
    pub vram: RefCell<NameTable>,
    pub pattern: RefCell<PatternTable>,
    pub palette: RefCell<PaletteTable>,
    pub col_palette: [Color; 64],
}

impl PpuMemory {
    pub fn new(ntsc: bool) -> Self {
        Self {
            reg: RefCell::new(Registers::default()),
            vram: RefCell::new(name_arr![0]),
            pattern: RefCell::new(pattern_arr![0]),
            palette: RefCell::new([0; TBL_PALETTE]),
            // I would prefer there a better way to do this
            col_palette: if ntsc {
                create_palette(X2C02)
            } else {
                create_palette(X2C07)
            },
        }
    }
}

impl RWPpuBus for PpuMemory {
    fn read(&self, addr: Addr) -> Option<u8> {
        let addr = addr & INTERN_PPU_MASK;
        match &addr {
            0x0000..=0x1FFF => {
                let tbl = ((addr & 0x1000) >> 12) as usize;
                let addr = (addr & 0x0FFF) as usize;
                let tbl = &self.pattern.borrow()[tbl];
                Some(tbl[addr])
            }
            0x2000..=0x3EFF => None,
            0x3F00..=0x3FFF => {
                let addr = addr & 0x001F;
                let addr = match addr {
                    0x0010 => 0x0000,
                    0x0014 => 0x0004,
                    0x0018 => 0x0008,
                    0x001C => 0x000C,
                    x => x,
                };

                let data = self.palette.borrow()[addr as usize]; //& (mask.grayscale ? 0x30 : 0x3F);
                Some(data)
            }
            _ => None,
        }
    }

    fn read_only(&self, addr: Addr) -> Option<u8> {
        let addr = addr & INTERN_PPU_MASK;
        match &addr {
            0x0000..=0x1FFF => {
                let tbl = ((addr & 0x1000) >> 12) as usize;
                let addr = (addr & 0x0FFF) as usize;
                let tbl = &self.pattern.borrow()[tbl];
                Some(tbl[addr])
            }
            0x2000..=0x3EFF => None,
            0x3F00..=0x3FFF => {
                let addr = addr & 0x001F;
                let addr = match addr {
                    0x0010 => 0x0000,
                    0x0014 => 0x0004,
                    0x0018 => 0x0008,
                    0x001C => 0x000C,
                    x => x,
                };

                let data = self.palette.borrow()[addr as usize]; //& (mask.grayscale ? 0x30 : 0x3F);
                Some(data)
            }
            _ => None,
        }
    }

    fn write(&mut self, addr: Addr, data: u8) -> Option<()> {
        let addr = addr & INTERN_PPU_MASK;
        match &addr {
            0x0000..=0x1FFF => {
                let tbl = ((addr & 0x1000) >> 12) as usize;
                let addr = (addr & 0x0FFF) as usize;
                let tbl = &mut self.pattern.borrow_mut()[tbl];
                tbl[addr] = data;
                Some(())
            }
            0x2000..=0x3EFF => None,
            0x3F00..=0x3FFF => {
                let addr = addr & 0x001F;
                let addr = match addr {
                    0x0010 => 0x0000,
                    0x0014 => 0x0004,
                    0x0018 => 0x0008,
                    0x001C => 0x000C,
                    x => x,
                };

                self.palette.borrow_mut()[addr as usize] = data;
                Some(())
            }
            _ => None,
        }
    }
}
