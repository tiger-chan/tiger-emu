// https://www.nesdev.org/wiki/NROM

use crate::ines::{
    cart::{Cartridge, CharacterMemory, ProgramMemory},
    io::RwMapper,
    Byte, Word,
};

// CPU $6000-$7FFF: Family Basic only: PRG RAM, mirrored as
// necessary to fill entire 8 KiB window, write protectable with an
// external switch
const RAM_LO: Word = 0x6000;
const RAM_HI: Word = 0x7FFF;
const RAM_MASK: Word = 0x1FFF;
const RAM_SIZE: usize = 0x2000;
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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Nrom {
    ram: ProgramMemory,
    prg: ProgramMemory,
    chr: CharacterMemory,
    prg_bnk: u8,
    chr_bnk: u8,
}

impl Default for Nrom {
    fn default() -> Self {
        Self {
            ram: vec![0; RAM_SIZE],
            prg: vec![],
            chr: vec![0; CHR_SIZE],
            prg_bnk: 0,
            chr_bnk: 0,
        }
    }
}

impl RwMapper for Nrom {
    fn read_chr(&self, addr: Word) -> Byte {
        if (CHR_LO..=CHR_HI).contains(&addr) {
            self.chr[addr as usize]
        } else {
            0
        }
    }

    fn read_prg(&self, addr: Word) -> Byte {
        match addr {
            RAM_LO..=RAM_HI => {
                let masked = addr & RAM_MASK;
                self.ram[masked as usize]
            }
            PRG_LO..=PRG_HI => {
                let masked = addr
                    & if self.prg_bnk > 1 {
                        PRG_MASK
                    } else {
                        PRG_MIRROR_MASK
                    };
                self.prg[masked as usize]
            }
            _ => 0,
        }
    }

    fn write_chr(&mut self, addr: Word, data: Byte) -> Byte {
        if (CHR_LO..=CHR_HI).contains(&addr) && self.chr_bnk == 0 {
            let tmp = self.chr[addr as usize];
            self.chr[addr as usize] = data;
            tmp
        } else {
            0
        }
    }

    fn write_prg(&mut self, addr: Word, data: Byte) -> Byte {
        if (RAM_LO..=RAM_HI).contains(&addr) {
            let masked = addr & RAM_MASK;
            let tmp = self.prg[masked as usize];
            self.prg[masked as usize] = data;
            tmp
        } else {
            0
        }
    }
}

impl From<Cartridge> for Nrom {
    fn from(value: Cartridge) -> Self {
        Self {
            chr: value.chr,
            chr_bnk: value.chr_bnk,
            prg: value.prg,
            prg_bnk: value.prg_bnk,
            ram: vec![0; RAM_SIZE],
        }
    }
}
