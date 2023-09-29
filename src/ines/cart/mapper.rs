mod nrom;

use std::{cell::RefCell, rc::Rc};

pub use nrom::Nrom;

use crate::ines::{io::RwMapper, Byte, Word};

use super::Cartridge;

pub type MapperRef = Rc<RefCell<Mapper>>;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Mapper {
    Nrom(Nrom),
}

impl Default for Mapper {
    fn default() -> Self {
        Self::Nrom(Nrom::default())
    }
}

impl RwMapper for Mapper {
    fn read_chr(&self, addr: Word) -> Byte {
        match self {
            Mapper::Nrom(x) => x.read_chr(addr),
        }
    }

    fn read_prg(&self, addr: Word) -> Byte {
        match self {
            Mapper::Nrom(x) => x.read_prg(addr),
        }
    }

    fn write_chr(&mut self, addr: Word, data: Byte) {
        match self {
            Mapper::Nrom(x) => x.write_chr(addr, data),
        }
    }

    fn write_prg(&mut self, addr: Word, data: Byte) {
        match self {
            Mapper::Nrom(x) => x.write_prg(addr, data),
        }
    }
}

impl From<Cartridge> for Mapper {
    fn from(value: Cartridge) -> Self {
        let header = &value.header;
        let mpr_id = (header.mpr2 & 0xF0) | header.mpr1 >> 4;
        match mpr_id {
            0 => Self::Nrom(Nrom::from(value)),
            _ => {
                unimplemented!("Mapper {} not implemented", mpr_id)
            }
        }
    }
}
