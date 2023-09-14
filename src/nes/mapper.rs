use self::nrom::Nrom;

mod ines_header;
mod nrom;

pub use ines_header::INesHeader;

use super::Addr;

pub trait NesCpuMapper {
    fn read(&self, addr: Addr) -> Option<Addr>;
    fn write(&self, addr: Addr) -> Option<Addr>;
}

pub trait NesPpuMapper {
    fn read(&self, addr: Addr) -> Option<Addr>;
    fn write(&self, addr: Addr) -> Option<Addr>;
}

pub enum Mapper {
    NROM(Nrom),
}

impl Mapper {
    fn cpu(&self) -> &dyn NesCpuMapper {
        match &self {
            Self::NROM(x) => x,
        }
    }

    fn ppu(&self) -> &dyn NesPpuMapper {
        match &self {
            Self::NROM(x) => x,
        }
    }
}

impl From<&INesHeader> for Mapper {
    fn from(value: &INesHeader) -> Self {
        let mpr_id = (value.mpr2 & 0xF0) | value.mpr1 >> 4;
        match mpr_id {
            0 => Self::NROM(Nrom::new(value.prg_chunks)),
            1 => {
                unimplemented!("Mapper {} not implemented", mpr_id)
            }
            2 => {
                unimplemented!("Mapper {} not implemented", mpr_id)
            }
            _ => {
                unimplemented!("Mapper {} not implemented", mpr_id)
            }
        }
    }
}

impl NesCpuMapper for Mapper {
    fn read(&self, addr: Addr) -> Option<Addr> {
        self.cpu().read(addr)
    }

    fn write(&self, addr: Addr) -> Option<Addr> {
        self.cpu().write(addr)
    }
}

impl NesPpuMapper for Mapper {
    fn read(&self, addr: Addr) -> Option<Addr> {
        self.ppu().read(addr)
    }

    fn write(&self, addr: Addr) -> Option<Addr> {
        self.ppu().write(addr)
    }
}
