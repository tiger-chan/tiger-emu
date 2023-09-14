use super::{NesCpuMapper, NesPpuMapper};

pub struct Nrom {
    prg_bnks: u8,
}

impl Nrom {
    pub fn new(prg_bnks: u8) -> Self {
        Self { prg_bnks }
    }
}

impl Default for Nrom {
    fn default() -> Self {
        Self { prg_bnks: 0 }
    }
}

impl NesCpuMapper for Nrom {
    fn read(&self, addr: crate::nes::Addr) -> Option<crate::nes::Addr> {
        if (0x8000..=0xFFFF).contains(&addr) {
            Some(addr & if self.prg_bnks > 1 { 0x7FFF } else { 0x3FFF })
        } else {
            None
        }
    }

    fn write(&self, addr: crate::nes::Addr) -> Option<crate::nes::Addr> {
        if (0x8000..=0xFFFF).contains(&addr) {
            Some(0)
        } else {
            None
        }
    }
}

impl NesPpuMapper for Nrom {
    fn read(&self, addr: crate::nes::Addr) -> Option<crate::nes::Addr> {
        if (0x0000..=0x1FFF).contains(&addr) {
            Some(addr)
        } else {
            None
        }
    }

    fn write(&self, _: crate::nes::Addr) -> Option<crate::nes::Addr> {
        // Do nothing
        None
    }
}
