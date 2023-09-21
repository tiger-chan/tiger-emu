use super::{NesCpuMapper, NesPpuMapper};

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Nrom {
    prg_bnks: u8,
    chr_bnks: u8,
}

impl Nrom {
    pub fn new(prg_bnks: u8, chr_bnks: u8) -> Self {
        Self { prg_bnks, chr_bnks }
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
            Some(addr & if self.prg_bnks > 1 { 0x7FFF } else { 0x3FFF })
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

    fn write(&self, addr: crate::nes::Addr) -> Option<crate::nes::Addr> {
        if (0x0000..=0x1FFF).contains(&addr) {
            if self.chr_bnks == 0 {
                Some(addr)
            }
            else {
                None
            }
        } else {
            None
        }
    }
}
