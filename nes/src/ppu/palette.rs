use std::ops::Sub;

use crate::{
    io::{ReadDevice, RwDevice, WriteDevice},
    Byte, Word,
};

pub const RAM_LO: Word = 0x3F00;
pub const RAM_HI: Word = 0x3FFF;
pub const RAM_SIZE: usize = 0x0020;

pub fn palette(addr: Word) -> usize {
    const RAM_MASK: Word = 0x001F;
    const UNMIRRORED_MASK: Word = 0x0003;
    let addr_lo = addr & RAM_MASK;
    let is_mirrored = ((addr_lo & UNMIRRORED_MASK == 0) && ((addr_lo & 0x0010) == 0x0010)) as Word;
    addr_lo.sub(is_mirrored * 0x0010) as usize
}

#[derive(Debug)]
pub struct PaletteTable(pub [Byte; RAM_SIZE]);

impl Default for PaletteTable {
    fn default() -> Self {
        Self([0; RAM_SIZE])
    }
}

impl RwDevice for PaletteTable {}

impl ReadDevice for PaletteTable {
    fn read(&self, addr: Word) -> Byte {
        let masked = palette(addr);
        self.0[masked]
    }
}

impl WriteDevice for PaletteTable {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        let masked = palette(addr);
        let tmp = self.0[masked];
        self.0[masked] = data;
        tmp
    }
}

#[cfg(test)]
mod tests {
    use crate::ppu::palette::palette;

    #[test]
    fn lo_addr_no_mirroring() {
        for i in 0x00..0x0f {
            assert_eq!(i, palette(i as u16));
        }
    }

    #[test]
    fn hi_addr_no_mirroring() {
        for i in [
            0x11, 0x12, 0x13, 0x15, 0x16, 0x17, 0x19, 0x1A, 0x1B, 0x1D, 0x1E,
        ] {
            assert_eq!(i, palette(i as u16));
        }
    }

    #[test]
    fn hi_addr_mirrored() {
        for i in [0x10, 0x14, 0x18, 0x1C] {
            assert_eq!(i & 0x0F, palette(i as u16));
        }
    }

    #[test]
    fn extended_range_partial_mirroring() {
        for (m, i) in [
            0x11, 0x12, 0x13, 0x15, 0x16, 0x17, 0x19, 0x1A, 0x1B, 0x1D, 0x1E,
        ]
        .iter()
        .enumerate()
        {
            let addr = i + m * 0x20;
            assert_eq!(*i, palette(addr as u16));
        }
    }

    #[test]
    fn extended_range_full_mirroring() {
        for (m, i) in [0x10, 0x14, 0x18, 0x1C].iter().enumerate() {
            let addr = i + m * 0x20;
            assert_eq!(*i & 0x0F, palette(addr as u16));
        }
    }
}
