use crate::{
    io::{ReadDevice, RwDevice, WriteDevice},
    Byte, Word,
};

const NAMETABLE_SIZE: usize = 0x0400;
const NAMETABLE_MASK: Word = 0x3FF;
pub const SIZE: Word = 0x0400;
pub const LO: Word = 0x2000;
pub const ATTR_LO: Word = 0x23C0;

#[derive(Debug)]
pub struct NameTable(pub [Byte; NAMETABLE_SIZE]);

impl Default for NameTable {
    fn default() -> Self {
        Self([0; NAMETABLE_SIZE])
    }
}

impl RwDevice for NameTable {}

impl ReadDevice for NameTable {
    fn read(&self, addr: Word) -> Byte {
        let masked = (addr & NAMETABLE_MASK) as usize;
        self.0[masked]
    }
}

impl WriteDevice for NameTable {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        let masked = (addr & NAMETABLE_MASK) as usize;
        let tmp = self.0[masked];
        self.0[masked] = data;
        tmp
    }
}
