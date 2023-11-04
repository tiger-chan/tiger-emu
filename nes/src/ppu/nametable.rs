use crate::{
    io::{ReadDevice, RwDevice, WriteDevice},
    Byte, Word,
};

const NAMETABLE: usize = 0x0400;
const NAMETABLE_MASK: Word = 0x3FF;

#[derive(Debug)]
pub struct NameTable(pub [Byte; NAMETABLE]);

impl Default for NameTable {
    fn default() -> Self {
        Self([0; NAMETABLE])
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
