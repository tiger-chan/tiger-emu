use crate::{
    io::{ReadDevice, RwDevice, WriteDevice},
    Byte, Word,
};

const PATTERN_TBL: usize = 0x1000;
const PATTERN_TBL_SIZE: usize = 2 * PATTERN_TBL;
const PATTERN_MASK: Word = 0x1FFF;

#[derive(Debug)]
pub struct PatternTable(pub [Byte; PATTERN_TBL_SIZE]);

impl Default for PatternTable {
    fn default() -> Self {
        Self([0; PATTERN_TBL_SIZE])
    }
}

impl RwDevice for PatternTable {}

impl ReadDevice for PatternTable {
    fn read(&self, addr: Word) -> Byte {
        let masked = (addr & PATTERN_MASK) as usize;
        self.0[masked]
    }
}

impl WriteDevice for PatternTable {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        let masked = (addr & PATTERN_MASK) as usize;
        let tmp = self.0[masked];
        self.0[masked] = data;
        tmp
    }
}
