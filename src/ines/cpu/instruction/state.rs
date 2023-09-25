use crate::ines::Word;

use super::{AddrModeData, OperData};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct InstructionState {
    pub addr: Word,
    pub addr_data: AddrModeData,
    pub oper: OperData,
    pub tmp: Word,
}

impl Default for InstructionState {
    fn default() -> Self {
        Self {
            addr: 0x0000,
            addr_data: AddrModeData::A,
            oper: OperData::None,
            tmp: 0,
        }
    }
}
