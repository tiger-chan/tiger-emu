use super::{AddrModeData, OperData};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum InstructionResult {
    Clock,
    Result(AddrModeData, OperData),
}
