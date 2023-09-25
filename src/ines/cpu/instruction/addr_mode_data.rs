use crate::ines::{Byte, Word};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum AddrModeData {
    A,
    Abs(Byte, Byte),
    Abx(Byte, Byte),
    Aby(Byte, Byte),
    Imm(Byte),
    Imp,
    Ind(Byte, Byte, Word),
    Izx(Byte, Word),
    Izy(Byte, Word),
    Rel(Byte, Word),
    Zpg(Byte),
    Zpx(Byte, Byte),
    Zpy(Byte, Byte),
}
