use crate::{Byte, Word};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum AddrModeData {
    A,
    Abs(Byte, Byte),
    Abx(Byte, Byte, Word),
    Aby(Byte, Byte, Word),
    Imm(Byte),
    Imp,
    Ind(Byte, Byte, Word),
    Izx(Byte, Byte, Word),
    Izy(Byte, Word, Word),
    Rel(Byte, Word),
    Zpg(Byte),
    Zpx(Byte, Byte),
    Zpy(Byte, Byte),
}
