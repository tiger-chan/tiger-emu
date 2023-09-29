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

impl AddrModeData {
    #[cfg(test)]
    pub fn nestest_log_addr1(&self) -> String {
        match &self {
            AddrModeData::A | AddrModeData::Imp => {
                format!("{:<6}", "")
            }
            AddrModeData::Abs(lo, hi)
            | AddrModeData::Abx(lo, hi)
            | AddrModeData::Aby(lo, hi) => {
                format!("{:>02X} {:>02X} ", lo, hi)
            }
            AddrModeData::Imm(val) => {
                format!("{:>02X}     ", val)
            }
            AddrModeData::Ind(lo, hi, _) => {
                format!("{:>02X} {:>02X}  ", lo, hi)
            }
            AddrModeData::Izx(lo, _)
            | AddrModeData::Izy(lo, _)
            | AddrModeData::Rel(lo, _)
            | AddrModeData::Zpx(lo, _)
            | AddrModeData::Zpy(lo, _) => {
                format!("{:>02X}     ", lo)
            }
            AddrModeData::Zpg(lo) => {
                format!("{:>02X}     ", lo)
            }
        }
    }

    #[cfg(test)]
    pub fn nestest_log_addr2(&self) -> String {
        match &self {
            AddrModeData::A | AddrModeData::Imp => {
                format!("{:<28}", "")
            }
            AddrModeData::Abs(lo, hi) => {
                format!("${:>04X}{:23}", *lo as Word | (*hi as Word) << 8, "")
            }
            AddrModeData::Abx(_, _)
            | AddrModeData::Aby(_, _)
            | AddrModeData::Imm(_)
            | AddrModeData::Ind(_, _, _)
            | AddrModeData::Izx(_, _)
            | AddrModeData::Izy(_, _)
            | AddrModeData::Rel(_, _)
            | AddrModeData::Zpx(_, _)
            | AddrModeData::Zpy(_, _)
            | AddrModeData::Zpg(_) => {
                format!("{:>8}", "")
            }
        }
    }
}
