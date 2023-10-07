use crate::ines::{Byte, Word};

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

impl AddrModeData {
    #[cfg(test)]
    pub fn nestest_log_addr1(&self) -> String {
        match &self {
            AddrModeData::A | AddrModeData::Imp => {
                String::from("")
            }
            AddrModeData::Abs(lo, hi)
            | AddrModeData::Abx(lo, hi, _)
            | AddrModeData::Aby(lo, hi, _)
            | AddrModeData::Ind(lo, hi, _) => {
                format!("{:>02X} {:>02X}", lo, hi)
            }
            AddrModeData::Imm(val) => {
                format!("{:>02X}", val)
            }
            AddrModeData::Izx(lo, _, _)
            | AddrModeData::Izy(lo, _, _)
            | AddrModeData::Rel(lo, _)
            | AddrModeData::Zpx(lo, _)
            | AddrModeData::Zpy(lo, _) => {
                format!("{:>02X}", lo)
            }
            AddrModeData::Zpg(lo) => {
                format!("{:>02X}", lo)
            }
        }
    }

    #[cfg(test)]
    pub fn nestest_log_addr2(&self) -> String {
        match &self {
            AddrModeData::A => String::from("A"),
            AddrModeData::Imp => String::from(""),
            AddrModeData::Abs(lo, hi) => {
                format!("${:>04X}", *lo as Word | (*hi as Word) << 8)
            }
            AddrModeData::Aby(lo, hi, addr) => {
                format!("${:>02X}{:>02X},Y @ {:>04X}", hi, lo, addr)
            }
            AddrModeData::Abx(lo, hi, addr) => {
                format!("${:>02X}{:>02X},X @ {:>04X}", hi, lo, addr)
            }
            AddrModeData::Imm(bb) => {
                format!("#${:>02X}", bb)
            }
            AddrModeData::Zpg(ll) => {
                format!("${:>02X}", ll)
            }
            AddrModeData::Izx(ll, ptr, addr) => {
                format!("(${:>02X},X) @ {:>02X} = {:>04X}", ll, ptr, addr)
            }
            AddrModeData::Izy(ll, ptr, addr) => {
                format!("(${:>02X}),Y = {:>04X} @ {:>04X}", ll, ptr, addr)
            }
            AddrModeData::Ind(lo, hi, addr) => {
                format!("(${:>02X}{:>02X}) = {:>04X}", hi, lo, addr)
            }
            AddrModeData::Zpx(lo, addr) => {
                format!("${:>02X},X @ {:>02X}", lo, addr)
            }
            AddrModeData::Zpy(lo, addr) => {
                format!("${:>02X},Y @ {:>02X}", lo, addr)
            }
            AddrModeData::Rel(_, _) => String::from(""),
        }
    }
}
