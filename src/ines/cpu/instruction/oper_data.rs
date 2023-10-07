#[cfg(test)]
use super::AddrMode;
use crate::ines::{Byte, Word};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum OperData {
    Word(Word),
    Byte(Byte),
    None,
}

impl OperData {
    #[cfg(test)]
    pub fn nestest_log(&self, addr: AddrMode) -> String {
        match self {
            OperData::Word(w) => {
                format!("${:<04X}", w)
            }
            OperData::Byte(b) => match addr {
                AddrMode::IMM => String::from(""),
                AddrMode::IMP => String::from(""),
                _ => {
                    format!(" = {:<02X}", b)
                }
            },
            _ => "".to_owned(),
        }
    }
}
