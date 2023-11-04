use crate::{Byte, Word};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum OperData {
    Word(Word),
    Byte(Byte),
    None,
}
