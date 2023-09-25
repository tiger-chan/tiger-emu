use crate::ines::{Word, Byte};

use super::Status;

///```text
/// PC   program counter              (16 bit)
/// AC   accumulator                  ( 8 bit)
/// X    X register                   ( 8 bit)
/// Y    Y register                   ( 8 bit)
/// SR   status register  [NV-BDIZC]  ( 8 bit)
/// SP   stack pointer                ( 8 bit)
///```
/// Note: The status register (SR) is also known as the P register.
#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
pub struct Registers {
    /// Program Counter
    pub pc: Word,
    /// Accumulator Register
    pub ac: Byte,
    /// X Register
    pub x: Byte,
    /// Y Register
    pub y: Byte,
    /// Status Register
    pub p: Status,
    /// Stack Pointer (points to location on bus)
    pub sp: Byte,
}
