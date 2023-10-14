use crate::{Byte, Word};

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
#[derive(Default, Clone, Copy, PartialEq, PartialOrd)]
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

impl std::fmt::Display for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "pc: {:<04X}, ac: {:<02X}, x: {:<02X}, y: {:<02X}, p: {:<02X}, sp: {:<02X}",
            self.pc, self.ac, self.x, self.y, self.p.0, self.sp
        )
    }
}

impl std::fmt::Debug for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "pc: {:<04X}, ac: {:<02X}, x: {:<02X}, y: {:<02X}, p: {:<02X}, sp: {:<02X}",
            self.pc, self.ac, self.x, self.y, self.p.0, self.sp
        )
    }
}
