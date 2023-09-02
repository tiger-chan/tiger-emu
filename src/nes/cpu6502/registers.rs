///
/// PC   program counter              (16 bit)
///
/// AC   accumulator                  ( 8 bit)
///
/// X    X register                   ( 8 bit)
///
/// Y    Y register                   ( 8 bit)
///
/// SR   status register  [NV-BDIZC]  ( 8 bit)
///
/// SP   stack pointer                ( 8 bit)
///
/// Note: The status register (SR) is also known as the P register.
///

#[derive(Default)]
pub struct Registers {
    /// Program Counter
    pub pc: u16,
    /// Accumulator Register
    pub ac: u8,
    /// X Register
    pub x: u8,
    /// Y Register
    pub y: u8,
    /// Status Register
    pub p: u8,
    /// Stack Pointer (points to location on bus)
    pub sp: u8,
}

///
/// SR Flags (bit 7 to bit 0)
///
/// N	Negative
/// V	Overflow
/// -	ignored
/// B	Break
/// D	Decimal (use BCD for arithmetics)
/// I	Interrupt (IRQ disable)
/// Z	Zero
/// C	Carry

pub mod status_flags {
    /// Carry Bit
    pub const C: u8 = 1 << 0;
    /// Zero
    pub const Z: u8 = 1 << 1;
    /// Disable Interrupts
    pub const I: u8 = 1 << 2;
    /// Decimal Mode (unused in this implementation)
    pub const D: u8 = 1 << 3;
    /// Break
    pub const B: u8 = 1 << 4;
    /// Unused
    pub const U: u8 = 1 << 5;
    /// Overflow
    pub const V: u8 = 1 << 6;
    /// Negative
    pub const N: u8 = 1 << 7;
}
