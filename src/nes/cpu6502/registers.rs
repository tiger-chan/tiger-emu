use core::fmt;
use std::ops::{Add, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

///```
/// PC   program counter              (16 bit)
/// AC   accumulator                  ( 8 bit)
/// X    X register                   ( 8 bit)
/// Y    Y register                   ( 8 bit)
/// SR   status register  [NV-BDIZC]  ( 8 bit)
/// SP   stack pointer                ( 8 bit)
///```
/// Note: The status register (SR) is also known as the P register.
///

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
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
    pub p: StatusReg,
    /// Stack Pointer (points to location on bus)
    pub sp: u8,
}

///
/// ```text
/// 7  bit  0
/// ---- ----
/// NV1B DIZC
/// |||| ||||
/// |||| |||+- Carry
/// |||| ||+-- Zero
/// |||| |+--- Interrupt Disable
/// |||| +---- Decimal
/// |||+------ (No CPU effect; see: the B flag)
/// ||+------- (No CPU effect; always pushed as 1)
/// |+-------- Overflow
/// +--------- Negative
/// ```
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct StatusReg(u8);

impl StatusReg {
    pub const fn new(val: u8) -> Self {
        Self(val)
    }

    pub fn set(&mut self, f: StatusReg, v: bool) -> &mut Self {
        match v {
            true => self.bitor_assign(f),
            false => self.bitand_assign(!f),
        }
        self
    }

    pub fn get(&self, f: StatusReg) -> StatusReg {
        Self((*self & f == f) as u8)
    }

    /// The zero flag (Z) indicates a value of all zero bits and the
    /// negative flag (N) indicates the presence of a set sign bit in
    /// bit-position 7. These flags are always updated, whenever a
    /// value is transferred to a CPU register (A,X,Y) and as a result
    /// of any logical ALU operations. The Z and N flags are also
    /// updated by increment and decrement operations acting on a
    /// memory location.
    ///
    /// All arithmetic operations update the Z, N, C and V flags.
    pub const Z: StatusReg = StatusReg::new(1 << 1);
    /// negative flag (N) indicates the presence of a set sign bit in
    /// bit-position 7. These flags are always updated, whenever a
    /// value is transferred to a CPU register (A,X,Y) and as a result
    /// of any logical ALU operations. The Z and N flags are also
    /// updated by increment and decrement operations acting on a
    /// memory location.
    ///
    /// All arithmetic operations update the Z, N, C and V flags.
    pub const N: StatusReg = StatusReg::new(1 << 7);
    /// The carry flag (C) flag is used as a buffer and as a borrow in
    /// arithmetic operations. Any comparisons will update this
    /// additionally to the Z and N flags, as do shift and rotate
    /// operations.
    ///
    /// All arithmetic operations update the Z, N, C and V flags.
    pub const C: StatusReg = StatusReg::new(1 << 0);
    /// The overflow flag (V) indicates overflow with signed binary
    /// arithmetics. As a signed byte represents a range of -128 to
    /// +127, an overflow can never occur when the operands are of
    /// opposite sign, since the result will never exceed this range.
    /// Thus, overflow may only occur, if both operands are of the
    /// same sign. Then, the result must be also of the same sign.
    /// Otherwise, overflow is detected and the overflow flag is set.
    /// (I.e., both operands have a zero in the sign position at bit
    /// 7, but bit 7 of the result is 1, or, both operands have the
    /// sign-bit set, but the result is positive.)
    ///
    /// All arithmetic operations update the Z, N, C and V flags.
    pub const V: StatusReg = StatusReg::new(1 << 6);
    /// The decimal flag (D) sets the ALU to binary coded decimal
    /// (BCD) mode for additions and subtractions (ADC, SBC).
    pub const D: StatusReg = StatusReg::new(1 << 3);
    /// The interrupt inhibit flag (I) blocks any maskable interrupt
    /// requests (IRQ).
    pub const I: StatusReg = StatusReg::new(1 << 2);
    /// The break flag (B) is not an actual flag implemented in a
    /// register, and rather appears only, when the status register is
    /// pushed onto or pulled from the stack. When pushed, it will be
    /// 1 when transfered by a BRK or PHP instruction, and zero
    /// otherwise (i.e., when pushed by a hardware interrupt). When
    /// pulled into the status register (by PLP or on RTI), it will be
    /// ignored.
    ///
    /// In other words, the break flag will be inserted, whenever the
    /// status register is transferred to the stack by software (BRK
    /// or PHP), and will be zero, when transferred by hardware. Since
    /// there is no actual slot for the break flag, it will be always
    /// ignored, when retrieved (PLP or RTI). The break flag is not
    /// accessed by the CPU at anytime and there is no internal
    /// representation. Its purpose is more for patching, to discern
    /// an interrupt caused by a BRK instruction from a normal
    /// interrupt initiated by hardware.
    pub const B: StatusReg = StatusReg::new(1 << 4);
    /// Unused / Ignored
    pub const U: StatusReg = StatusReg::new(1 << 5);
}

impl From<u8> for StatusReg {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl From<StatusReg> for u8 {
    fn from(value: StatusReg) -> Self {
        value.0
    }
}

impl From<StatusReg> for u16 {
    fn from(value: StatusReg) -> Self {
        value.0 as u16
    }
}

impl Add<u8> for StatusReg {
    type Output = u8;
    fn add(self, rhs: u8) -> Self::Output {
        self.0 + rhs
    }
}

impl Add<StatusReg> for u8 {
    type Output = u8;
    fn add(self, rhs: StatusReg) -> Self::Output {
        self + rhs.0
    }
}

impl Add<u16> for StatusReg {
    type Output = u16;
    fn add(self, rhs: u16) -> Self::Output {
        self.0 as u16 + rhs
    }
}

impl Add<StatusReg> for u16 {
    type Output = u16;
    fn add(self, rhs: StatusReg) -> Self::Output {
        self + rhs.0 as u16
    }
}

impl BitOr for StatusReg {
    type Output = StatusReg;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitOr<u8> for StatusReg {
    type Output = StatusReg;
    fn bitor(self, rhs: u8) -> Self::Output {
        Self(self.0 | rhs)
    }
}

impl BitOrAssign for StatusReg {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}

impl BitOrAssign<u8> for StatusReg {
    fn bitor_assign(&mut self, rhs: u8) {
        self.0 |= rhs
    }
}

impl BitAnd for StatusReg {
    type Output = StatusReg;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitAnd<u8> for StatusReg {
    type Output = StatusReg;
    fn bitand(self, rhs: u8) -> Self::Output {
        Self(self.0 & rhs)
    }
}

impl BitAndAssign for StatusReg {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0
    }
}

impl BitAndAssign<u8> for StatusReg {
    fn bitand_assign(&mut self, rhs: u8) {
        self.0 &= rhs
    }
}

impl BitXor for StatusReg {
    type Output = StatusReg;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0 ^ rhs.0)
    }
}

impl BitXor<u8> for StatusReg {
    type Output = StatusReg;
    fn bitxor(self, rhs: u8) -> Self::Output {
        Self(self.0 ^ rhs)
    }
}

impl BitXorAssign for StatusReg {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0
    }
}

impl BitXorAssign<u8> for StatusReg {
    fn bitxor_assign(&mut self, rhs: u8) {
        self.0 ^= rhs
    }
}

impl PartialEq<u8> for StatusReg {
    fn eq(&self, other: &u8) -> bool {
        &self.0 == other
    }
}

impl Not for StatusReg {
    type Output = StatusReg;
    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl fmt::Display for StatusReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let func = |f, i, e| match self.get(f) == 1 {
            true => i,
            false => e,
        };

        let n = func(StatusReg::N, 'N', '.');
        let v = func(StatusReg::V, 'V', '.');
        let u = func(StatusReg::U, '-', '.');
        let b = func(StatusReg::B, 'B', '.');
        let d = func(StatusReg::D, 'D', '.');
        let i = func(StatusReg::I, 'I', '.');
        let z = func(StatusReg::Z, 'Z', '.');
        let c = func(StatusReg::C, 'C', '.');

        write!(
            f,
            "{:?}{:?}{:?}{:?}{:?}{:?}{:?}{:?}",
            n, v, u, b, d, i, z, c
        )
    }
}
