use crate::ines::registers::{
    bit_and, bit_or, bit_xor, display, not, partial_eq, reg_add_impl, reg_from_impl,
};
use core::fmt;
use std::ops::{Add, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

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
pub struct Status(u8);

impl Status {
    pub const fn new(val: u8) -> Self {
        Self(val)
    }

    pub fn set(&mut self, f: Status, v: bool) -> &mut Self {
        match v {
            true => self.bitor_assign(f),
            false => self.bitand_assign(!f),
        }
        self
    }

    pub fn get(&self, f: Status) -> Status {
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
    pub const Z: Status = Status::new(1 << 1);

    /// negative flag (N) indicates the presence of a set sign bit in
    /// bit-position 7. These flags are always updated, whenever a
    /// value is transferred to a CPU register (A,X,Y) and as a result
    /// of any logical ALU operations. The Z and N flags are also
    /// updated by increment and decrement operations acting on a
    /// memory location.
    ///
    /// All arithmetic operations update the Z, N, C and V flags.
    pub const N: Status = Status::new(1 << 7);

    /// The carry flag (C) flag is used as a buffer and as a borrow in
    /// arithmetic operations. Any comparisons will update this
    /// additionally to the Z and N flags, as do shift and rotate
    /// operations.
    ///
    /// All arithmetic operations update the Z, N, C and V flags.
    pub const C: Status = Status::new(1 << 0);

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
    pub const V: Status = Status::new(1 << 6);

    /// The decimal flag (D) sets the ALU to binary coded decimal
    /// (BCD) mode for additions and subtractions (ADC, SBC).
    pub const D: Status = Status::new(1 << 3);

    /// The interrupt inhibit flag (I) blocks any maskable interrupt
    /// requests (IRQ).
    pub const I: Status = Status::new(1 << 2);

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
    pub const B: Status = Status::new(1 << 4);

    /// Unused / Ignored
    pub const U: Status = Status::new(1 << 5);
}

not!(Status);
reg_from_impl!(Status, u8);
reg_add_impl!(Status, u8);
bit_or!(Status, u8);
bit_and!(Status, u8);
bit_xor!(Status, u8);
partial_eq!(Status, u8);

reg_from_impl!(Status, u16);
reg_add_impl!(Status, u16);
display!(Status [N, V, U, B, D, I, Z, C], [N, V, -, B, D, I, Z, C]);
