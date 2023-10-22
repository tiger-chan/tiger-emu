pub mod cart;
mod clock_counter;
mod console;
mod cpu;
pub mod io;
mod ppu;
mod registers;

pub trait Clocked {
    type Item;
    fn clock(&mut self) -> Option<Self::Item>;
}

use clock_counter::*;
pub use console::*;
pub use cpu::{Cpu, CpuCtrl, Registers, Status};

pub use ppu::{HEIGHT, WIDTH};

/// Bytes, Words, Addressing
/// 8 bit bytes, 16 bit words in lobyte-hibyte representation (Little-Endian).
/// 16 bit address range, operands follow instruction codes.
///
/// Signed values are two's complement, sign in bit 7 (most significant bit).
/// (%11111111 = $FF = -1, %10000000 = $80 = -128, %01111111 = $7F = +127)
/// Signed binary and binary coded decimal (BCD) arithmetic modes.
pub type Word = u16;
pub type Byte = u8;

/// Processor Stack
///
/// LIFO, top-down, 8 bit range, 0x0100 - 0x01FF
pub const PS: Word = 0x0100;

/// Processor Stack End
///
/// LIFO, top-down, 8 bit range, 0x0100 - 0x01FF
#[allow(unused)]
pub const PS_HI: Word = 0x01FF;

pub const HI_MASK: Word = 0xFF00;

pub const LO_MASK: Word = 0x00FF;

// System Vectors

/// $FFFA, $FFFB ... NMI (Non-Maskable Interrupt) vector, 16-bit (LB, HB)
#[allow(dead_code)]
pub const NMI_LO: Word = 0xFFFA;

/// $FFFA, $FFFB ... NMI (Non-Maskable Interrupt) vector, 16-bit (LB, HB)
#[allow(dead_code)]
pub const NMI_HI: Word = 0xFFFB;

/// $FFFC, $FFFD ... RES (Reset) vector, 16-bit (LB, HB)
#[allow(dead_code)]
pub const RES_LO: Word = 0xFFFC;

/// $FFFC, $FFFD ... RES (Reset) vector, 16-bit (LB, HB)
#[allow(dead_code)]
pub const RES_HI: Word = 0xFFFD;

/// $FFFE, $FFFF ... IRQ (Interrupt Request) vector, 16-bit (LB, HB)
pub const IRQ_LO: Word = 0xFFFE;

/// $FFFC–$FFFD: Reset vector
#[allow(dead_code)]
pub const RESET_HI: Word = 0xFFFD;

/// $FFFC–$FFFD: Reset vector
pub const RESET_LO: Word = 0xFFFC;

/// $FFFE, $FFFF ... IRQ (Interrupt Request) vector, 16-bit (LB, HB)
#[allow(dead_code)]
pub const IRQ_HI: Word = 0xFFFF;

pub const CPU_RAM: usize = 64 * 1024;

pub mod prelude {
    pub use super::console::*;
    pub use super::io;
    pub use super::{Byte, Clocked, Word};
    pub use super::ppu::Color;
    pub mod cpu {
        pub use crate::cpu::{AddrMode, AddrModeData, InstructionState, OperData, OperType};
    }
}
