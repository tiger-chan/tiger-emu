mod clock_counter;
mod console;
mod cpu;
mod io;
mod registers;

use clock_counter::*;
pub use console::*;

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

/// $FFFE, $FFFF ... IRQ (Interrupt Request) vector, 16-bit (LB, HB)
#[allow(dead_code)]
pub const IRQ_HI: Word = 0xFFFF;

pub const CPU_RAM: usize = 64 * 1024;

pub const CPU_RAM_MASK: Word = 0x07FF;

//pub const PPU_RAM_MASK: Word = 0x0007;
