use std::{cell::RefCell, rc::Rc};

use crate::cpu::CPU;

use super::{Bus6502, Cpu6502, RAM};

/// Bytes, Words, Addressing
/// 8 bit bytes, 16 bit words in lobyte-hibyte representation (Little-Endian).
/// 16 bit address range, operands follow instruction codes.
///
/// Signed values are two's complement, sign in bit 7 (most significant bit).
/// (%11111111 = $FF = -1, %10000000 = $80 = -128, %01111111 = $7F = +127)
/// Signed binary and binary coded decimal (BCD) arithmetic modes.
type Addr = u16;

/// Processor Stack
///
/// LIFO, top-down, 8 bit range, 0x0100 - 0x01FF
pub const PS: Addr = 0x0100;

#[allow(dead_code)]
/// Processor Stack End
///
/// LIFO, top-down, 8 bit range, 0x0100 - 0x01FF
pub const PS_HI: Addr = 0x01FF;

pub const HI_MASK: Addr = 0xFF00;
pub const LO_MASK: Addr = 0x00FF;

// System Vectors
/// $FFFA, $FFFB ... NMI (Non-Maskable Interrupt) vector, 16-bit (LB, HB)
pub const NMI_LO: Addr = 0xFFFA;
/// $FFFA, $FFFB ... NMI (Non-Maskable Interrupt) vector, 16-bit (LB, HB)
pub const NMI_HI: Addr = 0xFFFB;

/// $FFFC, $FFFD ... RES (Reset) vector, 16-bit (LB, HB)
pub const RES_LO: Addr = 0xFFFC;

/// $FFFC, $FFFD ... RES (Reset) vector, 16-bit (LB, HB)
pub const RES_HI: Addr = 0xFFFD;

/// $FFFE, $FFFF ... IRQ (Interrupt Request) vector, 16-bit (LB, HB)
pub const IRQ_LO: Addr = 0xFFFE;

/// $FFFE, $FFFF ... IRQ (Interrupt Request) vector, 16-bit (LB, HB)
pub const IRQ_HI: Addr = 0xFFFF;

pub struct Board {
    pub bus: Rc<RefCell<Bus6502>>,
    pub cpu: Rc<RefCell<Cpu6502>>,
}

impl Board {
    pub fn new() -> Self {
        let board = Self {
            bus: Rc::new(RefCell::new(Bus6502::new())),
            cpu: Rc::new(RefCell::new(Cpu6502::new())),
        };

        board.bus.borrow_mut().connect_cpu(&board.cpu);
        board.cpu.borrow_mut().connect_bus(&board.bus);

        board
    }

    pub fn set_prog(&mut self, program: &[u8; RAM]) {
        self.bus.borrow_mut().set_ram(program);

        self.cpu.borrow_mut().disssemble(0x0000, 0xFFFF);
    }

    pub fn reset(&mut self) {
        self.cpu.borrow_mut().reset();
    }
}
