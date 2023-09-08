use std::{cell::RefCell, rc::Rc};

use egui::RichText;

use crate::{
    bus::Bus,
    cpu::CPU,
    gui::{CpuDisplay, MemoryDisplay, DIAGNOSTIC_FONT},
    motherboard::Motherboard,
};

use super::{Cpu6502, Ppu2C02};

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

//pub const RAM: usize = 0x0800;
pub const RAM: usize = 64 * 1024;

pub struct BoardBus {
    ram: Rc<RefCell<[u8; RAM]>>,
}

impl BoardBus {
    pub fn new() -> Self {
        Self {
            ram: Rc::new(RefCell::new([0; RAM])),
        }
    }
}

impl Bus for BoardBus {
    fn read(&self, addr: u16) -> u8 {
        self.ram.borrow()[addr as usize]
    }

    fn read_only(&self, addr: u16) -> u8 {
        self.ram.borrow()[addr as usize]
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.ram.borrow_mut()[addr as usize] = data;
    }
}

pub struct Board {
    pub cpu: Rc<RefCell<Cpu6502>>,
    pub ppu: Rc<RefCell<Ppu2C02>>,
    pub bus: BoardBus,
}

impl Board {
    pub fn new() -> Self {
        Self {
            cpu: Rc::new(RefCell::new(Cpu6502::new())),
            ppu: Rc::new(RefCell::new(Ppu2C02::new())),
            bus: BoardBus::new(),
        }
    }

    pub fn set_prog(&mut self, program: &[u8; RAM]) {
        *self.bus.ram.borrow_mut() = *program;

        self.cpu.borrow_mut().disssemble(&self.bus, 0x0000, 0xFFFF);
    }
}

impl Bus for Board {
    fn read(&self, addr: u16) -> u8 {
        self.bus.read(addr)
    }

    fn read_only(&self, addr: u16) -> u8 {
        self.bus.read_only(addr)
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.bus.write(addr, data);
    }
}

impl Motherboard for Board {
    fn clock(&mut self) {
        self.cpu.borrow_mut().clock(&mut self.bus);
    }

    fn reset(&mut self) {
        self.cpu.borrow_mut().reset(&mut self.bus);
    }

    fn irq(&mut self) {
        self.cpu.borrow_mut().irq(&mut self.bus);
    }

    fn nmi(&mut self) {
        self.cpu.borrow_mut().nmi(&mut self.bus);
    }
}

impl MemoryDisplay for Board {
    fn draw_mem(&self, ui: &mut egui::Ui, addr: u16, rows: u8, cols: u8) {
        ui.vertical(|ui| {
            let mem_block: Vec<u8> = self
                .bus
                .ram
                .borrow()
                .iter()
                .skip(addr as usize)
                .take(rows as usize * cols as usize)
                .map(|x| *x)
                .collect();
            for (i, chunk) in mem_block.chunks(cols as usize).enumerate() {
                let mut str = String::with_capacity(cols as usize * 3 + 6);
                str.push_str(format!("{:>04X} ", addr + (i as u16 * cols as u16)).as_str());
                for mem in chunk {
                    str.push_str(format!("{:>02X} ", mem).as_str());
                }

                ui.label(RichText::new(str).font(DIAGNOSTIC_FONT));
            }
        });
    }
}

impl CpuDisplay for Board {
    fn draw_code(&self, ui: &mut egui::Ui, instruction_count: i8) {
        self.cpu.borrow().draw_code(ui, instruction_count);
    }

    fn draw_cpu(&self, ui: &mut egui::Ui) {
        self.cpu.borrow().draw_cpu(ui);
    }

    fn step(&mut self) {
        let mut cpu = self.cpu.borrow_mut();
        while cpu.state.cc != 0 {
            cpu.clock(&mut self.bus);
        }
        cpu.clock(&mut self.bus);
        while cpu.state.cc != 0 {
            cpu.clock(&mut self.bus);
        }
    }
}
