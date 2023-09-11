use std::{
    cell::RefCell,
    ops::AddAssign,
    rc::{Rc, Weak},
};

use egui::RichText;

use crate::{
    bus::Bus,
    cpu::CPU,
    gui::{CpuDisplay, MemoryDisplay, DIAGNOSTIC_FONT},
    motherboard::Motherboard, ppu_bus::PpuBus,
	cartridge::Cartridge,
};

use super::{Cpu6502, Ppu2C02, Cartridge as Cart};

/// Bytes, Words, Addressing
/// 8 bit bytes, 16 bit words in lobyte-hibyte representation (Little-Endian).
/// 16 bit address range, operands follow instruction codes.
///
/// Signed values are two's complement, sign in bit 7 (most significant bit).
/// (%11111111 = $FF = -1, %10000000 = $80 = -128, %01111111 = $7F = +127)
/// Signed binary and binary coded decimal (BCD) arithmetic modes.
pub type Addr = u16;

/// Processor Stack
///
/// LIFO, top-down, 8 bit range, 0x0100 - 0x01FF
pub const PS: Addr = 0x0100;

/// Processor Stack End
///
/// LIFO, top-down, 8 bit range, 0x0100 - 0x01FF
#[allow(unused)]
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

pub const RAM: usize = 0x0800;
//pub const RAM: usize = 64 * 1024;

#[allow(unused)]
pub const CPU_RAM: Addr = 0x0800;
pub const CPU_RAM_MASK: Addr = 0x07FF;
pub const PPU_RAM_MASK: Addr = 0x0007;

pub struct BoardBus {
    ram: Weak<RefCell<[u8; RAM]>>,
    ppu: Weak<RefCell<Ppu2C02>>,
    cart: Weak<RefCell<Cart>>,
}

impl BoardBus {
	#[allow(unused)]
    pub fn new_cpu(ram: &Rc<RefCell<[u8; RAM]>>) -> Self {
        Self {
            ram: Rc::downgrade(ram),
			ppu: Weak::new(),
			cart: Weak::new(),
        }
    }
   
    pub fn new(ram: &Rc<RefCell<[u8; RAM]>>, ppu: &Rc<RefCell<Ppu2C02>>, cart: &Rc<RefCell<Cart>>) -> Self {
        Self {
            ram: Rc::downgrade(ram),
			ppu: Rc::downgrade(ppu),
			cart: Rc::downgrade(cart),
        }
    }
}

impl Bus for BoardBus {
    fn read(&self, addr: u16) -> u8 {
		let result = match self.cart.upgrade() {
			Some(cart) => cart.borrow().cpu_read(addr),
			None => None
		};

		match result {
			Some(v) => v,
			None => {
				match addr {
					0x0000..=0x1FFF => {
						let addr = (addr & CPU_RAM_MASK) as usize;
						self.ram.upgrade().expect("Ram is connected").borrow()[addr]
					}
					0x2000..=0x3FFF => {
						let addr = (addr & PPU_RAM_MASK) as usize;
						self.ppu.upgrade().expect("PPU is connected").borrow().cpu_read(addr as Addr)
					}
					_ => 0
				}
			}
		}
    }

    fn read_only(&self, addr: u16) -> u8 {
		let result = match self.cart.upgrade() {
			Some(cart) => cart.borrow().cpu_read_only(addr),
			None => None
		};

		match result {
			Some(v) => v,
			None => {
				match addr {
					0x0000..=0x1FFF => {
						let addr = (addr & CPU_RAM_MASK) as usize;
						self.ram.upgrade().expect("Ram is connected").borrow()[addr]
					}
					0x2000..=0x3FFF => {
						let addr = (addr & PPU_RAM_MASK) as usize;
						self.ppu.upgrade().expect("PPU is connected").borrow().cpu_read_only(addr as Addr)
					}
					_ => 0
				}
			}
		}
    }

    fn write(&mut self, addr: u16, data: u8) {
		let result = match self.cart.upgrade() {
			Some(cart) => cart.borrow().cpu_read(addr),
			None => None
		};

		match result {
			Some(_) => (),
			None => {
				match addr {
					0x0000..=0x1FFF => {
						let addr = (addr & CPU_RAM_MASK) as usize;
						self.ram.upgrade().expect("Ram is connected").borrow_mut()[addr] = data
					}
					0x2000..=0x3FFF => {
						let addr = (addr & PPU_RAM_MASK) as usize;
						self.ppu.upgrade().expect("PPU is connected").borrow_mut().cpu_write(addr as Addr, data)
					}
					_ => ()
				}
			}
		}
    }
}

pub struct ClockBusContext<'a> {
    source: &'a mut dyn Bus,
    pub rw_count: RefCell<u8>,
}

impl<'a> ClockBusContext<'a> {
    pub fn new(bus: &'a mut dyn Bus) -> Self {
        Self {
            source: bus,
            rw_count: RefCell::new(0),
        }
    }
}

impl Bus for ClockBusContext<'_> {
    fn read(&self, addr: u16) -> u8 {
        self.rw_count.borrow_mut().add_assign(1);
        self.source.read(addr)
    }

    fn read_only(&self, addr: u16) -> u8 {
        self.source.read_only(addr)
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.rw_count.borrow_mut().add_assign(1);
        self.source.write(addr, data);
    }
}

pub struct Board {
    cpu: Rc<RefCell<Cpu6502>>,
    ppu: Rc<RefCell<Ppu2C02>>,
    ram: Rc<RefCell<[u8; RAM]>>,
	cart: Rc<RefCell<Cart>>,
    bus: BoardBus,
	tcc: u64,
}

impl Board {
    pub fn new() -> Self {
        let ram = Rc::new(RefCell::new([0; RAM]));
		let ppu = Rc::new(RefCell::new(Ppu2C02::new()));
		let cart = Rc::new(RefCell::new(Cart::new()));
        Self {
            cpu: Rc::new(RefCell::new(Cpu6502::new())),
            bus: BoardBus::new(&ram, &ppu, &cart),
            ppu: ppu,
			cart: cart,
            ram: ram,
			tcc: 0,
        }
    }

    pub fn set_prog(&mut self, program: &[u8; RAM]) {
        *self.ram.borrow_mut() = *program;

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
		self.tcc = self.tcc.wrapping_add(1);
    }

    fn reset(&mut self) {
		self.tcc = 0;
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
        while self.cpu.borrow().cc != 0 {
            self.clock();
        }
        self.clock();
        while self.cpu.borrow().cc != 0 {
            self.clock();
        }
    }
}
