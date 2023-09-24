use std::{
    cell::{Ref, RefCell},
    ops::{AddAssign, RangeInclusive},
    rc::{Rc, Weak},
};

use egui::RichText;

use crate::{
    bus::Bus,
    cpu::Cpu,
    gui::{BoardCommand, CpuDisplay, MemoryDisplay, PpuDisplay, DIAGNOSTIC_FONT},
    motherboard::Motherboard,
};

use super::{cpu6502, Cartridge as Cart, Cpu6502, Ppu2C02, RangeRWCpuBus};

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

pub struct RangedBoardBus {
    devices: Vec<Weak<RefCell<dyn RangeRWCpuBus>>>,
}

impl RangedBoardBus {
    pub fn new() -> Self {
        Self { devices: vec![] }
    }

    pub fn push<T>(&mut self, device: &Rc<RefCell<T>>)
    where
        T: RangeRWCpuBus + 'static,
    {
        let tmp = Rc::downgrade(device);
        self.devices.push(tmp);
    }
}

impl Bus for RangedBoardBus {
    fn read(&self, addr: u16) -> u8 {
        for device in &self.devices {
            let result = match device.upgrade() {
                Some(device) => {
                    if device.borrow().accepted_range().contains(&addr) {
                        device.borrow().read(addr)
                    } else {
                        None
                    }
                }
                _ => None,
            };

            match result {
                Some(r) => {
                    return r;
                }
                _ => continue,
            }
        }

        0
    }

    fn read_only(&self, addr: u16) -> u8 {
        for device in &self.devices {
            let result = match device.upgrade() {
                Some(device) => {
                    if device.borrow().accepted_range().contains(&addr) {
                        device.borrow().read_only(addr)
                    } else {
                        None
                    }
                }
                _ => None,
            };

            match result {
                Some(r) => {
                    return r;
                }
                _ => continue,
            }
        }

        0
    }

    fn write(&mut self, addr: u16, data: u8) {
        for device in &self.devices {
            let result = match device.upgrade() {
                Some(device) => {
                    if device.borrow().accepted_range().contains(&addr) {
                        device.borrow_mut().write(addr, data)
                    } else {
                        None
                    }
                }
                _ => None,
            };

            match result {
                Some(_) => {
                    return;
                }
                _ => continue,
            }
        }
    }
}

pub struct BoardRam {
    ram: [u8; RAM],
}

impl RangeRWCpuBus for BoardRam {
    fn accepted_range(&self) -> RangeInclusive<Addr> {
        0x0000..=0x1FFF
    }

    fn read(&self, addr: Addr) -> Option<u8> {
        let addr = (addr & CPU_RAM_MASK) as usize;
        Some(self.ram[addr])
    }

    fn read_only(&self, addr: Addr) -> Option<u8> {
        let addr = (addr & CPU_RAM_MASK) as usize;
        Some(self.ram[addr])
    }

    fn write(&mut self, addr: Addr, data: u8) -> Option<()> {
        let addr = (addr & CPU_RAM_MASK) as usize;
        self.ram[addr] = data;
        Some(())
    }
}

impl BoardRam {
    pub fn new() -> Self {
        Self { ram: [0; RAM] }
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
    ram: Rc<RefCell<BoardRam>>,
    cart: Rc<RefCell<Cart>>,
    bus: RangedBoardBus,
    tcc: u64,
}

impl Board {
    pub fn new() -> Self {
        let ram = Rc::new(RefCell::new(BoardRam::new()));
        let ppu = Rc::new(RefCell::new(Ppu2C02::new(true)));
        let cart = Rc::new(RefCell::new(Cart::new()));

        let mut bus = RangedBoardBus::new();
        bus.push(&cart);
        bus.push(&ram);
        bus.push(&ppu);

        ppu.borrow_mut().bus.cartridge(&cart);

        Self {
            cpu: Rc::new(RefCell::new(Cpu6502::new())),
            bus,
            ppu,
            cart,
            ram,
            tcc: 0,
        }
    }

    #[allow(unused)]
    pub fn cpu(&self) -> Ref<Cpu6502> {
        self.cpu.borrow()
    }

    #[allow(unused)]
    pub fn ppu(&self) -> Ref<Ppu2C02> {
        self.ppu.borrow()
    }

    #[allow(unused)]
    pub fn pc(&mut self, addr: Addr) {
        self.cpu.borrow_mut().reg.pc = addr;
    }

    #[allow(unused)]
    pub fn tcc(&self) -> u64 {
        self.tcc
    }

    #[allow(unused)]
    pub fn instruction_state(&self) -> ((u16, u16), cpu6502::InstructionState) {
        let x = self.ppu.borrow().cycle;
        let y = self.ppu.borrow().scanline;
        let last_cpu = self.cpu.borrow().prev_instruct;
        ((x, y), last_cpu)
    }

    #[allow(unused)]
    pub fn run_until(&mut self, addr: Addr) {
        let mut pc = self.cpu.borrow().reg.pc;
        while pc != addr {
            self.clock();
            pc = self.cpu.borrow().reg.pc
        }
    }

    #[allow(unused)]
    pub fn run_pc(&mut self, addr: Addr) {
        while self.cpu().cc != 0 {
            self.clock();
        }
        self.pc(addr);
        while self.cpu().cc == 0 {
            self.clock();
        }
        while self.cpu().cc != 0 {
            self.clock();
        }
    }

    #[allow(unused)]
    pub fn set_prog(&mut self, program: &[u8; RAM]) {
        self.ram.borrow_mut().ram = *program;

        self.cpu.borrow_mut().disssemble(&self.bus, 0x0000, 0xFFFF);
    }

    pub fn load_cart(&mut self, cart: Cart) {
        *self.cart.borrow_mut() = cart;

        self.cpu.borrow_mut().disssemble(&self.bus, 0x0000, 0xFFFF);
    }

    pub fn draw(&self, pixels: &mut [u8]) {
        self.ppu.borrow().draw(pixels);
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
        // PPU runs once every 4 master clock cycles
        self.ppu.borrow_mut().clock(&mut self.bus);

        // CPU runs once every 12 master clock cycles
        if self.tcc % 3 == 0 {
            self.cpu.borrow_mut().clock(&mut self.bus);
        }

        if self.ppu.borrow().nmi() {
            self.cpu.borrow_mut().nmi(&mut self.bus);
        }

        self.tcc = self.tcc.wrapping_add(1);
    }

    fn reset(&mut self) {
        self.tcc = 0;
        self.ppu.borrow_mut().reset();
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
            let addr = addr as usize;
            let end_addr = addr + (rows as usize * cols as usize);
            let range = addr..end_addr;
            let mem_block: Vec<u8> = range.map(|x| self.bus.read_only(x as Addr)).collect();

            for (i, chunk) in mem_block.chunks(cols as usize).enumerate() {
                let mut str = String::with_capacity(cols as usize * 3 + 6);
                str.push_str(format!("{:>04X} ", addr as Addr + (i as u16 * cols as u16)).as_str());
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
}

impl BoardCommand for Board {
    fn step(&mut self) {
        self.clock();
        while self.cpu.borrow().cc != 0 {
            self.clock();
        }

        self.clock();
        while self.cpu.borrow().cc == 0 {
            self.clock();
        }
    }

    fn frame(&mut self) {
        self.clock();
        while !self.ppu.borrow().frame_complete() {
            self.clock();
        }
        self.clock();
        while self.cpu.borrow().cc != 0 {
            self.clock();
        }

        self.ppu.borrow_mut().debug_reset_complete();
    }
}

impl PpuDisplay for Board {
    fn draw_pattern_tbl(&self, ui: &mut egui::Ui, tbl: u8, palette: u8) {
        self.ppu
            .borrow_mut()
            .draw_pattern_tbl(ui, tbl as Addr, palette as Addr);
    }
}
