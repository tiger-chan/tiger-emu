use super::Cpu6502;
use crate::bus::Bus;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
pub const RAM: usize = 64 * 1024;
//pub const RAM: usize = 0x0800;
//const MAX_ADDR: u16 = 0xFFFF;

type Cpu = RefCell<Cpu6502>;

pub struct Bus6502 {
    ram: [u8; RAM],
    cpu: Option<Weak<Cpu>>,
}

impl Bus6502 {
    pub fn new() -> Self {
        Bus6502 {
            cpu: None,
            ram: [0; RAM],
        }
    }

    pub fn connect_cpu(&mut self, cpu: &Rc<Cpu>) {
        self.cpu = Some(Rc::downgrade(&cpu));
    }

    pub fn set_ram(&mut self, ram: &[u8; RAM]) {
        self.ram = *ram;
    }
}

impl Bus for Bus6502 {
    fn read(&self, addr: u16) -> u8 {
        self.ram[addr as usize]
    }

    fn read_only(&self, addr: u16) -> u8 {
        self.ram[addr as usize]
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.ram[addr as usize] = data;
    }
}
