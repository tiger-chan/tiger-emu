use crate::{cartridge::Cartridge, bus::Bus};

pub trait PpuBus {
	fn clock(bus: &mut dyn Bus, cart: &mut dyn Cartridge);
	
    fn cpu_read(&self, addr: u16) -> u8;
    fn cpu_read_only(&self, addr: u16) -> u8;
    fn cpu_write(&mut self, addr: u16, data: u8);
    
	fn ppu_read(&self, addr: u16) -> u8;
    fn ppu_read_only(&self, addr: u16) -> u8;
    fn ppu_write(&mut self, addr: u16, data: u8);
}
