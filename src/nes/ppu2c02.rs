use std::cell::RefCell;
use std::rc::Rc;

use crate::ppu_bus::PpuBus;
use crate::nes::board::PPU_RAM_MASK;

use super::Addr;

const INTERN_PPU_MASK: Addr = 0x3FFF;

const TBL_NAME: usize = 0x0400;
const TBL_NAME_COUNT: usize = 2;
const TBL_PALETTE: usize = 0x0020;

type NameTable = [[u8; TBL_NAME]; TBL_NAME_COUNT];
type PaletteTable = [u8; TBL_PALETTE];

macro_rules! name_arr {
	($value:literal) => {
		[[$value; TBL_NAME]; TBL_NAME_COUNT]
	};
}

pub struct Ppu2C02 {
	vram: Rc<RefCell<NameTable>>,
	palette: Rc<RefCell<PaletteTable>>,
}

impl Ppu2C02 {
    pub fn new() -> Self {
		let vram = Rc::new(RefCell::new(name_arr![0]));
		let palette = Rc::new(RefCell::new([0; TBL_PALETTE]));
        Self {
			vram: vram,
			palette: palette,
		}
    }
}


impl PpuBus for Ppu2C02 {
	fn clock(bus: &mut dyn crate::bus::Bus, cart: &mut dyn crate::cartridge::Cartridge) {

	}

	fn cpu_read(&self, addr: u16) -> u8 {
		let _addr = addr & PPU_RAM_MASK;
		unimplemented!();
	}

	fn cpu_read_only(&self, addr: u16) -> u8 {
		let _addr = addr & PPU_RAM_MASK;
		unimplemented!();
	}

	fn cpu_write(&mut self, addr: u16, data: u8) {
		let _addr = addr & PPU_RAM_MASK;
		unimplemented!();
	}

	fn ppu_read(&self, addr: u16) -> u8 {
		let _addr = addr & INTERN_PPU_MASK;
		unimplemented!();
	}

	fn ppu_read_only(&self, addr: u16) -> u8 {
		let _addr = addr & INTERN_PPU_MASK;
		unimplemented!();
	}

	fn ppu_write(&mut self, addr: u16, data: u8) {
		let _addr = addr & INTERN_PPU_MASK;
		unimplemented!();
	}
}
