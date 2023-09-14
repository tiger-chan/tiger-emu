use std::rc::Rc;
use std::{cell::RefCell, rc::Weak};

use crate::bus::Bus;
use crate::nes::board::PPU_RAM_MASK;

use super::{Addr, RWPpuBus, RangeRWCpuBus};

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

pub struct PpuBus {
    devices: [Weak<RefCell<dyn RWPpuBus>>; 2],
}

impl PpuBus {
    pub fn new(ppu: &Rc<RefCell<Ppu2C02>>) -> Self {
        let tmp = Rc::downgrade(&ppu);
        Self {
            devices: [tmp.clone(), tmp],
        }
    }

    pub fn cartridge<T>(&mut self, device: &Rc<RefCell<T>>)
    where
        T: RWPpuBus + 'static,
    {
        let tmp = Rc::downgrade(device);
        self.devices[1] = tmp;
    }
}

impl crate::ppu_bus::PpuBus for PpuBus {
    fn read(&self, addr: u16) -> u8 {
        for device in &self.devices {
            let result = match device.upgrade() {
                Some(device) => device.borrow().read(addr),
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
                Some(device) => device.borrow().read_only(addr),
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
                Some(device) => device.borrow_mut().write(addr, data),
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

#[allow(unused)]
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

    #[allow(unused)]
    fn clock(&mut self, _bus: &mut dyn Bus, _cart: &mut dyn crate::ppu_bus::PpuBus) {}
}

impl RangeRWCpuBus for Ppu2C02 {
    fn accepted_range(&self) -> std::ops::RangeInclusive<Addr> {
        0x2000..=0x3FFF
    }

    fn read(&self, addr: Addr) -> Option<u8> {
        let _addr = addr & PPU_RAM_MASK;
        None
    }

    fn read_only(&self, addr: Addr) -> Option<u8> {
        let _addr = addr & PPU_RAM_MASK;
        None
    }

    fn write(&mut self, addr: Addr, _data: u8) -> Option<()> {
        let _addr = addr & PPU_RAM_MASK;
        None
    }
}

impl RWPpuBus for Ppu2C02 {
    fn read(&self, addr: Addr) -> Option<u8> {
        let _addr = addr & INTERN_PPU_MASK;
        None
    }

    fn read_only(&self, addr: Addr) -> Option<u8> {
        let _addr = addr & INTERN_PPU_MASK;
        None
    }

    fn write(&mut self, addr: Addr, _data: u8) -> Option<()> {
        let _addr = addr & INTERN_PPU_MASK;
        None
    }
}
