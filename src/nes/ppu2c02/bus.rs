use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::nes::RWPpuBus;

use super::PpuMemory;

pub struct PpuBus {
    devices: [Weak<RefCell<dyn RWPpuBus>>; 2],
}

impl PpuBus {
    pub fn new(bus: &Rc<RefCell<PpuMemory>>) -> Self {
        let tmp = Rc::downgrade(bus);
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
