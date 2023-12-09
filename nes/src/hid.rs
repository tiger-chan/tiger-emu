use std::{cell::RefCell, fmt::Debug, rc::Rc};

use crate::{
    io::{ReadDevice, RwDevice, WriteDevice},
    Byte, Word,
};

mod standard;

pub use standard::{Standard, StandardButton};

pub trait Joypad: Debug {
    fn read(&self) -> Byte;

    fn read_mut(&mut self) -> Byte {
        self.read()
    }

    fn reset(&mut self);
    fn fill(&mut self, data: Byte);
}

pub type HidRef = Rc<RefCell<Hid>>;

#[derive(Debug, Default)]
pub struct Hid {
    joy: [Option<Rc<RefCell<dyn Joypad>>>; 2],
    strobe: bool,
}

impl Hid {
    pub fn connect(&mut self, port: usize, joypad: Rc<RefCell<dyn Joypad>>) -> &mut Self {
        assert!(port < 2, "HID Port must be less than 2");
        self.joy[port] = Some(joypad);
        self
    }
}

impl RwDevice for Hid {}

impl ReadDevice for Hid {
    fn read(&self, addr: Word) -> Byte {
        let masked = (addr & 0x01) as usize;
        if let Some(pad) = self.joy[masked].as_ref() {
            let mut pad = pad.borrow_mut();
            pad.read_mut()
        } else {
            0
        }
    }

    fn read_only(&self, addr: Word) -> Byte {
        let masked = (addr & 0x01) as usize;
        if let Some(pad) = self.joy[masked].as_ref() {
            pad.borrow_mut().read()
        } else {
            0
        }
    }
}

impl WriteDevice for Hid {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        if addr & 0x01 == 0 {
            self.strobe = data > 0;
            if self.strobe {
                for pad in self.joy.iter().flatten() {
                    pad.borrow_mut().reset();
                }
            }
        }
        0
    }
}
