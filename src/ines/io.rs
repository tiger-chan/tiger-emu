use std::{cell::RefCell, rc::Rc};

use super::{Byte, Word};

pub trait ReadDevice {
    fn read(&self, addr: Word) -> Byte;
}

pub trait WriteDevice {
    fn write(&mut self, addr: Word, data: Byte);
}

pub trait RwDevice: ReadDevice + WriteDevice {}

#[allow(dead_code)]
pub type RwDeviceRef = Rc<RefCell<dyn RwDevice>>;
