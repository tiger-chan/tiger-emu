use std::{cell::RefCell, rc::Rc};

use super::{Byte, Word};

pub trait ReadDevice {
    fn read(&self, addr: Word) -> Byte;
}

pub trait WriteDevice {
    fn write(&mut self, addr: Word, data: Byte);
}

pub trait RwDevice: ReadDevice + WriteDevice {}

pub trait RwMapper {
    fn read_prg(&self, addr: Word) -> Byte;
    fn read_chr(&self, addr: Word) -> Byte;
    fn write_prg(&mut self, addr: Word, data: Byte);
    fn write_chr(&mut self, addr: Word, data: Byte);
}

#[allow(dead_code)]
pub type RwDeviceRef = Rc<RefCell<dyn RwDevice>>;
