use std::{cell::RefCell, rc::Rc};

use crate::ppu::Color;

use super::{Byte, Word};

pub trait ReadDevice: std::fmt::Debug {
    fn read(&self, addr: Word) -> Byte;
}

pub trait ReadOnlyDevice: std::fmt::Debug {
    fn read_only(&self, addr: Word) -> Byte;
}

pub trait WriteDevice: std::fmt::Debug {
    fn write(&mut self, addr: Word, data: Byte) -> Byte;
}

pub trait DisplayDevice: std::fmt::Debug {
    fn write(&mut self, x: Word, y: Word, data: Color);
}

pub trait RwDevice: ReadDevice + WriteDevice {}

pub type RwDeviceRef = Rc<RefCell<dyn RwDevice>>;

#[derive(Debug)]
pub struct VoidDisplay;

impl DisplayDevice for VoidDisplay {
    fn write(&mut self, _: Word, _: Word, _: Color) {}
}
