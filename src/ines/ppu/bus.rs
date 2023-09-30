use crate::ines::{
    cart::MapperRef,
    io::{ReadDevice, RwDevice, RwDeviceRef, WriteDevice},
    Byte,
};

#[allow(unused)]
#[derive(Debug)]
pub struct Bus {
    cpu: RwDeviceRef,
    mpr: MapperRef,
}

impl Bus {
    #[allow(dead_code)]
    pub fn new(cpu: RwDeviceRef, mpr: MapperRef) -> Self {
        Self { cpu, mpr }
    }
}

impl RwDevice for Bus {}

impl ReadDevice for Bus {
    fn read(&self, _addr: crate::ines::Word) -> crate::ines::Byte {
        0
    }
}

impl WriteDevice for Bus {
    fn write(&mut self, _addr: crate::ines::Word, _data: crate::ines::Byte) -> Byte {
        0
    }
}
