use crate::{
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
    fn read(&self, _addr: crate::Word) -> crate::Byte {
        0
    }
}

impl WriteDevice for Bus {
    fn write(&mut self, _addr: crate::Word, _data: crate::Byte) -> Byte {
        0
    }
}
