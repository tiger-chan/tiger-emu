use crate::ines::{
    io::{ReadDevice, RwDevice, WriteDevice},
    Byte, Word, CPU_RAM, CPU_RAM_MASK,
};

const CPU_MIRROR: Word = 0x1FFF;

#[derive(Debug)]
pub struct Bus {
    ram: [Byte; CPU_RAM],
}

impl Bus {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self { ram: [0; CPU_RAM] }
    }
}

impl ReadDevice for Bus {
    fn read(&self, addr: Word) -> Byte {
        match addr {
            0x0000..=CPU_MIRROR => self.ram[(addr & CPU_RAM_MASK) as usize],
            _ => unimplemented!(),
        }
    }
}

impl WriteDevice for Bus {
    fn write(&mut self, addr: Word, data: Byte) {
        match addr {
            0x0000..=CPU_MIRROR => self.ram[(addr & CPU_RAM_MASK) as usize] = data,
            _ => unimplemented!(),
        }
    }
}

impl RwDevice for Bus {}
