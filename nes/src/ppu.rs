mod bus;
mod registers;

use std::{cell::RefCell, rc::Rc};

use super::{
    io::{ReadDevice, RwDevice, WriteDevice},
    Byte, Word,
};

pub use bus::Bus;

#[derive(Debug, Default, Clone, Copy)]
pub struct PpuState {
    pub scanline: u16,
    pub cycle: u16,
}

pub type PpuRef<PpuBus> = Rc<RefCell<Ppu<PpuBus>>>;

#[derive(Debug)]
pub struct Ppu<PpuBus: RwDevice> {
    bus: Option<PpuBus>,
    state: PpuState,
}

impl<PpuBus: RwDevice> Ppu<PpuBus> {
    #[allow(unused)]
    pub fn cur_state(&self) -> PpuState {
        PpuState { scanline: self.state.scanline, cycle: self.state.cycle }
    }
}


impl<PpuBus: RwDevice> Default for Ppu<PpuBus> {
    fn default() -> Self {
        Self {
            bus: None,
            state: PpuState::default(),
        }
    }
}

impl<PpuBus: RwDevice> Iterator for Ppu<PpuBus> {
    type Item = PpuState;
    fn next(&mut self) -> Option<Self::Item> {
        self.state.cycle = self.state.cycle.wrapping_add(1);

        if self.state.cycle == 341 {
            self.state.cycle = 0;
            self.state.scanline = self.state.scanline.wrapping_add(1);
        }

        if self.state.scanline == 261 {
            self.state.scanline = u16::MAX;
        }

        Some(self.state)
    }
}

impl<PpuBus: RwDevice> RwDevice for Ppu<PpuBus> {}

impl<PpuBus: RwDevice> ReadDevice for Ppu<PpuBus> {
    fn read(&self, addr: Word) -> Byte {
        if let Some(bus) = &self.bus {
            bus.read(addr)
        } else {
            0
        }
    }
}

impl<PpuBus: RwDevice> WriteDevice for Ppu<PpuBus> {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        if let Some(bus) = &mut self.bus {
            bus.write(addr, data)
        } else {
            0
        }
    }
}
