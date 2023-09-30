mod bus;

use std::{cell::RefCell, rc::Rc};

use super::io::RwDevice;

pub use bus::Bus;

#[derive(Debug, Default, Clone, Copy)]
pub struct PpuState {
    pub scanline: u16,
    pub cycle: u16,
}

pub type PpuRef<PpuBus> = Rc<RefCell<Ppu<PpuBus>>>;

#[derive(Debug)]
pub struct Ppu<PpuBus: RwDevice> {
    #[allow(unused)]
    bus: Option<PpuBus>,
    state: PpuState,
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
