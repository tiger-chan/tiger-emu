mod registers;
mod status_reg;
mod bus;
//mod instruction;

use std::{cell::RefCell, rc::Rc};

pub use registers::Registers;
pub use status_reg::Status;
pub use bus::Bus;

use super::io::RwDevice;

pub type CpuRef<CpuBus> = Rc<RefCell<Cpu<CpuBus>>>;

#[derive(Debug)]
pub struct Cpu<CpuBus: RwDevice> {
    reg: Registers,
    bus: Option<CpuBus>,
}

impl<CpuBus: RwDevice> Default for Cpu<CpuBus> {
    fn default() -> Self {
        Self {
            reg: Registers::default(),
            bus: None,
        }
    }
}

impl<CpuBus: RwDevice> Iterator for Cpu<CpuBus> {
    type Item = ();
    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
