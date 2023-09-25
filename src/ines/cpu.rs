mod bus;
mod instruction;
mod registers;
mod status_reg;

use std::{cell::RefCell, rc::Rc};

pub use bus::Bus;
pub use registers::Registers;
pub use status_reg::Status;

use self::instruction::InstructionIterator;

use super::io::RwDevice;

pub type CpuRef<CpuBus> = Rc<RefCell<Cpu<CpuBus>>>;

#[derive(Debug)]
pub struct Cpu<CpuBus: RwDevice> {
    reg: Registers,
    bus: Option<CpuBus>,
    instruction: InstructionIterator,
}

impl<CpuBus: RwDevice> Default for Cpu<CpuBus> {
    fn default() -> Self {
        Self {
            reg: Registers::default(),
            bus: None,
            instruction: InstructionIterator::default(),
        }
    }
}

impl<CpuBus: RwDevice> Iterator for Cpu<CpuBus> {
    type Item = ();
    fn next(&mut self) -> Option<Self::Item> {
        match self.instruction.next() {
            Some(result) => match result {
                instruction::InstructionResult::Clock => {
                    if let Some(bus) = &mut self.bus {
                        self.instruction.clock(&mut self.reg, bus);
                        Some(())
                    } else {
                        None
                    }
                }
                instruction::InstructionResult::Result(_addr, _oper) => Some(()),
            },
            None => None,
        }
    }
}
