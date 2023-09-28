mod bus;
mod instruction;
mod registers;
mod status_reg;

use std::{cell::RefCell, rc::Rc};

pub use bus::Bus;
pub use registers::Registers;
pub use status_reg::Status;

use instruction::{AddrModeData, InstructionIterator, OperData, OperType, INSTRUCTION_TYPE};

use self::instruction::OPER;

use super::io::RwDevice;

pub type CpuRef<CpuBus> = Rc<RefCell<Cpu<CpuBus>>>;

#[derive(Debug, Clone, Copy)]
pub struct InstructionState {
    pub reg: Registers,
    pub tcc: u64,
    pub opcode: u8,
    pub addr: AddrModeData,
    pub oper: OperData,
    pub op: OperType,
}

impl Default for InstructionState {
    fn default() -> Self {
        Self {
            reg: Registers::default(),
            tcc: 0,
            opcode: 0xFF,
            addr: AddrModeData::Imp,
            oper: OperData::None,
            op: OperType::ADC,
        }
    }
}

#[derive(Debug)]
pub struct Cpu<CpuBus: RwDevice> {
    reg: Registers,
    bus: Option<CpuBus>,
    instruction: InstructionIterator,
    tcc: u64,
    prev: InstructionState,
}

impl<CpuBus: RwDevice> Default for Cpu<CpuBus> {
    fn default() -> Self {
        Self {
            reg: Registers::default(),
            bus: None,
            instruction: InstructionIterator::default(),
            tcc: 0,
            prev: InstructionState::default(),
        }
    }
}

impl<CpuBus: RwDevice> Iterator for Cpu<CpuBus> {
    type Item = ();
    fn next(&mut self) -> Option<Self::Item> {
        match self.instruction.next() {
            Some(result) => match result {
                instruction::InstructionResult::Clock => {
                    let bus = self.bus.as_mut().expect("Bus is set");
                    self.instruction.clock(&mut self.reg, bus);
                    Some(())
                }
                instruction::InstructionResult::Result(addr, oper) => {
                    self.prev.addr = addr;
                    self.prev.oper = oper;
                    Some(())
                }
            },
            None => {
                let bus = self.bus.as_ref().expect("Bus is set");
                let opc = bus.read(self.reg.pc) as usize;

                self.prev = InstructionState {
                    reg: self.reg,
                    tcc: self.tcc,
                    opcode: opc as u8,
                    op: INSTRUCTION_TYPE[opc],
                    ..Default::default()
                };

                self.instruction = OPER[opc]();
                None
            }
        }
    }
}
