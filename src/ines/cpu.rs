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

use super::{io::RwDevice, Word};

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

pub enum CpuState {
    OperExecuting,
    OperComplete(InstructionState),
}

#[derive(Debug)]
pub struct Cpu<CpuBus: RwDevice> {
    reg: Registers,
    bus: Option<CpuBus>,
    instruction: InstructionIterator,
    tcc: u64,
    prev: InstructionState,
}

impl<CpuBus: RwDevice> Cpu<CpuBus> {
    pub fn configure_bus(&mut self, bus: CpuBus) {
        self.bus = Some(bus);
    }

    pub fn cur_pc(&self) -> Word {
        self.reg.pc
    }

    pub fn pc(&mut self, addr: Word) {
        self.reg.pc = addr;
    }

    pub fn waiting(&self) -> bool {
        self.instruction.waiting()
    }

    #[allow(unused)]
    pub fn reset(&mut self) {
        self.prev = InstructionState {
            reg: self.reg,
            tcc: self.tcc,
            opcode: 0xFF,
            op: OperType::BRK,
            ..Default::default()
        };
        self.instruction = instruction::reset();
    }
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
    type Item = CpuState;
    fn next(&mut self) -> Option<Self::Item> {
        self.tcc = self.tcc.wrapping_add(1);

        match self.instruction.next() {
            Some(_) => {
                let bus = self.bus.as_mut().expect("Bus is set");
                match self.instruction.clock(&mut self.reg, bus) {
                    instruction::InstructionResult::Clock => Some(CpuState::OperExecuting),
                    instruction::InstructionResult::Result(addr, oper) => {
                        self.prev.addr = addr;
                        self.prev.oper = oper;
                        Some(CpuState::OperComplete(self.prev))
                    }
                }
            }
            None => {
                let bus = self.bus.as_ref().expect("Bus is set");
                let opc = bus.read(self.reg.pc) as usize;

                self.reg.p.set(Status::U, true);

                self.prev = InstructionState {
                    reg: self.reg,
                    tcc: self.tcc.wrapping_sub(1),
                    opcode: opc as u8,
                    op: INSTRUCTION_TYPE[opc],
                    ..Default::default()
                };

                self.reg.pc = self.reg.pc.wrapping_add(1);
                self.instruction = OPER[opc]();
                None
            }
        }
    }
}
