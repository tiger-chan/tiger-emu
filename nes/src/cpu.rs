mod bus;
mod instruction;
mod registers;
mod status_reg;

use std::{cell::RefCell, rc::Rc};

pub use bus::{Bus, CpuCtrl};
pub use registers::Registers;
pub use status_reg::Status;

pub use instruction::{AddrMode, AddrModeData, OperData, OperType};

use instruction::{InstructionIterator, INSTRUCTION_TYPE};

use crate::Clocked;

use self::instruction::{ADDR_MODE, OPER};

use super::{io::RwDevice, Byte, Word};

pub type CpuRef<CpuBus> = Rc<RefCell<Cpu<CpuBus>>>;

#[derive(Debug, Clone, Copy)]
pub struct InstructionState {
    pub reg: Registers,
    pub tcc: u64,
    pub opcode: u8,
    pub addr: AddrModeData,
    pub oper: OperData,
    pub op: OperType,
    pub am: AddrMode,
}

impl Default for InstructionState {
    fn default() -> Self {
        Self {
            reg: Registers {
                sp: 0xFD,
                p: Status(0x34),
                ..Registers::default()
            },
            tcc: 0,
            opcode: 0xFF,
            addr: AddrModeData::Imp,
            oper: OperData::None,
            op: OperType::ADC,
            am: AddrMode::A,
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
    queued_pc: Option<Word>,
}

impl<CpuBus: RwDevice + CpuCtrl> Cpu<CpuBus> {
    pub fn configure_bus(&mut self, bus: CpuBus) {
        self.bus = Some(bus);
    }

    pub fn cur_pc(&self) -> Word {
        self.reg.pc
    }

    pub fn pc(&mut self, addr: Word) {
        self.queued_pc = Some(addr);
    }

    #[allow(unused)]
    pub fn cur_state(&self) -> InstructionState {
        if self.instruction.waiting() {
            let pc = if let Some(pc) = self.queued_pc {
                pc
            } else {
                self.reg.pc
            };

            let bus = self.bus.as_ref().expect("Bus is set");
            let opc = bus.read(pc) as usize;

            InstructionState {
                reg: self.reg,
                tcc: self.tcc,
                opcode: opc as u8,
                op: INSTRUCTION_TYPE[opc],
                am: ADDR_MODE[opc],
                ..Default::default()
            }
        } else {
            InstructionState {
                reg: self.reg,
                tcc: self.tcc,
                opcode: self.prev.opcode,
                addr: self.prev.addr,
                oper: self.prev.oper,
                op: self.prev.op,
                am: self.prev.am,
            }
        }
    }

    pub fn waiting(&self) -> bool {
        self.instruction.waiting()
    }

    #[allow(unused)]
    pub fn reset(&mut self) {
        if let Some(bus) = self.bus.as_mut() {
            bus.reset();
        }
        self.prev = InstructionState {
            reg: self.reg,
            tcc: self.tcc,
            opcode: 0xFF,
            op: OperType::BRK,
            ..Default::default()
        };
        self.instruction = instruction::reset();
    }

    #[allow(unused)]
    pub fn irq(&mut self) {
        self.prev = InstructionState {
            reg: self.reg,
            tcc: self.tcc,
            opcode: 0xFF,
            op: OperType::BRK,
            ..Default::default()
        };
        self.instruction = instruction::irq();
    }

    #[allow(unused)]
    pub fn nmi(&mut self) {
        self.prev = InstructionState {
            reg: self.reg,
            tcc: self.tcc,
            opcode: 0xFF,
            op: OperType::BRK,
            ..Default::default()
        };
        self.instruction = instruction::nmi();
    }

    pub fn read(&self, addr: Word) -> Byte {
        let bus = self.bus.as_ref().expect("Bus is set");
        bus.read(addr)
    }

    pub fn write(&mut self, addr: Word, data: Byte) -> Byte {
        let bus = self.bus.as_mut().expect("Bus is set");
        bus.write(addr, data)
    }

    pub fn set_reg(&mut self, reg: Registers) {
        self.reg = reg;
    }

    #[allow(unused)]
    pub fn run_pc(&mut self, addr: Word) -> CpuState {
        let mut state;
        if self.cur_pc() != addr {
            while !self.waiting() {
                self.clock();
            }
            self.pc(addr);
        }

        state = self.clock();
        while !self.waiting() {
            state = self.clock();
        }

        state.unwrap()
    }

    #[allow(unused)]
    pub fn run_until(&mut self, addr: Word) -> CpuState {
        let mut pc = self.cur_pc();
        let mut state = Some(CpuState::OperComplete(self.prev));
        while pc != addr {
            state = self.clock();
            pc = self.cur_pc();
        }

        state.unwrap()
    }

    pub fn complete_operation(&mut self) -> CpuState {
        let mut state = Some(CpuState::OperComplete(self.prev));
        while !self.waiting() {
            state = self.clock();
        }

        state.unwrap()
    }
}

impl<CpuBus: RwDevice + CpuCtrl> Default for Cpu<CpuBus> {
    fn default() -> Self {
        let state = InstructionState {
            reg: Registers::default(),
            tcc: 0,
            opcode: 0xFF,
            op: OperType::BRK,
            ..Default::default()
        };

        Self {
            reg: Registers::default(),
            bus: None,
            instruction: instruction::reset(),
            tcc: 0,
            prev: state,
            queued_pc: None,
        }
    }
}

impl<CpuBus: RwDevice + CpuCtrl> Clocked for Cpu<CpuBus> {
    type Item = CpuState;
    fn clock(&mut self) -> Option<Self::Item> {
        self.tcc = self.tcc.wrapping_add(1);

        match self.instruction.next() {
            Some(_) => {
                use instruction::InstructionResult::*;
                let bus = self.bus.as_mut().expect("Bus is set");
                match self.instruction.clock(&mut self.reg, bus) {
                    Clock => Some(CpuState::OperExecuting),
                    Result(addr, oper) => {
                        self.prev.addr = addr;
                        self.prev.oper = oper;
                        Some(CpuState::OperComplete(self.prev))
                    }
                }
            }
            None => {
                if let Some(pc) = self.queued_pc {
                    self.queued_pc = None;
                    self.reg.pc = pc;
                };

                let bus = self.bus.as_ref().expect("Bus is set");
                let opc = bus.read(self.reg.pc) as usize;

                self.reg.p.set(Status::U, true);

                self.prev = InstructionState {
                    reg: self.reg,
                    tcc: self.tcc.wrapping_sub(1),
                    opcode: opc as u8,
                    op: INSTRUCTION_TYPE[opc],
                    am: ADDR_MODE[opc],
                    ..Default::default()
                };

                self.reg.pc = self.reg.pc.wrapping_add(1);
                self.instruction = OPER[opc]();
                None
            }
        }
    }
}
