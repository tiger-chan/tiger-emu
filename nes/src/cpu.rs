mod bus;
mod instruction;
mod registers;
mod status_reg;

use std::{
    cell::RefCell,
    rc::Rc,
    sync::mpsc::{channel, Receiver, Sender},
};

pub use bus::{Bus, CpuCtrl};
pub use registers::Registers;
pub use status_reg::Status;

pub use instruction::{AddrMode, AddrModeData, OperData, OperType, ADDR_MODE, INSTRUCTION_TYPE};

use instruction::{InstructionIterator, OPER};

use crate::{
    io::{ReadDevice, WriteDevice},
    Clocked,
};

use super::{io::RwDevice, Byte, Word};

pub type CpuRef<CpuBus> = Rc<RefCell<Cpu<CpuBus>>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Message {
    Irq,
    Nmi,
    Pc(Word),
    Reset,
}

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
    msg_rcv: Receiver<Message>,
    msg_snd: Sender<Message>,
}

impl<CpuBus: RwDevice + CpuCtrl> Cpu<CpuBus> {
    pub fn configure_bus(&mut self, bus: CpuBus) {
        self.bus = Some(bus);
    }

    pub fn cur_pc(&self) -> Word {
        self.reg.pc
    }

    pub fn pc(&mut self, addr: Word) {
        self.reg.pc = addr;
    }

    pub fn queue_pc(&mut self, addr: Word) {
        let _ = self.msg_snd.send(Message::Pc(addr));
    }

    #[allow(unused)]
    pub fn cur_state(&self) -> InstructionState {
        if self.instruction.waiting() {
            let pc = self.next_pc();

            if let Some(bus) = self.bus.as_ref() {
                let opc = bus.read(self.reg.pc) as usize;

                InstructionState {
                    reg: self.reg,
                    tcc: self.tcc,
                    opcode: opc as u8,
                    op: INSTRUCTION_TYPE[opc],
                    am: ADDR_MODE[opc],
                    ..Default::default()
                }
            } else {
                InstructionState::default()
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

    pub fn signal(&self) -> Sender<Message> {
        self.msg_snd.clone()
    }

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
        if let Some(bus) = self.bus.as_ref() {
            bus.read(addr)
        } else {
            0
        }
    }

    pub fn write(&mut self, addr: Word, data: Byte) -> Byte {
        if let Some(bus) = self.bus.as_mut() {
            bus.write(addr, data)
        } else {
            0
        }
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

    fn process_messages(&mut self) {
        while let Ok(msg) = self.msg_rcv.try_recv() {
            match msg {
                Message::Pc(pc) => {
                    self.reg.pc = pc;
                }
                Message::Irq => {
                    self.irq();
                    return;
                }
                Message::Reset => {
                    self.reset();
                    return;
                }
                Message::Nmi => {
                    self.nmi();
                    return;
                }
            }
        }
    }

    fn next_pc(&self) -> Word {
        let mut msgs = [Message::Reset; 3];
        let mut idx = 0;

        while let Ok(msg) = self.msg_rcv.try_recv() {
            msgs[idx] = msg;
            idx += 1;
        }

        let mut addr = self.reg.pc;
        for item in msgs.iter().take(idx + 1) {
            if let Message::Pc(pc) = item {
                println!("Peeked PC from queue {} to {}", self.reg.pc, pc);
                addr = *pc;
            }
            let _ = self.msg_snd.send(*item);
        }

        addr
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

        let (msg_snd, msg_rcv) = channel();
        Self {
            reg: Registers::default(),
            bus: None,
            instruction: instruction::reset(),
            tcc: 0,
            prev: state,
            msg_rcv,
            msg_snd,
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
                if let Some(bus) = self.bus.as_mut() {
                    match self.instruction.clock(&mut self.reg, bus) {
                        Clock => Some(CpuState::OperExecuting),
                        Result(addr, oper) => {
                            self.prev.addr = addr;
                            self.prev.oper = oper;
                            Some(CpuState::OperComplete(self.prev))
                        }
                    }
                } else {
                    None
                }
            }
            None => {
                self.process_messages();

                if let Some(bus) = self.bus.as_ref() {
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
                }
                None
            }
        }
    }
}

impl<CpuBus: RwDevice> RwDevice for Cpu<CpuBus> {}

impl<CpuBus: RwDevice> ReadDevice for Cpu<CpuBus> {
    fn read(&self, addr: Word) -> Byte {
        if let Some(bus) = &self.bus {
            bus.read(addr)
        } else {
            0
        }
    }
}

impl<CpuBus: RwDevice> WriteDevice for Cpu<CpuBus> {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        if let Some(bus) = &mut self.bus {
            bus.write(addr, data)
        } else {
            0
        }
    }
}
