use crate::{
    bus::Bus,
    ines::{Word, Byte, HI_MASK, LO_MASK},
};

use super::registers::{CpuRegisters, Status};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum AddrModeData {
    A,
    Abs(Byte, Byte),
    Abx(Byte, Byte),
    Aby(Byte, Byte),
    Imm(Byte),
    Imp,
    Ind(Byte, Byte, Word),
    Izx(Byte, Word),
    Izy(Byte, Word),
    Rel(Byte, Word),
    Zpg(Byte),
    Zpx(Byte, Byte),
    Zpy(Byte, Byte),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum OperData {
    None,
}

pub enum InstructionResult {
    Clock,
    Result(AddrModeData, OperData),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct InstructionState {
    pub addr: Word,
    pub addr_data: AddrModeData,
    pub oper: OperData,
    pub tmp: Word,
}

impl Default for InstructionState {
    fn default() -> Self {
        Self {
            addr: 0x0000,
            addr_data: AddrModeData::A,
            oper: OperData::None,
            tmp: 0,
        }
    }
}

fn spin(_: &mut CpuRegisters, _: &mut dyn Bus, _: &mut InstructionState) -> i8 {
    0
}

type OperationHandler = fn(&mut CpuRegisters, &mut dyn Bus, &mut InstructionState) -> i8;
pub struct InstructionIterator {
    pub state: InstructionState,
    pub cc: u8,
    pub operations: [OperationHandler; 9],
    len: usize,
    cur: i8,
}

impl InstructionIterator {
    #[allow(unused)]
    pub fn new(am: &[OperationHandler], ops: &[OperationHandler]) -> Self {
        let mut operations: [OperationHandler; 9] = [spin; 9];

        for (i, op) in am.iter().enumerate() {
            operations[i] = *op;
        }

        for (i, op) in ops.iter().enumerate() {
            operations[i + am.len()] = *op;
        }

        Self {
            state: InstructionState::default(),
            cc: 0,
            operations,
            cur: 0,
            len: am.len() + ops.len(),
        }
    }

    #[allow(unused)]
    pub fn clock(&mut self, reg: &mut CpuRegisters, bus: &mut dyn Bus) {
        self.cc += 1;
        let oper = &self.operations[(self.cur - 1) as usize];
        let skip_count = oper(reg, bus, &mut self.state);
        self.cur += skip_count;
    }
}

impl Iterator for InstructionIterator {
    type Item = InstructionResult;
    fn next(&mut self) -> Option<Self::Item> {
        let len = self.len as i8;
        match self.cur {
            x if x == len => {
                self.cur += 1;
                Some(Self::Item::Result(self.state.addr_data, self.state.oper))
            }
            x if x < len => {
                self.cur += 1;
                Some(Self::Item::Clock)
            }
            _ => None,
        }
    }
}

macro_rules! count_idents {
    ($($idents:ident),* $(,)*) => {
        {
            #[allow(dead_code, non_camel_case_types)]
            enum Idents { $($idents,)* __CountIdentsLast }
            const COUNT: usize = Idents::__CountIdentsLast as usize;
            COUNT
        }
    };
}

macro_rules! steps {
    () => { };

    ($op:ident, $($n:ident($reg:ident, $bus:ident, $state:ident) $b:block)* [$($steps:ident,)*]) => {
        $(
            #[allow(unused)]
            fn $n($reg: &mut CpuRegisters, $bus: &mut dyn Bus, $state: &mut InstructionState) -> i8 {
                $b
            }
        )*

        const $op: [OperationHandler; count_idents!($($steps,)*)]  = [$($steps,)*];
    };
}

steps!(BNE,
    bne_00(reg, _bus, state) {
        let pc = reg.pc;
        state.addr = pc.wrapping_add(state.addr);
        if reg.p.get(Status::Z) == 0 { 0 } else { 2 }
    }

    bne_01(reg, bus, state) {
        let pc = reg.pc;
        reg.pc = state.addr;
        if state.addr & HI_MASK != pc & HI_MASK { 0 } else { 1 }
    }

    [bne_00, bne_01, spin,]
);

const NEG_MASK: u16 = 0x0080;

pub fn is_lo_zero(v: u16) -> bool {
    v & LO_MASK == 0
}

pub fn is_zero(v: u8) -> bool {
    v == 0
}

pub fn is_neg(v: u16) -> bool {
    v & NEG_MASK == NEG_MASK
}

macro_rules! addr_mode {
    () => {};
    (&BB) => {
        fn rel(reg: &mut CpuRegisters, bus: &mut dyn Bus, state: &mut InstructionState) -> i8 {
            let rel_addr = bus.read(reg.pc) as Word;
            reg.pc = reg.pc.wrapping_add(1);
        
            let addr = if is_neg(rel_addr) {
                rel_addr | HI_MASK
            } else {
                rel_addr
            };
        
            state.addr_data = AddrModeData::Rel(rel_addr as Byte, addr);
            state.addr = addr;
            0
        }

        const REL: [OperationHandler; 1] = [rel];
    };

    (steps &BB) => {
        REL
    }
}


macro_rules! make_instruction {
    ([$opc:tt] $op:tt $($am:tt)*) => {
        #[allow(dead_code)]
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![steps $($am)*];
            InstructionIterator::new(&addr, &$op)
        }
    };

    // (BNE $($am:tt)*) => {
    //     #[allow(unused)]
    //     fn op_d0() -> InstructionIterator {
    //         InstructionIterator::new(&addr_mode![$($am)*], &[bne_00, bne_01, spin])
    //     }
    // };
}

addr_mode!(&BB);
make_instruction![[op_d0] BNE &BB];

#[cfg(test)]
mod test {
    use std::{cell::RefCell, rc::Rc};

    use super::*;

    #[test]
    fn adc_imm() {
        let mut reg = CpuRegisters::default();
        let ram = Rc::new(RefCell::new(BoardRam::new()));
        let mut bus = RangedBoardBus::new();
        bus.push(&ram);

        reg.p = Status::U;
        reg.ac = 0x05;
        reg.pc = 0x00; // Where the next instruction will be loaded
        bus.write(0x00, 0x05); // Immediate mode value

        let mut bus = ClockBusContext::new(&mut bus);
        bus.read(0x0000); // Cycle to simulate the op code read.

        let addr_mode: Vec<OperationHandler> = vec![
            |reg: &mut CpuRegisters, _bus: &mut dyn Bus, state: &mut InstructionState| -> i8 {
                let addr: Addr = reg.pc;
                reg.pc = reg.pc.wrapping_add(1);
                state.addr_data = AddrModeData::Imm(addr as u8);
                state.addr = addr;
                0
            },
        ];

        let operations: Vec<OperationHandler> = vec![
            |reg: &mut CpuRegisters, bus: &mut dyn Bus, state: &mut InstructionState| -> i8 {
                let data = bus.read(state.addr) as u16;
                let ac = reg.ac as u16;

                let tmp = data + ac + reg.p.get(Status::C);
                reg.p
                    .set(Status::C, tmp > 255)
                    .set(Status::Z, is_lo_zero(tmp))
                    .set(Status::V, is_neg(!(ac ^ data) & ac ^ tmp))
                    .set(Status::N, is_neg(tmp));

                reg.ac = tmp as u8;
                0
            },
        ];

        let mut iter = InstructionIterator::new(&addr_mode, &operations);
        while let Some(result) = iter.next() {
            match result {
                InstructionResult::Result(_, _) => {}
                InstructionResult::Clock => {
                    iter.clock(&mut reg, &mut bus);
                }
            }
        }

        assert_eq!(reg.ac, 0x0A);
        assert_eq!(reg.p.get(Status::N), 0);
        assert_eq!(reg.p.get(Status::C), 0);
        assert_eq!(reg.p.get(Status::Z), 0);
        assert_eq!(*bus.rw_count.borrow(), 2, "rw_count");
    }

    #[test]
    fn bne_no_branch() {
        let mut reg = CpuRegisters::default();
        let ram = Rc::new(RefCell::new(BoardRam::new()));
        let mut bus = RangedBoardBus::new();
        bus.push(&ram);

        reg.p = Status::U | Status::Z;
        reg.pc = 0x00; // Where the next instruction will be loaded
        bus.write(0x00, 0x05); // Immediate mode value

        let mut bus = ClockBusContext::new(&mut bus);
        bus.read(0x0000); // Cycle to simulate the op code read.

        let mut iter = op_d0();

        while let Some(result) = iter.next() {
            match result {
                InstructionResult::Result(_, _) => {}
                InstructionResult::Clock => {
                    iter.clock(&mut reg, &mut bus);
                }
            }
        }

        assert_eq!(reg.pc, 0x01, "PC");
        assert_eq!(iter.cc, 2, "cycle count");
    }

    #[test]
    fn bne_branch_same_page() {
        let mut reg = CpuRegisters::default();
        let ram = Rc::new(RefCell::new(BoardRam::new()));
        let mut bus = RangedBoardBus::new();
        bus.push(&ram);

        reg.p = Status::U;
        reg.pc = 0x00; // Where the next instruction will be loaded
        bus.write(0x00, 0x05); // Immediate mode value

        let mut bus = ClockBusContext::new(&mut bus);
        bus.read(0x0000); // Cycle to simulate the op code read.

        let mut iter = op_d0();

        while let Some(result) = iter.next() {
            match result {
                InstructionResult::Result(_, _) => {}
                InstructionResult::Clock => {
                    iter.clock(&mut reg, &mut bus);
                }
            }
        }

        assert_eq!(reg.pc, 0x06, "PC");
        assert_eq!(iter.cc, 3, "cycle count");
    }

    #[test]
    fn bne_branch_new_page() {
        let mut reg = CpuRegisters::default();
        let ram = Rc::new(RefCell::new(BoardRam::new()));
        let mut bus = RangedBoardBus::new();
        bus.push(&ram);

        reg.p = Status::U;
        reg.pc = 0xFD; // Where the next instruction will be loaded
        bus.write(0xFD, 0x05); // Immediate mode value

        let mut bus = ClockBusContext::new(&mut bus);
        bus.read(0x0000); // Cycle to simulate the op code read.

        let mut iter = op_d0();

        while let Some(result) = iter.next() {
            match result {
                InstructionResult::Result(_, _) => {}
                InstructionResult::Clock => {
                    iter.clock(&mut reg, &mut bus);
                }
            }
        }

        assert_eq!(reg.pc, 0x00FD + 0x0001 + 0x0005, "PC");
        assert_eq!(iter.cc, 4, "cycle count");
    }
}
