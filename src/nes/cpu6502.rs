mod address_mode;
mod instructions;
mod opcode;
mod registers;

use super::Bus6502;
use crate::{bus::Bus, cpu::CPU};
use std::cell::RefCell;
use std::rc::{Rc, Weak};

use opcode::{Operation, OPERATIONS};
use registers::{status_flags as SF, Registers};

type BusCell = RefCell<Bus6502>;

fn flag(s: u8, f: u8) -> u8 {
    if s & f == f {
        1
    } else {
        0
    }
}

fn set_flag(s: u8, f: u8, v: bool) -> u8 {
    match v {
        true => s | f,
        false => s & !f,
    }
}

#[derive(Default)]
pub(super) struct State {
    // Represents the working input value to the ALU
    pub fetched: u8,
    // All used memory addresses end up in here
    pub addr_abs: u16,
    // Represents absolute address following a branch
    pub addr_rel: u16,
    // Is the instruction byte
    pub opcode: u8,
    // A global accumulation of the number of clocks
    pub clock_count: u32,
    // Current remaining cycles
    pub cc: u8,
    // The current operation value
    pub op: Operation,
}

pub struct Cpu6502 {
    pub(super) reg: Registers,
    pub(super) state: State,
    bus: Option<Weak<BusCell>>,
}

impl Cpu6502 {
    pub fn new() -> Self {
        Self {
            bus: None,
            reg: Registers::default(),
            state: State::default(),
        }
    }

    pub fn connect_bus(&mut self, bus: &Rc<BusCell>) {
        self.bus = Some(Rc::downgrade(bus));
    }
}

impl CPU for Cpu6502 {
    fn read(&self, addr: u16) -> u8 {
        match &self.bus {
            Some(bus) => match bus.upgrade() {
                Some(bus) => bus.as_ref().borrow().read(addr),
                _ => 0x00,
            },
            _ => 0x00,
        }
    }

    fn write(&self, addr: u16, data: u8) {
        match &self.bus {
            Some(bus) => match bus.upgrade() {
                Some(bus) => bus.as_ref().borrow_mut().write(addr, data),
                _ => {}
            },
            _ => {}
        }
    }

    fn clock(&mut self) {
        if self.state.cc == 0 {
            self.state.opcode = self.read(self.reg.pc);

            self.reg.p = set_flag(self.reg.p, SF::U, true);
            self.reg.pc += 1;

            self.state.op = OPERATIONS[self.state.opcode as usize].into();
            self.state.cc = self.state.cc;

            let am_additional_cc = (self.state.op.am_fn)(self);
            let op_additional_cc = (self.state.op.op_fn)(self);

            self.state.cc += am_additional_cc & op_additional_cc;

            self.reg.p = set_flag(self.reg.p, SF::U, true);
        }

        self.state.clock_count += 1;
        self.state.cc -= 1;
    }

    fn reset(&mut self) {
        let addr = 0xFFFC;
        let lo = self.read(addr + 0) as u16;
        let hi = self.read(addr + 1) as u16;

        self.reg.pc = hi << 8 | lo;

        self.reg.ac = 0;
        self.reg.x = 0;
        self.reg.y = 0;
        self.reg.sp = 0xFD;
        self.reg.p = SF::U;

        self.state.fetched = 0;
        self.state.addr_abs = 0;
        self.state.addr_rel = 0;

        self.state.cc = 8;
    }

    fn irq(&mut self) {
        if flag(self.reg.p, SF::I) == 0 {
            // Push the program counter to the stack. It's 16-bits dont
            // forget so that takes two pushes
            let mut sp = self.reg.sp as u16;
            let mut pc = self.reg.pc;
            self.write(0x0100 + sp, (pc >> 8) as u8);
            sp -= 1;
            self.write(0x0100 + sp, pc as u8);
            sp -= 1;

            // Push flags indicating that there was an interupt
            let mut p = self.reg.p;
            p = set_flag(p, SF::B, false);
            p = set_flag(p, SF::U, true);
            p = set_flag(p, SF::I, true);
            self.reg.p = p;
            self.write(0x0100 + sp, p);
            sp -= 1;

            // Load fixed program counter
            let addr = 0xFFFE;
            let lo = self.read(addr + 0) as u16;
            let hi = (self.read(addr + 1) as u16) << 8;
            pc = hi | lo;

            self.reg.sp = sp as u8;
            self.reg.pc = pc;

            self.state.cc = 7;
        }
    }

    fn nmi(&mut self) {
        // Push the program counter to the stack. It's 16-bits dont
        // forget so that takes two pushes
        let mut sp = self.reg.sp as u16;
        let mut pc = self.reg.pc;
        self.write(0x0100 + sp, (pc >> 8) as u8);
        sp -= 1;
        self.write(0x0100 + sp, pc as u8);
        sp -= 1;

        // Push flags indicating that there was an interupt
        let mut p = self.reg.p;
        p = set_flag(p, SF::B, false);
        p = set_flag(p, SF::U, true);
        p = set_flag(p, SF::I, true);
        self.reg.p = p;
        self.write(0x0100 + sp, p);
        sp -= 1;

        // Load fixed program counter
        let addr = 0xFFFA;
        let lo = self.read(addr + 0) as u16;
        let hi = (self.read(addr + 1) as u16) << 8;
        pc = hi | lo;

        self.reg.sp = sp as u8;
        self.reg.pc = pc;

        self.state.cc = 8;
    }
}
