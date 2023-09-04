mod address_mode;
mod instructions;
mod opcode;
mod registers;

use self::address_mode::AddrMode;

use super::Bus6502;
use crate::gui::{DebugCpu, CURSOR, DIAGNOSTIC_FONT, DISABLED, ENABLED};
use crate::{bus::Bus, cpu::CPU};
use std::cell::RefCell;
use std::ops::Range;
use std::rc::{Rc, Weak};

use egui::RichText;
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

struct Assembly {
    lines: Vec<String>,
    mapping: Vec<usize>,
}

impl Assembly {
    pub fn new() -> Self {
        Self {
            lines: vec![],
            mapping: vec![],
        }
    }
    pub fn with_capacity(size: usize) -> Self {
        Self {
            lines: Vec::with_capacity(size / 2),
            mapping: Vec::with_capacity(size),
        }
    }

    pub fn push(&mut self, line: String, addr_range: Range<u32>) {
        let idx = self.lines.len();
        self.lines.push(line);

        for x in addr_range {
            self.mapping.insert(x as usize, idx);
        }
    }

    pub fn get(&self, addr: u16) -> Option<&str> {
        if (addr as usize) < self.mapping.len() {
            let idx = self.mapping[addr as usize];
            Some(&self.lines[idx])
        } else {
            None
        }
    }

    pub fn get_range(&self, addr: u16, dist: i8) -> Vec<&str> {
        if (addr as usize) < self.mapping.len() {
            let origin = self.mapping[addr as usize];
            let (start, end) = if dist.is_negative() {
                (origin.saturating_sub(-dist as usize), origin)
            } else {
                (origin, origin.saturating_add(dist as usize))
            };
            let vec: Vec<&str> = self
                .lines
                .iter()
                .skip(start)
                .take(end - start)
                .map(|x| x.as_str())
                // .enumerate()
                // .filter_map(|(idx, x)| {
                // 	if (start..=end).contains(&idx) {
                // 		Some(x.as_str())
                // 	}
                // 	else {
                // 		None
                // 	}
                // })
                .collect();
            vec
        } else {
            vec![]
        }
    }
}

pub struct Cpu6502 {
    pub(super) reg: Registers,
    pub(super) state: State,
    bus: Option<Weak<BusCell>>,
    asm: Assembly,
}

impl Cpu6502 {
    pub fn new() -> Self {
        Self {
            bus: None,
            reg: Registers::default(),
            state: State::default(),
            asm: Assembly::new(),
        }
    }

    pub fn connect_bus(&mut self, bus: &Rc<BusCell>) {
        self.bus = Some(Rc::downgrade(bus));
    }

    pub fn disssemble(&mut self, start: u16, end: u16) {
        let mut addr = start as u32;

        self.asm = Assembly::with_capacity((end - start) as usize);

        while addr < end as u32 {
            let ln_addr = addr as u16;

            let opcode = self.read_only(ln_addr);
            addr += 1;
            let op = &OPERATIONS[opcode as usize];

            let line = match op.am {
                AddrMode::IMP => format!("${:>04X}: {:?} {{{:?}}}", ln_addr, op.op, op.am),
                AddrMode::IMM | AddrMode::ZPG => {
                    let lo = self.read_only(addr as u16);
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} #${:>02X} {{{:?}}}",
                        ln_addr, op.op, lo, op.am
                    )
                }
                AddrMode::ZPX => {
                    let lo = self.read_only(addr as u16);
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} #${:>02X}, X {{{:?}}}",
                        ln_addr, op.op, lo, op.am
                    )
                }
                AddrMode::ZPY => {
                    let lo = self.read_only(addr as u16);
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} #${:>02X}, Y {{{:?}}}",
                        ln_addr, op.op, lo, op.am
                    )
                }
                AddrMode::IZX => {
                    let lo = self.read_only(addr as u16);
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} (${:>02X}, X) {{{:?}}}",
                        ln_addr, op.op, lo, op.am
                    )
                }
                AddrMode::IZY => {
                    let lo = self.read_only(addr as u16);
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} (${:>02X}, Y) {{{:?}}}",
                        ln_addr, op.op, lo, op.am
                    )
                }
                AddrMode::ABS => {
                    let lo = self.read_only(addr as u16) as u16;
                    addr += 1;
                    let hi = (self.read_only(addr as u16) as u16) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} ${:>04X} {{{:?}}}",
                        ln_addr, op.op, val, op.am
                    )
                }
                AddrMode::ABX => {
                    let lo = self.read_only(addr as u16) as u16;
                    addr += 1;
                    let hi = (self.read_only(addr as u16) as u16) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} ${:>04X}, X {{{:?}}}",
                        ln_addr, op.op, val, op.am
                    )
                }
                AddrMode::ABY => {
                    let lo = self.read_only(addr as u16) as u16;
                    addr += 1;
                    let hi = (self.read_only(addr as u16) as u16) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} ${:>04X}, Y {{{:?}}}",
                        ln_addr, op.op, val, op.am
                    )
                }
                AddrMode::IND => {
                    let lo = self.read_only(addr as u16) as u16;
                    addr += 1;
                    let hi = (self.read_only(addr as u16) as u16) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} (${:>04X}) {{{:?}}}",
                        ln_addr, op.op, val, op.am
                    )
                }
                AddrMode::REL => {
                    let lo = self.read_only(addr as u16) as u16;
                    addr += 1;
                    let rel = addr as u16 + lo;
                    format!(
                        "${:>04X}: {:?} ${:>02X} [${:>04X}] {{{:?}}}",
                        ln_addr, op.op, lo, rel, op.am
                    )
                }
            };

            self.asm.push(line, (ln_addr as u32)..addr);
        }
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

    fn read_only(&self, addr: u16) -> u8 {
        match &self.bus {
            Some(bus) => match bus.upgrade() {
                Some(bus) => bus.as_ref().borrow().read_only(addr),
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

impl DebugCpu for Cpu6502 {
    fn draw_cpu(&self, ui: &mut egui::Ui) {
        ui.vertical(|ui| {
            ui.horizontal(|ui| {
                let f = |f| match flag(self.reg.p, f) == 0 {
                    true => DISABLED,
                    false => ENABLED,
                };

                ui.label(RichText::new("Status: ").font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("N").color(f(SF::N)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("V").color(f(SF::V)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("U").color(f(SF::U)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("B").color(f(SF::B)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("D").color(f(SF::D)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("I").color(f(SF::I)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("Z").color(f(SF::Z)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("C").color(f(SF::C)).font(DIAGNOSTIC_FONT));
            });

            ui.vertical(|ui| {
                ui.label(
                    RichText::new(format!("PC: {:>04X} [{:>5}]", self.reg.pc, self.reg.pc))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new(format!("AC: {:>04X} [{:>5}]", self.reg.ac, self.reg.ac))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new(format!(" X: {:>04X} [{:>5}]", self.reg.x, self.reg.x))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new(format!(" Y: {:>04X} [{:>5}]", self.reg.y, self.reg.y))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new(format!("SP: {:>04X} [{:>5}]", self.reg.sp, self.reg.sp))
                        .font(DIAGNOSTIC_FONT),
                );
            });
        });
    }

    fn draw_code(&self, ui: &mut egui::Ui, instruction_count: i8) {
        ui.vertical(|ui| {
            let half = instruction_count / 2;
            let range = self.asm.get_range(self.reg.pc, -(half + 1));
            for str in range.iter().skip(1) {
                ui.label(RichText::new(*str).font(DIAGNOSTIC_FONT));
            }

            match self.asm.get(self.reg.pc) {
                Some(str) => {
                    ui.label(RichText::new(str).font(DIAGNOSTIC_FONT).color(CURSOR));
                }
                None => {}
            }

            let range = self.asm.get_range(self.reg.pc, half + 1);
            for str in range.iter().skip(1) {
                ui.label(RichText::new(*str).font(DIAGNOSTIC_FONT));
            }
        });
    }
}
