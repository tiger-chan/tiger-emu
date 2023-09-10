mod address_mode;
mod instructions;
mod opcode;
mod registers;

use self::address_mode::AddrMode;

use super::board::{PS, RES_LO, RES_HI, IRQ_LO, IRQ_HI, NMI_LO, NMI_HI};
use crate::gui::{CpuDisplay, CURSOR, DIAGNOSTIC_FONT, DISABLED, ENABLED};
use crate::nes::cpu6502::registers::StatusReg;
use crate::{bus::Bus, cpu::CPU};
use std::ops::Range;

use egui::RichText;
use log::debug;
use opcode::{Operation, OPERATIONS, ADDER_MODE, INSTRUCTION_TYPE, OPER};
use registers::Registers;

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
    asm: Assembly,
}

impl Cpu6502 {
    pub fn new() -> Self {
        Self {
            reg: Registers::default(),
            state: State::default(),
            asm: Assembly::new(),
        }
    }

    pub fn disssemble(&mut self, bus: &dyn Bus, start: u16, end: u16) {
        let mut addr = start as u32;

        self.asm = Assembly::with_capacity((end - start) as usize);

        while addr < end as u32 {
            let ln_addr = addr as u16;

            let opcode = bus.read_only(ln_addr) as usize;
            addr += 1;
            let op = INSTRUCTION_TYPE[opcode];
			let am = ADDER_MODE[opcode];

            let line = match am {
                AddrMode::A | AddrMode::IMP => format!("${:>04X}: {:?} {{{:?}}}", ln_addr, op, am),
                AddrMode::IMM | AddrMode::ZPG => {
                    let lo = bus.read_only(addr as u16);
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} #${:>02X} {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                AddrMode::ZPX => {
                    let lo = bus.read_only(addr as u16);
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} #${:>02X}, X {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                AddrMode::ZPY => {
                    let lo = bus.read_only(addr as u16);
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} #${:>02X}, Y {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                AddrMode::IZX => {
                    let lo = bus.read_only(addr as u16);
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} (${:>02X}, X) {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                AddrMode::IZY => {
                    let lo = bus.read_only(addr as u16);
                    addr += 1;
                    format!(
                        "${:>04X}: {:?} (${:>02X}, Y) {{{:?}}}",
                        ln_addr, op, lo, am
                    )
                }
                AddrMode::ABS => {
                    let lo = bus.read_only(addr as u16) as u16;
                    addr += 1;
                    let hi = (bus.read_only(addr as u16) as u16) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} ${:>04X} {{{:?}}}",
                        ln_addr, op, val, am
                    )
                }
                AddrMode::ABX => {
                    let lo = bus.read_only(addr as u16) as u16;
                    addr += 1;
                    let hi = (bus.read_only(addr as u16) as u16) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} ${:>04X}, X {{{:?}}}",
                        ln_addr, op, val, am
                    )
                }
                AddrMode::ABY => {
                    let lo = bus.read_only(addr as u16) as u16;
                    addr += 1;
                    let hi = (bus.read_only(addr as u16) as u16) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} ${:>04X}, Y {{{:?}}}",
                        ln_addr, op, val, am
                    )
                }
                AddrMode::IND => {
                    let lo = bus.read_only(addr as u16) as u16;
                    addr += 1;
                    let hi = (bus.read_only(addr as u16) as u16) << 8;
                    addr += 1;
                    let val = hi | lo;
                    format!(
                        "${:>04X}: {:?} (${:>04X}) {{{:?}}}",
                        ln_addr, op, val, am
                    )
                }
                AddrMode::REL => {
                    let lo = bus.read_only(addr as u16) as u16;
                    addr += 1;
                    let rel = addr as u16 + lo;
                    format!(
                        "${:>04X}: {:?} ${:>02X} [${:>04X}] {{{:?}}}",
                        ln_addr, op, lo, rel, am
                    )
                }
            };

            self.asm.push(line, (ln_addr as u32)..addr);
        }
    }
}

impl CPU for Cpu6502 {
    fn clock(&mut self, bus: &mut dyn Bus) {
		log::trace!("clock");
        if self.state.cc == 0 {
			let pc = self.reg.pc;
            self.state.opcode = bus.read(self.reg.pc);

            self.reg.p.set(StatusReg::U, true);
            self.reg.pc += 1;

            self.state.op = OPERATIONS[self.state.opcode as usize].into();
            self.state.cc = self.state.op.cc;

            let am_additional_cc = (self.state.op.am_fn)(self, bus);
            let op_additional_cc = (self.state.op.op_fn)(self, bus);

            self.state.cc += am_additional_cc & op_additional_cc;

			self.reg.p.set(StatusReg::U, true);

			{
				let f = |f, i, e| {
					match self.reg.p.get(f) == 1 {
						true => i,
						false => e
					}
				};

				debug!("{:>010}:00 {} ({}) PC:{:>04X} XXX AC:{:>02X} X:{:>02X} Y:{:>02X} {:?}{:?}{:?}{:?}{:?}{:?}{:?}{:?} SP:{:>02X}",
				self.state.clock_count, self.state.op.op, self.state.op.am, pc, self.reg.ac, self.reg.x, self.reg.y,
				f(StatusReg::N, 'N', '.'), f(StatusReg::V, 'V', '.'),	f(StatusReg::U, '-', '.'),	
				f(StatusReg::B, 'B', '.'), f(StatusReg::D, 'D', '.'),	f(StatusReg::I, 'I', '.'),	
				f(StatusReg::Z, 'Z', '.'),	f(StatusReg::C, 'C', '.'), self.reg.sp);
			}
        }

        self.state.clock_count += 1;
        self.state.cc -= 1;
    }

	/// Start/Reset Operations
	/// An active-low reset line allows to hold the processor in a known disabled
	/// state, while the system is initialized. As the reset line goes high, the
	/// processor performs a start sequence of 7 cycles, at the end of which the
	/// program counter (PC) is read from the address provided in the 16-bit reset
	/// vector at $FFFC (LB-HB). Then, at the eighth cycle, the processor transfers
	/// control by performing a JMP to the provided address.
	///
	/// Any other initializations are left to the thus executed program. (Notably,
	/// instructions exist for the initialization and loading of all registers, but
	/// for the program counter, which is provided by the reset vector at $FFFC.)
    fn reset(&mut self, bus: &mut dyn Bus) {
        let lo = bus.read(RES_LO) as u16;
        let hi = bus.read(RES_HI) as u16;

        self.reg.pc = hi << 8 | lo;

        self.reg.ac = 0;
        self.reg.x = 0;
        self.reg.y = 0;
        self.reg.sp = 0xFD;
        self.reg.p = StatusReg::U;

        self.state.fetched = 0;
        self.state.addr_abs = 0;
        self.state.addr_rel = 0;

        self.state.cc = 8;
    }

	/// A hardware interrupt (maskable IRQ and non-maskable NMI), will cause
	/// the processor to put first the address currently in the program
	/// counter onto the stack (in HB-LB order), followed by the value of
	/// the status register. (The stack will now contain, seen from the
	/// bottom or from the most recently added byte, SR PC-L PC-H with the
	/// stack pointer pointing to the address below the stored contents of
	/// status register.) Then, the processor will divert its control flow
	/// to the address provided in the two word-size interrupt vectors at
	/// $FFFA (IRQ) and $FFFE (NMI).
	///
	/// A set interrupt disable flag will inhibit the execution of an IRQ,
	/// but not of a NMI, which will be executed anyways.
	///
	/// The break instruction (BRK) behaves like a NMI, but will push the
	/// value of PC+2 onto the stack to be used as the return address. Also,
	/// as with any software initiated transfer of the status register to
	/// the stack, the break flag will be found set on the respective value
	/// pushed onto the stack. Then, control is transferred to the address
	/// in the NMI-vector at $FFFE.
	///
	/// In any way, the interrupt disable flag is set to inhibit any further
	/// IRQ as control is transferred to the interrupt handler specified by
	/// the respective interrupt vector.
    fn irq(&mut self, bus: &mut dyn Bus) {
        if self.reg.p.get(StatusReg::I) == 0 {
            // Push the program counter to the stack. It's 16-bits dont
            // forget so that takes two pushes
            let mut sp = self.reg.sp as u16;
            let mut pc = self.reg.pc;
            bus.write(PS + sp, (pc >> 8) as u8);
            sp -= 1;
            bus.write(PS + sp, pc as u8);
            sp -= 1;

            // Push flags indicating that there was an interupt
            let mut p = self.reg.p;
            p.set(StatusReg::B, false)
            .set(StatusReg::U, true)
            .set(StatusReg::I, true);
            self.reg.p = p;
            bus.write(PS + sp, p.into());
            sp -= 1;

            // Load fixed program counter
            let lo = bus.read(IRQ_LO) as u16;
            let hi = (bus.read(IRQ_HI) as u16) << 8;
            pc = hi | lo;

            self.reg.sp = sp as u8;
            self.reg.pc = pc;

            self.state.cc = 7;
        }
    }

	/// A hardware interrupt (maskable IRQ and non-maskable NMI), will cause
	/// the processor to put first the address currently in the program
	/// counter onto the stack (in HB-LB order), followed by the value of
	/// the status register. (The stack will now contain, seen from the
	/// bottom or from the most recently added byte, SR PC-L PC-H with the
	/// stack pointer pointing to the address below the stored contents of
	/// status register.) Then, the processor will divert its control flow
	/// to the address provided in the two word-size interrupt vectors at
	/// $FFFA (IRQ) and $FFFE (NMI).
	///
	/// A set interrupt disable flag will inhibit the execution of an IRQ,
	/// but not of a NMI, which will be executed anyways.
	///
	/// The break instruction (BRK) behaves like a NMI, but will push the
	/// value of PC+2 onto the stack to be used as the return address. Also,
	/// as with any software initiated transfer of the status register to
	/// the stack, the break flag will be found set on the respective value
	/// pushed onto the stack. Then, control is transferred to the address
	/// in the NMI-vector at $FFFE.
	///
	/// In any way, the interrupt disable flag is set to inhibit any further
	/// IRQ as control is transferred to the interrupt handler specified by
	/// the respective interrupt vector.
    fn nmi(&mut self, bus: &mut dyn Bus) {
        // Push the program counter to the stack. It's 16-bits dont
        // forget so that takes two pushes
        let mut sp = self.reg.sp as u16;
        let mut pc = self.reg.pc;
        bus.write(PS + sp, (pc >> 8) as u8);
        sp -= 1;
        bus.write(PS + sp, pc as u8);
        sp -= 1;

        // Push flags indicating that there was an interupt
        let mut p = self.reg.p;
        p.set(StatusReg::B, false)
        .set(StatusReg::U, true)
        .set(StatusReg::I, true);
        self.reg.p = p;
        bus.write(PS + sp, p.into());
        sp -= 1;

        // Load fixed program counter
        let lo = bus.read(NMI_LO) as u16;
        let hi = (bus.read(NMI_HI) as u16) << 8;
        pc = hi | lo;

        self.reg.sp = sp as u8;
        self.reg.pc = pc;

        self.state.cc = 8;
    }
}

impl CpuDisplay for Cpu6502 {
    fn draw_cpu(&self, ui: &mut egui::Ui) {
        ui.vertical(|ui| {
            ui.horizontal(|ui| {
                let f = |f| match self.reg.p.get(f) == 0 {
                    true => DISABLED,
                    false => ENABLED,
                };

                ui.label(RichText::new("Status: ").font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("N").color(f(StatusReg::N)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("V").color(f(StatusReg::V)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("U").color(f(StatusReg::U)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("B").color(f(StatusReg::B)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("D").color(f(StatusReg::D)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("I").color(f(StatusReg::I)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("Z").color(f(StatusReg::Z)).font(DIAGNOSTIC_FONT));
                ui.label(RichText::new("C").color(f(StatusReg::C)).font(DIAGNOSTIC_FONT));
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

	fn step(&mut self) {
	}
}
