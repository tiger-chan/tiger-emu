// https://www.masswerk.at/6502/6502_instruction_set.html

use core::fmt;

use crate::nes::board::{HI_MASK, LO_MASK, PS};

use super::{address_mode::AddrMode, registers::StatusReg, Cpu6502, CPU};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instruction {
    /// add with carry
    ADC,
    /// and (with accumulator)
    AND,
    /// arithmetic shift left
    ASL,
    /// branch on carry clear
    BCC,
    /// branch on carry set
    BCS,
    /// branch on equal (zero set)
    BEQ,
    /// bit test
    BIT,
    /// branch on minus (negative set)
    BMI,
    /// branch on not equal (zero clear)
    BNE,
    /// branch on plus (negative clear)
    BPL,
    /// break / interrupt
    BRK,
    /// branch on overflow clear
    BVC,
    /// branch on overflow set
    BVS,
    /// clear carry
    CLC,
    /// clear decimal
    CLD,
    /// clear interrupt disable
    CLI,
    /// clear overflow
    CLV,
    /// compare (with accumulator)
    CMP,
    /// compare with X
    CPX,
    /// compare with Y
    CPY,
    /// decrement
    DEC,
    /// decrement X
    DEX,
    /// decrement Y
    DEY,
    /// exclusive or (with accumulator)
    EOR,
    /// increment
    INC,
    /// increment X
    INX,
    /// increment Y
    INY,
    /// jump
    JMP,
    /// jump subroutine
    JSR,
    /// load accumulator
    LDA,
    /// load X
    LDX,
    /// load Y
    LDY,
    /// logical shift right
    LSR,
    /// no operation
    NOP,
    /// or with accumulator
    ORA,
    /// push accumulator
    PHA,
    /// push processor status (SR)
    PHP,
    /// pull accumulator
    PLA,
    /// pull processor status (SR)
    PLP,
    /// rotate left
    ROL,
    /// rotate right
    ROR,
    /// return from interrupt
    RTI,
    /// return from subroutine
    RTS,
    /// subtract with carry
    SBC,
    /// set carry
    SEC,
    /// set decimal
    SED,
    /// set interrupt disable
    SEI,
    /// store accumulator
    STA,
    /// store X
    STX,
    /// store Y
    STY,
    /// transfer accumulator to X
    TAX,
    /// transfer accumulator to Y
    TAY,
    /// transfer stack pointer to X
    TSX,
    /// transfer X to accumulator
    TXA,
    /// transfer X to stack pointer
    TXS,
    /// transfer Y to accumulator
    TYA,

    /// ILLEGAL
    XXX,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type OperFn = fn(cpu: &mut Cpu6502) -> u8;

fn fetch(cpu: &mut Cpu6502) -> u8 {
    if cpu.state.op.am != AddrMode::IMP {
        cpu.state.fetched = cpu.read(cpu.state.addr_abs);
    }
    cpu.state.fetched
}

const NEG_MASK: u16 = 0x0080;

fn is_lo_zero(v: u16) -> bool {
    v & LO_MASK == 0
}

fn is_zero(v: u8) -> bool {
    v == 0
}

fn is_neg(v: u16) -> bool {
    v & NEG_MASK == NEG_MASK
}

pub(super) fn adc(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu) as u16;
    let mut p = cpu.reg.p;
    let ac = cpu.reg.ac as u16;

    // Add is performed in 16-bit domain for emulation to capture any
    // carry bit, which will exist in bit 8 of the 16-bit word
    let tmp = ac + fetched + p.get(StatusReg::C);
    p.set(StatusReg::C, tmp > 255)
        .set(StatusReg::Z, is_lo_zero(tmp))
        .set(StatusReg::V, is_neg(!(ac ^ fetched as u16) & ac ^ tmp))
        .set(StatusReg::N, is_neg(tmp));

    cpu.reg.p = p;
    cpu.reg.ac = (tmp & LO_MASK) as u8;

    1
}

pub(super) fn and(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu);
    let tmp = cpu.reg.ac & fetched;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(tmp))
        .set(StatusReg::N, is_neg(tmp as u16));
    cpu.reg.p = p;
    cpu.reg.ac = tmp;

    1
}

pub(super) fn asl(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu) as u16;
    let tmp = fetched << 1;
    let mut p = cpu.reg.p;
    p.set(StatusReg::C, tmp & HI_MASK != 0)
        .set(StatusReg::Z, is_lo_zero(tmp))
        .set(StatusReg::N, is_neg(tmp));
    cpu.reg.p = p;

    let tmp = (tmp & LO_MASK) as u8;
    if cpu.state.op.am == AddrMode::IMP {
        cpu.reg.ac = tmp;
    } else {
        cpu.write(cpu.state.addr_abs, tmp)
    }

    0
}

pub(super) fn bcc(cpu: &mut Cpu6502) -> u8 {
    if cpu.reg.p.get(StatusReg::C) == 0 {
        cpu.state.cc += 1;

        let pc = cpu.reg.pc;
        let addr_abs = pc + cpu.state.addr_rel;
        cpu.state.addr_abs = addr_abs;

        if (addr_abs & HI_MASK) != (pc & HI_MASK) {
            cpu.state.cc += 1;
        }

        cpu.reg.pc = addr_abs;
    }

    0
}

pub(super) fn bcs(cpu: &mut Cpu6502) -> u8 {
    if cpu.reg.p.get(StatusReg::C) == 1 {
        cpu.state.cc += 1;

        let pc = cpu.reg.pc;
        let addr_abs = pc + cpu.state.addr_rel;
        cpu.state.addr_abs = addr_abs;

        if (addr_abs & HI_MASK) != (pc & HI_MASK) {
            cpu.state.cc += 1;
        }

        cpu.reg.pc = addr_abs;
    }

    0
}

pub(super) fn beq(cpu: &mut Cpu6502) -> u8 {
    if cpu.reg.p.get(StatusReg::Z) == 1 {
        cpu.state.cc += 1;

        let pc = cpu.reg.pc;
        let addr_abs = pc + cpu.state.addr_rel;
        cpu.state.addr_abs = addr_abs;

        if (addr_abs & HI_MASK) != (pc & HI_MASK) {
            cpu.state.cc += 1;
        }

        cpu.reg.pc = addr_abs;
    }

    0
}

pub(super) fn bit(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu);
    let tmp = cpu.reg.ac & fetched;

    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(tmp))
        .set(StatusReg::N, is_neg(fetched as u16))
        .set(StatusReg::V, (fetched & 0x40) > 0);
    cpu.reg.p = p;

    0
}

pub(super) fn bmi(cpu: &mut Cpu6502) -> u8 {
    if cpu.reg.p.get(StatusReg::N) == 1 {
        cpu.state.cc += 1;

        let pc = cpu.reg.pc;
        let addr_abs = pc + cpu.state.addr_rel;
        cpu.state.addr_abs = addr_abs;

        if (addr_abs & HI_MASK) != (pc & HI_MASK) {
            cpu.state.cc += 1;
        }

        cpu.reg.pc = addr_abs;
    }

    0
}

pub(super) fn bne(cpu: &mut Cpu6502) -> u8 {
    if cpu.reg.p.get(StatusReg::Z) == 0 {
        cpu.state.cc += 1;

        let pc = cpu.reg.pc;
        let addr_abs = pc.wrapping_add(cpu.state.addr_rel);
        cpu.state.addr_abs = addr_abs;

        if (addr_abs & HI_MASK) != (pc & HI_MASK) {
            cpu.state.cc += 1;
        }

        cpu.reg.pc = addr_abs;
    }

    0
}

pub(super) fn bpl(cpu: &mut Cpu6502) -> u8 {
    if cpu.reg.p.get(StatusReg::N) == 0 {
        cpu.state.cc += 1;

        let pc = cpu.reg.pc;
        let addr_abs = pc + cpu.state.addr_rel;
        cpu.state.addr_abs = addr_abs;

        if (addr_abs & HI_MASK) != (pc & HI_MASK) {
            cpu.state.cc += 1;
        }

        cpu.reg.pc = addr_abs;
    }

    0
}

pub(super) fn brk(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.pc += 1;

    let mut p = cpu.reg.p;
    p.set(StatusReg::I, true);

    let mut sp = cpu.reg.sp as u16;
    let pc = cpu.reg.pc;
    cpu.write(PS + sp, (pc >> 8) as u8);
    sp -= 1;

    cpu.write(PS + sp, pc as u8);
    sp -= 1;

    p.set(StatusReg::B, true);
    cpu.write(PS + sp as u16, p.into());
    p.set(StatusReg::B, false);

    cpu.reg.p = p;
    cpu.reg.sp = sp as u8;

    cpu.reg.pc = cpu.read(0xFFFE) as u16 | ((cpu.read(0xFFFF) as u16) << 8);

    0
}

pub(super) fn bvc(cpu: &mut Cpu6502) -> u8 {
    if cpu.reg.p.get(StatusReg::V) == 0 {
        cpu.state.cc += 1;

        let pc = cpu.reg.pc;
        let addr_abs = pc + cpu.state.addr_rel;
        cpu.state.addr_abs = addr_abs;

        if (addr_abs & HI_MASK) != (pc & HI_MASK) {
            cpu.state.cc += 1;
        }

        cpu.reg.pc = addr_abs;
    }

    0
}

pub(super) fn bvs(cpu: &mut Cpu6502) -> u8 {
    if cpu.reg.p.get(StatusReg::V) == 1 {
        cpu.state.cc += 1;

        let pc = cpu.reg.pc;
        let addr_abs = pc + cpu.state.addr_rel;
        cpu.state.addr_abs = addr_abs;

        if (addr_abs & HI_MASK) != (pc & HI_MASK) {
            cpu.state.cc += 1;
        }

        cpu.reg.pc = addr_abs;
    }

    0
}

pub(super) fn clc(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.p.set(StatusReg::C, false);
    0
}

pub(super) fn cld(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.p.set(StatusReg::D, false);
    0
}

pub(super) fn cli(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.p.set(StatusReg::I, false);
    0
}

pub(super) fn clv(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.p.set(StatusReg::V, false);
    0
}

pub(super) fn cmp(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu);
    let ac = cpu.reg.ac;
    let tmp = ac as u16 - fetched as u16;

    let mut p = cpu.reg.p;
    p.set(StatusReg::C, ac >= fetched)
        .set(StatusReg::Z, is_lo_zero(tmp))
        .set(StatusReg::N, is_neg(tmp));
    cpu.reg.p = p;

    1
}

pub(super) fn cpx(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu);
    let x = cpu.reg.x;
    let tmp = x as u16 - fetched as u16;

    let mut p = cpu.reg.p;
    p.set(StatusReg::C, x >= fetched)
        .set(StatusReg::Z, is_lo_zero(tmp))
        .set(StatusReg::N, is_neg(tmp));
    cpu.reg.p = p;

    0
}

pub(super) fn cpy(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu);
    let y = cpu.reg.y;
    let tmp = y as u16 - fetched as u16;

    let mut p = cpu.reg.p;
    p.set(StatusReg::C, y >= fetched)
        .set(StatusReg::Z, is_lo_zero(tmp))
        .set(StatusReg::N, is_neg(tmp));
    cpu.reg.p = p;

    0
}

pub(super) fn dec(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu) as u16;
    let tmp = fetched - 1;
    cpu.write(cpu.state.addr_abs, (tmp & LO_MASK) as u8);

    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_lo_zero(tmp))
        .set(StatusReg::N, is_neg(tmp));
    cpu.reg.p = p;

    0
}

pub(super) fn dex(cpu: &mut Cpu6502) -> u8 {
    let x = cpu.reg.x - 1;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(x))
        .set(StatusReg::N, is_neg(x as u16));
    cpu.reg.p = p;
    cpu.reg.x = x;

    0
}

pub(super) fn dey(cpu: &mut Cpu6502) -> u8 {
    let y = cpu.reg.y - 1;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(y))
        .set(StatusReg::N, is_neg(y as u16));
    cpu.reg.p = p;
    cpu.reg.y = y;

    0
}

pub(super) fn eor(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu) as u16;
    let tmp = cpu.reg.ac as u16 ^ fetched;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_lo_zero(tmp))
        .set(StatusReg::N, is_neg(tmp));
    cpu.reg.p = p;
    cpu.reg.ac = tmp as u8;

    1
}

pub(super) fn inc(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu) as u16;
    let tmp = fetched + 1;
    cpu.write(cpu.state.addr_abs, (tmp & LO_MASK) as u8);

    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_lo_zero(tmp));
    p.set(StatusReg::N, is_neg(tmp));
    cpu.reg.p = p;

    0
}

pub(super) fn inx(cpu: &mut Cpu6502) -> u8 {
    let x = cpu.reg.x + 1;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(x))
        .set(StatusReg::N, is_neg(x as u16));
    cpu.reg.p = p;
    cpu.reg.x = x;

    0
}

pub(super) fn iny(cpu: &mut Cpu6502) -> u8 {
    let y = cpu.reg.y + 1;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(y))
        .set(StatusReg::N, is_neg(y as u16));
    cpu.reg.p = p;
    cpu.reg.x = y;

    0
}

pub(super) fn jmp(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.pc = cpu.state.addr_abs;
    0
}

pub(super) fn jsr(cpu: &mut Cpu6502) -> u8 {
    let pc = cpu.reg.pc - 1;
    let mut sp = cpu.reg.sp as u16;

    cpu.write(PS + sp, (pc >> 8) as u8);
    sp -= 1;
    cpu.write(PS + sp, (pc & LO_MASK) as u8);
    sp -= 1;

    cpu.reg.sp = sp as u8;
    cpu.reg.pc = cpu.state.addr_abs;
    0
}

pub(super) fn lda(cpu: &mut Cpu6502) -> u8 {
    let ac = fetch(cpu);
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(ac))
        .set(StatusReg::N, is_neg(ac as u16));
    cpu.reg.ac = ac;
    cpu.reg.p = p;
    1
}

pub(super) fn ldx(cpu: &mut Cpu6502) -> u8 {
    let x = fetch(cpu);
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(x))
        .set(StatusReg::N, is_neg(x as u16));
    cpu.reg.x = x;
    cpu.reg.p = p;
    1
}

pub(super) fn ldy(cpu: &mut Cpu6502) -> u8 {
    let y = fetch(cpu);
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(y))
        .set(StatusReg::N, is_neg(y as u16));
    cpu.reg.y = y;
    cpu.reg.p = p;
    1
}

pub(super) fn lsr(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu);
    let tmp = fetched >> 1;
    let mut p = cpu.reg.p;
    p.set(StatusReg::C, fetched & 0x0001 == 0x0001)
        .set(StatusReg::Z, is_zero(tmp))
        .set(StatusReg::N, is_neg(tmp as u16));

    cpu.reg.p = p;

    if cpu.state.op.am == AddrMode::IMP {
        cpu.reg.ac = tmp;
    } else {
        cpu.write(cpu.state.addr_abs, tmp);
    }
    0
}

pub(super) fn nop(cpu: &mut Cpu6502) -> u8 {
    match cpu.state.opcode {
        // Not all NOPs are equal
        // based on https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
        // and will add more based on game compatibility,
        // I'd like to cover all illegal opcodes too
        0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => 1,
        _ => 0,
    }
}

pub(super) fn ora(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu);
    let ac = cpu.reg.ac | fetched;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(ac))
        .set(StatusReg::N, is_neg(ac as u16));
    cpu.reg.ac = ac;
    cpu.reg.p = p;
    1
}

pub(super) fn pha(cpu: &mut Cpu6502) -> u8 {
    let sp = cpu.reg.sp;
    cpu.write(PS + sp as u16, cpu.reg.ac);
    cpu.reg.sp = sp - 1;
    0
}

pub(super) fn php(cpu: &mut Cpu6502) -> u8 {
    let sp = cpu.reg.sp;
    let mut p = cpu.reg.p;
    cpu.write(PS + sp as u16, (p | StatusReg::B | StatusReg::U).into());
    p.set(StatusReg::B, false).set(StatusReg::U, false);
    cpu.reg.p = p;
    cpu.reg.sp = sp - 1;
    0
}

pub(super) fn pla(cpu: &mut Cpu6502) -> u8 {
    let sp = (cpu.reg.sp + 1) as u16;
    let ac = cpu.read(PS + sp);
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(ac))
        .set(StatusReg::N, is_neg(ac as u16));
    cpu.reg.p = p;
    cpu.reg.sp = sp as u8;
    cpu.reg.ac = ac;
    0
}

pub(super) fn plp(cpu: &mut Cpu6502) -> u8 {
    let sp = (cpu.reg.sp + 1) as u16;
    let mut p = StatusReg::from(cpu.read(PS + sp));
    p.set(StatusReg::U, true);
    cpu.reg.sp = sp as u8;
    cpu.reg.p = p;
    0
}

pub(super) fn rol(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu) as u16;
    let mut p = cpu.reg.p;
    let tmp = u16::from(cpu.reg.p.get(StatusReg::C)) << 7 | (fetched >> 1);
    p.set(StatusReg::C, tmp & 0x01 == 0x01)
        .set(StatusReg::Z, is_lo_zero(tmp))
        .set(StatusReg::N, is_neg(tmp));

    cpu.reg.p = p;

    if cpu.state.op.am == AddrMode::IMP {
        cpu.reg.ac = tmp as u8;
    } else {
        cpu.write(cpu.state.addr_abs, tmp as u8);
    }

    0
}

pub(super) fn ror(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu) as u16;
    let mut p = cpu.reg.p;
    let tmp = (fetched << 1) | u16::from(cpu.reg.p.get(StatusReg::C));
    p.set(StatusReg::C, tmp & HI_MASK > 0)
        .set(StatusReg::Z, is_lo_zero(tmp))
        .set(StatusReg::N, is_neg(tmp));
    cpu.reg.p = p;

    if cpu.state.op.am == AddrMode::IMP {
        cpu.reg.ac = tmp as u8;
    } else {
        cpu.write(cpu.state.addr_abs, tmp as u8);
    }

    0
}

pub(super) fn rti(cpu: &mut Cpu6502) -> u8 {
    let mut sp = cpu.reg.sp + 1;
    let mut p = StatusReg::from(cpu.read(PS + sp as u16));
    p &= !StatusReg::B;
    p &= !StatusReg::U;
    cpu.reg.p = p;

    sp += 1;
    let mut pc = cpu.read(PS + sp as u16) as u16;
    sp += 1;
    pc = pc | (cpu.read(PS + sp as u16) as u16) << 8;

    cpu.reg.sp = sp;
    cpu.reg.pc = pc;

    0
}

pub(super) fn rts(cpu: &mut Cpu6502) -> u8 {
    let mut sp = cpu.reg.sp + 1;
    let mut pc = cpu.read(PS + sp as u16) as u16;
    sp += 1;
    pc = pc | (cpu.read(PS + sp as u16) as u16) << 8;

    cpu.reg.sp = sp;
    cpu.reg.pc = pc;

    0
}

pub(super) fn sec(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.p.set(StatusReg::C, true);
    0
}

pub(super) fn sed(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.p.set(StatusReg::D, true);
    0
}

pub(super) fn sei(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.p.set(StatusReg::I, true);
    0
}

pub(super) fn sbc(cpu: &mut Cpu6502) -> u8 {
    let fetched = fetch(cpu) as u16;
    // Operating in 16-bit domain to capture carry out

    let mut p = cpu.reg.p;
    let val = fetched ^ LO_MASK;
    let ac = cpu.reg.ac as u16;
    let tmp = ac + val + u16::from(p.get(StatusReg::C));

    p.set(StatusReg::C, (tmp & HI_MASK) > 0)
        .set(StatusReg::Z, is_lo_zero(tmp))
        .set(StatusReg::V, is_neg((tmp ^ ac) & (tmp ^ val)))
        .set(StatusReg::N, is_neg(tmp));

    cpu.reg.p = p;
    cpu.reg.ac = tmp as u8;

    1
}

pub(super) fn sta(cpu: &mut Cpu6502) -> u8 {
    cpu.write(cpu.state.addr_abs, cpu.reg.ac);
    0
}

pub(super) fn stx(cpu: &mut Cpu6502) -> u8 {
    cpu.write(cpu.state.addr_abs, cpu.reg.x);
    0
}

pub(super) fn sty(cpu: &mut Cpu6502) -> u8 {
    cpu.write(cpu.state.addr_abs, cpu.reg.y);
    0
}

pub(super) fn tax(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.x = cpu.reg.ac;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(cpu.reg.x))
        .set(StatusReg::N, is_neg(cpu.reg.x as u16));
    cpu.reg.p = p;

    0
}

pub(super) fn tay(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.y = cpu.reg.ac;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(cpu.reg.y))
        .set(StatusReg::N, is_neg(cpu.reg.y as u16));
    cpu.reg.p = p;

    0
}

pub(super) fn tsx(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.x = cpu.reg.sp;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(cpu.reg.x))
        .set(StatusReg::N, is_neg(cpu.reg.x as u16));
    cpu.reg.p = p;

    0
}

pub(super) fn txa(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.ac = cpu.reg.x;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(cpu.reg.ac))
        .set(StatusReg::N, is_neg(cpu.reg.ac as u16));
    cpu.reg.p = p;

    0
}

pub(super) fn txs(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.sp = cpu.reg.x;
    0
}

pub(super) fn tya(cpu: &mut Cpu6502) -> u8 {
    cpu.reg.ac = cpu.reg.y;
    let mut p = cpu.reg.p;
    p.set(StatusReg::Z, is_zero(cpu.reg.ac))
        .set(StatusReg::N, is_neg(cpu.reg.ac as u16));
    cpu.reg.p = p;

    0
}

fn xxx(_cpu: &mut Cpu6502) -> u8 {
    0
}

impl From<Instruction> for OperFn {
    fn from(value: Instruction) -> Self {
        match value {
            Instruction::ADC => adc,
            Instruction::AND => and,
            Instruction::ASL => asl,
            Instruction::BCC => bcc,
            Instruction::BCS => bcs,
            Instruction::BEQ => beq,
            Instruction::BIT => bit,
            Instruction::BMI => bmi,
            Instruction::BNE => bne,
            Instruction::BPL => bpl,
            Instruction::BRK => brk,
            Instruction::BVC => bvc,
            Instruction::BVS => bvs,
            Instruction::CLC => clc,
            Instruction::CLD => cld,
            Instruction::CLI => cli,
            Instruction::CLV => clv,
            Instruction::CMP => cmp,
            Instruction::CPX => cpx,
            Instruction::CPY => cpy,
            Instruction::DEC => dec,
            Instruction::DEX => dex,
            Instruction::DEY => dey,
            Instruction::EOR => eor,
            Instruction::INC => inc,
            Instruction::INX => inx,
            Instruction::INY => iny,
            Instruction::JMP => jmp,
            Instruction::JSR => jsr,
            Instruction::LDA => lda,
            Instruction::LDX => ldx,
            Instruction::LDY => ldy,
            Instruction::LSR => lsr,
            Instruction::NOP => nop,
            Instruction::ORA => ora,
            Instruction::PHA => pha,
            Instruction::PHP => php,
            Instruction::PLA => pla,
            Instruction::PLP => plp,
            Instruction::ROL => rol,
            Instruction::ROR => ror,
            Instruction::RTI => rti,
            Instruction::RTS => rts,
            Instruction::SBC => sbc,
            Instruction::SEC => sec,
            Instruction::SED => sed,
            Instruction::SEI => sei,
            Instruction::STA => sta,
            Instruction::STX => stx,
            Instruction::STY => sty,
            Instruction::TAX => tax,
            Instruction::TAY => tay,
            Instruction::TSX => tsx,
            Instruction::TXA => txa,
            Instruction::TXS => txs,
            Instruction::TYA => tya,
            Instruction::XXX => xxx,
        }
    }
}
