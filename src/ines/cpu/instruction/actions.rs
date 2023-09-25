use crate::ines::{
    cpu::{Registers, Status},
    io::RwDevice,
    Byte, Word, HI_MASK, LO_MASK,
};

use super::{AddrModeData, InstructionState, Operation};

const NEG_MASK: u16 = 0x0080;

macro_rules! is_zero {
    ($v:expr) => {
        $v & LO_MASK == 0
    };
}

macro_rules! is_neg {
    ($v:expr) => {
        $v & NEG_MASK == NEG_MASK
    };
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
    ($op:ident [$($steps:tt),* $(,)*]) => {
        pub const $op: [Operation; count_idents!($($steps,)*)]  = [$($steps,)*];
    };
}

pub(super) fn spin(_: &mut Registers, _: &mut dyn RwDevice, _: &mut InstructionState) -> i8 {
    0
}

pub mod addr {
    use super::*;

    /// Implied - OPC
    ///
    /// operand implied
    ///
    /// These instructions act directly on one or more registers or flags
    /// internal to the CPU. Therefor, these instructions are principally
    /// single-byte instructions, lacking an explicit operand. The operand
    /// is implied, as it is already provided by the very instruction.
    ///
    /// Instructions targeting exclusively the contents of the accumulator
    /// may or may not be denoted by using an explicit "A" as the operand,
    /// depending on the flavor of syntax. (This may be regarded as a
    /// special address mode of its own, but it is really a special case of
    /// an implied instruction. It is still a single-byte instruction and no
    /// operand is provided in machine language.)
    fn imp(_: &mut Registers, _: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        state.addr_data = AddrModeData::Imp;
        state.addr = 0;
        0
    }

    fn imm(reg: &mut Registers, _: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let addr = reg.pc;
        reg.pc = reg.pc.wrapping_add(1);
        state.addr_data = AddrModeData::Imm(addr as u8);
        state.addr = addr;
        0
    }

    fn rel(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let rel_addr = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        let addr = if is_neg!(rel_addr) {
            rel_addr | HI_MASK
        } else {
            rel_addr
        };

        state.addr_data = AddrModeData::Rel(rel_addr as Byte, addr);
        state.addr = addr;
        0
    }

    /// Immediate - OPC #$BB
    ///
    /// operand is byte BB
    ///
    /// Here, a literal operand is given immediately after the instruction.
    /// The operand is always an 8-bit value and the total instruction
    /// length is always 2 bytes. In memory, the operand is a single byte
    /// following immediately after the instruction code. In assembler, the
    /// mode is usually indicated by a "#" prefix adjacent to the operand.
    pub const IMM: [Operation; 1] = [imm];

    pub const IMP: [Operation; 1] = [imp];

    /// Relative - OPC $BB
    ///
    /// branch target is PC + signed offset BB ***
    ///
    /// This final address mode is exlusive to conditional branch
    /// instructions, which branch in the execution path depending on the
    /// state of a given CPU flag. Here, the instruction provides only a
    /// relative offset, which is added to the contents of the program
    /// counter (PC) as it points to the immediate next instruction. The
    /// relative offset is a signed single byte value in two's complement
    /// encoding (giving a range of −128…+127), which allows for branching
    /// up to half a page forwards and backwards.
    ///
    /// On the one hand, this makes these instructions compact, fast and
    /// relocatable at the same time. On the other hand, we have to mind
    /// that our branch target is no farther away than half a memory page.
    ///
    /// Generally, an assembler will take care of this and we only have to
    /// provide the target address, not having to worry about relative
    /// addressing.
    ///
    /// These instructions are always of 2 bytes length and perform in 2 CPU
    /// cycles, if the branch is not taken (the condition resolving to
    /// 'false'), and 3 cycles, if the branch is taken (when the condition
    /// is true). If a branch is taken and the target is on a different
    /// page, this adds another CPU cycle (4 in total).
    pub const REL: [Operation; 1] = [rel];
}

macro_rules! addr_mode {
    () => {
        []
    };
    (#&BB) => {
        addr::IMM
    };
    (&BB) => {
        addr::REL
    };
}

macro_rules! am_const {
    ([$code:ident] A) => {
        /// A Accumulator - OPC A
        pub const $code: AddrMode = AddrMode::A;
    };

    ([$code:ident] &LLHH) => {
        /// Absolute - OPC $LLHH
        pub const $code: AddrMode = AddrMode::ABS;
    };

    ([$code:ident] &LLHH,X) => {
        /// Absolute, X-indexed - OPC $LLHH,X
        pub const $code: AddrMode = AddrMode::ABX;
    };

    ([$code:ident] &LLHH,Y) => {
        /// Absolute, Y-indexed - OPC $LLHH,Y
        pub const $code: AddrMode = AddrMode::ABY;
    };

    ([$code:ident] #&BB) => {
        /// Immediate - OPC #$BB
        pub const $code: AddrMode = AddrMode::IMM;
    };

    ([$code:ident]) => {
        /// Implied - OPC
        pub const $code: AddrMode = AddrMode::IMP;
    };

    ([$code:ident] (&LLHH)) => {
        /// Indirect - OPC ($LLHH)
        pub const $code: AddrMode = AddrMode::IND;
    };

    ([$code:ident] (&LL,X)) => {
        /// Indirect, X-indexed - OPC ($LL,X)
        pub const $code: AddrMode = AddrMode::IZX;
    };

    ([$code:ident] (&LL),Y) => {
        /// Indirect, Y-indexed - OPC ($LL),Y
        pub const $code: AddrMode = AddrMode::IZY;
    };

    ([$code:ident] &BB) => {
        /// Relative - OPC $BB
        pub const $code: AddrMode = AddrMode::REL;
    };

    ([$code:ident] &LL) => {
        /// Zeropage - OPC $LL
        pub const $code: AddrMode = AddrMode::ZPG;
    };

    ([$code:ident] &LL,X) => {
        /// Zeropage, X-indexed - OPC $LL,X
        pub const $code: AddrMode = AddrMode::ZPX;
    };

    ([$code:ident] &LL,Y) => {
        /// Zeropage, Y-indexed - OPC $LL,Y
        pub const $code: AddrMode = AddrMode::ZPY;
    };
}

pub mod act {
    use super::*;

    fn adc(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let data = bus.read(state.addr) as Word;
        let ac = reg.ac as Word;

        let tmp = data + ac + reg.p.get(Status::C);
        reg.p
            .set(Status::C, tmp > 255)
            .set(Status::Z, is_zero!(tmp))
            .set(Status::V, is_neg!(!(ac ^ data) & ac ^ tmp))
            .set(Status::N, is_neg!(tmp));

        reg.ac = tmp as u8;
        0
    }

    fn branch_page_check(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> i8 {
        let pc = reg.pc;
        reg.pc = state.addr;
        if state.addr & HI_MASK != pc & HI_MASK {
            0
        } else {
            1
        }
    }

    fn bne_00(reg: &mut Registers, _: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let pc = reg.pc;
        state.addr = pc.wrapping_add(state.addr);
        if reg.p.get(Status::Z) == 0 {
            0
        } else {
            2
        }
    }

    steps! {ADC [adc]}
    steps! {BNE [bne_00, branch_page_check, spin]}

    steps! {NOP [spin]}
}

macro_rules! make_instruction {
    ([$opc:tt, $ami:tt, $inst:tt] $op:tt $($am:tt)*) => {
        am_const!([$ami] $($am)*);
        pub const $inst: OperType = OperType::$op;

        #[allow(dead_code)]
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::$op;
            InstructionIterator::new(&addr, work)
        }
    };
}

pub(super) use addr_mode;
pub(super) use am_const;
pub(super) use make_instruction;
