use crate::ines::{
    cpu::{Registers, Status},
    io::RwDevice,
    Byte, Word, HI_MASK, LO_MASK,
};

use super::{AddrModeData, InstructionState, Operation, OperData};

macro_rules! is_zero {
    ($v:expr) => {
        $v & 0xFF == 0
    };
}

macro_rules! is_neg {
    ($v:expr) => {
        $v & 0x80 == 0x80
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

    fn a(_: &mut Registers, _: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        state.addr_data = AddrModeData::A;
        state.addr = 0;
        0
    }

    fn abs_00(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let lo = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = lo;
        0
    }

    fn abs_01(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let lo = state.addr;
        let hi = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = lo | hi << 8;
        state.addr_data = AddrModeData::Abs(lo as Byte, hi as Byte);
        0
    }

    fn abx_00(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let lo = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = lo;
        0
    }

    fn abx_01(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let lo = state.addr;
        let hi = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        let addr = lo | hi << 8;
        state.addr = addr + reg.x as Word;
        state.addr_data = AddrModeData::Abx(lo as Byte, hi as Byte);
        if addr >> 8 != hi {
            0
        } else {
            1
        }
    }

    fn aby_00(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let lo = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = lo;
        0
    }

    fn aby_01(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let lo = state.addr;
        let hi = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        let addr = lo | hi << 8;
        state.addr = addr + reg.y as Word;
        state.addr_data = AddrModeData::Aby(lo as Byte, hi as Byte);
        if addr >> 8 != hi {
            0
        } else {
            1
        }
    }

    fn imp(_: &mut Registers, _: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        state.addr_data = AddrModeData::Imp;
        state.addr = 0;
        0
    }

    fn ind_00(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let ptr_lo = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);
        state.addr = ptr_lo;
        0
    }

    fn ind_01(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let ptr_lo = state.addr;
        let ptr_hi = (bus.read(reg.pc) as Word) << 8;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = ptr_lo | ptr_hi;
        0
    }

    fn ind_02(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let ptr = state.addr;
        let lo = bus.read(ptr) as Word;
        state.tmp = lo;
        0
    }

    fn ind_03(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let ptr = state.addr;
        let lo = state.tmp;

        let hi = if ptr & LO_MASK == LO_MASK {
            // Simulate page boundary hardware bug
            (bus.read(ptr & 0xFF00) as Word) << 8
        } else {
            (bus.read(ptr + 0x0001) as Word) << 8
        };

        let addr = lo | hi;
        state.addr = addr;
        state.addr_data = AddrModeData::Ind(ptr as u8, (ptr >> 8) as u8, addr);
        0
    }

    fn izx_00(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        state.tmp = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);
        0
    }

    fn izx_01(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let ptr_lo = state.tmp;
        let ptr = ptr_lo + reg.x as Word;

        state.addr = bus.read(ptr) as Word;
        0
    }

    fn izx_02(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let ptr_lo = state.tmp;
        let ptr = ptr_lo + reg.x as Word;

        let hi = (bus.read(ptr + 1) as Word) << 8;
        state.addr |= hi;

        state.addr_data = AddrModeData::Izx(ptr_lo as Byte, state.addr);
        0
    }

    fn izy_00(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        state.tmp = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);
        0
    }

    fn izy_01(_: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let ptr = state.tmp;
        state.addr = bus.read(ptr) as Word;
        0
    }

    fn izy_02(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let ptr = state.tmp;
        let lo = state.addr;
        let hi = (bus.read(ptr + 1) as Word) << 8;

        state.addr = (lo | hi) + reg.y as Word;
        state.addr_data = AddrModeData::Izy(lo as Byte, state.addr);
        if state.addr & HI_MASK != hi {
            0
        } else {
            1
        }
    }

    fn imm(reg: &mut Registers, _: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let addr = reg.pc;
        reg.pc = reg.pc.wrapping_add(1);
        state.addr_data = AddrModeData::Imm(addr as Byte);
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

    fn zpg(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let addr = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = addr;
        state.addr_data = AddrModeData::Zpg(addr as Byte);
        0
    }

    fn zpx(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let lo = bus.read(reg.pc) as Word;
        let lo_off = lo + reg.x as Word;
        let addr = lo_off & LO_MASK;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = addr;
        state.addr_data = AddrModeData::Zpx(lo as Byte, addr as Byte);
        0
    }

    fn zpy(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let lo = bus.read(reg.pc) as Word;
        let lo_off = lo + reg.y as Word;
        let addr = lo_off & LO_MASK;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = addr;
        state.addr_data = AddrModeData::Zpy(lo as Byte, addr as Byte);
        0
    }

    /// A Accumulator - OPC A
    ///
    /// operand is AC (implied single byte instruction)
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
    pub const A: [Operation; 1] = [a];

    /// Absolute - OPC $LLHH
    ///
    /// operand is address $HHLL *
    ///
    /// Absolute addressing modes provides the 16-bit address of a memory
    /// location, the contents of which used as the operand to the
    /// instruction. In machine language, the address is provided in two
    /// bytes immediately after the instruction (making these 3-byte
    /// instructions) in low-byte, high-byte order (LLHH) or little-endian.
    /// In assembler, conventional numbers (HHLL order or big-endian words)
    /// are used to provide the address.
    ///
    /// Absolute addresses are also used for the jump instructions JMP and
    /// JSR to provide the address for the next instruction to continue with
    /// in the control flow.
    pub const ABS: [Operation; 2] = [abs_00, abs_01];

    /// Absolute, X-indexed - OPC $LLHH,X
    ///
    /// operand is address; effective address is address incremented by X with carry **
    ///
    /// Indexed addressing adds the contents of either the X-register or the
    /// Y-register to the provided address to give the effective address,
    /// which provides the operand.
    ///
    /// These instructions are usefull to e.g., load values from tables or
    /// to write to a continuous segment of memory in a loop. The most basic
    /// forms are "absolute,X" and "absolute,X", where either the X- or the
    /// Y-register, respectively, is added to a given base address. As the
    /// base address is a 16-bit value, these are generally 3-byte
    /// instructions. Since there is an additional operation to perform to
    /// determine the effective address, these instructions are one cycle
    /// slower than those using absolute addressing mode.*
    ///
    /// *) If the addition of the contents of the index register effects in
    /// a change of the high-byte given by the base address so that the
    /// effective address is on the next memory page, the additional
    /// operation to increment the high-byte takes another CPU cycle. This
    /// is also known as a crossing of page boundaries.
    pub const ABX: [Operation; 3] = [abx_00, abx_01, spin];

    /// Absolute, Y-indexed - OPC $LLHH,Y
    ///
    /// operand is address; effective address is address incremented by Y with carry **
    ///
    /// Indexed addressing adds the contents of either the X-register or the
    /// Y-register to the provided address to give the effective address,
    /// which provides the operand.
    ///
    /// These instructions are usefull to e.g., load values from tables or
    /// to write to a continuous segment of memory in a loop. The most basic
    /// forms are "absolute,X" and "absolute,X", where either the X- or the
    /// Y-register, respectively, is added to a given base address. As the
    /// base address is a 16-bit value, these are generally 3-byte
    /// instructions. Since there is an additional operation to perform to
    /// determine the effective address, these instructions are one cycle
    /// slower than those using absolute addressing mode.*
    ///
    /// *) If the addition of the contents of the index register effects in
    /// a change of the high-byte given by the base address so that the
    /// effective address is on the next memory page, the additional
    /// operation to increment the high-byte takes another CPU cycle. This
    /// is also known as a crossing of page boundaries.
    pub const ABY: [Operation; 3] = [aby_00, aby_01, spin];

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
    pub const IMP: [Operation; 1] = [imp];

    /// Indirect - OPC ($LLHH)
    ///
    /// operand is address; effective address is contents of word at address: C.w($HHLL)
    ///
    /// This mode looks up a given address and uses the contents of this
    /// address and the next one (in LLHH little-endian order) as the
    /// effective address. In its basic form, this mode is available for the
    /// JMP instruction only. (Its generally use is jump vectors and jump tables.)
    ///
    /// Like the absolute JMP instruction it uses a 16-bit address (3 bytes
    /// in total), but takes two additional CPU cycles to execute, since
    /// there are two additional bytes to fetch for the lookup of the
    /// effective jump target.
    ///
    /// Generally, indirect addressing is denoted by putting the lookup
    /// address in parenthesis.
    pub const IND: [Operation; 4] = [ind_00, ind_01, ind_02, ind_03];

    /// Indirect, X-indexed - OPC ($LL,X)
    ///
    /// operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
    ///
    /// Indexed indirect address modes are generally available only for
    /// instructions supplying an operand to the accumulator (LDA, STA, ADC,
    /// SBC, AND, ORA, EOR, etc). The placement of the index register inside
    /// or outside of the parenthesis indicating the address lookup will
    /// give you clue what these instructions are doing.
    ///
    /// Pre-indexed indirect address mode is only available in combination
    /// with the X-register. It works much like the "zero-page,X" mode, but,
    /// after the X-register has been added to the base address, instead of
    /// directly accessing this, an additional lookup is performed, reading
    /// the contents of resulting address and the next one (in LLHH little-
    /// endian order), in order to determine the effective address.
    ///
    /// Like with "zero-page,X" mode, the total instruction length is 2
    /// bytes, but there are two additional CPU cycles in order to fetch
    /// the effective 16-bit address. As "zero-page,X" mode, a lookup address
    /// will never overflow into the next page, but will simply wrap around
    /// in the zero-page.
    ///
    /// These instructions are useful, whenever we want to loop over a table
    /// of pointers to disperse addresses, or where we want to apply the
    /// same operation to various addresses, which we have stored as a table
    /// in the zero-page.
    pub const IZX: [Operation; 4] = [izx_00, izx_01, izx_02, spin];

    /// Indirect, Y-indexed - OPC ($LL),Y
    ///
    /// operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
    ///
    /// Post-indexed indirect addressing is only available in combination
    /// with the Y-register. As indicated by the indexing term ",Y" being
    /// appended to the outside of the parenthesis indicating the indirect
    /// lookup, here, a pointer is first read (from the given zero-page
    /// address) and resolved and only then the contents of the Y-register
    /// is added to this to give the effective address.
    ///
    /// Like with "zero-page,Y" mode, the total instruction length is 2
    /// bytes, but there it takes an additional CPU cycles to resolve and
    /// index the 16-bit pointer. As with "absolute,X" mode, the effective
    /// address may overflow into the next page, in the case of which the
    /// execution uses an extra CPU cycle.
    ///
    /// These instructions are useful, wherever we want to perform lookups
    /// on varying bases addresses or whenever we want to loop over tables,
    /// the base address of which we have stored in the zero-page.
    pub const IZY: [Operation; 4] = [izy_00, izy_01, izy_02, spin];

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

    /// Zeropage - OPC $LL
    ///
    /// operand is zeropage address (hi-byte is zero, address = $00LL)
    ///
    /// The 16-bit address space available to the 6502 is thought to consist
    /// of 256 "pages" of 256 memory locations each ($00…$FF). In this model
    /// the high-byte of an address gives the page number and the low-byte a
    /// location inside this page. The very first of these pages, where the
    /// high-byte is zero (addresses $0000…$00FF), is somewhat special.
    ///
    /// The zero-page address mode is similar to absolute address mode, but
    /// these instructions use only a single byte for the operand, the low-
    /// byte, while the high-byte is assumed to be zero by definition.
    /// Therefore, these instructions have a total length of just two bytes
    /// (one less than absolute mode) and take one CPU cycle less to
    /// execute, as there is one byte less to fetch.
    pub const ZPG: [Operation; 1] = [zpg];

    /// Zeropage, X-indexed - OPC $LL,X
    ///
    /// operand is zeropage address; effective address is address incremented by X without carry **
    ///
    /// As with absolute addressing, there is also a zero-page mode for
    /// indexed addressing. However, this is generally only available with
    /// the X-register. (The only exception to this is LDX, which has an
    /// indexed zero-page mode utilizing the Y-register.)
    ///
    /// As we have already seen with normal zero-page mode, these
    /// instructions are one byte less in total length (two bytes) and take
    /// one CPU cycle less than instructions in absolute indexed mode.
    ///
    /// Unlike absolute indexed instructions with 16-bit base addresses,
    /// zero-page indexed instructions never affect the high-byte of the
    /// effective address, which will simply wrap around in the zero-page,
    /// and there is no penalty for crossing any page boundaries.
    pub const ZPX: [Operation; 1] = [zpx];

    /// Zeropage, Y-indexed - OPC $LL,Y
    ///
    /// operand is zeropage address; effective address is address incremented by Y without carry **
    ///
    /// As with absolute addressing, there is also a zero-page mode for
    /// indexed addressing. However, this is generally only available with
    /// the X-register. (The only exception to this is LDX, which has an
    /// indexed zero-page mode utilizing the Y-register.)
    ///
    /// As we have already seen with normal zero-page mode, these
    /// instructions are one byte less in total length (two bytes) and take
    /// one CPU cycle less than instructions in absolute indexed mode.
    ///
    /// Unlike absolute indexed instructions with 16-bit base addresses,
    /// zero-page indexed instructions never affect the high-byte of the
    /// effective address, which will simply wrap around in the zero-page,
    /// and there is no penalty for crossing any page boundaries.
    pub const ZPY: [Operation; 1] = [zpy];
}

macro_rules! addr_mode {
    (A) => {
        /// A Accumulator - OPC A
        addr::A
    };

    () => {
        addr::IMP
    };

    (&LLHH) => {
        addr::ABS
    };

    (&LLHH,X) => {
        addr::ABX
    };

    (&LLHH,Y) => {
        addr::ABY
    };

    (#&BB) => {
        addr::IMM
    };

    ((&LLHH)) => {
        addr::IND
    };

    ((&LL,X)) => {
        addr::IZX
    };

    ((&LL),Y) => {
        addr::IZY
    };

    (&BB) => {
        addr::REL
    };

    (&LL) => {
        addr::ZPG
    };

    (&LL,X) => {
        addr::ZPX
    };

    (&LL,Y) => {
        addr::ZPY
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

        reg.ac = tmp as Byte;
        0
    }

    fn and(reg: &mut Registers, bus: &mut dyn RwDevice, state: &mut InstructionState) -> i8 {
        let addr = state.addr;
        let data = bus.read(addr);
        let ac = reg.ac;
        let tmp = data & ac;

        reg.p
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        reg.ac = tmp;

        state.oper = OperData::None;
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
    steps! {AND [and]}
    steps! {BNE [bne_00, branch_page_check, spin]}

    steps! {NOP [spin]}
}

macro_rules! make_instruction {
    ([$opc:tt, $ami:tt, $inst:tt] $op:tt $($am:tt)*) => {
        #[allow(dead_code)]
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::$op;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        pub const $inst: OperType = OperType::$op;
    };
}

pub(super) use addr_mode;
pub(super) use am_const;
pub(super) use make_instruction;
