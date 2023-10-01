use crate::ines::{
    cpu::{Registers, Status},
    io::RwDevice,
    Byte, Word, HI_MASK, LO_MASK,
};

use super::{AddrModeData, InstructionState, OperData, Operation, OperationResult};

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

macro_rules! is_overflow {
    ($a:expr, $m:expr, $r:expr) => {
        (!($a ^ $m) & ($a ^ $r)) & 0x80 == 0x80
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

pub(super) fn spin(
    _: &mut Registers,
    _: &mut dyn RwDevice,
    _: &mut InstructionState,
) -> OperationResult {
    OperationResult::None
}

pub mod addr {
    use super::*;

    fn a(_: &mut Registers, _: &mut dyn RwDevice, state: &mut InstructionState) -> OperationResult {
        state.addr_data = AddrModeData::A;
        state.addr = 0;
        OperationResult::Instant
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

    fn abs_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let lo = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = lo;
        OperationResult::None
    }

    fn abs_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let lo = state.addr;
        let hi = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = lo | hi << 8;
        state.addr_data = AddrModeData::Abs(lo as Byte, hi as Byte);
        OperationResult::None
    }

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

    fn abs_jmp_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let lo = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = lo;
        OperationResult::None
    }

    fn abs_jmp_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let lo = state.addr;
        let hi = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = lo | hi << 8;
        state.addr_data = AddrModeData::Abs(lo as Byte, hi as Byte);
        OperationResult::Instant
    }

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
    pub const ABS_JMP: [Operation; 2] = [abs_jmp_00, abs_jmp_01];

    fn abx_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let lo = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = lo;
        OperationResult::Instant
    }

    fn abx_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let lo = state.addr;
        let hi = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        let addr = lo | hi << 8;
        state.addr = addr + reg.x as Word;
        state.addr_data = AddrModeData::Abx(lo as Byte, hi as Byte);
        if addr >> 8 != hi {
            OperationResult::None
        } else {
            OperationResult::Skip(1)
        }
    }

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

    fn aby_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let lo = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = lo;
        OperationResult::Instant
    }

    fn aby_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let lo = state.addr;
        let hi = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        let addr = lo | hi << 8;
        state.addr = addr + reg.y as Word;
        state.addr_data = AddrModeData::Aby(lo as Byte, hi as Byte);
        if addr >> 8 != hi {
            OperationResult::None
        } else {
            OperationResult::Skip(1)
        }
    }

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

    fn imp(
        _: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        state.addr_data = AddrModeData::Imp;
        state.addr = 0;
        OperationResult::Instant
    }

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

    fn ind_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let ptr_lo = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);
        state.addr = ptr_lo;
        OperationResult::None
    }

    fn ind_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let ptr_lo = state.addr;
        let ptr_hi = (bus.read(reg.pc) as Word) << 8;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = ptr_lo | ptr_hi;
        OperationResult::None
    }

    fn ind_02(
        _: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let ptr = state.addr;
        let lo = bus.read(ptr) as Word;
        state.tmp = lo;
        OperationResult::None
    }

    fn ind_03(
        _: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
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
        state.addr_data = AddrModeData::Ind(ptr as Byte, (ptr >> 8) as Byte, addr);
        OperationResult::Instant
    }

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

    fn izx_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        state.tmp = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);
        OperationResult::Instant
    }

    fn izx_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let ptr_lo = state.tmp;
        let ptr = ptr_lo + reg.x as Word;

        state.addr = bus.read(ptr) as Word;
        OperationResult::None
    }

    fn izx_02(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let ptr_lo = state.tmp;
        let ptr = ptr_lo + reg.x as Word;

        let hi = (bus.read(ptr + 1) as Word) << 8;
        state.addr |= hi;

        state.addr_data = AddrModeData::Izx(ptr_lo as Byte, state.addr);
        OperationResult::None
    }

    /// Indirect, X-indexed - OPC ($LL,X)
    ///
    /// operand is zeropage address; effective address is word in (LL + X, LL +
    /// X + 1), inc. without carry: C.w($00LL + X)
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

    fn izy_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        state.tmp = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);
        OperationResult::Instant
    }

    fn izy_01(
        _: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let ptr = state.tmp;
        state.addr = bus.read(ptr) as Word;
        OperationResult::None
    }

    fn izy_02(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let ptr = state.tmp;
        let lo = state.addr;
        let hi = (bus.read(ptr + 1) as Word) << 8;

        state.addr = (lo | hi) + reg.y as Word;
        state.addr_data = AddrModeData::Izy(lo as Byte, state.addr);
        if state.addr & HI_MASK != hi {
            OperationResult::None
        } else {
            OperationResult::Skip(1)
        }
    }

    /// Indirect, Y-indexed - OPC ($LL),Y
    ///
    /// operand is zeropage address; effective address is word in (LL, LL + 1)
    /// incremented by Y with carry: C.w($00LL) + Y
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

    fn imm(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = reg.pc;
        reg.pc = reg.pc.wrapping_add(1);
        state.addr_data = AddrModeData::Imm(bus.read(addr));
        state.addr = addr;
        OperationResult::Instant
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

    fn rel(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let rel_addr = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        let addr = if is_neg!(rel_addr) {
            rel_addr | HI_MASK
        } else {
            rel_addr
        };

        state.addr_data = AddrModeData::Rel(rel_addr as Byte, addr);
        state.addr = addr;
        OperationResult::Instant
    }

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

    fn zpg_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);

        state.addr = addr;
        state.addr_data = AddrModeData::Zpg(state.addr as Byte);
        OperationResult::None
    }

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
    pub const ZPG: [Operation; 1] = [zpg_00];

    fn zpx_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        state.addr = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);
        OperationResult::None
    }

    fn zpx_01(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let lo = state.addr;
        let lo_off = lo + reg.x as Word;
        state.addr = lo_off & LO_MASK;

        state.addr_data = AddrModeData::Zpx(lo as Byte, state.addr as Byte);
        OperationResult::None
    }

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
    pub const ZPX: [Operation; 2] = [zpx_00, zpx_01];

    fn zpy_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        state.addr = bus.read(reg.pc) as Word;
        reg.pc = reg.pc.wrapping_add(1);
        OperationResult::None
    }

    fn zpy_01(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let lo = state.addr;
        let lo_off = lo + reg.y as Word;
        state.addr = lo_off & LO_MASK;

        state.addr_data = AddrModeData::Zpy(lo as Byte, state.addr as Byte);
        OperationResult::None
    }

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
    pub const ZPY: [Operation; 2] = [zpy_00, zpy_01];
}

macro_rules! addr_mode {
    (A) => {
        addr::A
    };

    () => {
        addr::IMP
    };

    (JMP &LLHH) => {
        addr::ABS_JMP
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

    (JMP (&LLHH)) => {
        addr::IND
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
    use crate::ines::{IRQ_LO, NMI_LO, PS};

    use super::*;

    fn adc(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let data = bus.read(state.addr) as Word;
        let ac = reg.ac as Word;

        let tmp = data + ac + reg.p.get(Status::C);
        reg.p
            .set(Status::C, tmp > 255)
            .set(Status::Z, is_zero!(tmp))
            .set(Status::V, is_overflow!(ac, data, tmp))
            .set(Status::N, is_neg!(tmp));

        reg.ac = tmp as Byte;
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {ADC [adc]}

    fn and(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        let data = bus.read(addr);
        let ac = reg.ac;
        let tmp = data & ac;

        reg.p
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        reg.ac = tmp;

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {AND [and]}

    fn asl(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;

        let data = bus.read(addr) as Word;
        let tmp = data << 1;
        let mut p = reg.p;

        p.set(Status::C, tmp & HI_MASK != 0)
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        reg.p = p;

        let tmp = tmp as Byte;
        match &state.addr_data {
            AddrModeData::A | AddrModeData::Imp => {
                reg.ac = tmp;
            }
            _ => {
                bus.write(addr, tmp);
            }
        }

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {ASL [asl]}

    fn branch_page_check(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let pc = reg.pc;
        reg.pc = state.addr;
        if state.addr & HI_MASK != pc & HI_MASK {
            OperationResult::None
        } else {
            OperationResult::Skip(1)
        }
    }

    fn bcc_00(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let pc = reg.pc;
        state.addr = pc.wrapping_add(state.addr);
        state.oper = OperData::Word(state.addr);
        if reg.p.get(Status::C) == 0 {
            OperationResult::None
        } else {
            OperationResult::Skip(2)
        }
    }

    steps! {BCC [bcc_00, branch_page_check, spin]}

    fn bcs_00(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        state.addr = reg.pc.wrapping_add(state.addr);
        state.oper = OperData::Word(state.addr);
        if reg.p.get(Status::C) == 1 {
            OperationResult::None
        } else {
            OperationResult::Skip(2)
        }
    }

    steps! {BCS [bcs_00, branch_page_check, spin]}

    fn beq_00(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let pc = reg.pc;
        state.addr = pc.wrapping_add(state.addr);
        state.oper = OperData::Word(state.addr);
        if reg.p.get(Status::Z) == 1 {
            OperationResult::None
        } else {
            OperationResult::Skip(2)
        }
    }

    steps! {BEQ [beq_00, branch_page_check, spin]}

    fn bit(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        let data = bus.read(addr);
        let tmp = data & reg.ac;

        reg.p
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(data))
            .set(Status::V, data & 0x40 == 0x40);

        state.oper = OperData::Byte(data);
        OperationResult::None
    }

    steps! {BIT [bit]}

    fn bmi_00(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let pc = reg.pc;
        state.addr = pc.wrapping_add(state.addr);
        state.oper = OperData::Word(state.addr);
        if reg.p.get(Status::N) == 1 {
            OperationResult::None
        } else {
            OperationResult::Skip(2)
        }
    }

    steps! {BMI [bmi_00, branch_page_check, spin]}

    fn bne_00(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let pc = reg.pc;
        state.addr = pc.wrapping_add(state.addr);
        state.oper = OperData::Word(state.addr);
        if reg.p.get(Status::Z) == 0 {
            OperationResult::None
        } else {
            OperationResult::Skip(2)
        }
    }

    steps! {BNE [bne_00, branch_page_check, spin]}

    fn bpl_00(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let pc = reg.pc;
        state.addr = pc.wrapping_add(state.addr);
        state.oper = OperData::Word(state.addr);
        if reg.p.get(Status::N) == 0 {
            OperationResult::None
        } else {
            OperationResult::Skip(2)
        }
    }

    steps! {BPL [bpl_00, branch_page_check, spin]}

    /// #  address R/W description
    /// ```text
    /// --- ------- --- -----------------------------------------------
    /// 1    PC     R  fetch opcode (and discard it - $00 (BRK) is forced into the opcode register instead)
    /// 2    PC     R  read next instruction byte (actually the same as above, since PC increment is suppressed. Also discarded.)
    /// 3  $0100,S  W  push PCH on stack, decrement S
    /// 4  $0100,S  W  push PCL on stack, decrement S
    /// *** At this point, the signal status determines which interrupt vector is used ***
    /// 5  $0100,S  W  push P on stack (with B flag *clear*), decrement S
    /// 6   A       R  fetch PCL (A = FFFE for IRQ, A = FFFA for NMI), set I flag
    /// 7   A       R  fetch PCH (A = FFFF for IRQ, A = FFFB for NMI)
    /// ```
    fn brk_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        bus.read(reg.pc);
        OperationResult::None
    }

    fn brk_02(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        bus.read(reg.pc);
        OperationResult::None
    }

    fn brk_03(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        bus.write(PS.wrapping_add(reg.sp as Word), (reg.pc >> 8) as Byte);
        reg.sp = reg.sp.wrapping_sub(1);
        OperationResult::None
    }

    fn brk_04(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        bus.write(PS.wrapping_add(reg.sp as Word), reg.pc as Byte);
        reg.sp = reg.sp.wrapping_sub(1);
        state.tmp = if reg.p & Status::I == Status::I {
            IRQ_LO
        } else {
            NMI_LO
        };
        OperationResult::None
    }

    fn brk_05(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        let p = reg.p | Status::B;
        bus.write(PS.wrapping_add(reg.sp as Word), p.into());
        reg.sp = reg.sp.wrapping_sub(1);
        OperationResult::None
    }

    fn brk_06(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        state.addr = bus.read(state.tmp) as Word;
        reg.p.set(Status::I, true);
        OperationResult::None
    }

    fn brk_07(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        state.addr |= (bus.read(state.tmp + 1) as Word) << 8;
        reg.p.set(Status::I, true);
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {BRK [brk_01, brk_02, brk_03, brk_04, brk_05, brk_06, brk_07]}

    fn bvc_00(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let pc = reg.pc;
        state.addr = pc.wrapping_add(state.addr);
        state.oper = OperData::Word(state.addr);
        if reg.p.get(Status::V) == 0 {
            OperationResult::None
        } else {
            OperationResult::Skip(2)
        }
    }

    steps! {BVC [bvc_00, branch_page_check, spin]}

    fn bvs_00(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let pc = reg.pc;
        state.addr = pc.wrapping_add(state.addr);
        state.oper = OperData::Word(state.addr);
        if reg.p.get(Status::V) == 1 {
            OperationResult::None
        } else {
            OperationResult::Skip(2)
        }
    }

    steps! {BVS [bvs_00, branch_page_check, spin]}

    fn clc(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.p.set(Status::C, false);
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {CLC [clc]}

    fn cld(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.p.set(Status::D, false);
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {CLD [cld]}

    fn cli(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.p.set(Status::I, false);
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {CLI [cli]}

    fn clv(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.p.set(Status::V, false);
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {CLV [clv]}

    fn cmp(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let data = bus.read(state.addr) as Word;
        let ac = reg.ac as Word;
        let tmp = ac.wrapping_sub(data);

        reg.p
            .set(Status::C, ac >= data)
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {CMP [cmp]}

    fn cpx(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let data = bus.read(state.addr) as Word;
        let x = reg.x as Word;
        let tmp = x.wrapping_sub(data);

        reg.p
            .set(Status::C, x >= data)
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {CPX [cpx]}

    fn cpy(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let data = bus.read(state.addr) as Word;
        let y = reg.y as Word;
        let tmp = y.wrapping_sub(data);

        reg.p
            .set(Status::C, y >= data)
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {CPY [cpy]}

    fn dec(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let data = bus.read(state.addr) as Word;
        let tmp = data.wrapping_sub(1);

        bus.write(state.addr, tmp as Byte);
        reg.p
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {DEC [dec]}

    fn dex(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.x = reg.x.wrapping_sub(1);

        reg.p
            .set(Status::Z, is_zero!(reg.x))
            .set(Status::N, is_neg!(reg.x));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {DEX [dex]}

    fn dey(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.y = reg.y.wrapping_sub(1);

        reg.p
            .set(Status::Z, is_zero!(reg.y))
            .set(Status::N, is_neg!(reg.y));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {DEY [dey]}

    fn eor(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        let data = bus.read(addr) as Word;
        let tmp = reg.ac as Word ^ data;
        reg.ac = tmp as Byte;

        reg.p
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {EOR [eor]}

    fn inc(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let data = bus.read(state.addr) as Word;
        let tmp = data.wrapping_add(1);

        bus.write(state.addr, tmp as Byte);
        reg.p
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {INC [inc]}

    fn inx(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.x = reg.x.wrapping_add(1);

        reg.p
            .set(Status::Z, is_zero!(reg.x))
            .set(Status::N, is_neg!(reg.x));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {INX [inx]}

    fn iny(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.y = reg.y.wrapping_add(1);

        reg.p
            .set(Status::Z, is_zero!(reg.y))
            .set(Status::N, is_neg!(reg.y));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {INY [iny]}

    fn jmp(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.pc = state.addr;

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {JMP [jmp]}

    fn jsr_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        bus.write(PS.wrapping_add(reg.sp as Word), (reg.pc >> 8) as Byte);
        reg.sp = reg.sp.wrapping_sub(1);
        OperationResult::None
    }

    fn jsr_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        bus.write(PS.wrapping_add(reg.sp as Word), reg.pc as Byte);
        reg.sp = reg.sp.wrapping_sub(1);
        OperationResult::None
    }

    fn jsr_02(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.pc = state.addr;

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {JSR [jsr_00, jsr_01, jsr_02]}

    fn lda(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        let data = bus.read(addr);
        reg.ac = data;
        reg.p
            .set(Status::Z, is_zero!(data))
            .set(Status::N, is_neg!(data as Word));

        state.oper = OperData::Byte(data);
        OperationResult::None
    }

    steps! {LDA [lda]}

    fn ldx(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        let data = bus.read(addr);
        reg.x = data;
        reg.p
            .set(Status::Z, is_zero!(data))
            .set(Status::N, is_neg!(data));

        state.oper = OperData::Byte(data);
        OperationResult::None
    }

    steps! {LDX [ldx]}

    fn ldy(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        let data = bus.read(addr);
        reg.y = data;
        reg.p
            .set(Status::Z, is_zero!(data))
            .set(Status::N, is_neg!(data));

        state.oper = OperData::Byte(data);
        OperationResult::None
    }

    steps! {LDY [ldy]}

    fn lsr(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        let data = bus.read(addr);
        let tmp = data >> 1;
        reg.x = data;
        reg.p
            .set(Status::C, data & 0x0001 == 0x0001)
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        match &state.addr_data {
            AddrModeData::A | AddrModeData::Imp => {
                reg.ac = tmp;
            }
            _ => {
                bus.write(addr, tmp);
            }
        }

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {LSR [lsr]}

    fn nop(
        _: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {NOP [nop]}

    fn ora(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        let data = bus.read(addr);
        let ac = reg.ac | data;

        reg.ac = ac;
        reg.p
            .set(Status::Z, is_zero!(ac))
            .set(Status::N, is_neg!(ac));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {ORA [ora]}

    fn pha_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        bus.write(PS.wrapping_add(reg.sp as Word), reg.ac);

        OperationResult::None
    }

    fn pha_01(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.sp = reg.sp.wrapping_sub(1);

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {PHA [pha_00, pha_01]}

    fn php_00(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let p = reg.p | Status::B | Status::U;
        bus.write(PS.wrapping_add(reg.sp as Word), p.into());
        reg.sp = reg.sp.wrapping_sub(1);

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {PHP [spin, php_00]}

    fn pla_00(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        reg.sp = reg.sp.wrapping_add(1);

        OperationResult::None
    }

    fn pla_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.ac = bus.read(PS.wrapping_add(reg.sp as Word));
        reg.p
            .set(Status::Z, is_zero!(reg.ac))
            .set(Status::N, is_neg!(reg.ac));

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {PLA [spin, pla_00, pla_01]}

    fn plp_00(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        reg.sp = reg.sp.wrapping_add(1);
        OperationResult::None
    }

    fn plp_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let tmp = bus.read(PS.wrapping_add(reg.sp as Word));
        reg.p = Status::from(tmp) & !(Status::U | Status::B);

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {PLP [spin, plp_00, plp_01]}

    fn rol(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        let data = bus.read(addr) as Word;
        let tmp = Word::from(reg.p.get(Status::C)) << 7 | data >> 1;
        reg.p
            .set(Status::C, tmp & 0x01 == 0x01)
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        match &state.addr_data {
            AddrModeData::A | AddrModeData::Imp => {
                reg.ac = tmp as Byte;
            }
            _ => {
                bus.write(addr, tmp as Byte);
            }
        }

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {ROL [rol]}

    fn ror(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        let data = bus.read(addr) as Word;
        let tmp = Word::from(reg.p.get(Status::C)) | data << 1;
        reg.p
            .set(Status::C, tmp & 0x01 == 0x01)
            .set(Status::Z, is_zero!(tmp))
            .set(Status::N, is_neg!(tmp));

        match &state.addr_data {
            AddrModeData::A | AddrModeData::Imp => {
                reg.ac = tmp as Byte;
            }
            _ => {
                bus.write(addr, tmp as Byte);
            }
        }

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {ROR [ror]}

    fn rti(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let mut sp = reg.sp as Word + 1;
        let status = bus.read(PS.wrapping_add(sp));
        reg.p = Status::from(status);
        reg.p &= !Status::B;
        reg.p &= !Status::U;

        sp += 1;
        let lo_pc = bus.read(PS.wrapping_add(sp)) as Word;
        sp += 1;
        let hi_pc = (bus.read(PS.wrapping_add(sp)) as Word) << 8;
        reg.pc = lo_pc | hi_pc;
        reg.sp = sp as Byte;

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {RTI [rti]}

    fn rts_00(
        _: &mut Registers,
        _: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        OperationResult::None
    }

    fn rts_01(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.sp = reg.sp.wrapping_add(1);
        state.tmp = bus.read(PS.wrapping_add(reg.sp as Word)) as Word;

        OperationResult::None
    }

    fn rts_02(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.sp = reg.sp.wrapping_add(1);
        let hi_pc = (bus.read(PS.wrapping_add(reg.sp as Word)) as Word) << 8;
        state.tmp |= hi_pc;

        OperationResult::None
    }

    fn rts_03(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.pc = state.tmp;

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {RTS [spin, rts_00, rts_01, rts_02, rts_03]}

    fn sbc(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let addr = state.addr;
        // Operating in 16-bit domain to capture carry out
        let data = bus.read(addr) as Word;
        let ac = reg.ac as Word;
        let val = data ^ LO_MASK;

        let tmp = ac + val + reg.p.get(Status::C);

        reg.p
            .set(Status::C, tmp > 255)
            .set(Status::Z, is_zero!(tmp))
            .set(Status::V, is_overflow!(ac, val, tmp))
            .set(Status::N, is_neg!(tmp));

        reg.ac = tmp as Byte;

        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {SBC [sbc]}

    fn sec(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.p.set(Status::C, true);
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {SEC [sec]}

    fn sed(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.p.set(Status::D, true);
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {SED [sed]}

    fn sei(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.p.set(Status::I, true);
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {SEI [sei]}

    fn sta(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let tmp = bus.write(state.addr, reg.ac);
        state.oper = OperData::Byte(tmp);
        OperationResult::None
    }

    steps! {STA [sta]}

    fn stx(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        let tmp = bus.write(state.addr, reg.x);
        state.oper = OperData::Byte(tmp);
        OperationResult::None
    }

    steps! {STX [stx]}

    fn sty(
        reg: &mut Registers,
        bus: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        bus.write(state.addr, reg.y);
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {STY [sty]}

    fn tax(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.x = reg.ac;
        reg.p
            .set(Status::Z, is_zero!(reg.x))
            .set(Status::N, is_neg!(reg.x));
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {TAX [tax]}

    fn tay(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.y = reg.ac;
        reg.p
            .set(Status::Z, is_zero!(reg.y))
            .set(Status::N, is_neg!(reg.y));
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {TAY [tay]}

    fn tsx(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.x = reg.sp;
        reg.p
            .set(Status::Z, is_zero!(reg.x))
            .set(Status::N, is_neg!(reg.x));
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {TSX [tsx]}

    fn txa(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.ac = reg.x;
        reg.p
            .set(Status::Z, is_zero!(reg.ac))
            .set(Status::N, is_neg!(reg.ac));
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {TXA [txa]}

    fn txs(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.sp = reg.x;
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {TXS [txs]}

    fn tya(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.ac = reg.y;
        reg.p
            .set(Status::Z, is_zero!(reg.ac))
            .set(Status::N, is_neg!(reg.ac));
        state.oper = OperData::None;
        OperationResult::None
    }

    steps! {TYA [tya]}

    // Illegal op codes

    fn jam(
        _: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        // repeatedly perform this action, effectively halt the cpu until a
        // hardware reset is performed
        state.oper = OperData::None;
        OperationResult::Skip(-1)
    }

    steps! {JAM [jam]}

    /// https://www.nesdev.org/wiki/CPU_interrupts#IRQ_and_NMI_tick-by-tick_execution
    ///
    /// IRQ and NMI tick-by-tick execution
    /// For exposition and to emphasize similarity with BRK, here's the
    /// tick-by-tick breakdown of IRQ and NMI (derived from Visual 6502). A
    /// reset also goes through the same sequence, but suppresses writes,
    /// decrementing the stack pointer thrice without modifying memory. This is
    /// why the I flag is always set on reset.
    ///
    /// #  address R/W description
    /// ```text
    /// --- ------- --- -----------------------------------------------
    /// 1    PC     R  fetch opcode (and discard it - $00 (BRK) is forced into the opcode register instead)
    /// 2    PC     R  read next instruction byte (actually the same as above, since PC increment is suppressed. Also discarded.)
    /// 3  $0100,S  W  push PCH on stack, decrement S
    /// 4  $0100,S  W  push PCL on stack, decrement S
    /// *** At this point, the signal status determines which interrupt vector is used ***
    /// 5  $0100,S  W  push P on stack (with B flag *clear*), decrement S
    /// 6   A       R  fetch PCL (A = FFFE for IRQ, A = FFFA for NMI), set I flag
    /// 7   A       R  fetch PCH (A = FFFF for IRQ, A = FFFB for NMI)
    /// ```
    fn rest_03(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        reg.sp = reg.sp.wrapping_sub(1);
        OperationResult::None
    }

    fn reset_04(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        state: &mut InstructionState,
    ) -> OperationResult {
        reg.sp = reg.sp.wrapping_sub(1);
        state.tmp = if reg.p & Status::I == Status::I {
            IRQ_LO
        } else {
            NMI_LO
        };
        OperationResult::None
    }

    fn reset_05(
        reg: &mut Registers,
        _: &mut dyn RwDevice,
        _: &mut InstructionState,
    ) -> OperationResult {
        reg.sp = reg.sp.wrapping_sub(1);
        OperationResult::None
    }

    steps! {RESET [brk_01, brk_02, rest_03, reset_04, reset_05, brk_06, brk_07]}
}

macro_rules! make_instruction {
    // ([$opc:tt, $ami:tt, $inst:tt] $op:tt $($am:tt)*) => {
    //     #[allow(dead_code)]
    //     fn $opc() -> InstructionIterator {
    //         let addr = addr_mode![$($am)*];
    //         let work = &act::$op;
    //         InstructionIterator::new(&addr, work)
    //     }

    //     am_const!([$ami] $($am)*);
    //     #[allow(dead_code)]
    //     pub const $inst: OperType = OperType::$op;
    // };

    ([$opc:tt, $ami:tt, $inst:tt] ADC $($am:tt)*) => {
        /// # ADC
		/// Add Memory to Accumulator with Carry
		///```text
		/// A + M + C -> A, C                 N  Z  C  I  D  V
		///                                   +  +  +  -  -  +
		/// addressing   assembler       opc    bytes    cycles
		/// immediate    ADC #oper       69     2        2
		/// zeropage     ADC oper        65     2        3
		/// zeropage,X   ADC oper,X      75     2        4
		/// absolute     ADC oper        6D     3        4
		/// absolute,X   ADC oper,X      7D     3        4*
		/// absolute,Y   ADC oper,Y      79     3        4*
		/// (indirect,X) ADC (oper,X)    61     2        6
		/// (indirect),Y ADC (oper),Y    71     2        5*
		/// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::ADC;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// # ADC
		/// Add Memory to Accumulator with Carry
		///```text
		/// A + M + C -> A, C                 N  Z  C  I  D  V
		///                                   +  +  +  -  -  +
		/// addressing   assembler       opc    bytes    cycles
		/// immediate    ADC #oper       69     2        2
		/// zeropage     ADC oper        65     2        3
		/// zeropage,X   ADC oper,X      75     2        4
		/// absolute     ADC oper        6D     3        4
		/// absolute,X   ADC oper,X      7D     3        4*
		/// absolute,Y   ADC oper,Y      79     3        4*
		/// (indirect,X) ADC (oper,X)    61     2        6
		/// (indirect),Y ADC (oper),Y    71     2        5*
		/// ```
        pub const $inst: OperType = OperType::ADC;
    };

    ([$opc:tt, $ami:tt, $inst:tt] AND $($am:tt)*) => {
        /// AND
		/// AND Memory with Accumulator
		///```text
		/// A AND M -> A                      N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// immediate    AND #oper       29      2       2
		/// zeropage     AND oper        25      2       3
		/// zeropage,X   AND oper,X      35      2       4
		/// absolute     AND oper        2D      3       4
		/// absolute,X   AND oper,X      3D      3       4*
		/// absolute,Y   AND oper,Y      39      3       4*
		/// (indirect,X) AND (oper,X)    21      2       6
		/// (indirect),Y AND (oper),Y    31      2       5*
		/// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::AND;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// AND
		/// AND Memory with Accumulator
		///```text
		/// A AND M -> A                      N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// immediate    AND #oper       29      2       2
		/// zeropage     AND oper        25      2       3
		/// zeropage,X   AND oper,X      35      2       4
		/// absolute     AND oper        2D      3       4
		/// absolute,X   AND oper,X      3D      3       4*
		/// absolute,Y   AND oper,Y      39      3       4*
		/// (indirect,X) AND (oper,X)    21      2       6
		/// (indirect),Y AND (oper),Y    31      2       5*
		/// ```
        pub const $inst: OperType = OperType::AND;
    };

    ([$opc:tt, $ami:tt, $inst:tt] ASL $($am:tt)*) => {
        //// ASL
        /// Shift Left One Bit (Memory or Accumulator)
        ///```text
        /// C <- [76543210] <- 0              N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// accumulator  ASL A           0A      1       2
        /// zeropage     ASL oper        06      2       5
        /// zeropage,X   ASL oper,X      16      2       6
        /// absolute     ASL oper        0E      3       6
        /// absolute,X   ASL oper,X      1E      3       7
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::ASL;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        //// ASL
        /// Shift Left One Bit (Memory or Accumulator)
        ///```text
        /// C <- [76543210] <- 0              N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// accumulator  ASL A           0A      1       2
        /// zeropage     ASL oper        06      2       5
        /// zeropage,X   ASL oper,X      16      2       6
        /// absolute     ASL oper        0E      3       6
        /// absolute,X   ASL oper,X      1E      3       7
        /// ```
        pub const $inst: OperType = OperType::ASL;
    };

    ([$opc:tt, $ami:tt, $inst:tt] BCC $($am:tt)*) => {
        /// BCC
        /// Branch on Carry Clear
        ///```text
        /// branch on C = 0                   N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// relative     BCC oper        90      2       2**
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::BCC;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// BCC
        /// Branch on Carry Clear
        ///```text
        /// branch on C = 0                   N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// relative     BCC oper        90      2       2**
        /// ```
        pub const $inst: OperType = OperType::BCC;
    };

    ([$opc:tt, $ami:tt, $inst:tt] BCS $($am:tt)*) => {
        /// BCS
        /// Branch on Carry Set
        ///```text
        /// branch on C = 1                         N Z C I D V
        ///                                         - - - - - -
        /// addressing   assembler       opc     bytes   cycles
        /// relative     BCS oper        B0      2       2**
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::BCS;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// BCS
        /// Branch on Carry Set
        ///```text
        /// branch on C = 1                         N Z C I D V
        ///                                         - - - - - -
        /// addressing   assembler       opc     bytes   cycles
        /// relative     BCS oper        B0      2       2**
        /// ```
        pub const $inst: OperType = OperType::BCS;
    };

    ([$opc:tt, $ami:tt, $inst:tt] BEQ $($am:tt)*) => {
       /// BEQ
        /// Branch on Result Zero
        ///```text
        /// branch on Z = 1                   N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// relative     BEQ oper        F0      2       2**
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::BEQ;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// BEQ
        /// Branch on Result Zero
        ///```text
        /// branch on Z = 1                   N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// relative     BEQ oper        F0      2       2**
        /// ```
        pub const $inst: OperType = OperType::BEQ;
    };

    ([$opc:tt, $ami:tt, $inst:tt] BIT $($am:tt)*) => {
        /// BIT
        /// Test Bits in Memory with Accumulator
        ///
        /// bits 7 and 6 of operand are transfered to bit 7 and 6 of SR (N,V);
        /// the zero-flag is set according to the result of the operand AND
        /// the accumulator (set, if the result is zero, unset otherwise).
        /// This allows a quick check of a few bits at once without affecting
        /// any of the registers, other than the status register (SR).
        ///```text
        /// A AND M, M7 -> N, M6 -> V         N  Z  C  I  D  V
        ///                                   M7 +  -  -  -  M6
        /// addressing   assembler       opc     bytes   cycles
        /// zeropage     BIT oper        24      2       3
        /// absolute     BIT oper        2C      3       4
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::BIT;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// BIT
        /// Test Bits in Memory with Accumulator
        ///
        /// bits 7 and 6 of operand are transfered to bit 7 and 6 of SR (N,V);
        /// the zero-flag is set according to the result of the operand AND
        /// the accumulator (set, if the result is zero, unset otherwise).
        /// This allows a quick check of a few bits at once without affecting
        /// any of the registers, other than the status register (SR).
        ///```text
        /// A AND M, M7 -> N, M6 -> V         N  Z  C  I  D  V
        ///                                   M7 +  -  -  -  M6
        /// addressing   assembler       opc     bytes   cycles
        /// zeropage     BIT oper        24      2       3
        /// absolute     BIT oper        2C      3       4
        /// ```
        pub const $inst: OperType = OperType::BIT;
    };

    ([$opc:tt, $ami:tt, $inst:tt] BMI $($am:tt)*) => {
        /// BMI
        /// Branch on Result Minus
        ///```text
        /// branch on N = 1                   N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// relative     BMI oper        30      2       2**
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::BMI;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// BMI
        /// Branch on Result Minus
        ///```text
        /// branch on N = 1                   N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// relative     BMI oper        30      2       2**
        /// ```
        pub const $inst: OperType = OperType::BMI;
    };

    ([$opc:tt, $ami:tt, $inst:tt] BNE $($am:tt)*) => {
        /// BNE
        /// Branch on Result not Zero
        ///```text
        /// branch on Z = 0                   N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// relative     BNE oper        D0      2       2**
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::BNE;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// BNE
        /// Branch on Result not Zero
        ///```text
        /// branch on Z = 0                   N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// relative     BNE oper        D0      2       2**
        /// ```
        pub const $inst: OperType = OperType::BNE;
    };

    ([$opc:tt, $ami:tt, $inst:tt] BPL $($am:tt)*) => {
        /// BPL
        /// Branch on Result Plus
        ///```text
        /// branch on N = 0                   N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// relative     BPL oper        10      2       2**
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::BPL;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// BPL
        /// Branch on Result Plus
        ///```text
        /// branch on N = 0                   N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// relative     BPL oper        10      2       2**
        /// ```
        pub const $inst: OperType = OperType::BPL;
    };

    ([$opc:tt, $ami:tt, $inst:tt] BRK $($am:tt)*) => {
        /// BRK
		/// Force Break
		///
		/// BRK initiates a software interrupt similar to a hardware
		/// interrupt (IRQ). The return address pushed to the stack is
		/// PC+2, providing an extra byte of spacing for a break mark
		/// (identifying a reason for the break.)
		/// The status register will be pushed to the stack with the break
		/// flag set to 1. However, when retrieved during RTI or by a PLP
		/// instruction, the break flag will be ignored.
		/// The interrupt disable flag is not set automatically.
		///```text
		/// interrupt,                        N  Z  C  I  D  V
		/// push PC+2, push SR                -  -  -  1  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// implied      BRK             00      1       7
		/// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::BRK;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// BRK
		/// Force Break
		///
		/// BRK initiates a software interrupt similar to a hardware
		/// interrupt (IRQ). The return address pushed to the stack is
		/// PC+2, providing an extra byte of spacing for a break mark
		/// (identifying a reason for the break.)
		/// The status register will be pushed to the stack with the break
		/// flag set to 1. However, when retrieved during RTI or by a PLP
		/// instruction, the break flag will be ignored.
		/// The interrupt disable flag is not set automatically.
		///```text
		/// interrupt,                        N  Z  C  I  D  V
		/// push PC+2, push SR                -  -  -  1  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// implied      BRK             00      1       7
		/// ```
        pub const $inst: OperType = OperType::BRK;
    };

    ([$opc:tt, $ami:tt, $inst:tt] BVC $($am:tt)*) => {
        /// BVC
		/// Branch on Overflow Clear
		///```text
		/// branch on V = 0                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BVC oper        50      2       2**
		/// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::BVC;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// BVC
		/// Branch on Overflow Clear
		///```text
		/// branch on V = 0                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BVC oper        50      2       2**
		/// ```
        pub const $inst: OperType = OperType::BVC;
    };

    ([$opc:tt, $ami:tt, $inst:tt] BVS $($am:tt)*) => {
        /// BVS
		/// Branch on Overflow Set
		///```text
		/// branch on V = 1                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BVS oper        70      2       2**
		/// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::BVS;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// BVS
		/// Branch on Overflow Set
		///```text
		/// branch on V = 1                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BVS oper        70      2       2**
		/// ```
        pub const $inst: OperType = OperType::BVS;
    };

    ([$opc:tt, $ami:tt, $inst:tt] CLC $($am:tt)*) => {
        /// CLC
        /// Clear Carry Flag
        ///```text
        /// 0 -> C                            N  Z  C  I  D  V
        ///                                   -  -  0  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// implied      CLC             18      1       2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::CLC;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// CLC
        /// Clear Carry Flag
        ///```text
        /// 0 -> C                            N  Z  C  I  D  V
        ///                                   -  -  0  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// implied      CLC             18      1       2
        /// ```
        pub const $inst: OperType = OperType::CLC;
    };

    ([$opc:tt, $ami:tt, $inst:tt] CLD $($am:tt)*) => {
        /// CLD
        /// Clear Decimal Mode
        ///```text
        /// 0 -> D                            N  Z  C  I  D  V
        ///                                   -  -  -  -  0  -
        /// addressing   assembler       opc     bytes   cycles
        /// implied      CLD             D8      1       2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::CLD;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// CLD
        /// Clear Decimal Mode
        ///```text
        /// 0 -> D                            N  Z  C  I  D  V
        ///                                   -  -  -  -  0  -
        /// addressing   assembler       opc     bytes   cycles
        /// implied      CLD             D8      1       2
        /// ```
        pub const $inst: OperType = OperType::CLD;
    };

    ([$opc:tt, $ami:tt, $inst:tt] CLI $($am:tt)*) => {
        /// CLI
        /// Clear Interrupt Disable Bit
        ///```text
        /// 0 -> I                            N  Z  C  I  D  V
        ///                                   -  -  -  0  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// implied      CLI             58      1       2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::CLI;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// CLI
        /// Clear Interrupt Disable Bit
        ///```text
        /// 0 -> I                            N  Z  C  I  D  V
        ///                                   -  -  -  0  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// implied      CLI             58      1       2
        /// ```
        pub const $inst: OperType = OperType::CLI;
    };

    ([$opc:tt, $ami:tt, $inst:tt] CLV $($am:tt)*) => {
        /// CLV
        /// Clear Interrupt Disable Bit
        ///```text
        /// 0 -> I                            N  Z  C  I  D  V
        ///                                   -  -  -  -  -  0
        /// addressing   assembler       opc     bytes   cycles
        /// implied      CLV             B8      1       2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::CLV;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// CLV
        /// Clear Interrupt Disable Bit
        ///```text
        /// 0 -> I                            N  Z  C  I  D  V
        ///                                   -  -  -  -  -  0
        /// addressing   assembler       opc     bytes   cycles
        /// implied      CLV             B8      1       2
        /// ```
        pub const $inst: OperType = OperType::CLV;
    };

    ([$opc:tt, $ami:tt, $inst:tt] CMP $($am:tt)*) => {
        /// CMP
        /// Compare Memory with Accumulator
        ///```text
        /// A - M                             N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// immediate    CMP #oper       C9      2       2
        /// zeropage     CMP oper        C5      2       3
        /// zeropage,X   CMP oper,X      D5      2       4
        /// absolute     CMP oper        CD      3       4
        /// absolute,X   CMP oper,X      DD      3       4*
        /// absolute,Y   CMP oper,Y      D9      3       4*
        /// (indirect,X) CMP (oper,X)    C1      2       6
        /// (indirect),Y CMP (oper),Y    D1      2       5*
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::CMP;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// CMP
        /// Compare Memory with Accumulator
        ///```text
        /// A - M                             N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// immediate    CMP #oper       C9      2       2
        /// zeropage     CMP oper        C5      2       3
        /// zeropage,X   CMP oper,X      D5      2       4
        /// absolute     CMP oper        CD      3       4
        /// absolute,X   CMP oper,X      DD      3       4*
        /// absolute,Y   CMP oper,Y      D9      3       4*
        /// (indirect,X) CMP (oper,X)    C1      2       6
        /// (indirect),Y CMP (oper),Y    D1      2       5*
        /// ```
        pub const $inst: OperType = OperType::CMP;
    };

    ([$opc:tt, $ami:tt, $inst:tt] CPX $($am:tt)*) => {
        /// CPX
        /// Compare Memory and Index X
        ///```text
        /// X - M                             N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// immediate    CPX #oper       E0      2       2
        /// zeropage     CPX oper        E4      2       3
        /// absolute     CPX oper        EC      3       4
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::CPX;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// CPX
        /// Compare Memory and Index X
        ///```text
        /// X - M                             N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// immediate    CPX #oper       E0      2       2
        /// zeropage     CPX oper        E4      2       3
        /// absolute     CPX oper        EC      3       4
        /// ```
        pub const $inst: OperType = OperType::CPX;
    };

    ([$opc:tt, $ami:tt, $inst:tt] CPY $($am:tt)*) => {
        /// CPY
        /// Compare Memory and Index Y
        ///```text
        /// Y - M                             N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// immediate    CPY #oper       C0      2       2
        /// zeropage     CPY oper        C4      2       3
        /// absolute     CPY oper        CC      3       4
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::CPY;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// CPY
        /// Compare Memory and Index Y
        ///```text
        /// Y - M                             N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// immediate    CPY #oper       C0      2       2
        /// zeropage     CPY oper        C4      2       3
        /// absolute     CPY oper        CC      3       4
        /// ```
        pub const $inst: OperType = OperType::CPY;
    };

    ([$opc:tt, $ami:tt, $inst:tt] DEC $($am:tt)*) => {
        /// DEC
        /// Decrement Memory by One
        ///```text
        /// M - 1 -> M                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// zeropage     DEC oper        C6      2       5
        /// zeropage,X   DEC oper,X      D6      2       6
        /// absolute     DEC oper        CE      3       6
        /// absolute,X   DEC oper,X      DE      3       7
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::DEC;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// DEC
        /// Decrement Memory by One
        ///```text
        /// M - 1 -> M                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// zeropage     DEC oper        C6      2       5
        /// zeropage,X   DEC oper,X      D6      2       6
        /// absolute     DEC oper        CE      3       6
        /// absolute,X   DEC oper,X      DE      3       7
        /// ```
        pub const $inst: OperType = OperType::DEC;
    };

    ([$opc:tt, $ami:tt, $inst:tt] DEX $($am:tt)*) => {
        /// DEX
        /// Decrement Index X by One
        ///```text
        /// X - 1 -> X                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// implied      DEX             CA      1       2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::DEX;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// DEX
        /// Decrement Index X by One
        ///```text
        /// X - 1 -> X                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// implied      DEX             CA      1       2
        /// ```
        pub const $inst: OperType = OperType::DEX;
    };

    ([$opc:tt, $ami:tt, $inst:tt] DEY $($am:tt)*) => {
        /// DEY
        /// Decrement Index Y by One
        ///```text
        /// Y - 1 -> Y                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// implied      DEY             88      1       2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::DEY;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// DEY
        /// Decrement Index Y by One
        ///```text
        /// Y - 1 -> Y                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc     bytes   cycles
        /// implied      DEY             88      1       2
        /// ```
        pub const $inst: OperType = OperType::DEY;
    };

    ([$opc:tt, $ami:tt, $inst:tt] EOR $($am:tt)*) => {
        /// EOR
        /// Exclusive-OR Memory with Accumulator
        ///```text
        /// A EOR M -> A                      N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    EOR #oper       49     2        2
        /// zeropage     EOR oper        45     2        3
        /// zeropage,X   EOR oper,X      55     2        4
        /// absolute     EOR oper        4D     3        4
        /// absolute,X   EOR oper,X      5D     3        4*
        /// absolute,Y   EOR oper,Y      59     3        4*
        /// (indirect,X) EOR (oper,X)    41     2        6
        /// (indirect),Y EOR (oper),Y    51     2        5*
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::EOR;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// EOR
        /// Exclusive-OR Memory with Accumulator
        ///```text
        /// A EOR M -> A                      N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    EOR #oper       49     2        2
        /// zeropage     EOR oper        45     2        3
        /// zeropage,X   EOR oper,X      55     2        4
        /// absolute     EOR oper        4D     3        4
        /// absolute,X   EOR oper,X      5D     3        4*
        /// absolute,Y   EOR oper,Y      59     3        4*
        /// (indirect,X) EOR (oper,X)    41     2        6
        /// (indirect),Y EOR (oper),Y    51     2        5*
        /// ```
        pub const $inst: OperType = OperType::EOR;
    };

    ([$opc:tt, $ami:tt, $inst:tt] INC $($am:tt)*) => {
        /// INC
        /// Increment Memory by One
        ///```text
        /// M + 1 -> M                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// zeropage     INC oper        E6     2        5
        /// zeropage,X   INC oper,X      F6     2        6
        /// absolute     INC oper        EE     3        6
        /// absolute,X   INC oper,X      FE     3        7
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::INC;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// INC
        /// Increment Memory by One
        ///```text
        /// M + 1 -> M                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// zeropage     INC oper        E6     2        5
        /// zeropage,X   INC oper,X      F6     2        6
        /// absolute     INC oper        EE     3        6
        /// absolute,X   INC oper,X      FE     3        7
        /// ```
        pub const $inst: OperType = OperType::INC;
    };

    ([$opc:tt, $ami:tt, $inst:tt] INX $($am:tt)*) => {
        /// INX
        /// Increment Index X by One
        ///```text
        /// X + 1 -> X                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      INX             E8     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::INX;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// INX
        /// Increment Index X by One
        ///```text
        /// X + 1 -> X                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      INX             E8     1        2
        /// ```
        pub const $inst: OperType = OperType::INX;
    };

    ([$opc:tt, $ami:tt, $inst:tt] INY $($am:tt)*) => {
        /// INY
        /// Increment Index Y by One
        ///```text
        /// Y + 1 -> Y                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      INY             C8     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::INY;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// INY
        /// Increment Index Y by One
        ///```text
        /// Y + 1 -> Y                        N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      INY             C8     1        2
        /// ```
        pub const $inst: OperType = OperType::INY;
    };

    ([$opc:tt, $ami:tt, $inst:tt] JMP $($am:tt)*) => {
        /// JMP
        /// Jump to New Location
        ///```text
        /// push (PC+2),                      N  Z  C  I  D  V
        /// (PC+1) -> PCL                     -  -  -  -  -  -
        /// (PC+2) -> PCH
        /// addressing   assembler       opc    bytes    cycles
        /// absolute     JMP oper        4C     3        3
        /// indirect     JMP (oper)      6C     3        5
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![JMP $($am)*];
            let work = &act::JMP;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// JMP
        /// Jump to New Location
        ///```text
        /// push (PC+2),                      N  Z  C  I  D  V
        /// (PC+1) -> PCL                     -  -  -  -  -  -
        /// (PC+2) -> PCH
        /// addressing   assembler       opc    bytes    cycles
        /// absolute     JMP oper        4C     3        3
        /// indirect     JMP (oper)      6C     3        5
        /// ```
        pub const $inst: OperType = OperType::JMP;
    };

    ([$opc:tt, $ami:tt, $inst:tt] JSR $($am:tt)*) => {
        /// JSR
        /// Jump to New Location Saving Return Address
        ///```text
        /// push (PC+2),                      N  Z  C  I  D  V
        /// (PC+1) -> PCL                     -  -  -  -  -  -
        /// (PC+2) -> PCH
        /// addressing   assembler       opc    bytes    cycles
        /// absolute     JSR oper        20     3        6
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::JSR;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// JSR
        /// Jump to New Location Saving Return Address
        ///```text
        /// push (PC+2),                      N  Z  C  I  D  V
        /// (PC+1) -> PCL                     -  -  -  -  -  -
        /// (PC+2) -> PCH
        /// addressing   assembler       opc    bytes    cycles
        /// absolute     JSR oper        20     3        6
        /// ```
        pub const $inst: OperType = OperType::JSR;
    };

    ([$opc:tt, $ami:tt, $inst:tt] LDA $($am:tt)*) => {
        /// LDA
        /// Load Accumulator with Memory
        ///```text
        /// M -> A                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    LDA #oper       A9     2        2
        /// zeropage     LDA oper        A5     2        3
        /// zeropage,X   LDA oper,X      B5     2        4
        /// absolute     LDA oper        AD     3        4
        /// absolute,X   LDA oper,X      BD     3        4*
        /// absolute,Y   LDA oper,Y      B9     3        4*
        /// (indirect,X) LDA (oper,X)    A1     2        6
        /// (indirect),Y LDA (oper),Y    B1     2        5*
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::LDA;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// LDA
        /// Load Accumulator with Memory
        ///```text
        /// M -> A                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    LDA #oper       A9     2        2
        /// zeropage     LDA oper        A5     2        3
        /// zeropage,X   LDA oper,X      B5     2        4
        /// absolute     LDA oper        AD     3        4
        /// absolute,X   LDA oper,X      BD     3        4*
        /// absolute,Y   LDA oper,Y      B9     3        4*
        /// (indirect,X) LDA (oper,X)    A1     2        6
        /// (indirect),Y LDA (oper),Y    B1     2        5*
        /// ```
        pub const $inst: OperType = OperType::LDA;
    };

    ([$opc:tt, $ami:tt, $inst:tt] LDX $($am:tt)*) => {
        /// LDX
        /// Load Index X with Memory
        ///```text
        /// M -> X                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    LDX #oper       A2     2        2
        /// zeropage     LDX oper        A6     2        3
        /// zeropage,Y   LDX oper,Y      B6     2        4
        /// absolute     LDX oper        AE     3        4
        /// absolute,Y   LDX oper,Y      BE     3        4*
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::LDX;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// LDX
        /// Load Index X with Memory
        ///```text
        /// M -> X                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    LDX #oper       A2     2        2
        /// zeropage     LDX oper        A6     2        3
        /// zeropage,Y   LDX oper,Y      B6     2        4
        /// absolute     LDX oper        AE     3        4
        /// absolute,Y   LDX oper,Y      BE     3        4*
        /// ```
        pub const $inst: OperType = OperType::LDX;
    };

    ([$opc:tt, $ami:tt, $inst:tt] LDY $($am:tt)*) => {
        /// LDY
        /// Load Index Y with Memory
        ///```text
        /// M -> Y                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    LDY #oper       A0     2        2
        /// zeropage     LDY oper        A4     2        3
        /// zeropage,X   LDY oper,X      B4     2        4
        /// absolute     LDY oper        AC     3        4
        /// absolute,X   LDY oper,X      BC     3        4*
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::LDY;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// LDY
        /// Load Index Y with Memory
        ///```text
        /// M -> Y                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    LDY #oper       A0     2        2
        /// zeropage     LDY oper        A4     2        3
        /// zeropage,X   LDY oper,X      B4     2        4
        /// absolute     LDY oper        AC     3        4
        /// absolute,X   LDY oper,X      BC     3        4*
        /// ```
        pub const $inst: OperType = OperType::LDY;
    };

    ([$opc:tt, $ami:tt, $inst:tt] LSR $($am:tt)*) => {
        /// LSR
        /// Shift One Bit Right (Memory or Accumulator)
        ///```text
        /// 0 -> [76543210] -> C              N  Z  C  I  D  V
        ///                                   0  +  +  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// accumulator  LSR A           4A     1        2
        /// zeropage     LSR oper        46     2        5
        /// zeropage,X   LSR oper,X      56     2        6
        /// absolute     LSR oper        4E     3        6
        /// absolute,X   LSR oper,X      5E     3        7
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::LSR;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// LSR
        /// Shift One Bit Right (Memory or Accumulator)
        ///```text
        /// 0 -> [76543210] -> C              N  Z  C  I  D  V
        ///                                   0  +  +  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// accumulator  LSR A           4A     1        2
        /// zeropage     LSR oper        46     2        5
        /// zeropage,X   LSR oper,X      56     2        6
        /// absolute     LSR oper        4E     3        6
        /// absolute,X   LSR oper,X      5E     3        7
        /// ```
        pub const $inst: OperType = OperType::LSR;
    };

    ([$opc:tt, $ami:tt, $inst:tt] NOP $($am:tt)*) => {
        /// NOP
        /// No Operation
        ///```text
        /// ---                               N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      NOP             EA     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::NOP;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// NOP
        /// No Operation
        ///```text
        /// ---                               N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      NOP             EA     1        2
        /// ```
        pub const $inst: OperType = OperType::NOP;
    };

    ([$opc:tt, $ami:tt, $inst:tt] ORA $($am:tt)*) => {
        /// ORA
        /// OR Memory with Accumulator
        ///```text
        /// A OR M -> A                       N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    ORA #oper       09     2        2
        /// zeropage     ORA oper        05     2        3
        /// zeropage,X   ORA oper,X      15     2        4
        /// absolute     ORA oper        0D     3        4
        /// absolute,X   ORA oper,X      1D     3        4*
        /// absolute,Y   ORA oper,Y      19     3        4*
        /// (indirect,X) ORA (oper,X)    01     2        6
        /// (indirect),Y ORA (oper),Y    11     2        5*
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::ORA;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// ORA
        /// OR Memory with Accumulator
        ///```text
        /// A OR M -> A                       N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    ORA #oper       09     2        2
        /// zeropage     ORA oper        05     2        3
        /// zeropage,X   ORA oper,X      15     2        4
        /// absolute     ORA oper        0D     3        4
        /// absolute,X   ORA oper,X      1D     3        4*
        /// absolute,Y   ORA oper,Y      19     3        4*
        /// (indirect,X) ORA (oper,X)    01     2        6
        /// (indirect),Y ORA (oper),Y    11     2        5*
        /// ```
        pub const $inst: OperType = OperType::ORA;
    };

    ([$opc:tt, $ami:tt, $inst:tt] PHA $($am:tt)*) => {
        /// PHA
        /// Push Accumulator on Stack
        ///```text
        /// push A                            N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      PHA             48     1        3
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::PHA;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// PHA
        /// Push Accumulator on Stack
        ///```text
        /// push A                            N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      PHA             48     1        3
        /// ```
        pub const $inst: OperType = OperType::PHA;
    };

    ([$opc:tt, $ami:tt, $inst:tt] PHP $($am:tt)*) => {
        /// PHP
        /// Push Processor Status on Stack
        ///
        /// The status register will be pushed with the break
        /// flag and bit 5 set to 1.
        ///```text
        /// push SR                           N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      PHP             08     1        3
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::PHP;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// PHP
        /// Push Processor Status on Stack
        ///
        /// The status register will be pushed with the break
        /// flag and bit 5 set to 1.
        ///```text
        /// push SR                           N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      PHP             08     1        3
        /// ```
        pub const $inst: OperType = OperType::PHP;
    };

    ([$opc:tt, $ami:tt, $inst:tt] PLA $($am:tt)*) => {
        /// PLA
        /// Pull Accumulator from Stack
        ///```text
        /// pull A                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      PLA             68     1        4
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::PLA;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// PLA
        /// Pull Accumulator from Stack
        ///```text
        /// pull A                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      PLA             68     1        4
        /// ```
        pub const $inst: OperType = OperType::PLA;
    };

    ([$opc:tt, $ami:tt, $inst:tt] PLP $($am:tt)*) => {
        /// PLP
        /// Pull Processor Status from Stack
        ///
        /// The status register will be pulled with the break
        /// flag and bit 5 ignored.
        ///```text
        /// pull SR                           N  Z  C  I  D  V
        ///                                      from stack
        /// addressing   assembler       opc    bytes    cycles
        /// implied      PLP             28     1        4
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::PLP;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// PLP
        /// Pull Processor Status from Stack
        ///
        /// The status register will be pulled with the break
        /// flag and bit 5 ignored.
        ///```text
        /// pull SR                           N  Z  C  I  D  V
        ///                                      from stack
        /// addressing   assembler       opc    bytes    cycles
        /// implied      PLP             28     1        4
        /// ```
        pub const $inst: OperType = OperType::PLP;
    };

    ([$opc:tt, $ami:tt, $inst:tt] ROL $($am:tt)*) => {
        /// ROL
        /// Rotate One Bit Left (Memory or Accumulator)
        ///```text
        /// C <- [76543210] <- C              N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// accumulator  ROL A           2A     1        2
        /// zeropage     ROL oper        26     2        5
        /// zeropage,X   ROL oper,X      36     2        6
        /// absolute     ROL oper        2E     3        6
        /// absolute,X   ROL oper,X      3E     3        7
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::ROL;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// ROL
        /// Rotate One Bit Left (Memory or Accumulator)
        ///```text
        /// C <- [76543210] <- C              N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// accumulator  ROL A           2A     1        2
        /// zeropage     ROL oper        26     2        5
        /// zeropage,X   ROL oper,X      36     2        6
        /// absolute     ROL oper        2E     3        6
        /// absolute,X   ROL oper,X      3E     3        7
        /// ```
        pub const $inst: OperType = OperType::ROL;
    };

    ([$opc:tt, $ami:tt, $inst:tt] ROR $($am:tt)*) => {
        /// ROR
        /// Rotate One Bit Right (Memory or Accumulator)
        ///```text
        /// C -> [76543210] -> C              N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// accumulator  ROR A           6A     1        2
        /// zeropage     ROR oper        66     2        5
        /// zeropage,X   ROR oper,X      76     2        6
        /// absolute     ROR oper        6E     3        6
        /// absolute,X   ROR oper,X      7E     3        7
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::ROR;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// ROR
        /// Rotate One Bit Right (Memory or Accumulator)
        ///```text
        /// C -> [76543210] -> C              N  Z  C  I  D  V
        ///                                   +  +  +  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// accumulator  ROR A           6A     1        2
        /// zeropage     ROR oper        66     2        5
        /// zeropage,X   ROR oper,X      76     2        6
        /// absolute     ROR oper        6E     3        6
        /// absolute,X   ROR oper,X      7E     3        7
        /// ```
        pub const $inst: OperType = OperType::ROR;
    };

    ([$opc:tt, $ami:tt, $inst:tt] RTI $($am:tt)*) => {
        /// RTI
        /// Return from Interrupt
        ///
        /// The status register is pulled with the break flag
        /// and bit 5 ignored. Then PC is pulled from the stack.
        ///```text
        /// pull SR, pull PC                  N  Z  C  I  D  V
        ///                                      from stack
        /// addressing   assembler       opc    bytes    cycles
        /// implied      RTI             40     1        6
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::RTI;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// RTI
        /// Return from Interrupt
        ///
        /// The status register is pulled with the break flag
        /// and bit 5 ignored. Then PC is pulled from the stack.
        ///```text
        /// pull SR, pull PC                  N  Z  C  I  D  V
        ///                                      from stack
        /// addressing   assembler       opc    bytes    cycles
        /// implied      RTI             40     1        6
        /// ```
        pub const $inst: OperType = OperType::RTI;
    };

    ([$opc:tt, $ami:tt, $inst:tt] RTS $($am:tt)*) => {
        /// RTS
        /// Return from Subroutine
        ///```text
        /// pull PC, PC+1 -> PC               N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      RTS             60     1        6
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::RTS;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// RTS
        /// Return from Subroutine
        ///```text
        /// pull PC, PC+1 -> PC               N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      RTS             60     1        6
        /// ```
        pub const $inst: OperType = OperType::RTS;
    };

    ([$opc:tt, $ami:tt, $inst:tt] SBC $($am:tt)*) => {
        /// SBC
        /// Subtract Memory from Accumulator with Borrow
        ///```text
        /// A - M - C̅ -> A                   N  Z  C  I  D  V
        ///                                   +  +  +  -  -  +
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    SBC #oper       E9     2        2
        /// zeropage     SBC oper        E5     2        3
        /// zeropage,X   SBC oper,X      F5     2        4
        /// absolute     SBC oper        ED     3        4
        /// absolute,X   SBC oper,X      FD     3        4*
        /// absolute,Y   SBC oper,Y      F9     3        4*
        /// (indirect,X) SBC (oper,X)    E1     2        6
        /// (indirect),Y SBC (oper),Y    F1     2        5*
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::SBC;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// SBC
        /// Subtract Memory from Accumulator with Borrow
        ///```text
        /// A - M - C̅ -> A                   N  Z  C  I  D  V
        ///                                   +  +  +  -  -  +
        /// addressing   assembler       opc    bytes    cycles
        /// immediate    SBC #oper       E9     2        2
        /// zeropage     SBC oper        E5     2        3
        /// zeropage,X   SBC oper,X      F5     2        4
        /// absolute     SBC oper        ED     3        4
        /// absolute,X   SBC oper,X      FD     3        4*
        /// absolute,Y   SBC oper,Y      F9     3        4*
        /// (indirect,X) SBC (oper,X)    E1     2        6
        /// (indirect),Y SBC (oper),Y    F1     2        5*
        /// ```
        pub const $inst: OperType = OperType::SBC;
    };

    ([$opc:tt, $ami:tt, $inst:tt] SEC $($am:tt)*) => {
        /// SEC
        /// Set Carry Flag
        ///```text
        /// 1 -> C                            N  Z  C  I  D  V
        ///                                   -  -  1  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      SEC             38     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::SEC;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// SEC
        /// Set Carry Flag
        ///```text
        /// 1 -> C                            N  Z  C  I  D  V
        ///                                   -  -  1  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      SEC             38     1        2
        /// ```
        pub const $inst: OperType = OperType::SEC;
    };

    ([$opc:tt, $ami:tt, $inst:tt] SED $($am:tt)*) => {
        /// SED
        /// Set Decimal Flag
        ///```text
        /// 1 -> D                            N  Z  C  I  D  V
        ///                                   -  -  -  -  1  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      SED             F8     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::SED;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// SED
        /// Set Decimal Flag
        ///```text
        /// 1 -> D                            N  Z  C  I  D  V
        ///                                   -  -  -  -  1  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      SED             F8     1        2
        /// ```
        pub const $inst: OperType = OperType::SED;
    };

    ([$opc:tt, $ami:tt, $inst:tt] SEI $($am:tt)*) => {
        /// SEI
        /// Set Interrupt Disable Status
        ///```text
        /// 1 -> I                            N  Z  C  I  D  V
        ///                                   -  -  -  1  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      SEI             78     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::SEI;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// SEI
        /// Set Interrupt Disable Status
        ///```text
        /// 1 -> I                            N  Z  C  I  D  V
        ///                                   -  -  -  1  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      SEI             78     1        2
        /// ```
        pub const $inst: OperType = OperType::SEI;
    };

    ([$opc:tt, $ami:tt, $inst:tt] STA $($am:tt)*) => {
        /// STA
        /// Store Accumulator in Memory
        ///```text
        /// A -> M                            N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// zeropage     STA oper        85     2        3
        /// zeropage,X   STA oper,X      95     2        4
        /// absolute     STA oper        8D     3        4
        /// absolute,X   STA oper,X      9D     3        5
        /// absolute,Y   STA oper,Y      99     3        5
        /// (indirect,X) STA (oper,X)    81     2        6
        /// (indirect),Y STA (oper),Y    91     2        6
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::STA;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// STA
        /// Store Accumulator in Memory
        ///```text
        /// A -> M                            N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// zeropage     STA oper        85     2        3
        /// zeropage,X   STA oper,X      95     2        4
        /// absolute     STA oper        8D     3        4
        /// absolute,X   STA oper,X      9D     3        5
        /// absolute,Y   STA oper,Y      99     3        5
        /// (indirect,X) STA (oper,X)    81     2        6
        /// (indirect),Y STA (oper),Y    91     2        6
        /// ```
        pub const $inst: OperType = OperType::STA;
    };

    ([$opc:tt, $ami:tt, $inst:tt] STX $($am:tt)*) => {
        /// STX
        /// Store Index X in Memory
        ///```text
        /// X -> M                            N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// zeropage     STX oper        86     2        3
        /// zeropage,Y   STX oper,Y      96     2        4
        /// absolute     STX oper        8E     3        4
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::STX;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// STX
        /// Store Index X in Memory
        ///```text
        /// X -> M                            N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// zeropage     STX oper        86     2        3
        /// zeropage,Y   STX oper,Y      96     2        4
        /// absolute     STX oper        8E     3        4
        /// ```
        pub const $inst: OperType = OperType::STX;
    };

    ([$opc:tt, $ami:tt, $inst:tt] STY $($am:tt)*) => {
        /// STY
        /// Sore Index Y in Memory
        ///```text
        /// Y -> M                            N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// zeropage     STY oper        84     2        3
        /// zeropage,X   STY oper,X      94     2        4
        /// absolute     STY oper        8C     3        4
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::STY;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// STY
        /// Sore Index Y in Memory
        ///```text
        /// Y -> M                            N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// zeropage     STY oper        84     2        3
        /// zeropage,X   STY oper,X      94     2        4
        /// absolute     STY oper        8C     3        4
        /// ```
        pub const $inst: OperType = OperType::STY;
    };

    ([$opc:tt, $ami:tt, $inst:tt] TAX $($am:tt)*) => {
        /// TAX
        /// Transfer Accumulator to Index X
        ///```text
        /// A -> X                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TAX             AA     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::TAX;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// TAX
        /// Transfer Accumulator to Index X
        ///```text
        /// A -> X                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TAX             AA     1        2
        /// ```
        pub const $inst: OperType = OperType::TAX;
    };

    ([$opc:tt, $ami:tt, $inst:tt] TAY $($am:tt)*) => {
        /// TAY
        /// Transfer Accumulator to Index Y
        ///```text
        /// A -> Y                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TAY             A8     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::TAY;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// TAY
        /// Transfer Accumulator to Index Y
        ///```text
        /// A -> Y                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TAY             A8     1        2
        /// ```
        pub const $inst: OperType = OperType::TAY;
    };

    ([$opc:tt, $ami:tt, $inst:tt] TSX $($am:tt)*) => {
        /// TSX
        /// Transfer Stack Pointer to Index X
        ///```text
        /// SP -> X                           N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TSX             BA     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::TSX;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// TSX
        /// Transfer Stack Pointer to Index X
        ///```text
        /// SP -> X                           N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TSX             BA     1        2
        /// ```
        pub const $inst: OperType = OperType::TSX;
    };

    ([$opc:tt, $ami:tt, $inst:tt] TXA $($am:tt)*) => {
        /// TXA
        /// Transfer Index X to Accumulator
        ///```text
        /// X -> A                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TXA             8A     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::TXA;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// TXA
        /// Transfer Index X to Accumulator
        ///```text
        /// X -> A                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TXA             8A     1        2
        /// ```
        pub const $inst: OperType = OperType::TXA;
    };

    ([$opc:tt, $ami:tt, $inst:tt] TXS $($am:tt)*) => {
        /// TXS
        /// Transfer Index X to Stack Register
        ///```text
        /// X -> SP                           N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TXS             9A     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::TXS;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// TXS
        /// Transfer Index X to Stack Register
        ///```text
        /// X -> SP                           N  Z  C  I  D  V
        ///                                   -  -  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TXS             9A     1        2
        /// ```
        pub const $inst: OperType = OperType::TXS;
    };

    ([$opc:tt, $ami:tt, $inst:tt] TYA $($am:tt)*) => {
        /// TYA
        /// Transfer Index Y to Accumulator
        ///```text
        /// Y -> A                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TYA             98     1        2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::TYA;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// TYA
        /// Transfer Index Y to Accumulator
        ///```text
        /// Y -> A                            N  Z  C  I  D  V
        ///                                   +  +  -  -  -  -
        /// addressing   assembler       opc    bytes    cycles
        /// implied      TYA             98     1        2
        /// ```
        pub const $inst: OperType = OperType::TYA;
    };

    ([$opc:tt, $ami:tt, $inst:tt] JAM $($am:tt)*) => {
        /// # JAM (KIL, HLT)
        ///```text
        /// These instructions freeze the CPU.
        /// The processor will be trapped infinitely in T1 phase with $FF on the data bus. — Reset required.
        /// Instruction codes: 02, 12, 22, 32, 42, 52, 62, 72, 92, B2, D2, F2
        /// ```
        fn $opc() -> InstructionIterator {
            let addr = addr_mode![$($am)*];
            let work = &act::JAM;
            InstructionIterator::new(&addr, work)
        }

        am_const!([$ami] $($am)*);
        /// # JAM (KIL, HLT)
        ///```text
        /// These instructions freeze the CPU.
        /// The processor will be trapped infinitely in T1 phase with $FF on the data bus. — Reset required.
        /// Instruction codes: 02, 12, 22, 32, 42, 52, 62, 72, 92, B2, D2, F2
        /// ```
        pub const $inst: OperType = OperType::JAM;
    };
}

pub(super) use addr_mode;
pub(super) use am_const;
pub(super) use make_instruction;
