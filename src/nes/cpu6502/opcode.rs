use super::{
    address_mode::AddrMode,
    instructions::Instruction,
    Registers, StatusReg,
};
use crate::{
    bus::Bus,
    nes::{Addr, HI_MASK, IRQ_HI, IRQ_LO, LO_MASK, PS},
};
use log::trace;

pub type Instruc = fn (&mut Registers, &mut dyn Bus);

const INOOP: Instruc = op_ea;

pub const OPER: [Instruc; 256] = [
	op_00, op_01, op_02, INOOP, INOOP, op_05, op_06, INOOP, op_08, op_09, op_0a, INOOP, INOOP, op_0d, op_0e, INOOP,
	op_10, op_11, op_12, INOOP, INOOP, op_15, op_16, INOOP, op_18, op_19, INOOP, INOOP, INOOP, op_1d, op_1e, INOOP,
	op_20, op_21, op_22, INOOP, op_24, op_25, op_26, INOOP, op_28, op_29, op_2a, INOOP, op_2c, op_2d, op_2e, INOOP,
	op_30, op_31, op_32, INOOP, INOOP, op_35, op_36, INOOP, op_38, op_39, INOOP, INOOP, INOOP, op_3d, op_3e, INOOP,
	op_40, op_41, op_42, INOOP, INOOP, op_45, op_46, INOOP, op_48, op_49, op_4a, INOOP, op_4c, op_4d, op_4e, INOOP,
	op_50, op_51, op_52, INOOP, INOOP, op_55, op_56, INOOP, op_58, op_59, INOOP, INOOP, INOOP, op_5d, op_5e, INOOP,
	op_60, op_61, op_62, INOOP, INOOP, op_65, op_66, INOOP, op_68, op_69, op_6a, INOOP, op_6c, op_6d, op_6e, INOOP,
	op_70, op_71, op_72, INOOP, INOOP, op_75, op_76, INOOP, op_78, op_79, INOOP, INOOP, INOOP, op_7d, op_7e, INOOP,
	INOOP, op_81, INOOP, INOOP, op_84, op_85, op_86, INOOP, op_88, INOOP, op_8a, INOOP, op_8c, op_8d, op_8e, INOOP,
	op_90, op_91, op_92, INOOP, op_94, op_95, op_96, INOOP, op_98, op_99, op_9a, INOOP, INOOP, op_9d, INOOP, INOOP,
	op_a0, op_a1, op_a2, INOOP, op_a4, op_a5, op_a6, INOOP, op_a8, op_a9, op_aa, INOOP, op_ac, op_ad, op_ae, INOOP,
	op_b0, op_b1, op_b2, INOOP, op_b4, op_b5, op_b6, INOOP, op_b8, op_b9, op_ba, INOOP, op_bc, op_bd, op_be, INOOP,
	op_c0, op_c1, INOOP, INOOP, op_c4, op_c5, op_c6, INOOP, op_c8, op_c9, op_ca, INOOP, op_cc, op_cd, op_ce, INOOP,
	op_d0, op_d1, op_d2, INOOP, INOOP, op_d5, op_d6, INOOP, op_d8, op_d9, INOOP, INOOP, INOOP, op_dd, op_de, INOOP,
	op_e0, op_e1, INOOP, INOOP, op_e4, op_e5, op_e6, INOOP, op_e8, op_e9, op_ea, INOOP, op_ec, op_ed, op_ee, INOOP,
	op_f0, op_f1, op_f2, INOOP, INOOP, op_f5, op_f6, INOOP, op_f8, op_f9, INOOP, INOOP, INOOP, op_fd, op_fe, INOOP,
];

const INOAM: AddrMode = AddrMode::IMP;

pub const ADDER_MODE: [AddrMode; 256] = [
	AM_00, AM_01, AM_02, INOAM, INOAM, AM_05, AM_06, INOAM, AM_08, AM_09, AM_0A, INOAM, INOAM, AM_0D, AM_0E, INOAM,
	AM_10, AM_11, AM_12, INOAM, INOAM, AM_15, AM_16, INOAM, AM_18, AM_19, INOAM, INOAM, INOAM, AM_1D, AM_1E, INOAM,
	AM_20, AM_21, AM_22, INOAM, AM_24, AM_25, AM_26, INOAM, AM_28, AM_29, AM_2A, INOAM, AM_2C, AM_2D, AM_2E, INOAM,
	AM_30, AM_31, AM_32, INOAM, INOAM, AM_35, AM_36, INOAM, AM_38, AM_39, INOAM, INOAM, INOAM, AM_3D, AM_3E, INOAM,
	AM_40, AM_41, AM_42, INOAM, INOAM, AM_45, AM_46, INOAM, AM_48, AM_49, AM_4A, INOAM, AM_4C, AM_4D, AM_4E, INOAM,
	AM_50, AM_51, AM_52, INOAM, INOAM, AM_55, AM_56, INOAM, AM_58, AM_59, INOAM, INOAM, INOAM, AM_5D, AM_5E, INOAM,
	AM_60, AM_61, AM_62, INOAM, INOAM, AM_65, AM_66, INOAM, AM_68, AM_69, AM_6A, INOAM, AM_6C, AM_6D, AM_6E, INOAM,
	AM_70, AM_71, AM_72, INOAM, INOAM, AM_75, AM_76, INOAM, AM_78, AM_79, INOAM, INOAM, INOAM, AM_7D, AM_7E, INOAM,
	INOAM, AM_81, INOAM, INOAM, AM_84, AM_85, AM_86, INOAM, AM_88, INOAM, AM_8A, INOAM, AM_8C, AM_8D, AM_8E, INOAM,
	AM_90, AM_91, AM_92, INOAM, AM_94, AM_95, AM_96, INOAM, AM_98, AM_99, AM_9A, INOAM, INOAM, AM_9D, INOAM, INOAM,
	AM_A0, AM_A1, AM_A2, INOAM, AM_A4, AM_A5, AM_A6, INOAM, AM_A8, AM_A9, AM_AA, INOAM, AM_AC, AM_AD, AM_AE, INOAM,
	AM_B0, AM_B1, AM_B2, INOAM, AM_B4, AM_B5, AM_B6, INOAM, AM_B8, AM_B9, AM_BA, INOAM, AM_BC, AM_BD, AM_BE, INOAM,
	AM_C0, AM_C1, INOAM, INOAM, AM_C4, AM_C5, AM_C6, INOAM, AM_C8, AM_C9, AM_CA, INOAM, AM_CC, AM_CD, AM_CE, INOAM,
	AM_D0, AM_D1, AM_D2, INOAM, INOAM, AM_D5, AM_D6, INOAM, AM_D8, AM_D9, INOAM, INOAM, INOAM, AM_DD, AM_DE, INOAM,
	AM_E0, AM_E1, INOAM, INOAM, AM_E4, AM_E5, AM_E6, INOAM, AM_E8, AM_E9, AM_EA, INOAM, AM_EC, AM_ED, AM_EE, INOAM,
	AM_F0, AM_F1, AM_F2, INOAM, INOAM, AM_F5, AM_F6, INOAM, AM_F8, AM_F9, INOAM, INOAM, INOAM, AM_FD, AM_FE, INOAM,
];

const INOIN: Instruction = Instruction::XXX;

pub const INSTRUCTION_TYPE: [Instruction; 256] =[
	IN_00, IN_01, IN_02, INOIN, INOIN, IN_05, IN_06, INOIN, IN_08, IN_09, IN_0A, INOIN, INOIN, IN_0D, IN_0E, INOIN,
	IN_10, IN_11, IN_12, INOIN, INOIN, IN_15, IN_16, INOIN, IN_18, IN_19, INOIN, INOIN, INOIN, IN_1D, IN_1E, INOIN,
	IN_20, IN_21, IN_22, INOIN, IN_24, IN_25, IN_26, INOIN, IN_28, IN_29, IN_2A, INOIN, IN_2C, IN_2D, IN_2E, INOIN,
	IN_30, IN_31, IN_32, INOIN, INOIN, IN_35, IN_36, INOIN, IN_38, IN_39, INOIN, INOIN, INOIN, IN_3D, IN_3E, INOIN,
	IN_40, IN_41, IN_42, INOIN, INOIN, IN_45, IN_46, INOIN, IN_48, IN_49, IN_4A, INOIN, IN_4C, IN_4D, IN_4E, INOIN,
	IN_50, IN_51, IN_52, INOIN, INOIN, IN_55, IN_56, INOIN, IN_58, IN_59, INOIN, INOIN, INOIN, IN_5D, IN_5E, INOIN,
	IN_60, IN_61, IN_62, INOIN, INOIN, IN_65, IN_66, INOIN, IN_68, IN_69, IN_6A, INOIN, IN_6C, IN_6D, IN_6E, INOIN,
	IN_70, IN_71, IN_72, INOIN, INOIN, IN_75, IN_76, INOIN, IN_78, IN_79, INOIN, INOIN, INOIN, IN_7D, IN_7E, INOIN,
	INOIN, IN_81, INOIN, INOIN, IN_84, IN_85, IN_86, INOIN, IN_88, INOIN, IN_8A, INOIN, IN_8C, IN_8D, IN_8E, INOIN,
	IN_90, IN_91, IN_92, INOIN, IN_94, IN_95, IN_96, INOIN, IN_98, IN_99, IN_9A, INOIN, INOIN, IN_9D, INOIN, INOIN,
	IN_A0, IN_A1, IN_A2, INOIN, IN_A4, IN_A5, IN_A6, INOIN, IN_A8, IN_A9, IN_AA, INOIN, IN_AC, IN_AD, IN_AE, INOIN,
	IN_B0, IN_B1, IN_B2, INOIN, IN_B4, IN_B5, IN_B6, INOIN, IN_B8, IN_B9, IN_BA, INOIN, IN_BC, IN_BD, IN_BE, INOIN,
	IN_C0, IN_C1, INOIN, INOIN, IN_C4, IN_C5, IN_C6, INOIN, IN_C8, IN_C9, IN_CA, INOIN, IN_CC, IN_CD, IN_CE, INOIN,
	IN_D0, IN_D1, IN_D2, INOIN, INOIN, IN_D5, IN_D6, INOIN, IN_D8, IN_D9, INOIN, INOIN, INOIN, IN_DD, IN_DE, INOIN,
	IN_E0, IN_E1, INOIN, INOIN, IN_E4, IN_E5, IN_E6, INOIN, IN_E8, IN_E9, IN_EA, INOIN, IN_EC, IN_ED, IN_EE, INOIN,
	IN_F0, IN_F1, IN_F2, INOIN, INOIN, IN_F5, IN_F6, INOIN, IN_F8, IN_F9, INOIN, INOIN, INOIN, IN_FD, IN_FE, INOIN,
];

macro_rules! is_implied {
    ($when:block, $otherwise:block) => {
        $when
    };

    ($when:block, $otherwise:block, A) => {
        $when
    };

    ($when:block, $otherwise:block, $($rest:tt)*) => {
        $otherwise
    };
}

macro_rules! am {
	($reg:expr, $bus:expr, A) => {
		// A Accumulator
		//
		// OPC A
		//
		// operand is AC (implied single byte instruction)
		//
		// These instructions act directly on one or more registers or flags
		// internal to the CPU. Therefor, these instructions are principally
		// single-byte instructions, lacking an explicit operand. The operand
		// is implied, as it is already provided by the very instruction.
		//
		// Instructions targeting exclusively the contents of the accumulator
		// may or may not be denoted by using an explicit "A" as the operand,
		// depending on the flavor of syntax. (This may be regarded as a
		// special address mode of its own, but it is really a special case of
		// an implied instruction. It is still a single-byte instruction and no
		// operand is provided in machine language.)
		{
            trace!("A	Accumulator	OPC A	operand is AC (implied single byte instruction)");
            let &mut _ = $reg;
            let &mut _ = $bus;
            0 as Addr
        }
	};

	($reg:expr, $bus:expr, &LLHH) => {
		// Absolute
		//
		// OPC $LLHH
		//
		// operand is address $HHLL *
		//
		// Absolute addressing modes provides the 16-bit address of a memory
		// location, the contents of which used as the operand to the
		// instruction. In machine language, the address is provided in two
		// bytes immediately after the instruction (making these 3-byte
		// instructions) in low-byte, high-byte order (LLHH) or little-endian.
		// In assembler, conventional numbers (HHLL order or big-endian words)
		// are used to provide the address.
		//
		// Absolute addresses are also used for the jump instructions JMP and
		// JSR to provide the address for the next instruction to continue with
		// in the control flow.
        {
            trace!("abs	absolute	OPC $LLHH	operand is address $HHLL *");
            let addr: Addr = $bus.read($reg.pc) as Addr;
            $reg.pc += 1;
            let addr = addr | ($bus.read($reg.pc) as Addr) << 8;
            $reg.pc += 1;
            addr
        }
	};

	($reg:expr, $bus:expr, &LLHH,X) => {
		// Absolute, X-indexed
		//
		// OPC $LLHH,X
		//
		// operand is address; effective address is address incremented by X with carry **
		//
		// Indexed addressing adds the contents of either the X-register or the
		// Y-register to the provided address to give the effective address,
		// which provides the operand.
		//
		// These instructions are usefull to e.g., load values from tables or
		// to write to a continuous segment of memory in a loop. The most basic
		// forms are "absolute,X" and "absolute,X", where either the X- or the
		// Y-register, respectively, is added to a given base address. As the
		// base address is a 16-bit value, these are generally 3-byte
		// instructions. Since there is an additional operation to perform to
		// determine the effective address, these instructions are one cycle
		// slower than those using absolute addressing mode.*
		//
		// *) If the addition of the contents of the index register effects in
		// a change of the high-byte given by the base address so that the
		// effective address is on the next memory page, the additional
		// operation to increment the high-byte takes another CPU cycle. This
		// is also known as a crossing of page boundaries.
        {
            trace!("abs,X	absolute, X-indexed	OPC $LLHH,X	operand is address; effective address is address incremented by X with carry **");
            let lo = $bus.read($reg.pc) as Addr;
            $reg.pc += 1;
            let hi = ($bus.read($reg.pc) as Addr) << 8;
            $reg.pc += 1;
            let addr = lo | hi;
            let addr = addr + $reg.x as Addr;

            if addr & HI_MASK != hi {
                // Extra cycle to get the new page
                addr
            }
            else {
                addr
            }
        }
	};

	($reg:expr, $bus:expr, &LLHH,Y) => {
		// Absolute, Y-indexed
		//
		// OPC $LLHH,Y
		//
		// operand is address; effective address is address incremented by Y with carry **
		//
		// Indexed addressing adds the contents of either the X-register or the
		// Y-register to the provided address to give the effective address,
		// which provides the operand.
		//
		// These instructions are usefull to e.g., load values from tables or
		// to write to a continuous segment of memory in a loop. The most basic
		// forms are "absolute,X" and "absolute,X", where either the X- or the
		// Y-register, respectively, is added to a given base address. As the
		// base address is a 16-bit value, these are generally 3-byte
		// instructions. Since there is an additional operation to perform to
		// determine the effective address, these instructions are one cycle
		// slower than those using absolute addressing mode.*
		//
		// *) If the addition of the contents of the index register effects in
		// a change of the high-byte given by the base address so that the
		// effective address is on the next memory page, the additional
		// operation to increment the high-byte takes another CPU cycle. This
		// is also known as a crossing of page boundaries.
        {
            trace!("abs,X	absolute, X-indexed	OPC $LLHH,X	operand is address; effective address is address incremented by X with carry **");
            let lo = $bus.read($reg.pc) as Addr;
            $reg.pc += 1;
            let hi = ($bus.read($reg.pc) as Addr) << 8;
            $reg.pc += 1;
            let addr = lo | hi;
            let addr = addr + $reg.y as Addr;

            if addr & HI_MASK != hi {
                $bus.read(0x0000); // Add cycle
                addr
            }
            else {
                addr
            }
        }
	};

	($reg:expr, $bus:expr, #&BB) => {
		// Immediate
		//
		// OPC #$BB
		//
		// operand is byte BB
		//
		// Here, a literal operand is given immediately after the instruction.
		// The operand is always an 8-bit value and the total instruction
		// length is always 2 bytes. In memory, the operand is a single byte
		// following immediately after the instruction code. In assembler, the
		// mode is usually indicated by a "#" prefix adjacent to the operand.
        {
            trace!("{}", "#	immediate	OPC #$BB	operand is byte BB");
            let _ = $bus;
            let addr: Addr = $reg.pc;
            $reg.pc += 1;
            addr
        }
	};

	($reg:expr, $bus:expr,) => {
		am!($reg, $bus)
	};

	($reg:expr, $bus:expr) => {
		// Implied
		//
		// OPC
		//
		// operand implied
		//
		// These instructions act directly on one or more registers or flags
		// internal to the CPU. Therefor, these instructions are principally
		// single-byte instructions, lacking an explicit operand. The operand
		// is implied, as it is already provided by the very instruction.
		//
		// Instructions targeting exclusively the contents of the accumulator
		// may or may not be denoted by using an explicit "A" as the operand,
		// depending on the flavor of syntax. (This may be regarded as a
		// special address mode of its own, but it is really a special case of
		// an implied instruction. It is still a single-byte instruction and no
		// operand is provided in machine language.)
        {
            trace!("impl	implied	OPC	operand implied");
            let &mut _ = $reg;
            let &mut _ = $bus;
            0 as Addr
        }
	};

	($reg:expr, $bus:expr, (&LLHH)) => {
		// Indirect
		//
		// OPC ($LLHH)
		//
		// operand is address; effective address is contents of word at address: C.w($HHLL)
		//
		// This mode looks up a given address and uses the contents of this
		// address and the next one (in LLHH little-endian order) as the
		// effective address. In its basic form, this mode is available for the
		// JMP instruction only. (Its generally use is jump vectors and jump tables.)
		//
		// Like the absolute JMP instruction it uses a 16-bit address (3 bytes
		// in total), but takes two additional CPU cycles to execute, since
		// there are two additional bytes to fetch for the lookup of the
		// effective jump target.
		//
		// Generally, indirect addressing is denoted by putting the lookup
		// address in parenthesis.
        {
            trace!("ind	indirect	OPC ($LLHH)	operand is address; effective address is contents of word at address: C.w($HHLL)");
            let lo = $bus.read($reg.pc) as Addr;
            $reg.pc += 1;
            let hi = ($bus.read($reg.pc) as Addr) << 8;
            $reg.pc += 1;
            let ptr = lo | hi;

            if lo == LO_MASK {
                // Simulate page boundary hardware bug
                let lo = $bus.read(ptr + 0x0000) as Addr;
                let hi = ($bus.read(ptr & 0xFF00) as Addr) << 8;
                lo | hi
            }
            else {
                let lo = $bus.read(ptr + 0x0000) as Addr;
                let hi = ($bus.read(ptr + 0x0001) as Addr) << 8;
                lo | hi
            }
        }
	};

	($reg:expr, $bus:expr, (&LL,X)) => {
		// Indirect, X-indexed
		//
		// OPC ($LL,X)
		//
		// operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
		//
		// Indexed indirect address modes are generally available only for
		// instructions supplying an operand to the accumulator (LDA, STA, ADC,
		// SBC, AND, ORA, EOR, etc). The placement of the index register inside
		// or outside of the parenthesis indicating the address lookup will
		// give you clue what these instructions are doing.
		//
		// Pre-indexed indirect address mode is only available in combination
		// with the X-register. It works much like the "zero-page,X" mode, but,
		// after the X-register has been added to the base address, instead of
		// directly accessing this, an additional lookup is performed, reading
		// the contents of resulting address and the next one (in LLHH little-
		// endian order), in order to determine the effective address.
		//
		// Like with "zero-page,X" mode, the total instruction length is 2
		// bytes, but there are two additional CPU cycles in order to fetch
		// the effective 16-bit address. As "zero-page,X" mode, a lookup address
		// will never overflow into the next page, but will simply wrap around
		// in the zero-page.
		//
		// These instructions are useful, whenever we want to loop over a table
		// of pointers to disperse addresses, or where we want to apply the
		// same operation to various addresses, which we have stored as a table
		// in the zero-page.
        {
            trace!("X,ind	X-indexed, indirect	OPC ($LL,X)	operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)");
            let lo_ptr = $bus.read($reg.pc) as Addr;
            $reg.pc += 1;
            let x = $reg.x as Addr;
            let ptr = lo_ptr + x;
			
            let lo = $bus.read(ptr + 0) as Addr;
            let hi = ($bus.read(ptr + 1) as Addr) << 8;
			$bus.read(0x0000); // Add cycle
            lo | hi
        }
	};

	($reg:expr, $bus:expr, (&LL),Y) => {
		// Indirect, Y-indexed
		//
		// OPC ($LL),Y
		//
		// operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
		//
		// Post-indexed indirect addressing is only available in combination
		// with the Y-register. As indicated by the indexing term ",Y" being
		// appended to the outside of the parenthesis indicating the indirect
		// lookup, here, a pointer is first read (from the given zero-page
		// address) and resolved and only then the contents of the Y-register
		// is added to this to give the effective address.
		//
		// Like with "zero-page,Y" mode, the total instruction length is 2
		// bytes, but there it takes an additional CPU cycles to resolve and
		// index the 16-bit pointer. As with "absolute,X" mode, the effective
		// address may overflow into the next page, in the case of which the
		// execution uses an extra CPU cycle.
		//
		// These instructions are useful, wherever we want to perform lookups
		// on varying bases addresses or whenever we want to loop over tables,
		// the base address of which we have stored in the zero-page.
        {
            trace!("ind,Y	indirect, Y-indexed	OPC ($LL),Y	operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y");
            let ptr = $bus.read($reg.pc) as Addr;
            $reg.pc += 1;

            let lo = $bus.read(ptr + 0) as Addr;
            let hi = ($bus.read(ptr + 1) as Addr) << 8;

            let addr = lo | hi;
            let y = $reg.y as Addr;
            let addr = addr + y;

            if addr & HI_MASK != hi {
				$bus.read(0x0000); // Add cycle
                addr
            }
            else {
                addr
            }
        }
	};

	($reg:expr, $bus:expr, &BB) => {
		// Relative
		//
		// OPC $BB
		//
		// branch target is PC + signed offset BB ***
		//
		// This final address mode is exlusive to conditional branch
		// instructions, which branch in the execution path depending on the
		// state of a given CPU flag. Here, the instruction provides only a
		// relative offset, which is added to the contents of the program
		// counter (PC) as it points to the immediate next instruction. The
		// relative offset is a signed single byte value in two's complement
		// encoding (giving a range of −128…+127), which allows for branching
		// up to half a page forwards and backwards.
		//
		// On the one hand, this makes these instructions compact, fast and
		// relocatable at the same time. On the other hand, we have to mind
		// that our branch target is no farther away than half a memory page.
		//
		// Generally, an assembler will take care of this and we only have to
		// provide the target address, not having to worry about relative
		// addressing.
		//
		// These instructions are always of 2 bytes length and perform in 2 CPU
		// cycles, if the branch is not taken (the condition resolving to
		// 'false'), and 3 cycles, if the branch is taken (when the condition
		// is true). If a branch is taken and the target is on a different
		// page, this adds another CPU cycle (4 in total).
		{
            trace!("rel	relative	OPC $BB	branch target is PC + signed offset BB ***");
            let addr = $bus.read($reg.pc) as Addr;
            $reg.pc += 1;
            if is_neg(addr) {
                addr | HI_MASK
            }
            else {
                addr
            }
        }
	};

	($reg:expr, $bus:expr, &LL) => {
		// Zeropage
		//
		// OPC $LL
		//
		// operand is zeropage address (hi-byte is zero, address = $00LL)
		//
		// The 16-bit address space available to the 6502 is thought to consist
		// of 256 "pages" of 256 memory locations each ($00…$FF). In this model
		// the high-byte of an address gives the page number and the low-byte a
		// location inside this page. The very first of these pages, where the
		// high-byte is zero (addresses $0000…$00FF), is somewhat special.
		//
		// The zero-page address mode is similar to absolute address mode, but
		// these instructions use only a single byte for the operand, the low-
		// byte, while the high-byte is assumed to be zero by definition.
		// Therefore, these instructions have a total length of just two bytes
		// (one less than absolute mode) and take one CPU cycle less to
		// execute, as there is one byte less to fetch.
        {
            trace!("zpg	zeropage	OPC $LL	operand is zeropage address (hi-byte is zero, address = $00LL)");
            let addr: Addr = $bus.read($reg.pc) as Addr;
            $reg.pc += 1;
            addr
        }
	};

	($reg:expr, $bus:expr, &LL,X) => {
		// Zeropage, X-indexed
		//
		// OPC $LL,X
		//
		// operand is zeropage address; effective address is address incremented by X without carry **
		//
		// As with absolute addressing, there is also a zero-page mode for
		// indexed addressing. However, this is generally only available with
		// the X-register. (The only exception to this is LDX, which has an
		// indexed zero-page mode utilizing the Y-register.)
		//
		// As we have already seen with normal zero-page mode, these
		// instructions are one byte less in total length (two bytes) and take
		// one CPU cycle less than instructions in absolute indexed mode.
		//
		// Unlike absolute indexed instructions with 16-bit base addresses,
		// zero-page indexed instructions never affect the high-byte of the
		// effective address, which will simply wrap around in the zero-page,
		// and there is no penalty for crossing any page boundaries.
        {
            trace!("zpg,X	zeropage, X-indexed	OPC $LL,X	operand is zeropage address; effective address is address incremented by X without carry **");
            let addr: Addr = $bus.read($reg.pc) as Addr;
            let addr =  addr + $reg.x as Addr;
            let addr = addr & LO_MASK;
            $reg.pc += 1;
            addr
        }
	};

	($reg:expr, $bus:expr, &LL,Y) => {
		// Zeropage, Y-indexed
		//
		// OPC $LL,Y
		//
		// operand is zeropage address; effective address is address incremented by Y without carry **
		//
		// As with absolute addressing, there is also a zero-page mode for
		// indexed addressing. However, this is generally only available with
		// the X-register. (The only exception to this is LDX, which has an
		// indexed zero-page mode utilizing the Y-register.)
		//
		// As we have already seen with normal zero-page mode, these
		// instructions are one byte less in total length (two bytes) and take
		// one CPU cycle less than instructions in absolute indexed mode.
		//
		// Unlike absolute indexed instructions with 16-bit base addresses,
		// zero-page indexed instructions never affect the high-byte of the
		// effective address, which will simply wrap around in the zero-page,
		// and there is no penalty for crossing any page boundaries.
        {
            trace!("zpg,Y	zeropage, Y-indexed	OPC $LL,Y	operand is zeropage address; effective address is address incremented by Y without carry **");
            let addr: Addr = $bus.read($reg.pc) as Addr;
            let addr =  addr + $reg.y as Addr;
            let addr = addr & LO_MASK;
            $reg.pc += 1;
            addr
        }
	};
}

macro_rules! am_const {
	([$code:ident] A) => {
		// A Accumulator
		//
		// OPC A
		//
		// operand is AC (implied single byte instruction)
		//
		// These instructions act directly on one or more registers or flags
		// internal to the CPU. Therefor, these instructions are principally
		// single-byte instructions, lacking an explicit operand. The operand
		// is implied, as it is already provided by the very instruction.
		//
		// Instructions targeting exclusively the contents of the accumulator
		// may or may not be denoted by using an explicit "A" as the operand,
		// depending on the flavor of syntax. (This may be regarded as a
		// special address mode of its own, but it is really a special case of
		// an implied instruction. It is still a single-byte instruction and no
		// operand is provided in machine language.)
		const $code: AddrMode = AddrMode::A;
	};

	([$code:ident] &LLHH) => {
		// Absolute
		//
		// OPC $LLHH
		//
		// operand is address $HHLL *
		//
		// Absolute addressing modes provides the 16-bit address of a memory
		// location, the contents of which used as the operand to the
		// instruction. In machine language, the address is provided in two
		// bytes immediately after the instruction (making these 3-byte
		// instructions) in low-byte, high-byte order (LLHH) or little-endian.
		// In assembler, conventional numbers (HHLL order or big-endian words)
		// are used to provide the address.
		//
		// Absolute addresses are also used for the jump instructions JMP and
		// JSR to provide the address for the next instruction to continue with
		// in the control flow.
        const $code: AddrMode = AddrMode::ABS;
	};

	([$code:ident] &LLHH,X) => {
		// Absolute, X-indexed
		//
		// OPC $LLHH,X
		//
		// operand is address; effective address is address incremented by X with carry **
		//
		// Indexed addressing adds the contents of either the X-register or the
		// Y-register to the provided address to give the effective address,
		// which provides the operand.
		//
		// These instructions are usefull to e.g., load values from tables or
		// to write to a continuous segment of memory in a loop. The most basic
		// forms are "absolute,X" and "absolute,X", where either the X- or the
		// Y-register, respectively, is added to a given base address. As the
		// base address is a 16-bit value, these are generally 3-byte
		// instructions. Since there is an additional operation to perform to
		// determine the effective address, these instructions are one cycle
		// slower than those using absolute addressing mode.*
		//
		// *) If the addition of the contents of the index register effects in
		// a change of the high-byte given by the base address so that the
		// effective address is on the next memory page, the additional
		// operation to increment the high-byte takes another CPU cycle. This
		// is also known as a crossing of page boundaries.
        const $code: AddrMode = AddrMode::ABX;
	};

	([$code:ident] &LLHH,Y) => {
		// Absolute, Y-indexed
		//
		// OPC $LLHH,Y
		//
		// operand is address; effective address is address incremented by Y with carry **
		//
		// Indexed addressing adds the contents of either the X-register or the
		// Y-register to the provided address to give the effective address,
		// which provides the operand.
		//
		// These instructions are usefull to e.g., load values from tables or
		// to write to a continuous segment of memory in a loop. The most basic
		// forms are "absolute,X" and "absolute,X", where either the X- or the
		// Y-register, respectively, is added to a given base address. As the
		// base address is a 16-bit value, these are generally 3-byte
		// instructions. Since there is an additional operation to perform to
		// determine the effective address, these instructions are one cycle
		// slower than those using absolute addressing mode.*
		//
		// *) If the addition of the contents of the index register effects in
		// a change of the high-byte given by the base address so that the
		// effective address is on the next memory page, the additional
		// operation to increment the high-byte takes another CPU cycle. This
		// is also known as a crossing of page boundaries.
        const $code: AddrMode = AddrMode::ABY;
	};

	([$code:ident] #&BB) => {
		// Immediate
		//
		// OPC #$BB
		//
		// operand is byte BB
		//
		// Here, a literal operand is given immediately after the instruction.
		// The operand is always an 8-bit value and the total instruction
		// length is always 2 bytes. In memory, the operand is a single byte
		// following immediately after the instruction code. In assembler, the
		// mode is usually indicated by a "#" prefix adjacent to the operand.
        const $code: AddrMode = AddrMode::IMM;
	};

	([$code:ident]) => {
		// Implied
		//
		// OPC
		//
		// operand implied
		//
		// These instructions act directly on one or more registers or flags
		// internal to the CPU. Therefor, these instructions are principally
		// single-byte instructions, lacking an explicit operand. The operand
		// is implied, as it is already provided by the very instruction.
		//
		// Instructions targeting exclusively the contents of the accumulator
		// may or may not be denoted by using an explicit "A" as the operand,
		// depending on the flavor of syntax. (This may be regarded as a
		// special address mode of its own, but it is really a special case of
		// an implied instruction. It is still a single-byte instruction and no
		// operand is provided in machine language.)
        const $code: AddrMode = AddrMode::IMP;
	};

	([$code:ident] (&LLHH)) => {
		// Indirect
		//
		// OPC ($LLHH)
		//
		// operand is address; effective address is contents of word at address: C.w($HHLL)
		//
		// This mode looks up a given address and uses the contents of this
		// address and the next one (in LLHH little-endian order) as the
		// effective address. In its basic form, this mode is available for the
		// JMP instruction only. (Its generally use is jump vectors and jump tables.)
		//
		// Like the absolute JMP instruction it uses a 16-bit address (3 bytes
		// in total), but takes two additional CPU cycles to execute, since
		// there are two additional bytes to fetch for the lookup of the
		// effective jump target.
		//
		// Generally, indirect addressing is denoted by putting the lookup
		// address in parenthesis.
        const $code: AddrMode = AddrMode::IND;
	};

	([$code:ident] (&LL,X)) => {
		// Indirect, X-indexed
		//
		// OPC ($LL,X)
		//
		// operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
		//
		// Indexed indirect address modes are generally available only for
		// instructions supplying an operand to the accumulator (LDA, STA, ADC,
		// SBC, AND, ORA, EOR, etc). The placement of the index register inside
		// or outside of the parenthesis indicating the address lookup will
		// give you clue what these instructions are doing.
		//
		// Pre-indexed indirect address mode is only available in combination
		// with the X-register. It works much like the "zero-page,X" mode, but,
		// after the X-register has been added to the base address, instead of
		// directly accessing this, an additional lookup is performed, reading
		// the contents of resulting address and the next one (in LLHH little-
		// endian order), in order to determine the effective address.
		//
		// Like with "zero-page,X" mode, the total instruction length is 2
		// bytes, but there are two additional CPU cycles in order to fetch
		// the effective 16-bit address. As "zero-page,X" mode, a lookup address
		// will never overflow into the next page, but will simply wrap around
		// in the zero-page.
		//
		// These instructions are useful, whenever we want to loop over a table
		// of pointers to disperse addresses, or where we want to apply the
		// same operation to various addresses, which we have stored as a table
		// in the zero-page.
        const $code: AddrMode = AddrMode::IZX;
	};

	([$code:ident] (&LL),Y) => {
		// Indirect, Y-indexed
		//
		// OPC ($LL),Y
		//
		// operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
		//
		// Post-indexed indirect addressing is only available in combination
		// with the Y-register. As indicated by the indexing term ",Y" being
		// appended to the outside of the parenthesis indicating the indirect
		// lookup, here, a pointer is first read (from the given zero-page
		// address) and resolved and only then the contents of the Y-register
		// is added to this to give the effective address.
		//
		// Like with "zero-page,Y" mode, the total instruction length is 2
		// bytes, but there it takes an additional CPU cycles to resolve and
		// index the 16-bit pointer. As with "absolute,X" mode, the effective
		// address may overflow into the next page, in the case of which the
		// execution uses an extra CPU cycle.
		//
		// These instructions are useful, wherever we want to perform lookups
		// on varying bases addresses or whenever we want to loop over tables,
		// the base address of which we have stored in the zero-page.
        const $code: AddrMode = AddrMode::IZY;
	};

	([$code:ident] &BB) => {
		// Relative
		//
		// OPC $BB
		//
		// branch target is PC + signed offset BB ***
		//
		// This final address mode is exlusive to conditional branch
		// instructions, which branch in the execution path depending on the
		// state of a given CPU flag. Here, the instruction provides only a
		// relative offset, which is added to the contents of the program
		// counter (PC) as it points to the immediate next instruction. The
		// relative offset is a signed single byte value in two's complement
		// encoding (giving a range of −128…+127), which allows for branching
		// up to half a page forwards and backwards.
		//
		// On the one hand, this makes these instructions compact, fast and
		// relocatable at the same time. On the other hand, we have to mind
		// that our branch target is no farther away than half a memory page.
		//
		// Generally, an assembler will take care of this and we only have to
		// provide the target address, not having to worry about relative
		// addressing.
		//
		// These instructions are always of 2 bytes length and perform in 2 CPU
		// cycles, if the branch is not taken (the condition resolving to
		// 'false'), and 3 cycles, if the branch is taken (when the condition
		// is true). If a branch is taken and the target is on a different
		// page, this adds another CPU cycle (4 in total).
		const $code: AddrMode = AddrMode::REL;
	};

	([$code:ident] &LL) => {
		// Zeropage
		//
		// OPC $LL
		//
		// operand is zeropage address (hi-byte is zero, address = $00LL)
		//
		// The 16-bit address space available to the 6502 is thought to consist
		// of 256 "pages" of 256 memory locations each ($00…$FF). In this model
		// the high-byte of an address gives the page number and the low-byte a
		// location inside this page. The very first of these pages, where the
		// high-byte is zero (addresses $0000…$00FF), is somewhat special.
		//
		// The zero-page address mode is similar to absolute address mode, but
		// these instructions use only a single byte for the operand, the low-
		// byte, while the high-byte is assumed to be zero by definition.
		// Therefore, these instructions have a total length of just two bytes
		// (one less than absolute mode) and take one CPU cycle less to
		// execute, as there is one byte less to fetch.
        const $code: AddrMode = AddrMode::ZPG;
	};

	([$code:ident] &LL,X) => {
		// Zeropage, X-indexed
		//
		// OPC $LL,X
		//
		// operand is zeropage address; effective address is address incremented by X without carry **
		//
		// As with absolute addressing, there is also a zero-page mode for
		// indexed addressing. However, this is generally only available with
		// the X-register. (The only exception to this is LDX, which has an
		// indexed zero-page mode utilizing the Y-register.)
		//
		// As we have already seen with normal zero-page mode, these
		// instructions are one byte less in total length (two bytes) and take
		// one CPU cycle less than instructions in absolute indexed mode.
		//
		// Unlike absolute indexed instructions with 16-bit base addresses,
		// zero-page indexed instructions never affect the high-byte of the
		// effective address, which will simply wrap around in the zero-page,
		// and there is no penalty for crossing any page boundaries.
        const $code: AddrMode = AddrMode::ZPX;
	};

	([$code:ident] &LL,Y) => {
		// Zeropage, Y-indexed
		//
		// OPC $LL,Y
		//
		// operand is zeropage address; effective address is address incremented by Y without carry **
		//
		// As with absolute addressing, there is also a zero-page mode for
		// indexed addressing. However, this is generally only available with
		// the X-register. (The only exception to this is LDX, which has an
		// indexed zero-page mode utilizing the Y-register.)
		//
		// As we have already seen with normal zero-page mode, these
		// instructions are one byte less in total length (two bytes) and take
		// one CPU cycle less than instructions in absolute indexed mode.
		//
		// Unlike absolute indexed instructions with 16-bit base addresses,
		// zero-page indexed instructions never affect the high-byte of the
		// effective address, which will simply wrap around in the zero-page,
		// and there is no penalty for crossing any page boundaries.
        const $code: AddrMode = AddrMode::ZPY;
	};
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

macro_rules! op {
	(#[$opc:tt, $am:tt, $inst:tt] $op:tt $($rest:tt)*) => {
		am_const!([$am] $($rest)*);

		const $inst: Instruction = Instruction::$op;

		op!([$opc] $op $($rest)*);

		//op_tests!(#$opc $($rest)*);
	};

    ([$opc:ident] ADC $($rest:tt)*) => {
		/// # ADC 
		/// Add Memory to Accumulator with Carry
		///```
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
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);

            let data = bus.read(addr) as u16;
            let ac = reg.ac as u16;

            let tmp = data + ac + reg.p.get(StatusReg::C);
            reg.p.set(StatusReg::C, tmp > 255)
             .set(StatusReg::Z, is_lo_zero(tmp))
             .set(StatusReg::V, is_neg(!(ac ^ data as u16) & ac ^ tmp))
             .set(StatusReg::N, is_neg(tmp));

            reg.ac = tmp as u8;
		}
	};

    ([$opc:ident] AND $($rest:tt)*) => {
		/// AND
		/// AND Memory with Accumulator
		///
		/// A AND M -> A                      N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// immediate    AND #oper       29      2       2
		/// zeropage     AND oper        25      2       3
		/// zeropage,X   AND oper,X      35      2       4
		/// absolute     AND oper        2D      3       4
		/// absolute,X   AND oper,X      3D      3       4*
		/// absolute,Y   AND oper,Y      39      3       4*
		/// (indirect,X) AND (oper,X)    21      2       6
		/// (indirect),Y AND (oper),Y    31      2       5*
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);

            let mut p = reg.p;
            let data = bus.read(addr);
            let ac = reg.ac;
            let tmp = data & ac;
            p.set(StatusReg::Z, is_zero(tmp))
             .set(StatusReg::N, is_neg(tmp as u16));
            reg.p = p;
            reg.ac = tmp;
		}
	};

    ([$opc:ident] ASL $($rest:tt)*) => {
		/// ASL
		/// Shift Left One Bit (Memory or Accumulator)
		///
		/// C <- [76543210] <- 0              N  Z  C  I  D  V
		///                                   +  +  +  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// accumulator  ASL A           0A      1       2
		/// zeropage     ASL oper        06      2       5
		/// zeropage,X   ASL oper,X      16      2       6
		/// absolute     ASL oper        0E      3       6
		/// absolute,X   ASL oper,X      1E      3       7
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);

            let data = bus.read(addr) as u16;
            let tmp = data << 1;
            let mut p = reg.p;

            p.set(StatusReg::C, tmp & HI_MASK != 0)
             .set(StatusReg::Z, is_lo_zero(tmp))
             .set(StatusReg::N, is_neg(tmp));

            reg.p = p;

            let tmp = tmp as u8;
            is_implied!(
                { reg.ac = tmp; },
                { bus.write(addr, tmp); },
				$($rest)*);
		}
	};

    ([$opc:ident] BCC $($rest:tt)*) => {
		/// BCC
		/// Branch on Carry Clear
		///
		/// branch on C = 0                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// relative     BCC oper        90      2       2**
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
            if reg.p.get(StatusReg::C) == 0 {
                bus.read(0x0000); // Add cycle

                let pc = reg.pc;
                let addr = pc.wrapping_add(addr);

                if addr & HI_MASK != pc & HI_MASK {
                    bus.read(0x0000); // Add cycle
                }

                reg.pc = addr;
            }
		}
	};

    ([$opc:ident] BCS $($rest:tt)*) => {
		/// BCS
		/// Branch on Carry Set
		///
		/// branch on C = 1                         N Z C I D V
		///                                         - - - - - -
		/// addressing	assembler   	opc 	bytes	cycles
		/// relative     BCS oper        B0      2       2**
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
            if reg.p.get(StatusReg::C) == 1 {
                bus.read(0x0000); // Add cycle

                let pc = reg.pc;
                let addr = pc.wrapping_add(addr);

                if addr & HI_MASK != pc & HI_MASK {
                    bus.read(0x0000); // Add cycle
                }

                reg.pc = addr;
            }
		}
	};

    ([$opc:ident] BEQ $($rest:tt)*) => {
		/// BEQ
		/// Branch on Result Zero
		///
		/// branch on Z = 1                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// relative     BEQ oper        F0      2       2**
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
            if reg.p.get(StatusReg::Z) == 1 {
                bus.read(0x0000); // Add cycle

                let pc = reg.pc;
                let addr = pc.wrapping_add(addr);

                if addr & HI_MASK != pc & HI_MASK {
                    bus.read(0x0000); // Add cycle
                }

                reg.pc = addr;
            }
		}
	};

    ([$opc:ident] BIT $($rest:tt)*) => {
		/// BIT
		/// Test Bits in Memory with Accumulator
		///
		/// bits 7 and 6 of operand are transfered to bit 7 and 6 of SR (N,V);
		/// the zero-flag is set according to the result of the operand AND
		/// the accumulator (set, if the result is zero, unset otherwise).
		/// This allows a quick check of a few bits at once without affecting
		/// any of the registers, other than the status register (SR).
		///
		/// A AND M, M7 -> N, M6 -> V         N  Z  C  I  D  V
		///                                   M7 +  -  -  -  M6
		/// addressing	assembler   	opc 	bytes	cycles
		/// zeropage     BIT oper        24      2       3
		/// absolute     BIT oper        2C      3       4
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
            let data = bus.read(addr);
            let tmp = data & reg.ac;

            let mut p = reg.p;
            p.set(StatusReg::Z, is_zero(tmp))
             .set(StatusReg::N, is_neg(data as u16))
             .set(StatusReg::V, (data & 0x40) > 0);
            reg.p = p;
		}
	};

    ([$opc:ident] BMI $($rest:tt)*) => {
		/// BMI
		/// Branch on Result Minus
		///
		/// branch on N = 1                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// relative     BMI oper        30      2       2**
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
            if reg.p.get(StatusReg::N) == 1 {
                bus.read(0x0000); // Add cycle

                let pc = reg.pc;
                let addr = pc.wrapping_add(addr);

                if addr & HI_MASK != pc & HI_MASK {
                    bus.read(0x0000); // Add cycle
                }

                reg.pc = addr;
            }
		}
	};

    ([$opc:ident] BNE $($rest:tt)*) => {
		/// BNE
		/// Branch on Result not Zero
		///
		/// branch on Z = 0                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// relative    BNE oper        D0      2       2**
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
            if reg.p.get(StatusReg::Z) == 0 {
				bus.read(0x0000); // Add cycle

                let pc = reg.pc;
                let addr = pc.wrapping_add(addr);

                if addr & HI_MASK != pc & HI_MASK {
                    bus.read(0x0000); // Add cycle
                }

                reg.pc = addr;
            }
		}
	};

    ([$opc:ident] BPL $($rest:tt)*) => {
		/// BPL
		/// Branch on Result Plus
		///
		/// branch on N = 0                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// relative     BPL oper        10      2       2**
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
            if reg.p.get(StatusReg::N) == 0 {
                bus.read(0x0000); // Add cycle

                let pc = reg.pc;
                let addr = pc.wrapping_add(addr);

                if addr & HI_MASK != pc & HI_MASK {
                    bus.read(0x0000); // Add cycle
                }

                reg.pc = addr;
            }
		}
	};

	([$opc:ident] BRK) => {
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
		///
		/// interrupt,                        N  Z  C  I  D  V
		/// push PC+2, push SR                -  -  -  1  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// implied      BRK             00      1       7
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			reg.pc += 1;
            let mut p = reg.p;
            p.set(StatusReg::I, true);

            let mut sp = reg.sp as Addr;
            let pc = reg.pc;
            bus.write(PS + sp, (pc >> 8) as u8);
            sp -= 1;

            bus.write(PS + sp, pc as u8);
            sp -= 1;

            p.set(StatusReg::B, true);
            bus.write(PS + sp, p.into());
            p.set(StatusReg::B, false);

            reg.p = p;
            reg.sp = sp as u8;
            let lo = bus.read(IRQ_LO) as Addr;
            let hi = (bus.read(IRQ_HI) as Addr) << 8;
            reg.pc = lo | hi;
		}
	};

    ([$opc:ident] BVC $($rest:tt)*) => {
		/// BVC
		/// Branch on Overflow Clear
		///
		/// branch on V = 0                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// relative     BVC oper        50      2       2**
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
            if reg.p.get(StatusReg::V) == 0 {
                bus.read(0x0000); // Add cycle

                let pc = reg.pc;
                let addr = pc.wrapping_add(addr);

                if addr & HI_MASK != pc & HI_MASK {
                    bus.read(0x0000); // Add cycle
                }

                reg.pc = addr;
            }
		}
	};

    ([$opc:ident] BVS $($rest:tt)*) => {
		/// BVS
		/// Branch on Overflow Set
		///
		/// branch on V = 1                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// relative     BVS oper        70      2       2**
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
            if reg.p.get(StatusReg::V) == 1 {
                bus.read(0x0000); // Add cycle

                let pc = reg.pc;
                let addr = pc.wrapping_add(addr);

                if addr & HI_MASK != pc & HI_MASK {
                    bus.read(0x0000); // Add cycle
                }

                reg.pc = addr;
            }
		}
	};

    ([$opc:ident] CLC $($rest:tt)*) => {
		// CLC
		// Clear Carry Flag
		//
		// 0 -> C                            N  Z  C  I  D  V
		//                                   -  -  0  -  -  -
		// addressing	assembler   	opc 	bytes	cycles
		// implied      CLC             18      1       2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            reg.p.set(StatusReg::C, false);
		}
	};

    ([$opc:ident] CLD $($rest:tt)*) => {
		/// CLD
		/// Clear Decimal Mode
		///
		/// 0 -> D                            N  Z  C  I  D  V
		///                                   -  -  -  -  0  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// implied      CLD             D8      1       2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            reg.p.set(StatusReg::D, false);
		}
	};

    ([$opc:ident] CLI $($rest:tt)*) => {
		/// CLI
		/// Clear Interrupt Disable Bit
		///
		/// 0 -> I                            N  Z  C  I  D  V
		///                                   -  -  -  0  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// implied      CLI             58      1       2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            reg.p.set(StatusReg::I, false);
		}
	};

    ([$opc:ident] CLV $($rest:tt)*) => {
		/// CLV
		/// Clear Interrupt Disable Bit
		///
		/// 0 -> I                            N  Z  C  I  D  V
		///                                   -  -  -  -  -  0
		/// addressing	assembler   	opc 	bytes	cycles
		/// implied      CLV             B8      1       2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            reg.p.set(StatusReg::V, false);
		}
	};

    ([$opc:ident] CMP $($rest:tt)*) => {
		/// CMP
		/// Compare Memory with Accumulator
		///
		/// A - M                             N  Z  C  I  D  V
		///                                   +  +  +  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// immediate    CMP #oper       C9      2       2
		/// zeropage     CMP oper        C5      2       3
		/// zeropage,X   CMP oper,X      D5      2       4
		/// absolute     CMP oper        CD      3       4
		/// absolute,X   CMP oper,X      DD      3       4*
		/// absolute,Y   CMP oper,Y      D9      3       4*
		/// (indirect,X) CMP (oper,X)    C1      2       6
		/// (indirect),Y CMP (oper),Y    D1      2       5*
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
            let data = bus.read(addr);
            let ac = reg.ac;
            let tmp = ac as u16 - data as u16;

            reg.p.set(StatusReg::C, ac >= data)
             .set(StatusReg::Z, is_lo_zero(tmp))
             .set(StatusReg::N, is_neg(tmp));
		}
	};

    ([$opc:ident] CPX $($rest:tt)*) => {
		/// CPX
		/// Compare Memory and Index X
		///
		/// X - M                             N  Z  C  I  D  V
		///                                   +  +  +  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// immediate    CPX #oper       E0      2       2
		/// zeropage     CPX oper        E4      2       3
		/// absolute     CPX oper        EC      3       4
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
            let data = bus.read(addr);
            let x = reg.x;
            let tmp = x as u16 - data as u16;

            reg.p.set(StatusReg::C, x >= data)
             .set(StatusReg::Z, is_lo_zero(tmp))
             .set(StatusReg::N, is_neg(tmp));
		}
	};

    ([$opc:ident] CPY $($rest:tt)*) => {
		/// CPY
		/// Compare Memory and Index Y
		///
		/// Y - M                             N  Z  C  I  D  V
		///                                   +  +  +  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// immediate    CPY #oper       C0      2       2
		/// zeropage     CPY oper        C4      2       3
		/// absolute     CPY oper        CC      3       4
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
            let data = bus.read(addr);
            let y = reg.y;
            let tmp = y as u16 - data as u16;

            reg.p.set(StatusReg::C, y >= data)
             .set(StatusReg::Z, is_lo_zero(tmp))
             .set(StatusReg::N, is_neg(tmp));
		}
	};

    ([$opc:ident] DEC $($rest:tt)*) => {
		/// DEC
		/// Decrement Memory by One
		///
		/// M - 1 -> M                        N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// zeropage     DEC oper        C6      2       5
		/// zeropage,X   DEC oper,X      D6      2       6
		/// absolute     DEC oper        CE      3       6
		/// absolute,X   DEC oper,X      DE      3       7
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
            let data = bus.read(addr) as u16;
            let tmp = data.wrapping_sub(1);

            bus.write(addr, tmp as u8);
            reg.p.set(StatusReg::Z, is_lo_zero(tmp))
            .set(StatusReg::N, is_neg(tmp));
		}
	};

    ([$opc:ident] DEX $($rest:tt)*) => {
		// DEX
		// Decrement Index X by One
		//
		// X - 1 -> X                        N  Z  C  I  D  V
		//                                   +  +  -  -  -  -
		// addressing	assembler   	opc 	bytes	cycles
		// implied      DEX             CA      1       2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            let tmp = reg.x.wrapping_sub(1);
            reg.x = tmp;

            reg.p.set(StatusReg::Z, is_zero(tmp))
            .set(StatusReg::N, is_neg(tmp as u16));
		}
	};

    ([$opc:ident] DEY $($rest:tt)*) => {
		/// DEY
		/// Decrement Index Y by One
		///
		/// Y - 1 -> Y                        N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing	assembler   	opc 	bytes	cycles
		/// implied      DEY             88      1       2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            let tmp = reg.y.wrapping_sub(1);
            reg.y = tmp;

            reg.p.set(StatusReg::Z, is_zero(tmp))
            .set(StatusReg::N, is_neg(tmp as u16));
		}
	};

	([$opc:ident] EOR $($rest:tt)*) => {
		/// EOR
		/// Exclusive-OR Memory with Accumulator
		///
		/// addressing	assembler	opc	bytes	cycles
		/// A EOR M -> A                      N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// immediate    EOR #oper       49     2        2
		/// zeropage     EOR oper        45     2        3
		/// zeropage,X   EOR oper,X	     55     2        4
		/// absolute     EOR oper        4D     3        4
		/// absolute,X   EOR oper,X      5D     3        4*
		/// absolute,Y   EOR oper,Y      59     3        4*
		/// (indirect,X) EOR (oper,X)    41     2        6
		/// (indirect),Y EOR (oper),Y    51     2        5*
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
			let data = bus.read(addr) as u16;
            let tmp = reg.ac as u16 ^ data;
			reg.ac = tmp as u8;

            reg.p.set(StatusReg::Z, is_lo_zero(tmp))
			.set(StatusReg::N, is_neg(tmp));
		}
	};

	([$opc:ident] INC $($rest:tt)*) => {
		/// INC
		/// Increment Memory by One
		///
		/// M + 1 -> M                        N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// zeropage     INC oper        E6     2        5
		/// zeropage,X   INC oper,X      F6     2        6
		/// absolute     INC oper        EE     3        6
		/// absolute,X   INC oper,X      FE     3        7
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
            let data = bus.read(addr) as u16;
            let tmp = data.wrapping_add(1);

            bus.write(addr, tmp as u8);
            reg.p.set(StatusReg::Z, is_lo_zero(tmp))
            .set(StatusReg::N, is_neg(tmp));
		}
	};

    ([$opc:ident] INX $($rest:tt)*) => {
		/// INX
		/// Increment Index X by One
		///
		/// X + 1 -> X                        N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      INX             E8     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            let tmp = reg.x.wrapping_add(1);
            reg.x = tmp;

            reg.p.set(StatusReg::Z, is_zero(tmp))
            .set(StatusReg::N, is_neg(tmp as u16));
		}
	};

    ([$opc:ident] INY $($rest:tt)*) => {
		/// INY
		/// Increment Index Y by One
		///
		/// Y + 1 -> Y                        N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      INY             C8     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            let tmp = reg.y.wrapping_add(1);
            reg.x = tmp;

            reg.p.set(StatusReg::Z, is_zero(tmp))
            .set(StatusReg::N, is_neg(tmp as u16));
		}
	};

	([$opc:ident] JAM $($rest:tt)*) => {
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
		}
	};

	([$opc:ident] JMP $($rest:tt)*) => {
		/// JMP
		/// Jump to New Location
		///
		/// push (PC+2),                      N  Z  C  I  D  V
		/// (PC+1) -> PCL                     -  -  -  -  -  -
		/// (PC+2) -> PCH
		/// addressing   assembler       opc    bytes    cycles
		/// absolute     JMP oper        4C     3        3
		/// indirect     JMP (oper)      6C     3        5
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            reg.pc = am!(reg, bus, $($rest)*);
		}
	};

	([$opc:ident] JSR $($rest:tt)*) => {
		/// JSR
		/// Jump to New Location Saving Return Address
		///
		/// push (PC+2),                      N  Z  C  I  D  V
		/// (PC+1) -> PCL                     -  -  -  -  -  -
		/// (PC+2) -> PCH
		/// addressing   assembler       opc    bytes    cycles
		/// absolute     JSR oper        20     3        6
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
			let pc = reg.pc;
			let mut sp = reg.sp as u16;

			bus.write(PS + sp, (pc >> 8) as u8);
			sp -= 1;
			bus.write(PS + sp, pc as u8);
			sp -= 1;

			reg.sp = sp as u8;
			reg.pc = addr;
		}
	};

	([$opc:ident] LDA $($rest:tt)*) => {
		/// LDA
		/// Load Accumulator with Memory
		///
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
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
			let data = bus.read(addr);
			reg.ac = data;
			reg.p.set(StatusReg::Z, is_zero(data))
			 .set(StatusReg::N, is_neg(data as u16));
		}
	};

	([$opc:ident] LDX $($rest:tt)*) => {
		/// LDX
		/// Load Index X with Memory
		///
		/// M -> X                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// immediate    LDX #oper       A2     2        2
		/// zeropage     LDX oper        A6     2        3
		/// zeropage,Y   LDX oper,Y      B6     2        4
		/// absolute     LDX oper        AE     3        4
		/// absolute,Y   LDX oper,Y      BE     3        4*
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
			let data = bus.read(addr);
			reg.x = data;
			reg.p.set(StatusReg::Z, is_zero(data))
			 .set(StatusReg::N, is_neg(data as u16));
		}
	};

	([$opc:ident] LDY $($rest:tt)*) => {
		/// LDY
		/// Load Index Y with Memory
		///
		/// M -> Y                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// immediate    LDY #oper       A0     2        2
		/// zeropage     LDY oper        A4     2        3
		/// zeropage,X   LDY oper,X      B4     2        4
		/// absolute     LDY oper        AC     3        4
		/// absolute,X   LDY oper,X      BC     3        4*
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
			let data = bus.read(addr);
			reg.y = data;
			reg.p.set(StatusReg::Z, is_zero(data))
			 .set(StatusReg::N, is_neg(data as u16));
		}
	};

	([$opc:ident] LSR $($rest:tt)*) => {
		/// LSR
		/// Shift One Bit Right (Memory or Accumulator)
		///
		/// 0 -> [76543210] -> C              N  Z  C  I  D  V
		///                                   0  +  +  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// accumulator  LSR A           4A     1        2
		/// zeropage     LSR oper        46     2        5
		/// zeropage,X   LSR oper,X      56     2        6
		/// absolute     LSR oper        4E     3        6
		/// absolute,X   LSR oper,X      5E     3        7
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
			let data = bus.read(addr);
			let tmp = data >> 1;
			reg.x = data;
			reg.p.set(StatusReg::C, data & 0x0001 == 0x0001)
			.set(StatusReg::Z, is_zero(tmp))
			.set(StatusReg::N, is_neg(tmp as u16));

			is_implied!(
				{ reg.ac = tmp; },
                { bus.write(addr, tmp); },
				$($rest)*
			)
		}
	};

	([$opc:ident] NOP $($rest:tt)*) => {
		/// NOP
		/// No Operation
		///
		/// ---                               N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      NOP             EA     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
		}
	};

	([$opc:ident] ORA $($rest:tt)*) => {
		/// ORA
		/// OR Memory with Accumulator
		///
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
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
			let data = bus.read(addr);
			let ac = reg.ac | data;

			reg.ac = ac;
			reg.p.set(StatusReg::Z, is_zero(ac))
			 .set(StatusReg::N, is_neg(ac as u16));
		}
	};

	([$opc:ident] PHA $($rest:tt)*) => {
		/// PHA
		/// Push Accumulator on Stack
		///
		/// push A                            N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      PHA             48     1        3
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			bus.write(PS + reg.sp as Addr, reg.ac);
			reg.sp -= 1;
		}
	};

	([$opc:ident] PHP $($rest:tt)*) => {
		/// PHP
		/// Push Processor Status on Stack
		///
		/// The status register will be pushed with the break
		/// flag and bit 5 set to 1.
		///
		/// push SR                           N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      PHP             08     1        3
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);

			let p = reg.p;
			bus.write(PS + reg.sp as Addr, (p | StatusReg::B | StatusReg::U).into());
			reg.p.set(StatusReg::B, false).set(StatusReg::U, false);
			reg.sp -= 1;
		}
	};

	([$opc:ident] PLA $($rest:tt)*) => {
		/// PLA
		/// Pull Accumulator from Stack
		///
		/// pull A                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      PLA             68     1        4
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			let sp = (reg.sp + 1) as Addr;
			let ac = bus.read(PS + sp);
			reg.p.set(StatusReg::Z, is_zero(ac))
			.set(StatusReg::N, is_neg(ac as u16));
			reg.sp = sp as u8;
			reg.ac = ac;
		}
	};

	([$opc:ident] PLP $($rest:tt)*) => {
		/// PLP
		/// Pull Processor Status from Stack
		///
		/// The status register will be pulled with the break
		/// flag and bit 5 ignored.
		///
		/// pull SR                           N  Z  C  I  D  V
		///                                      from stack
		/// addressing   assembler       opc    bytes    cycles
		/// implied      PLP             28     1        4
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			let sp = (reg.sp + 1) as Addr;
			let mut p = StatusReg::from(bus.read(PS + sp));
			p.set(StatusReg::U, true);
			reg.sp = sp as u8;
			reg.p = p;
		}
	};

	([$opc:ident] ROL $($rest:tt)*) => {
		/// ROL
		/// Rotate One Bit Left (Memory or Accumulator)
		///
		/// C <- [76543210] <- C              N  Z  C  I  D  V
		///                                   +  +  +  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// accumulator  ROL A           2A     1        2
		/// zeropage     ROL oper        26     2        5
		/// zeropage,X   ROL oper,X      36     2        6
		/// absolute     ROL oper        2E     3        6
		/// absolute,X   ROL oper,X      3E     3        7
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
			let data = bus.read(addr) as u16;
			let tmp = u16::from(reg.p.get(StatusReg::C)) << 7 | data >> 1;
			reg.p.set(StatusReg::C, tmp & 0x01 == 0x01)
			.set(StatusReg::Z, is_lo_zero(tmp))
			.set(StatusReg::N, is_neg(tmp));

			is_implied!(
                { reg.ac = tmp as u8; },
                { bus.write(addr, tmp as u8); }
            );
		}
	};

	([$opc:ident] ROR $($rest:tt)*) => {
		/// ROR
		/// Rotate One Bit Right (Memory or Accumulator)
		///
		/// C -> [76543210] -> C              N  Z  C  I  D  V
		///                                   +  +  +  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// accumulator  ROR A           6A     1        2
		/// zeropage     ROR oper        66     2        5
		/// zeropage,X   ROR oper,X      76     2        6
		/// absolute     ROR oper        6E     3        6
		/// absolute,X   ROR oper,X      7E     3        7
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
			let data = bus.read(addr) as u16;
			let tmp = u16::from(reg.p.get(StatusReg::C)) | data << 1;
			reg.p.set(StatusReg::C, tmp & HI_MASK > 0)
			.set(StatusReg::Z, is_lo_zero(tmp))
			.set(StatusReg::N, is_neg(tmp));

			is_implied!(
                { reg.ac = tmp as u8; },
                { bus.write(addr, tmp as u8); },
				$($rest)*
            );
		}
	};

	([$opc:ident] RTI $($rest:tt)*) => {
		/// RTI
		/// Return from Interrupt
		///
		/// The status register is pulled with the break flag
		/// and bit 5 ignored. Then PC is pulled from the stack.
		///
		/// pull SR, pull PC                  N  Z  C  I  D  V
		///                                      from stack
		/// addressing   assembler       opc    bytes    cycles
		/// implied      RTI             40     1        6
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest:tt)*);
			let mut sp = reg.sp as Addr + 1;
			let status = bus.read(PS + sp);
			reg.p = StatusReg::from(status);
			reg.p &= !StatusReg::B;
			reg.p &= !StatusReg::U;

			sp += 1;
			let lo_pc = bus.read(PS + sp) as Addr;
			sp += 1;
			let hi_pc = (bus.read(PS + sp) as Addr) << 8;
			reg.pc = lo_pc | hi_pc;
			reg.sp = sp as u8;
		}
	};

	([$opc:ident] RTS $($rest:tt)*) => {
		/// RTS
		/// Return from Subroutine
		///
		/// pull PC, PC+1 -> PC               N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      RTS             60     1        6
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest:tt)*);

			let mut sp = reg.sp as Addr + 1;
			let lo_pc = bus.read(PS + sp) as Addr;
			sp += 1;
			let hi_pc = (bus.read(PS + sp) as Addr) << 8;
			reg.pc = lo_pc | hi_pc;
			reg.sp = sp as u8;
		}
	};

	([$opc:ident] SBC $($rest:tt)*) => {
		/// SBC
		/// Subtract Memory from Accumulator with Borrow
		///
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
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
			// Operating in 16-bit domain to capture carry out
			let data = bus.read(addr) as u16;
			let ac = reg.ac as u16;
			let val = data ^ LO_MASK;

			let tmp = ac + val + u16::from(reg.p.get(StatusReg::C));

			reg.p.set(StatusReg::C, tmp > 255)
             .set(StatusReg::Z, is_lo_zero(tmp))
             .set(StatusReg::V, is_neg(!(ac ^ data as u16) & ac ^ tmp))
             .set(StatusReg::N, is_neg(tmp));

			reg.ac = tmp as u8;
		}
	};

	([$opc:ident] SEC $($rest:tt)*) => {
		/// SEC
		/// Set Carry Flag
		///
		/// 1 -> C                            N  Z  C  I  D  V
		///                                   -  -  1  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      SEC             38     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.p.set(StatusReg::C, true);
		}
	};

	([$opc:ident] SED $($rest:tt)*) => {
		/// SED
		/// Set Decimal Flag
		///
		/// 1 -> D                            N  Z  C  I  D  V
		///                                   -  -  -  -  1  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      SED             F8     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.p.set(StatusReg::D, true);
		}
	};

	([$opc:ident] SEI $($rest:tt)*) => {
		/// SEI
		/// Set Interrupt Disable Status
		///
		/// 1 -> I                            N  Z  C  I  D  V
		///                                   -  -  -  1  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      SEI             78     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.p.set(StatusReg::I, true);
		}
	};

	([$opc:ident] STA $($rest:tt)*) => {
		/// STA
		/// Store Accumulator in Memory
		///
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
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
			bus.write(addr, reg.ac);
		}
	};

	([$opc:ident] STX $($rest:tt)*) => {
		/// STX
		/// Store Index X in Memory
		///
		/// X -> M                            N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// zeropage     STX oper        86     2        3
		/// zeropage,Y   STX oper,Y      96     2        4
		/// absolute     STX oper        8E     3        4
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
			bus.write(addr, reg.x);
		}
	};

	([$opc:ident] STY $($rest:tt)*) => {
		/// STY
		/// Sore Index Y in Memory
		///
		/// Y -> M                            N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// zeropage     STY oper        84     2        3
		/// zeropage,X   STY oper,X      94     2        4
		/// absolute     STY oper        8C     3        4
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
			bus.write(addr, reg.y);
		}
	};

	([$opc:ident] TAX $($rest:tt)*) => {
		/// TAX
		/// Transfer Accumulator to Index X
		///
		/// A -> X                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TAX             AA     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.x = reg.ac;
			reg.p
			.set(StatusReg::Z, is_zero(reg.x))
			.set(StatusReg::N, is_neg(reg.x as u16));
		}
	};

	([$opc:ident] TAY $($rest:tt)*) => {
		/// TAY
		/// Transfer Accumulator to Index Y
		///
		/// A -> Y                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TAY             A8     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.y = reg.ac;
			reg.p
			.set(StatusReg::Z, is_zero(reg.y))
			.set(StatusReg::N, is_neg(reg.y as u16));
		}
	};

	([$opc:ident] TSX $($rest:tt)*) => {
		/// TSX
		/// Transfer Stack Pointer to Index X
		///
		/// SP -> X                           N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TSX             BA     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.x = reg.sp;
			reg.p
			.set(StatusReg::Z, is_zero(reg.x))
			.set(StatusReg::N, is_neg(reg.x as u16));
		}
	};

	([$opc:ident] TAX $($rest:tt)*) => {
		/// TAX
		/// Transfer Accumulator to Index X
		/// 
		/// A -> X                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TAX             AA     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.x = reg.ac;
			reg.p
			.set(StatusReg::Z, is_zero(reg.x))
			.set(StatusReg::N, is_neg(reg.x as u16));
		}
	};
	
	([$opc:ident] TXA $($rest:tt)*) => {
		/// TXA
		/// Transfer Index X to Accumulator
		///
		/// X -> A
		/// N	Z	C	I	D	V
		/// +	+	-	-	-	-
		/// X -> A                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TXA             8A     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.ac = reg.x;
			reg.p
			.set(StatusReg::Z, is_zero(reg.ac))
			.set(StatusReg::N, is_neg(reg.ac as u16));
		}
	};

	([$opc:ident] TXS $($rest:tt)*) => {
		/// TXS
		/// Transfer Index X to Stack Register
		///
		/// X -> SP                           N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TXS             9A     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.sp = reg.x;
		}
	};

	([$opc:ident] TAX $($rest:tt)*) => {
		/// TYA
		/// Transfer Index Y to Accumulator
		///
		/// Y -> A                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TYA             98     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.ac = reg.x;
			reg.p
			.set(StatusReg::Z, is_zero(cpu.reg.ac))
			.set(StatusReg::N, is_neg(cpu.reg.ac as u16));
		}
	};

	([$opc:ident] TYA $($rest:tt)*) => {
		/// TYA
		/// Transfer Index Y to Accumulator
		///
		/// Y -> A                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TYA             98     1        2
		fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.ac = reg.x;
			reg.p
			.set(StatusReg::Z, is_zero(reg.ac))
			.set(StatusReg::N, is_neg(reg.ac as u16));
		}
	};
}

op![#[op_69, AM_69, IN_69] ADC #&BB   ];
op![#[op_65, AM_65, IN_65] ADC &LL    ];
op![#[op_75, AM_75, IN_75] ADC &LL,X  ];
op![#[op_6d, AM_6D, IN_6D] ADC &LLHH  ];
op![#[op_7d, AM_7D, IN_7D] ADC &LLHH,X];
op![#[op_79, AM_79, IN_79] ADC &LLHH,Y];
op![#[op_61, AM_61, IN_61] ADC (&LL,X)];
op![#[op_71, AM_71, IN_71] ADC (&LL),Y];

op![#[op_29, AM_29, IN_29] AND #&BB   ];
op![#[op_25, AM_25, IN_25] AND &LL    ];
op![#[op_35, AM_35, IN_35] AND &LL,X  ];
op![#[op_2d, AM_2D, IN_2D] AND &LLHH  ];
op![#[op_3d, AM_3D, IN_3D] AND &LLHH,X];
op![#[op_39, AM_39, IN_39] AND &LLHH,Y];
op![#[op_21, AM_21, IN_21] AND (&LL,X)];
op![#[op_31, AM_31, IN_31] AND (&LL),Y];

op![#[op_0a, AM_0A, IN_0A] ASL A      ];
op![#[op_06, AM_06, IN_06] ASL &LL    ];
op![#[op_16, AM_16, IN_16] ASL &LL,X  ];
op![#[op_0e, AM_0E, IN_0E] ASL &LLHH  ];
op![#[op_1e, AM_1E, IN_1E] ASL &LLHH,X];

op![#[op_90, AM_90, IN_90] BCC &BB    ];

op![#[op_b0, AM_B0, IN_B0] BCS &BB    ];

op![#[op_f0, AM_F0, IN_F0] BEQ &BB    ];

op![#[op_24, AM_24, IN_24] BIT &BB    ];
op![#[op_2c, AM_2C, IN_2C] BIT &LLHH  ];

op![#[op_30, AM_30, IN_30] BMI &BB    ];

op![#[op_d0, AM_D0, IN_D0] BNE &BB    ];

op![#[op_10, AM_10, IN_10] BPL &BB    ];

op![#[op_00, AM_00, IN_00] BRK        ];

op![#[op_50, AM_50, IN_50] BVC &BB    ];

op![#[op_70, AM_70, IN_70] BVS &BB    ];

op![#[op_18, AM_18, IN_18] CLC        ];

op![#[op_d8, AM_D8, IN_D8] CLD        ];

op![#[op_58, AM_58, IN_58] CLI        ];

op![#[op_b8, AM_B8, IN_B8] CLV        ];

op![#[op_c9, AM_C9, IN_C9] CMP #&BB   ];
op![#[op_c5, AM_C5, IN_C5] CMP &LL    ];
op![#[op_d5, AM_D5, IN_D5] CMP &LL,X  ];
op![#[op_cd, AM_CD, IN_CD] CMP &LLHH  ];
op![#[op_dd, AM_DD, IN_DD] CMP &LLHH,X];
op![#[op_d9, AM_D9, IN_D9] CMP &LLHH,Y];
op![#[op_c1, AM_C1, IN_C1] CMP (&LL,X)];
op![#[op_d1, AM_D1, IN_D1] CMP (&LL),Y];

op![#[op_e0, AM_E0, IN_E0] CPX #&BB   ];
op![#[op_e4, AM_E4, IN_E4] CPX &LL    ];
op![#[op_ec, AM_EC, IN_EC] CPX &LLHH  ];

op![#[op_c0, AM_C0, IN_C0] CPY #&BB   ];
op![#[op_c4, AM_C4, IN_C4] CPY &LL    ];
op![#[op_cc, AM_CC, IN_CC] CPY &LLHH  ];

op![#[op_c6, AM_C6, IN_C6] DEC &LL    ];
op![#[op_d6, AM_D6, IN_D6] DEC &LL,X  ];
op![#[op_ce, AM_CE, IN_CE] DEC &LLHH  ];
op![#[op_de, AM_DE, IN_DE] DEC &LLHH,X];

op![#[op_ca, AM_CA, IN_CA] DEX        ];

op![#[op_88, AM_88, IN_88] DEY        ];

op![#[op_49, AM_49, IN_49] EOR #&BB   ];
op![#[op_45, AM_45, IN_45] EOR &LL    ];
op![#[op_55, AM_55, IN_55] EOR &LL,X  ];
op![#[op_4d, AM_4D, IN_4D] EOR &LLHH  ];
op![#[op_5d, AM_5D, IN_5D] EOR &LLHH,X];
op![#[op_59, AM_59, IN_59] EOR &LLHH,Y];
op![#[op_41, AM_41, IN_41] EOR (&LL,X)];
op![#[op_51, AM_51, IN_51] EOR (&LL),Y];

op![#[op_e6, AM_E6, IN_E6] INC &LL    ];
op![#[op_f6, AM_F6, IN_F6] INC &LL,X  ];
op![#[op_ee, AM_EE, IN_EE] INC &LLHH  ];
op![#[op_fe, AM_FE, IN_FE] INC &LLHH,X];

op![#[op_e8, AM_E8, IN_E8] INX        ];

op![#[op_c8, AM_C8, IN_C8] INY        ];

op![#[op_4c, AM_4C, IN_4C] JMP &LLHH  ];
op![#[op_6c, AM_6C, IN_6C] JMP (&LLHH)];

op![#[op_20, AM_20, IN_20] JSR &LLHH  ];

op![#[op_a9, AM_A9, IN_A9] LDA #&BB   ];
op![#[op_a5, AM_A5, IN_A5] LDA &LL    ];
op![#[op_b5, AM_B5, IN_B5] LDA &LL,X  ];
op![#[op_ad, AM_AD, IN_AD] LDA &LLHH  ];
op![#[op_bd, AM_BD, IN_BD] LDA &LLHH,X];
op![#[op_b9, AM_B9, IN_B9] LDA &LLHH,Y];
op![#[op_a1, AM_A1, IN_A1] LDA (&LL,X)];
op![#[op_b1, AM_B1, IN_B1] LDA (&LL),Y];

op![#[op_a2, AM_A2, IN_A2] LDX #&BB   ];
op![#[op_a6, AM_A6, IN_A6] LDX &LL    ];
op![#[op_b6, AM_B6, IN_B6] LDX &LL,Y  ];
op![#[op_ae, AM_AE, IN_AE] LDX &LLHH  ];
op![#[op_be, AM_BE, IN_BE] LDX &LLHH,Y];

op![#[op_a0, AM_A0, IN_A0] LDY #&BB   ];
op![#[op_a4, AM_A4, IN_A4] LDY &LL    ];
op![#[op_b4, AM_B4, IN_B4] LDY &LL,X  ];
op![#[op_ac, AM_AC, IN_AC] LDY &LLHH  ];
op![#[op_bc, AM_BC, IN_BC] LDY &LLHH,X];

op![#[op_4a, AM_4A, IN_4A] LSR A      ];
op![#[op_46, AM_46, IN_46] LSR &LL    ];
op![#[op_56, AM_56, IN_56] LSR &LL,X  ];
op![#[op_4e, AM_4E, IN_4E] LSR &LLHH  ];
op![#[op_5e, AM_5E, IN_5E] LSR &LLHH,X];

op![#[op_ea, AM_EA, IN_EA] NOP        ];

op![#[op_09, AM_09, IN_09] ORA #&BB   ];
op![#[op_05, AM_05, IN_05] ORA &LL    ];
op![#[op_15, AM_15, IN_15] ORA &LL,X  ];
op![#[op_0d, AM_0D, IN_0D] ORA &LLHH  ];
op![#[op_1d, AM_1D, IN_1D] ORA &LLHH,X];
op![#[op_19, AM_19, IN_19] ORA &LLHH,Y];
op![#[op_01, AM_01, IN_01] ORA (&LL,X)];
op![#[op_11, AM_11, IN_11] ORA (&LL),Y];

op![#[op_48, AM_48, IN_48] PHA        ];

op![#[op_08, AM_08, IN_08] PHP        ];

op![#[op_68, AM_68, IN_68] PLA        ];

op![#[op_28, AM_28, IN_28] PLP        ];

op![#[op_2a, AM_2A, IN_2A] ROL        ];
op![#[op_26, AM_26, IN_26] ROL &LL    ];
op![#[op_36, AM_36, IN_36] ROL &LL,X  ];
op![#[op_2e, AM_2E, IN_2E] ROL &LLHH  ];
op![#[op_3e, AM_3E, IN_3E] ROL &LLHH,X];

op![#[op_6a, AM_6A, IN_6A] ROR        ];
op![#[op_66, AM_66, IN_66] ROR &LL    ];
op![#[op_76, AM_76, IN_76] ROR &LL,X  ];
op![#[op_6e, AM_6E, IN_6E] ROR &LLHH  ];
op![#[op_7e, AM_7E, IN_7E] ROR &LLHH,X];

op![#[op_40, AM_40, IN_40] RTI        ];

op![#[op_60, AM_60, IN_60] RTS        ];

op![#[op_e9, AM_E9, IN_E9] SBC #&BB   ];
op![#[op_e5, AM_E5, IN_E5] SBC &LL    ];
op![#[op_f5, AM_F5, IN_F5] SBC &LL,X  ];
op![#[op_ed, AM_ED, IN_ED] SBC &LLHH  ];
op![#[op_fd, AM_FD, IN_FD] SBC &LLHH,X];
op![#[op_f9, AM_F9, IN_F9] SBC &LLHH,Y];
op![#[op_e1, AM_E1, IN_E1] SBC (&LL,X)];
op![#[op_f1, AM_F1, IN_F1] SBC (&LL),Y];

op![#[op_38, AM_38, IN_38] SEC        ];

op![#[op_f8, AM_F8, IN_F8] SED        ];

op![#[op_78, AM_78, IN_78] SEI        ];

op![#[op_85, AM_85, IN_85] STA &LL    ];
op![#[op_95, AM_95, IN_95] STA &LL,X  ];
op![#[op_8d, AM_8D, IN_8D] STA &LLHH  ];
op![#[op_9d, AM_9D, IN_9D] STA &LLHH,X];
op![#[op_99, AM_99, IN_99] STA &LLHH,Y];
op![#[op_81, AM_81, IN_81] STA (&LL,X)];
op![#[op_91, AM_91, IN_91] STA (&LL),Y];

op![#[op_86, AM_86, IN_86] STX &LL    ];
op![#[op_96, AM_96, IN_96] STX &LL,Y  ];
op![#[op_8e, AM_8E, IN_8E] STX &LLHH,Y];

op![#[op_84, AM_84, IN_84] STY &LL    ];
op![#[op_94, AM_94, IN_94] STY &LL,X  ];
op![#[op_8c, AM_8C, IN_8C] STY &LLHH,X];

op![#[op_aa, AM_AA, IN_AA] TAX        ];

op![#[op_a8, AM_A8, IN_A8] TAY        ];

op![#[op_ba, AM_BA, IN_BA] TSX        ];

op![#[op_8a, AM_8A, IN_8A] TXA        ];

op![#[op_9a, AM_9A, IN_9A] TXS        ];

op![#[op_98, AM_98, IN_98] TYA        ];

// Illegal 

// JAMS 02, 12, 22, 32, 42, 52, 62, 72, 92, B2, D2, F2
op![#[op_02, AM_02, IN_02] JAM        ];
op![#[op_12, AM_12, IN_12] JAM        ];
op![#[op_22, AM_22, IN_22] JAM        ];
op![#[op_32, AM_32, IN_32] JAM        ];
op![#[op_42, AM_42, IN_42] JAM        ];
op![#[op_52, AM_52, IN_52] JAM        ];
op![#[op_62, AM_62, IN_62] JAM        ];
op![#[op_72, AM_72, IN_72] JAM        ];
op![#[op_92, AM_92, IN_92] JAM        ];
op![#[op_b2, AM_B2, IN_B2] JAM        ];
op![#[op_d2, AM_D2, IN_D2] JAM        ];
op![#[op_f2, AM_F2, IN_F2] JAM        ];

#[cfg(test)]
mod test {
    use crate::nes::{BoardBus, board::ClockBusContext};

    use super::*;

    #[test]
    fn adc() {
		env_logger::builder()
        .filter_module("nes_ultra", log::LevelFilter::Debug)
        .init();
		
		{
			let mut reg = Registers::default();
			let mut bus = BoardBus::new();
	
			reg.p = StatusReg::U;
			reg.ac = 0x05;
			reg.pc = 0x00; // Where the next instruction will be loaded
			bus.write(0x00, 0x05); // Immediate mode value
			
			let mut bus = ClockBusContext::new(&mut bus);
			bus.read(0x0000); // Cycle to simulate the op code read.
			op_69(&mut reg, &mut bus);
			
			assert_eq!(reg.ac, 0x0A);
			assert_eq!(reg.p.get(StatusReg::N), 0);
			assert_eq!(reg.p.get(StatusReg::C), 0);
			assert_eq!(reg.p.get(StatusReg::Z), 0);
			assert_eq!(*bus.rw_count.borrow(), 2, "rw_count");
		}

		{
			let mut reg = Registers::default();
			let mut bus = BoardBus::new();
	
			reg.p = StatusReg::U;
			reg.ac = 0xE8;
			reg.pc = 0x00; // Where the next instruction will be loaded
			bus.write(0x00, 0xFD); // Zeropage mode value
			bus.write(0xFD, 0x08); // Pointed to value
			
			let mut bus = ClockBusContext::new(&mut bus);
			bus.read(0x0000); // Cycle to simulate the op code read.
			op_65(&mut reg, &mut bus);
			
			assert_eq!(reg.ac, 0xF0);
			assert_eq!(reg.p.get(StatusReg::N), 1);
			assert_eq!(reg.p.get(StatusReg::C), 0);
			assert_eq!(reg.p.get(StatusReg::Z), 0);
			assert_eq!(*bus.rw_count.borrow(), 3, "rw_count");
		}
		
		{
			let mut reg = Registers::default();
			let mut bus = BoardBus::new();
	
			reg.p = StatusReg::U;
			reg.ac = 0xFF;
			reg.x = 1;
			reg.pc = 0x00; // Where the next instruction will be loaded
			bus.write(0x00, 0xFD); // Zeropage,X mode value
			bus.write(0xFE, 0x01); // Pointed to value

			let mut bus = ClockBusContext::new(&mut bus);
			bus.read(0x0000); // Cycle to simulate the op code read.
			op_75(&mut reg, &mut bus);
			
			assert_eq!(reg.ac, 0x00);
			assert_eq!(reg.p.get(StatusReg::N), 0, "N");
			assert_eq!(reg.p.get(StatusReg::C), 1, "C");
			assert_eq!(reg.p.get(StatusReg::Z), 1, "Z");
			assert_eq!(*bus.rw_count.borrow(), 3, "rw_count");
		}
		
		{
			let mut reg = Registers::default();
			let mut bus = BoardBus::new();
	
			reg.p = StatusReg::U;
			reg.ac = 0xEF;
			reg.x = 1;
			reg.pc = 0x00; // Where the next instruction will be loaded
			bus.write(0x0000, 0xFE); // ABS LL mode value
			bus.write(0x0001, 0x01); // ABS HH mode value
			bus.write(0x01FE, 0x01); // Pointed to value (HHLL)

			let mut bus = ClockBusContext::new(&mut bus);
			bus.read(0x0000); // Cycle to simulate the op code read.
			op_6d(&mut reg, &mut bus);
			
			assert_eq!(reg.ac, 0xF0);
			assert_eq!(reg.p.get(StatusReg::N), 1, "N");
			assert_eq!(reg.p.get(StatusReg::C), 0, "C");
			assert_eq!(reg.p.get(StatusReg::Z), 0, "Z");
			assert_eq!(*bus.rw_count.borrow(), 4, "rw_count");
		}
		
		{
			let mut reg = Registers::default();
			let mut bus = BoardBus::new();
	
			reg.p = StatusReg::U;
			reg.ac = 0xEF;
			reg.x = 1;
			reg.pc = 0x00; // Where the next instruction will be loaded
			bus.write(0x0000, 0xFE); // ABS,X LL mode value
			bus.write(0x0001, 0x01); // ABS,X HH mode value
			bus.write(0x01FF, 0x01); // Pointed to value (HHLL)

			let mut bus = ClockBusContext::new(&mut bus);
			bus.read(0x0000); // Cycle to simulate the op code read.
			op_7d(&mut reg, &mut bus);
			
			assert_eq!(reg.ac, 0xF0);
			assert_eq!(reg.p.get(StatusReg::N), 1, "N");
			assert_eq!(reg.p.get(StatusReg::C), 0, "C");
			assert_eq!(reg.p.get(StatusReg::Z), 0, "Z");
			assert_eq!(*bus.rw_count.borrow(), 4, "rw_count");
		}

		{
			let mut reg = Registers::default();
			let mut bus = BoardBus::new();
	
			reg.p = StatusReg::U;
			reg.ac = 0xEF;
			reg.y = 2;
			reg.pc = 0x00; // Where the next instruction will be loaded
			bus.write(0x0000, 0xFE); // ABS,Y LL mode value
			bus.write(0x0001, 0x01); // ABS,Y HH mode value
			bus.write(0x0200, 0x01); // Pointed to value (HHLL)

			let mut bus = ClockBusContext::new(&mut bus);
			bus.read(0x0000); // Cycle to simulate the op code read.
			op_79(&mut reg, &mut bus);
			
			assert_eq!(reg.ac, 0xF0);
			assert_eq!(reg.p.get(StatusReg::N), 1, "N");
			assert_eq!(reg.p.get(StatusReg::C), 0, "C");
			assert_eq!(reg.p.get(StatusReg::Z), 0, "Z");
			assert_eq!(*bus.rw_count.borrow(), 5, "rw_count");
		}

		{
			let mut reg = Registers::default();
			let mut bus = BoardBus::new();
	
			reg.p = StatusReg::U;
			reg.ac = 0xEF;
			reg.x = 2;
			reg.pc = 0x00; // Where the next instruction will be loaded
			bus.write(0x0000, 0xFE); // (OPER,X) IND mode value
			bus.write(0x0100, 0x01); // IND addr LL
			bus.write(0x0101, 0x04); // IND addr HH
			bus.write(0x0401, 0x01); // Pointed to value (HHLL)

			let mut bus = ClockBusContext::new(&mut bus);
			bus.read(0x0000); // Cycle to simulate the op code read.
			op_61(&mut reg, &mut bus);
			
			assert_eq!(reg.ac, 0xF0);
			assert_eq!(reg.p.get(StatusReg::N), 1, "N");
			assert_eq!(reg.p.get(StatusReg::C), 0, "C");
			assert_eq!(reg.p.get(StatusReg::Z), 0, "Z");
			assert_eq!(*bus.rw_count.borrow(), 6, "rw_count");
		}
		
		{
			let mut reg = Registers::default();
			let mut bus = BoardBus::new();
	
			reg.p = StatusReg::U;
			reg.ac = 0x48;
			reg.y = 1;
			reg.pc = 0x00; // Where the next instruction will be loaded
			bus.write(0x0000, 0xFD); // (OPER,X) IND mode value
			bus.write(0x00FD, 0x01); // IND addr LL
			bus.write(0x00FE, 0x04); // IND addr HH
			bus.write(0x0402, 0x08); // Pointed to value (HHLL)

			let mut bus = ClockBusContext::new(&mut bus);
			bus.read(0x0000); // Cycle to simulate the op code read.
			op_71(&mut reg, &mut bus);

			assert_eq!(reg.ac, 0x50);
			assert_eq!(reg.p.get(StatusReg::N), 0, "N");
			assert_eq!(reg.p.get(StatusReg::C), 0, "C");
			assert_eq!(reg.p.get(StatusReg::Z), 0, "Z");
			assert_eq!(*bus.rw_count.borrow(), 5, "rw_count");
		}
    }
}
