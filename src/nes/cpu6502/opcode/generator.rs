use crate::nes::LO_MASK;

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
        pub const $code: AddrMode = AddrMode::A;
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
        pub const $code: AddrMode = AddrMode::ABS;
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
        pub const $code: AddrMode = AddrMode::ABX;
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
        pub const $code: AddrMode = AddrMode::ABY;
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
        pub const $code: AddrMode = AddrMode::IMM;
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
        pub const $code: AddrMode = AddrMode::IMP;
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
        pub const $code: AddrMode = AddrMode::IND;
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
        pub const $code: AddrMode = AddrMode::IZX;
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
        pub const $code: AddrMode = AddrMode::IZY;
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
        pub const $code: AddrMode = AddrMode::REL;
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
        pub const $code: AddrMode = AddrMode::ZPG;
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
        pub const $code: AddrMode = AddrMode::ZPX;
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
        pub const $code: AddrMode = AddrMode::ZPY;
    };
}

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

macro_rules! op {
	(#[$opc:tt, $am:tt, $inst:tt] $op:tt $($rest:tt)*) => {
		am_const!([$am] $($rest)*);

		pub const $inst: Instruction = Instruction::$op;

		op!([$opc] $op $($rest)*);

		//op_tests!(#$opc $($rest)*);
	};

    ([$opc:ident] ADC $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// branch on C = 0                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BCC oper        90      2       2**
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// branch on C = 1                         N Z C I D V
		///                                         - - - - - -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BCS oper        B0      2       2**
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// branch on Z = 1                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BEQ oper        F0      2       2**
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// A AND M, M7 -> N, M6 -> V         N  Z  C  I  D  V
		///                                   M7 +  -  -  -  M6
		/// addressing   assembler       opc     bytes   cycles
		/// zeropage     BIT oper        24      2       3
		/// absolute     BIT oper        2C      3       4
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// branch on N = 1                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BMI oper        30      2       2**
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// branch on Z = 0                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BNE oper        D0      2       2**
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// branch on N = 0                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// relative     BPL oper        10      2       2**
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// interrupt,                        N  Z  C  I  D  V
		/// push PC+2, push SR                -  -  -  1  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// implied      BRK             00      1       7
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			reg.pc += 1;
            let mut p = reg.p;
            p.set(StatusReg::I, true);

            let mut sp = reg.sp as Addr;
            let pc = reg.pc;
            bus.write(PS.wrapping_add(sp), (pc >> 8) as u8);
            sp = sp.wrapping_sub(1);

            bus.write(PS.wrapping_add(sp), pc as u8);
            sp = sp.wrapping_sub(1);

            p.set(StatusReg::B, true);
            bus.write(PS.wrapping_add(sp), p.into());
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
		///```text
		/// branch on V = 0                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BVC oper        50      2       2**
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// branch on V = 1                   N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// relative     BVS oper        70      2       2**
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		/// CLC
		/// Clear Carry Flag
		///```text
		/// 0 -> C                            N  Z  C  I  D  V
		///                                   -  -  0  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// implied      CLC             18      1       2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            reg.p.set(StatusReg::C, false);
		}
	};

    ([$opc:ident] CLD $($rest:tt)*) => {
		/// CLD
		/// Clear Decimal Mode
		///```text
		/// 0 -> D                            N  Z  C  I  D  V
		///                                   -  -  -  -  0  -
		/// addressing   assembler       opc     bytes   cycles
		/// implied      CLD             D8      1       2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            reg.p.set(StatusReg::D, false);
		}
	};

    ([$opc:ident] CLI $($rest:tt)*) => {
		/// CLI
		/// Clear Interrupt Disable Bit
		///```text
		/// 0 -> I                            N  Z  C  I  D  V
		///                                   -  -  -  0  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// implied      CLI             58      1       2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            reg.p.set(StatusReg::I, false);
		}
	};

    ([$opc:ident] CLV $($rest:tt)*) => {
		/// CLV
		/// Clear Interrupt Disable Bit
		///```text
		/// 0 -> I                            N  Z  C  I  D  V
		///                                   -  -  -  -  -  0
		/// addressing   assembler       opc     bytes   cycles
		/// implied      CLV             B8      1       2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            reg.p.set(StatusReg::V, false);
		}
	};

    ([$opc:ident] CMP $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
			let data = bus.read(addr) as u16;
            let ac = reg.ac as u16;
            let tmp = (ac).wrapping_sub(data);

            reg.p.set(StatusReg::C, ac >= data)
             .set(StatusReg::Z, is_lo_zero(tmp))
             .set(StatusReg::N, is_neg(tmp));
		}
	};

    ([$opc:ident] CPX $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
            let data = bus.read(addr) as u16;
            let x = reg.x as u16;
            let tmp = (x).wrapping_sub(data);

            reg.p.set(StatusReg::C, x >= data)
             .set(StatusReg::Z, is_lo_zero(tmp))
             .set(StatusReg::N, is_neg(tmp));
		}
	};

    ([$opc:ident] CPY $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
			let data = bus.read(addr) as u16;
            let y = reg.y as u16;
            let tmp = (y).wrapping_sub(data);

            reg.p.set(StatusReg::C, y >= data)
             .set(StatusReg::Z, is_lo_zero(tmp))
             .set(StatusReg::N, is_neg(tmp));
		}
	};

    ([$opc:ident] DEC $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
            let data = bus.read(addr) as u16;
            let tmp = data.wrapping_sub(1);

            bus.write(addr, tmp as u8);
            reg.p.set(StatusReg::Z, is_lo_zero(tmp))
            .set(StatusReg::N, is_neg(tmp));
		}
	};

    ([$opc:ident] DEX $($rest:tt)*) => {
		/// DEX
		/// Decrement Index X by One
		///```text
		/// X - 1 -> X                        N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// implied      DEX             CA      1       2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// Y - 1 -> Y                        N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc     bytes   cycles
		/// implied      DEY             88      1       2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// M + 1 -> M                        N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// zeropage     INC oper        E6     2        5
		/// zeropage,X   INC oper,X      F6     2        6
		/// absolute     INC oper        EE     3        6
		/// absolute,X   INC oper,X      FE     3        7
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// X + 1 -> X                        N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      INX             E8     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// Y + 1 -> Y                        N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      INY             C8     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
            let tmp = reg.y.wrapping_add(1);
            reg.x = tmp;

            reg.p.set(StatusReg::Z, is_zero(tmp))
            .set(StatusReg::N, is_neg(tmp as u16));
		}
	};

	([$opc:ident] JAM $($rest:tt)*) => {
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
		}
	};

	([$opc:ident] JMP $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            reg.pc = am!(reg, bus, $($rest)*);
		}
	};

	([$opc:ident] JSR $($rest:tt)*) => {
		/// JSR
		/// Jump to New Location Saving Return Address
		///```text
		/// push (PC+2),                      N  Z  C  I  D  V
		/// (PC+1) -> PCL                     -  -  -  -  -  -
		/// (PC+2) -> PCH
		/// addressing   assembler       opc    bytes    cycles
		/// absolute     JSR oper        20     3        6
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let addr = am!(reg, bus, $($rest)*);
			let pc = reg.pc;
			let mut sp = reg.sp as u16;

			bus.write(PS.wrapping_add(sp), (pc >> 8) as u8);
			sp = sp.wrapping_sub(1);
			bus.write(PS.wrapping_add(sp), pc as u8);
			sp = sp.wrapping_sub(1);

			reg.sp = sp as u8;
			reg.pc = addr;
		}
	};

	([$opc:ident] LDA $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// ---                               N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      NOP             EA     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
            let _ = am!(reg, bus, $($rest)*);
		}
	};

	([$opc:ident] ORA $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// push A                            N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      PHA             48     1        3
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			bus.write(PS + reg.sp as Addr, reg.ac);
			reg.sp = reg.sp.wrapping_sub(1);
		}
	};

	([$opc:ident] PHP $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);

			let p = reg.p;
			bus.write(PS + reg.sp as Addr, (p | StatusReg::B | StatusReg::U).into());
			reg.p.set(StatusReg::B, false).set(StatusReg::U, false);
			reg.sp = reg.sp.wrapping_sub(1);
		}
	};

	([$opc:ident] PLA $($rest:tt)*) => {
		/// PLA
		/// Pull Accumulator from Stack
		///```text
		/// pull A                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      PLA             68     1        4
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			let sp = (reg.sp + 1) as Addr;
			let ac = bus.read(PS.wrapping_add(sp));
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
		///```text
		/// pull SR                           N  Z  C  I  D  V
		///                                      from stack
		/// addressing   assembler       opc    bytes    cycles
		/// implied      PLP             28     1        4
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			let sp = (reg.sp + 1) as Addr;
			let mut p = StatusReg::from(bus.read(PS.wrapping_add(sp)));
			p.set(StatusReg::U, true);
			reg.sp = sp as u8;
			reg.p = p;
		}
	};

	([$opc:ident] ROL $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// pull SR, pull PC                  N  Z  C  I  D  V
		///                                      from stack
		/// addressing   assembler       opc    bytes    cycles
		/// implied      RTI             40     1        6
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest:tt)*);
			let mut sp = reg.sp as Addr + 1;
			let status = bus.read(PS.wrapping_add(sp));
			reg.p = StatusReg::from(status);
			reg.p &= !StatusReg::B;
			reg.p &= !StatusReg::U;

			sp += 1;
			let lo_pc = bus.read(PS.wrapping_add(sp)) as Addr;
			sp += 1;
			let hi_pc = (bus.read(PS.wrapping_add(sp)) as Addr) << 8;
			reg.pc = lo_pc | hi_pc;
			reg.sp = sp as u8;
		}
	};

	([$opc:ident] RTS $($rest:tt)*) => {
		/// RTS
		/// Return from Subroutine
		///```text
		/// pull PC, PC+1 -> PC               N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      RTS             60     1        6
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest:tt)*);

			let mut sp = reg.sp as Addr + 1;
			let lo_pc = bus.read(PS.wrapping_add(sp)) as Addr;
			sp += 1;
			let hi_pc = (bus.read(PS.wrapping_add(sp)) as Addr) << 8;
			reg.pc = lo_pc | hi_pc;
			reg.sp = sp as u8;
		}
	};

	([$opc:ident] SBC $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// 1 -> C                            N  Z  C  I  D  V
		///                                   -  -  1  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      SEC             38     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.p.set(StatusReg::C, true);
		}
	};

	([$opc:ident] SED $($rest:tt)*) => {
		/// SED
		/// Set Decimal Flag
		///```text
		/// 1 -> D                            N  Z  C  I  D  V
		///                                   -  -  -  -  1  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      SED             F8     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.p.set(StatusReg::D, true);
		}
	};

	([$opc:ident] SEI $($rest:tt)*) => {
		/// SEI
		/// Set Interrupt Disable Status
		///```text
		/// 1 -> I                            N  Z  C  I  D  V
		///                                   -  -  -  1  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      SEI             78     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.p.set(StatusReg::I, true);
		}
	};

	([$opc:ident] STA $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
			bus.write(addr, reg.ac);
		}
	};

	([$opc:ident] STX $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
			bus.write(addr, reg.x);
		}
	};

	([$opc:ident] STY $($rest:tt)*) => {
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
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let addr = am!(reg, bus, $($rest)*);
			bus.write(addr, reg.y);
		}
	};

	([$opc:ident] TAX $($rest:tt)*) => {
		/// TAX
		/// Transfer Accumulator to Index X
		///```text
		/// A -> X                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TAX             AA     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// A -> Y                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TAY             A8     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// SP -> X                           N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TSX             BA     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// A -> X                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TAX             AA     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// X -> A                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TXA             8A     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// X -> SP                           N  Z  C  I  D  V
		///                                   -  -  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TXS             9A     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.sp = reg.x;
		}
	};

	([$opc:ident] TAX $($rest:tt)*) => {
		/// TYA
		/// Transfer Index Y to Accumulator
		///```text
		/// Y -> A                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TYA             98     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
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
		///```text
		/// Y -> A                            N  Z  C  I  D  V
		///                                   +  +  -  -  -  -
		/// addressing   assembler       opc    bytes    cycles
		/// implied      TYA             98     1        2
		/// ```
		pub fn $opc(reg: &mut Registers, bus: &mut dyn Bus) {
			let _ = am!(reg, bus, $($rest)*);
			reg.ac = reg.x;
			reg.p
			.set(StatusReg::Z, is_zero(reg.ac))
			.set(StatusReg::N, is_neg(reg.ac as u16));
		}
	};
}

pub(crate) use am;
pub(crate) use am_const;
pub(crate) use is_implied;
pub(crate) use op;
