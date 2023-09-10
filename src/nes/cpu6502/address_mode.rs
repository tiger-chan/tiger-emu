use core::fmt;

use crate::bus::Bus;

use super::Cpu6502;

///
///   * 16-bit address words are little endian, lo(w)-byte first, followed by the hi(gh)-byte.
///     (An assembler will use a human readable, big-endian notation as in $HHLL.)
///
///  ** The available 16-bit address space is conceived as consisting of pages of 256 bytes each, with
///     address hi-bytes represententing the page index. An increment with carry may affect the hi-byte
///     and may thus result in a crossing of page boundaries, adding an extra cycle to the execution.
///     Increments without carry do not affect the hi-byte of an address and no page transitions do occur.
///     Generally, increments of 16-bit addresses include a carry, increments of zeropage addresses don't.
///     Notably this is not related in any way to the state of the carry bit of the accumulator.
///
/// *** Branch offsets are signed 8-bit values, -128 ... +127, negative offsets in two's complement.
///     Page transitions may occur and add an extra cycle to the exucution.
///
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AddrMode {
    /// Accumulator
    ///
    /// OPC A
    ///
    /// operand is AC (implied single byte instruction)
    A,
    /// Absolute
    ///
    /// OPC $LLHH
    ///
    /// operand is address $HHLL *
    ABS,
    /// Absolute, X-indexed
    ///
    /// OPC $LLHH,X
    ///
    /// operand is address; effective address is address incremented by X with carry **
    ABX,
    /// Absolute, Y-indexed
    ///
    /// OPC $LLHH,Y
    ///
    /// operand is address; effective address is address incremented by Y with carry **
    ABY,
    /// Immediate
    ///
    /// OPC #$BB
    ///
    /// operand is byte BB
    IMM,
    /// Implied
    ///
    /// OPC
    ///
    /// operand implied
    IMP,
    /// Indirect
    ///
    /// OPC ($LLHH)
    ///
    /// operand is address; effective address is contents of word at address: C.w($HHLL)
    IND,
    /// Indirect, X-indexed
    ///
    /// OPC ($LL,X)
    ///
    /// operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
    IZX,
    /// Indirect, Y-indexed
    ///
    /// OPC ($LL),Y
    ///
    /// operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
    IZY,
    /// Relative
    ///
    /// OPC $BB
    ///
    /// branch target is PC + signed offset BB ***
    REL,
    /// Zeropage
    ///
    /// OPC $LL
    ///
    /// operand is zeropage address (hi-byte is zero, address = $00LL)
    ZPG,
    /// Zeropage, X-indexed
    ///
    /// OPC $LL,X
    ///
    /// operand is zeropage address; effective address is address incremented by X without carry **
    ZPX,
    /// Zeropage, Y-indexed
    ///
    /// OPC $LL,Y
    ///
    /// operand is zeropage address; effective address is address incremented by Y without carry **
    ZPY,
}

impl fmt::Display for AddrMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type AddrFn = fn(cpu: &mut Cpu6502, &mut dyn Bus) -> u8;

/// Absolute
///
/// OPC $LLHH
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
pub(super) fn abs(cpu: &mut Cpu6502, bus: &mut dyn Bus) -> u8 {
    let lo = bus.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;
    let hi = (bus.read(cpu.reg.pc) as u16) << 8;
    cpu.reg.pc += 1;

    cpu.state.addr_abs = hi | lo;

    0
}

/// Absolute, X-indexed
///
/// OPC $LLHH,X
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
pub(super) fn abx(cpu: &mut Cpu6502, bus: &mut dyn Bus) -> u8 {
    let lo = bus.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;

    let hi = (bus.read(cpu.reg.pc) as u16) << 8;
    cpu.reg.pc += 1;

    cpu.state.addr_abs = hi | lo + cpu.reg.x as u16;

    if cpu.state.addr_abs & 0xFF00 != hi {
        1
    } else {
        0
    }
}

/// Absolute, Y-indexed
///
/// OPC $LLHH,Y
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
pub(super) fn aby(cpu: &mut Cpu6502, bus: &mut dyn Bus) -> u8 {
    let lo = bus.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;

    let hi = (bus.read(cpu.reg.pc) as u16) << 8;
    cpu.reg.pc += 1;

    cpu.state.addr_abs = hi | lo + cpu.reg.y as u16;

    if cpu.state.addr_abs & 0xFF00 != hi {
        1
    } else {
        0
    }
}

/// Immediate
///
/// OPC #$BB
///
/// operand is byte BB
///
/// Here, a literal operand is given immediately after the instruction.
/// The operand is always an 8-bit value and the total instruction
/// length is always 2 bytes. In memory, the operand is a single byte
/// following immediately after the instruction code. In assembler, the
/// mode is usually indicated by a "#" prefix adjacent to the operand.
pub(super) fn imm(cpu: &mut Cpu6502, _bus: &mut dyn Bus) -> u8 {
    cpu.state.addr_abs = cpu.reg.pc;
    cpu.reg.pc += 1;

    0
}

/// Implied
///
/// OPC
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
pub(super) fn imp(cpu: &mut Cpu6502, _bus: &mut dyn Bus) -> u8 {
    cpu.state.fetched = cpu.reg.ac;

    0
}

/// Indirect
///
/// OPC ($LLHH)
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
pub(super) fn ind(cpu: &mut Cpu6502, bus: &mut dyn Bus) -> u8 {
    let lo = bus.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;
    let hi = (bus.read(cpu.reg.pc) as u16) << 8;
    cpu.reg.pc += 1;

    let ptr = hi | lo;

    if lo == 0x00FF {
        // Simulate page boundary hardware bug
        let lo = bus.read(ptr + 0) as u16;
        let hi = (bus.read(ptr & 0xFF00) as u16) << 8;
        cpu.state.addr_abs = hi | lo;
    } else {
        // Simulate page boundary hardware bug
        let lo = bus.read(ptr + 0) as u16;
        let hi = (bus.read(ptr + 1) as u16) << 8;
        cpu.state.addr_abs = hi | lo;
    }

    0
}

/// Indirect, X-indexed
///
/// OPC ($LL,X)
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
pub(super) fn izx(cpu: &mut Cpu6502, bus: &mut dyn Bus) -> u8 {
    let t = bus.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;

    let x = cpu.reg.x as u16;

    let lo = bus.read((t + x) & 0x00FF) as u16;
    let hi = (bus.read((t + x + 1) & 0x00FF) as u16) << 8;

    cpu.state.addr_abs = hi | lo;

    0
}

/// Indirect, Y-indexed
///
/// OPC ($LL),Y
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
pub(super) fn izy(cpu: &mut Cpu6502, bus: &mut dyn Bus) -> u8 {
    let t = bus.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;

    let y = cpu.reg.y as u16;

    let lo = bus.read(t & 0x00FF) as u16;
    let hi = (bus.read((t + 1) & 0x00FF) as u16) << 8;

    cpu.state.addr_abs = hi | lo + y;

    if cpu.state.addr_abs & 0xFF00 != hi {
        1
    } else {
        0
    }
}

/// Relative
///
/// OPC $BB
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
pub(super) fn rel(cpu: &mut Cpu6502, bus: &mut dyn Bus) -> u8 {
    let addr = bus.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;
    cpu.state.addr_rel = if addr & 0x80 != 0 {
        addr | 0xFF00
    } else {
        addr
    };

    0
}

/// Zeropage
///
/// OPC $LL
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
pub(super) fn zpg(cpu: &mut Cpu6502, bus: &mut dyn Bus) -> u8 {
    cpu.state.addr_abs = bus.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;

    cpu.state.addr_abs = cpu.state.addr_abs & 0x00FF;

    0
}

/// Zeropage, X-indexed
///
/// OPC $LL,X
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
pub(super) fn zpx(cpu: &mut Cpu6502, bus: &mut dyn Bus) -> u8 {
    cpu.state.addr_abs = bus.read(cpu.reg.pc) as u16 + cpu.reg.x as u16;
    cpu.reg.pc += 1;

    cpu.state.addr_abs = cpu.state.addr_abs & 0x00FF;

    0
}

/// Zeropage, Y-indexed
///
/// OPC $LL,Y
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
pub(super) fn zpy(cpu: &mut Cpu6502, bus: &mut dyn Bus) -> u8 {
    cpu.state.addr_abs = bus.read(cpu.reg.pc) as u16 + cpu.reg.y as u16;
    cpu.reg.pc += 1;

    cpu.state.addr_abs = cpu.state.addr_abs & 0x00FF;

    0
}

impl From<AddrMode> for AddrFn {
    fn from(value: AddrMode) -> Self {
        match value {
			AddrMode::A => imp,
            AddrMode::ABS => abs,
            AddrMode::ABX => abx,
            AddrMode::ABY => aby,
            AddrMode::IMM => imm,
            AddrMode::IMP => imp,
            AddrMode::IND => ind,
            AddrMode::IZX => izx,
            AddrMode::IZY => izy,
            AddrMode::REL => rel,
            AddrMode::ZPG => zpg,
            AddrMode::ZPX => zpx,
            AddrMode::ZPY => zpy,
        }
    }
}
