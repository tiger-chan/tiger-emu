use core::fmt;

use super::{Cpu6502, CPU};

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
    // /// Accumulator
    // ///
    // /// OPC A
    // ///
    // /// operand is AC (implied single byte instruction)
    // A,
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

pub type AddrFn = fn(cpu: &mut Cpu6502) -> u8;

/// Absolute
///
/// OPC $LLHH
///
/// operand is address $HHLL *
pub(super) fn abs(cpu: &mut Cpu6502) -> u8 {
    let lo = cpu.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;
    let hi = (cpu.read(cpu.reg.pc) as u16) << 8;
    cpu.reg.pc += 1;

    cpu.state.addr_abs = hi | lo;

    0
}

/// Absolute, X-indexed
///
/// OPC $LLHH,X
///
/// operand is address; effective address is address incremented by X with carry **
pub(super) fn abx(cpu: &mut Cpu6502) -> u8 {
    let lo = cpu.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;

    let hi = (cpu.read(cpu.reg.pc) as u16) << 8;
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
pub(super) fn aby(cpu: &mut Cpu6502) -> u8 {
    let lo = cpu.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;

    let hi = (cpu.read(cpu.reg.pc) as u16) << 8;
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
pub(super) fn imm(cpu: &mut Cpu6502) -> u8 {
    cpu.state.addr_abs = cpu.reg.pc;
    cpu.reg.pc += 1;

    0
}

/// Implied
///
/// OPC
///
/// operand implied
pub(super) fn imp(cpu: &mut Cpu6502) -> u8 {
    cpu.state.fetched = cpu.reg.ac;

    0
}

/// Indirect
///
/// OPC ($LLHH)
///
/// operand is address; effective address is contents of word at address: C.w($HHLL)
pub(super) fn ind(cpu: &mut Cpu6502) -> u8 {
    let lo = cpu.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;
    let hi = (cpu.read(cpu.reg.pc) as u16) << 8;
    cpu.reg.pc += 1;

    let ptr = hi | lo;

    if lo == 0x00FF {
        // Simulate page boundary hardware bug
        let lo = cpu.read(ptr + 0) as u16;
        let hi = (cpu.read(ptr & 0xFF00) as u16) << 8;
        cpu.state.addr_abs = hi | lo;
    } else {
        // Simulate page boundary hardware bug
        let lo = cpu.read(ptr + 0) as u16;
        let hi = (cpu.read(ptr + 1) as u16) << 8;
        cpu.state.addr_abs = hi | lo;
    }

    0
}

/// Indirect, X-indexed
///
/// OPC ($LL,X)
///
/// operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
pub(super) fn izx(cpu: &mut Cpu6502) -> u8 {
    let t = cpu.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;

    let x = cpu.reg.x as u16;

    let lo = cpu.read((t + x) & 0x00FF) as u16;
    let hi = (cpu.read((t + x + 1) & 0x00FF) as u16) << 8;

    cpu.state.addr_abs = hi | lo;

    0
}

/// Indirect, Y-indexed
///
/// OPC ($LL),Y
///
/// operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
pub(super) fn izy(cpu: &mut Cpu6502) -> u8 {
    let t = cpu.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;

    let y = cpu.reg.y as u16;

    let lo = cpu.read(t & 0x00FF) as u16;
    let hi = (cpu.read((t + 1) & 0x00FF) as u16) << 8;

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
pub(super) fn rel(cpu: &mut Cpu6502) -> u8 {
    let addr = cpu.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;
    cpu.state.addr_rel = if addr & 0x80 != 0 { addr | 0xFF00 } else { addr };

    0
}

/// Zeropage
///
/// OPC $LL
///
/// operand is zeropage address (hi-byte is zero, address = $00LL)
pub(super) fn zpg(cpu: &mut Cpu6502) -> u8 {
    cpu.state.addr_abs = cpu.read(cpu.reg.pc) as u16;
    cpu.reg.pc += 1;

    cpu.state.addr_abs = cpu.state.addr_abs & 0x00FF;

    0
}

/// Zeropage, X-indexed
///
/// OPC $LL,X
///
/// operand is zeropage address; effective address is address incremented by X without carry **
pub(super) fn zpx(cpu: &mut Cpu6502) -> u8 {
    cpu.state.addr_abs = cpu.read(cpu.reg.pc) as u16 + cpu.reg.x as u16;
    cpu.reg.pc += 1;

    cpu.state.addr_abs = cpu.state.addr_abs & 0x00FF;

    0
}

/// Zeropage, Y-indexed
///
/// OPC $LL,Y
///
/// operand is zeropage address; effective address is address incremented by Y without carry **
pub(super) fn zpy(cpu: &mut Cpu6502) -> u8 {
    cpu.state.addr_abs = cpu.read(cpu.reg.pc) as u16 + cpu.reg.y as u16;
    cpu.reg.pc += 1;

    cpu.state.addr_abs = cpu.state.addr_abs & 0x00FF;

    0
}

impl From<AddrMode> for AddrFn {
    fn from(value: AddrMode) -> Self {
        match value {
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
