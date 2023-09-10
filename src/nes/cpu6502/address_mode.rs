use core::fmt;

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
