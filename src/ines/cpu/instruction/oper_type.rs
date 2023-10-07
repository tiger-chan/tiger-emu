// https://www.masswerk.at/6502/6502_instruction_set.html

use core::fmt;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OperType {
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
    /// ILLEGAL
    LAX,
    /// ILLEGAL
    XNOP,
    /// ILLEGAL
    SAX,
    /// ILLEGAL
    USBC,
    /// ILLEGAL
    JAM,
}

impl fmt::Display for OperType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::USBC => {
                write!(f, "*SBC")
            }
            Self::XNOP => {
                write!(f, "*NOP")
            }
            x if x >= &Self::XXX => {
                write!(f, "*{:?}", self)
            }
            _ => {
                write!(f, " {:?}", self)
            }
        }
    }
}
