mod actions;
mod addr_mode;
mod addr_mode_data;
mod iterator;
mod oper_data;
mod oper_type;
mod result;
mod state;

use actions::*;
pub use addr_mode::AddrMode;
pub use addr_mode_data::AddrModeData;
pub use iterator::InstructionIterator;
pub use oper_data::OperData;
pub use oper_type::OperType;
pub use result::InstructionResult;
pub use state::InstructionState;

use crate::ines::io::RwDevice;

use super::Registers;

pub type Operation = fn(&mut Registers, &mut dyn RwDevice, &mut InstructionState) -> i8;

pub type Instruc = fn() -> InstructionIterator;

const INOOP: Instruc = op_ea;

#[rustfmt::skip]
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

#[rustfmt::skip]
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

const INOIN: OperType = OperType::XXX;

#[rustfmt::skip]
pub const INSTRUCTION_TYPE: [OperType; 256] = [
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
make_instruction![[op_69, AM_69, IN_69] ADC #&BB   ];
make_instruction![[op_65, AM_65, IN_65] ADC &LL    ];
make_instruction![[op_75, AM_75, IN_75] ADC &LL,X  ];
make_instruction![[op_6d, AM_6D, IN_6D] ADC &LLHH  ];
make_instruction![[op_7d, AM_7D, IN_7D] ADC &LLHH,X];
make_instruction![[op_79, AM_79, IN_79] ADC &LLHH,Y];
make_instruction![[op_61, AM_61, IN_61] ADC (&LL,X)];
make_instruction![[op_71, AM_71, IN_71] ADC (&LL),Y];

/// # AND
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
make_instruction![[op_29, AM_29, IN_29] AND #&BB   ];
make_instruction![[op_25, AM_25, IN_25] AND &LL    ];
make_instruction![[op_35, AM_35, IN_35] AND &LL,X  ];
make_instruction![[op_2d, AM_2D, IN_2D] AND &LLHH  ];
make_instruction![[op_3d, AM_3D, IN_3D] AND &LLHH,X];
make_instruction![[op_39, AM_39, IN_39] AND &LLHH,Y];
make_instruction![[op_21, AM_21, IN_21] AND (&LL,X)];
make_instruction![[op_31, AM_31, IN_31] AND (&LL),Y];

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
make_instruction![[op_0a, AM_0A, IN_0A] ASL A      ];
make_instruction![[op_06, AM_06, IN_06] ASL &LL    ];
make_instruction![[op_16, AM_16, IN_16] ASL &LL,X  ];
make_instruction![[op_0e, AM_0E, IN_0E] ASL &LLHH  ];
make_instruction![[op_1e, AM_1E, IN_1E] ASL &LLHH,X];

/// BCC
/// Branch on Carry Clear
///```text
/// branch on C = 0                   N  Z  C  I  D  V
///                                   -  -  -  -  -  -
/// addressing   assembler       opc     bytes   cycles
/// relative     BCC oper        90      2       2**
/// ```
make_instruction![[op_90, AM_90, IN_90] BCC &BB    ];

/// BCS
/// Branch on Carry Set
///```text
/// branch on C = 1                         N Z C I D V
///                                         - - - - - -
/// addressing   assembler       opc     bytes   cycles
/// relative     BCS oper        B0      2       2**
/// ```
make_instruction![[op_b0, AM_B0, IN_B0] BCS &BB    ];

/// BEQ
/// Branch on Result Zero
///```text
/// branch on Z = 1                   N  Z  C  I  D  V
///                                   -  -  -  -  -  -
/// addressing   assembler       opc     bytes   cycles
/// relative     BEQ oper        F0      2       2**
/// ```
make_instruction![[op_f0, AM_F0, IN_F0] BEQ &BB    ];

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
make_instruction![[op_24, AM_24, IN_24] BIT &BB    ];
make_instruction![[op_2c, AM_2C, IN_2C] BIT &LLHH  ];

/// BMI
/// Branch on Result Minus
///```text
/// branch on N = 1                   N  Z  C  I  D  V
///                                   -  -  -  -  -  -
/// addressing   assembler       opc     bytes   cycles
/// relative     BMI oper        30      2       2**
/// ```
make_instruction![[op_30, AM_30, IN_30] BMI &BB    ];

/// BNE
/// Branch on Result not Zero
///```text
/// branch on Z = 0                   N  Z  C  I  D  V
///                                   -  -  -  -  -  -
/// addressing   assembler       opc     bytes   cycles
/// relative     BNE oper        D0      2       2**
/// ```
make_instruction![[op_d0, AM_D0, IN_D0] BNE &BB    ];

/// BPL
/// Branch on Result Plus
///```text
/// branch on N = 0                   N  Z  C  I  D  V
///                                   -  -  -  -  -  -
/// addressing   assembler       opc    bytes    cycles
/// relative     BPL oper        10      2       2**
/// ```
make_instruction![[op_10, AM_10, IN_10] BPL &BB    ];

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
make_instruction![[op_00, AM_00, IN_00] BRK        ];

/// BVC
/// Branch on Overflow Clear
///```text
/// branch on V = 0                   N  Z  C  I  D  V
///                                   -  -  -  -  -  -
/// addressing   assembler       opc     bytes   cycles
/// relative     BVC oper        50      2       2**
/// ```
make_instruction![[op_50, AM_50, IN_50] BVC &BB    ];

/// BVS
/// Branch on Overflow Set
///```text
/// branch on V = 1                   N  Z  C  I  D  V
///                                   -  -  -  -  -  -
/// addressing   assembler       opc     bytes   cycles
/// relative     BVS oper        70      2       2**
/// ```
make_instruction![[op_70, AM_70, IN_70] BVS &BB    ];

/// CLC
/// Clear Carry Flag
///```text
/// 0 -> C                            N  Z  C  I  D  V
///                                   -  -  0  -  -  -
/// addressing   assembler       opc     bytes   cycles
/// implied      CLC             18      1       2
/// ```
make_instruction![[op_18, AM_18, IN_18] CLC        ];

/// CLD
/// Clear Decimal Mode
///```text
/// 0 -> D                            N  Z  C  I  D  V
///                                   -  -  -  -  0  -
/// addressing   assembler       opc     bytes   cycles
/// implied      CLD             D8      1       2
/// ```
make_instruction![[op_d8, AM_D8, IN_D8] CLD        ];

/// CLI
/// Clear Interrupt Disable Bit
///```text
/// 0 -> I                            N  Z  C  I  D  V
///                                   -  -  -  0  -  -
/// addressing   assembler       opc     bytes   cycles
/// implied      CLI             58      1       2
/// ```
make_instruction![[op_58, AM_58, IN_58] CLI        ];

/// CLV
/// Clear Interrupt Disable Bit
///```text
/// 0 -> I                            N  Z  C  I  D  V
///                                   -  -  -  -  -  0
/// addressing   assembler       opc     bytes   cycles
/// implied      CLV             B8      1       2
/// ```
make_instruction![[op_b8, AM_B8, IN_B8] CLV        ];

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
make_instruction![[op_c9, AM_C9, IN_C9] CMP #&BB   ];
make_instruction![[op_c5, AM_C5, IN_C5] CMP &LL    ];
make_instruction![[op_d5, AM_D5, IN_D5] CMP &LL,X  ];
make_instruction![[op_cd, AM_CD, IN_CD] CMP &LLHH  ];
make_instruction![[op_dd, AM_DD, IN_DD] CMP &LLHH,X];
make_instruction![[op_d9, AM_D9, IN_D9] CMP &LLHH,Y];
make_instruction![[op_c1, AM_C1, IN_C1] CMP (&LL,X)];
make_instruction![[op_d1, AM_D1, IN_D1] CMP (&LL),Y];

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
make_instruction![[op_e0, AM_E0, IN_E0] CPX #&BB   ];
make_instruction![[op_e4, AM_E4, IN_E4] CPX &LL    ];
make_instruction![[op_ec, AM_EC, IN_EC] CPX &LLHH  ];

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
make_instruction![[op_c0, AM_C0, IN_C0] CPY #&BB   ];
make_instruction![[op_c4, AM_C4, IN_C4] CPY &LL    ];
make_instruction![[op_cc, AM_CC, IN_CC] CPY &LLHH  ];

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
make_instruction![[op_c6, AM_C6, IN_C6] DEC &LL    ];
make_instruction![[op_d6, AM_D6, IN_D6] DEC &LL,X  ];
make_instruction![[op_ce, AM_CE, IN_CE] DEC &LLHH  ];
make_instruction![[op_de, AM_DE, IN_DE] DEC &LLHH,X];

/// DEX
/// Decrement Index X by One
///```text
/// X - 1 -> X                        N  Z  C  I  D  V
///                                   +  +  -  -  -  -
/// addressing   assembler       opc     bytes   cycles
/// implied      DEX             CA      1       2
/// ```
make_instruction![[op_ca, AM_CA, IN_CA] DEX        ];

/// DEY
/// Decrement Index Y by One
///```text
/// Y - 1 -> Y                        N  Z  C  I  D  V
///                                   +  +  -  -  -  -
/// addressing   assembler       opc     bytes   cycles
/// implied      DEY             88      1       2
/// ```
make_instruction![[op_88, AM_88, IN_88] DEY        ];

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
make_instruction![[op_49, AM_49, IN_49] EOR #&BB   ];
make_instruction![[op_45, AM_45, IN_45] EOR &LL    ];
make_instruction![[op_55, AM_55, IN_55] EOR &LL,X  ];
make_instruction![[op_4d, AM_4D, IN_4D] EOR &LLHH  ];
make_instruction![[op_5d, AM_5D, IN_5D] EOR &LLHH,X];
make_instruction![[op_59, AM_59, IN_59] EOR &LLHH,Y];
make_instruction![[op_41, AM_41, IN_41] EOR (&LL,X)];
make_instruction![[op_51, AM_51, IN_51] EOR (&LL),Y];

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
make_instruction![[op_e6, AM_E6, IN_E6] INC &LL    ];
make_instruction![[op_f6, AM_F6, IN_F6] INC &LL,X  ];
make_instruction![[op_ee, AM_EE, IN_EE] INC &LLHH  ];
make_instruction![[op_fe, AM_FE, IN_FE] INC &LLHH,X];

/// INX
/// Increment Index X by One
///```text
/// X + 1 -> X                        N  Z  C  I  D  V
///                                   +  +  -  -  -  -
/// addressing   assembler       opc    bytes    cycles
/// implied      INX             E8     1        2
/// ```
make_instruction![[op_e8, AM_E8, IN_E8] INX        ];

/// INY
/// Increment Index Y by One
///```text
/// Y + 1 -> Y                        N  Z  C  I  D  V
///                                   +  +  -  -  -  -
/// addressing   assembler       opc    bytes    cycles
/// implied      INY             C8     1        2
/// ```
make_instruction![[op_c8, AM_C8, IN_C8] INY        ];

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
make_instruction![[op_4c, AM_4C, IN_4C] JMP &LLHH  ];
make_instruction![[op_6c, AM_6C, IN_6C] JMP (&LLHH)];

/// JSR
/// Jump to New Location Saving Return Address
///```text
/// push (PC+2),                      N  Z  C  I  D  V
/// (PC+1) -> PCL                     -  -  -  -  -  -
/// (PC+2) -> PCH
/// addressing   assembler       opc    bytes    cycles
/// absolute     JSR oper        20     3        6
/// ```
make_instruction![[op_20, AM_20, IN_20] JSR &LLHH  ];

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
make_instruction![[op_a9, AM_A9, IN_A9] LDA #&BB   ];
make_instruction![[op_a5, AM_A5, IN_A5] LDA &LL    ];
make_instruction![[op_b5, AM_B5, IN_B5] LDA &LL,X  ];
make_instruction![[op_ad, AM_AD, IN_AD] LDA &LLHH  ];
make_instruction![[op_bd, AM_BD, IN_BD] LDA &LLHH,X];
make_instruction![[op_b9, AM_B9, IN_B9] LDA &LLHH,Y];
make_instruction![[op_a1, AM_A1, IN_A1] LDA (&LL,X)];
make_instruction![[op_b1, AM_B1, IN_B1] LDA (&LL),Y];

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
make_instruction![[op_a2, AM_A2, IN_A2] LDX #&BB   ];
make_instruction![[op_a6, AM_A6, IN_A6] LDX &LL    ];
make_instruction![[op_b6, AM_B6, IN_B6] LDX &LL,Y  ];
make_instruction![[op_ae, AM_AE, IN_AE] LDX &LLHH  ];
make_instruction![[op_be, AM_BE, IN_BE] LDX &LLHH,Y];

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
make_instruction![[op_a0, AM_A0, IN_A0] LDY #&BB   ];
make_instruction![[op_a4, AM_A4, IN_A4] LDY &LL    ];
make_instruction![[op_b4, AM_B4, IN_B4] LDY &LL,X  ];
make_instruction![[op_ac, AM_AC, IN_AC] LDY &LLHH  ];
make_instruction![[op_bc, AM_BC, IN_BC] LDY &LLHH,X];

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
make_instruction![[op_4a, AM_4A, IN_4A] LSR A      ];
make_instruction![[op_46, AM_46, IN_46] LSR &LL    ];
make_instruction![[op_56, AM_56, IN_56] LSR &LL,X  ];
make_instruction![[op_4e, AM_4E, IN_4E] LSR &LLHH  ];
make_instruction![[op_5e, AM_5E, IN_5E] LSR &LLHH,X];

/// NOP
/// No Operation
///```text
/// ---                               N  Z  C  I  D  V
///                                   -  -  -  -  -  -
/// addressing   assembler       opc    bytes    cycles
/// implied      NOP             EA     1        2
/// ```
make_instruction![[op_ea, AM_EA, IN_EA] NOP        ];

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
make_instruction![[op_09, AM_09, IN_09] ORA #&BB   ];
make_instruction![[op_05, AM_05, IN_05] ORA &LL    ];
make_instruction![[op_15, AM_15, IN_15] ORA &LL,X  ];
make_instruction![[op_0d, AM_0D, IN_0D] ORA &LLHH  ];
make_instruction![[op_1d, AM_1D, IN_1D] ORA &LLHH,X];
make_instruction![[op_19, AM_19, IN_19] ORA &LLHH,Y];
make_instruction![[op_01, AM_01, IN_01] ORA (&LL,X)];
make_instruction![[op_11, AM_11, IN_11] ORA (&LL),Y];

/// PHA
/// Push Accumulator on Stack
///```text
/// push A                            N  Z  C  I  D  V
///                                   -  -  -  -  -  -
/// addressing   assembler       opc    bytes    cycles
/// implied      PHA             48     1        3
/// ```
make_instruction![[op_48, AM_48, IN_48] PHA        ];

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
make_instruction![[op_08, AM_08, IN_08] PHP        ];

/// PLA
/// Pull Accumulator from Stack
///```text
/// pull A                            N  Z  C  I  D  V
///                                   +  +  -  -  -  -
/// addressing   assembler       opc    bytes    cycles
/// implied      PLA             68     1        4
/// ```
make_instruction![[op_68, AM_68, IN_68] PLA        ];

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
make_instruction![[op_28, AM_28, IN_28] PLP        ];

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
make_instruction![[op_2a, AM_2A, IN_2A] ROL        ];
make_instruction![[op_26, AM_26, IN_26] ROL &LL    ];
make_instruction![[op_36, AM_36, IN_36] ROL &LL,X  ];
make_instruction![[op_2e, AM_2E, IN_2E] ROL &LLHH  ];
make_instruction![[op_3e, AM_3E, IN_3E] ROL &LLHH,X];

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
make_instruction![[op_6a, AM_6A, IN_6A] ROR        ];
make_instruction![[op_66, AM_66, IN_66] ROR &LL    ];
make_instruction![[op_76, AM_76, IN_76] ROR &LL,X  ];
make_instruction![[op_6e, AM_6E, IN_6E] ROR &LLHH  ];
make_instruction![[op_7e, AM_7E, IN_7E] ROR &LLHH,X];

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
make_instruction![[op_40, AM_40, IN_40] RTI        ];

/// RTS
/// Return from Subroutine
///```text
/// pull PC, PC+1 -> PC               N  Z  C  I  D  V
///                                   -  -  -  -  -  -
/// addressing   assembler       opc    bytes    cycles
/// implied      RTS             60     1        6
/// ```
make_instruction![[op_60, AM_60, IN_60] RTS        ];

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
make_instruction![[op_e9, AM_E9, IN_E9] SBC #&BB   ];
make_instruction![[op_e5, AM_E5, IN_E5] SBC &LL    ];
make_instruction![[op_f5, AM_F5, IN_F5] SBC &LL,X  ];
make_instruction![[op_ed, AM_ED, IN_ED] SBC &LLHH  ];
make_instruction![[op_fd, AM_FD, IN_FD] SBC &LLHH,X];
make_instruction![[op_f9, AM_F9, IN_F9] SBC &LLHH,Y];
make_instruction![[op_e1, AM_E1, IN_E1] SBC (&LL,X)];
make_instruction![[op_f1, AM_F1, IN_F1] SBC (&LL),Y];

// make_instruction![[op_38, AM_38, IN_38] SEC        ];

// make_instruction![[op_f8, AM_F8, IN_F8] SED        ];

// make_instruction![[op_78, AM_78, IN_78] SEI        ];

// make_instruction![[op_85, AM_85, IN_85] STA &LL    ];
// make_instruction![[op_95, AM_95, IN_95] STA &LL,X  ];
// make_instruction![[op_8d, AM_8D, IN_8D] STA &LLHH  ];
// make_instruction![[op_9d, AM_9D, IN_9D] STA &LLHH,X];
// make_instruction![[op_99, AM_99, IN_99] STA &LLHH,Y];
// make_instruction![[op_81, AM_81, IN_81] STA (&LL,X)];
// make_instruction![[op_91, AM_91, IN_91] STA (&LL),Y];

// make_instruction![[op_86, AM_86, IN_86] STX &LL    ];
// make_instruction![[op_96, AM_96, IN_96] STX &LL,Y  ];
// make_instruction![[op_8e, AM_8E, IN_8E] STX &LLHH,Y];

// make_instruction![[op_84, AM_84, IN_84] STY &LL    ];
// make_instruction![[op_94, AM_94, IN_94] STY &LL,X  ];
// make_instruction![[op_8c, AM_8C, IN_8C] STY &LLHH,X];

// make_instruction![[op_aa, AM_AA, IN_AA] TAX        ];

// make_instruction![[op_a8, AM_A8, IN_A8] TAY        ];

// make_instruction![[op_ba, AM_BA, IN_BA] TSX        ];

// make_instruction![[op_8a, AM_8A, IN_8A] TXA        ];

// make_instruction![[op_9a, AM_9A, IN_9A] TXS        ];

// make_instruction![[op_98, AM_98, IN_98] TYA        ];

// Illegal

// JAMS 02, 12, 22, 32, 42, 52, 62, 72, 92, B2, D2, F2
// make_instruction![[op_02, AM_02, IN_02] JAM        ];
// make_instruction![[op_12, AM_12, IN_12] JAM        ];
// make_instruction![[op_22, AM_22, IN_22] JAM        ];
// make_instruction![[op_32, AM_32, IN_32] JAM        ];
// make_instruction![[op_42, AM_42, IN_42] JAM        ];
// make_instruction![[op_52, AM_52, IN_52] JAM        ];
// make_instruction![[op_62, AM_62, IN_62] JAM        ];
// make_instruction![[op_72, AM_72, IN_72] JAM        ];
// make_instruction![[op_92, AM_92, IN_92] JAM        ];
// make_instruction![[op_b2, AM_B2, IN_B2] JAM        ];
// make_instruction![[op_d2, AM_D2, IN_D2] JAM        ];
// make_instruction![[op_f2, AM_F2, IN_F2] JAM        ];

#[cfg(test)]
mod test {
    use crate::ines::{
        cpu::{Bus, Status},
        io::{ReadDevice, WriteDevice},
    };

    use super::*;

    #[test]
    fn adc_imm() {
        let mut bus = Bus::new();
        let mut reg = Registers {
            ac: 0x05,
            p: Status::U,
            pc: 0x00, // Where the next instruction will be loaded
            ..Default::default()
        };

        bus.write(0x00, 0x05); // Immediate mode value

        let mut iter = op_69();
        while let Some(result) = iter.next() {
            match result {
                InstructionResult::Result(_, _) => {}
                InstructionResult::Clock => {
                    iter.clock(&mut reg, &mut bus);
                }
            }
        }

        assert_eq!(reg.ac, 0x0A);
        assert_eq!(reg.p.get(Status::N), 0);
        assert_eq!(reg.p.get(Status::C), 0);
        assert_eq!(reg.p.get(Status::Z), 0);
        assert_eq!(iter.cc, 2, "rw_count");
    }

    #[test]
    fn bne_no_branch() {
        let mut bus = Bus::new();
        let mut reg = Registers {
            p: Status::U | Status::Z,
            pc: 0x00, // Where the next instruction will be loaded
            ..Default::default()
        };

        bus.write(0x00, 0x05); // Immediate mode value

        bus.read(0x0000); // Cycle to simulate the op code read.

        let mut iter = op_d0();

        while let Some(result) = iter.next() {
            match result {
                InstructionResult::Result(_, _) => {}
                InstructionResult::Clock => {
                    iter.clock(&mut reg, &mut bus);
                }
            }
        }

        assert_eq!(reg.pc, 0x01, "PC");
        assert_eq!(iter.cc, 2, "cycle count");
    }

    #[test]
    fn bne_branch_same_page() {
        let mut bus = Bus::new();
        let mut reg = Registers {
            p: Status::U,
            pc: 0x00, // Where the next instruction will be loaded
            ..Default::default()
        };
        bus.write(0x00, 0x05); // Immediate mode value

        bus.read(0x0000); // Cycle to simulate the op code read.

        let mut iter = op_d0();

        while let Some(result) = iter.next() {
            match result {
                InstructionResult::Result(_, _) => {}
                InstructionResult::Clock => {
                    iter.clock(&mut reg, &mut bus);
                }
            }
        }

        assert_eq!(reg.pc, 0x06, "PC");
        assert_eq!(iter.cc, 3, "cycle count");
    }

    #[test]
    fn bne_branch_new_page() {
        let mut bus = Bus::new();
        let mut reg = Registers {
            p: Status::U,
            pc: 0xFD, // Where the next instruction will be loaded
            ..Default::default()
        };
        bus.write(0xFD, 0x05); // Immediate mode value

        bus.read(0x0000); // Cycle to simulate the op code read.

        let mut iter = op_d0();

        while let Some(result) = iter.next() {
            match result {
                InstructionResult::Result(_, _) => {}
                InstructionResult::Clock => {
                    iter.clock(&mut reg, &mut bus);
                }
            }
        }

        assert_eq!(reg.pc, 0x00FD + 0x0001 + 0x0005, "PC");
        assert_eq!(iter.cc, 4, "cycle count");
    }
}
