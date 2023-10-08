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

pub enum OperationResult {
    None,
    Instant,
    Skip(i8),
    SkipInstant(i8),
}

pub type Operation =
    fn(&mut Registers, &mut dyn RwDevice, &mut InstructionState) -> OperationResult;

pub type Instruc = fn() -> InstructionIterator;

const INOOP: Instruc = op_ea;

/// https://www.masswerk.at/6502/6502_instruction_set.html
#[allow(dead_code)]
#[rustfmt::skip]
pub const OPER: [Instruc; 256] = [
    op_00, op_01, op_02, op_03, op_04, op_05, op_06, op_07, op_08, op_09, op_0a, INOOP, op_0c, op_0d, op_0e, op_0f,
    op_10, op_11, op_12, op_13, op_14, op_15, op_16, op_17, op_18, op_19, op_1a, op_1b, op_1c, op_1d, op_1e, op_1f,
    op_20, op_21, op_22, op_23, op_24, op_25, op_26, op_27, op_28, op_29, op_2a, INOOP, op_2c, op_2d, op_2e, op_2f,
    op_30, op_31, op_32, op_33, op_34, op_35, op_36, op_37, op_38, op_39, op_3a, op_3b, op_3c, op_3d, op_3e, op_3f,
    op_40, op_41, op_42, op_43, op_44, op_45, op_46, op_47, op_48, op_49, op_4a, INOOP, op_4c, op_4d, op_4e, op_4f,
    op_50, op_51, op_52, op_53, op_54, op_55, op_56, op_57, op_58, op_59, op_5a, op_5b, op_5c, op_5d, op_5e, op_5f,
    op_60, op_61, op_62, op_63, op_64, op_65, op_66, op_67, op_68, op_69, op_6a, INOOP, op_6c, op_6d, op_6e, op_6f,
    op_70, op_71, op_72, op_73, op_74, op_75, op_76, op_77, op_78, op_79, op_7a, op_7b, op_7c, op_7d, op_7e, op_7f,
    op_80, op_81, op_82, op_83, op_84, op_85, op_86, op_87, op_88, op_89, op_8a, INOOP, op_8c, op_8d, op_8e, op_8f,
    op_90, op_91, op_92, INOOP, op_94, op_95, op_96, op_97, op_98, op_99, op_9a, INOOP, INOOP, op_9d, INOOP, INOOP,
    op_a0, op_a1, op_a2, op_a3, op_a4, op_a5, op_a6, op_a7, op_a8, op_a9, op_aa, INOOP, op_ac, op_ad, op_ae, op_af,
    op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7, op_b8, op_b9, op_ba, INOOP, op_bc, op_bd, op_be, op_bf,
    op_c0, op_c1, op_c2, op_c3, op_c4, op_c5, op_c6, op_c7, op_c8, op_c9, op_ca, INOOP, op_cc, op_cd, op_ce, op_cf,
    op_d0, op_d1, op_d2, op_d3, op_d4, op_d5, op_d6, op_d7, op_d8, op_d9, op_da, op_db, op_dc, op_dd, op_de, op_df,
    op_e0, op_e1, op_e2, op_e3, op_e4, op_e5, op_e6, op_e7, op_e8, op_e9, op_ea, op_eb, op_ec, op_ed, op_ee, op_ef,
    op_f0, op_f1, op_f2, op_f3, op_f4, op_f5, op_f6, op_f7, op_f8, op_f9, op_fa, op_fb, op_fc, op_fd, op_fe, op_ff,
];

const INOAM: AddrMode = AddrMode::IMP;

/// https://www.masswerk.at/6502/6502_instruction_set.html
#[allow(dead_code)]
#[rustfmt::skip]
pub const ADDR_MODE: [AddrMode; 256] = [
    AM_00, AM_01, AM_02, AM_03, AM_04, AM_05, AM_06, AM_07, AM_08, AM_09, AM_0A, INOAM, AM_0C, AM_0D, AM_0E, AM_0F,
    AM_10, AM_11, AM_12, AM_13, AM_14, AM_15, AM_16, AM_17, AM_18, AM_19, AM_1A, AM_1B, AM_1C, AM_1D, AM_1E, AM_1F,
    AM_20, AM_21, AM_22, AM_23, AM_24, AM_25, AM_26, AM_27, AM_28, AM_29, AM_2A, INOAM, AM_2C, AM_2D, AM_2E, AM_2F,
    AM_30, AM_31, AM_32, AM_33, AM_34, AM_35, AM_36, AM_37, AM_38, AM_39, AM_3A, AM_3B, AM_3C, AM_3D, AM_3E, AM_3F,
    AM_40, AM_41, AM_42, AM_43, AM_44, AM_45, AM_46, AM_47, AM_48, AM_49, AM_4A, INOAM, AM_4C, AM_4D, AM_4E, AM_4F,
    AM_50, AM_51, AM_52, AM_53, AM_54, AM_55, AM_56, AM_57, AM_58, AM_59, AM_5A, AM_5B, AM_5C, AM_5D, AM_5E, AM_5F,
    AM_60, AM_61, AM_62, AM_63, AM_64, AM_65, AM_66, AM_67, AM_68, AM_69, AM_6A, INOAM, AM_6C, AM_6D, AM_6E, AM_6F,
    AM_70, AM_71, AM_72, AM_73, AM_74, AM_75, AM_76, AM_77, AM_78, AM_79, AM_7A, AM_7B, AM_7C, AM_7D, AM_7E, AM_7F,
    AM_80, AM_81, AM_82, AM_83, AM_84, AM_85, AM_86, AM_87, AM_88, AM_89, AM_8A, INOAM, AM_8C, AM_8D, AM_8E, AM_8F,
    AM_90, AM_91, AM_92, INOAM, AM_94, AM_95, AM_96, AM_97, AM_98, AM_99, AM_9A, INOAM, INOAM, AM_9D, INOAM, INOAM,
    AM_A0, AM_A1, AM_A2, AM_A3, AM_A4, AM_A5, AM_A6, AM_A7, AM_A8, AM_A9, AM_AA, INOAM, AM_AC, AM_AD, AM_AE, AM_AF,
    AM_B0, AM_B1, AM_B2, AM_B3, AM_B4, AM_B5, AM_B6, AM_B7, AM_B8, AM_B9, AM_BA, INOAM, AM_BC, AM_BD, AM_BE, AM_BF,
    AM_C0, AM_C1, AM_C2, AM_C3, AM_C4, AM_C5, AM_C6, AM_C7, AM_C8, AM_C9, AM_CA, INOAM, AM_CC, AM_CD, AM_CE, AM_CF,
    AM_D0, AM_D1, AM_D2, AM_D3, AM_D4, AM_D5, AM_D6, AM_D7, AM_D8, AM_D9, AM_DA, AM_DB, AM_DC, AM_DD, AM_DE, AM_DF,
    AM_E0, AM_E1, AM_E2, AM_E3, AM_E4, AM_E5, AM_E6, AM_E7, AM_E8, AM_E9, AM_EA, AM_EB, AM_EC, AM_ED, AM_EE, AM_EF,
    AM_F0, AM_F1, AM_F2, AM_F3, AM_F4, AM_F5, AM_F6, AM_F7, AM_F8, AM_F9, AM_FA, AM_FB, AM_FC, AM_FD, AM_FE, AM_FF,
];

const INOIN: OperType = OperType::XXX;

/// https://www.masswerk.at/6502/6502_instruction_set.html
#[allow(dead_code)]
#[rustfmt::skip]
pub const INSTRUCTION_TYPE: [OperType; 256] = [
    IN_00, IN_01, IN_02, IN_03, IN_04, IN_05, IN_06, IN_07, IN_08, IN_09, IN_0A, INOIN, IN_0C, IN_0D, IN_0E, IN_0F,
    IN_10, IN_11, IN_12, IN_13, IN_14, IN_15, IN_16, IN_17, IN_18, IN_19, IN_1A, IN_1B, IN_1C, IN_1D, IN_1E, IN_1F,
    IN_20, IN_21, IN_22, IN_23, IN_24, IN_25, IN_26, IN_27, IN_28, IN_29, IN_2A, INOIN, IN_2C, IN_2D, IN_2E, IN_2F,
    IN_30, IN_31, IN_32, IN_33, IN_34, IN_35, IN_36, IN_37, IN_38, IN_39, IN_3A, IN_3B, IN_3C, IN_3D, IN_3E, IN_3F,
    IN_40, IN_41, IN_42, IN_43, IN_44, IN_45, IN_46, IN_47, IN_48, IN_49, IN_4A, INOIN, IN_4C, IN_4D, IN_4E, IN_4F,
    IN_50, IN_51, IN_52, IN_53, IN_54, IN_55, IN_56, IN_57, IN_58, IN_59, IN_5A, IN_5B, IN_5C, IN_5D, IN_5E, IN_5F,
    IN_60, IN_61, IN_62, IN_63, IN_64, IN_65, IN_66, IN_67, IN_68, IN_69, IN_6A, INOIN, IN_6C, IN_6D, IN_6E, IN_6F,
    IN_70, IN_71, IN_72, IN_73, IN_74, IN_75, IN_76, IN_77, IN_78, IN_79, IN_7A, IN_7B, IN_7C, IN_7D, IN_7E, IN_7F,
    IN_80, IN_81, IN_82, IN_83, IN_84, IN_85, IN_86, IN_87, IN_88, IN_89, IN_8A, INOIN, IN_8C, IN_8D, IN_8E, IN_8F,
    IN_90, IN_91, IN_92, INOIN, IN_94, IN_95, IN_96, IN_97, IN_98, IN_99, IN_9A, INOIN, INOIN, IN_9D, INOIN, INOIN,
    IN_A0, IN_A1, IN_A2, IN_A3, IN_A4, IN_A5, IN_A6, IN_A7, IN_A8, IN_A9, IN_AA, INOIN, IN_AC, IN_AD, IN_AE, IN_AF,
    IN_B0, IN_B1, IN_B2, IN_B3, IN_B4, IN_B5, IN_B6, IN_B7, IN_B8, IN_B9, IN_BA, INOIN, IN_BC, IN_BD, IN_BE, IN_BF,
    IN_C0, IN_C1, IN_C2, IN_C3, IN_C4, IN_C5, IN_C6, IN_C7, IN_C8, IN_C9, IN_CA, INOIN, IN_CC, IN_CD, IN_CE, IN_CF,
    IN_D0, IN_D1, IN_D2, IN_D3, IN_D4, IN_D5, IN_D6, IN_D7, IN_D8, IN_D9, IN_DA, IN_DB, IN_DC, IN_DD, IN_DE, IN_DF,
    IN_E0, IN_E1, IN_E2, IN_E3, IN_E4, IN_E5, IN_E6, IN_E7, IN_E8, IN_E9, IN_EA, IN_EB, IN_EC, IN_ED, IN_EE, IN_EF,
    IN_F0, IN_F1, IN_F2, IN_F3, IN_F4, IN_F5, IN_F6, IN_F7, IN_F8, IN_F9, IN_FA, IN_FB, IN_FC, IN_FD, IN_FE, IN_FF,
];

mod legal {
    use super::*;

    make_instruction![[op_69, AM_69, IN_69] ADC #&BB    ];
    make_instruction![[op_65, AM_65, IN_65] ADC &LL     ];
    make_instruction![[op_75, AM_75, IN_75] ADC &LL,X   ];
    make_instruction![[op_6d, AM_6D, IN_6D] ADC &LLHH   ];
    make_instruction![[op_7d, AM_7D, IN_7D] ADC &LLHH,X ];
    make_instruction![[op_79, AM_79, IN_79] ADC &LLHH,Y ];
    make_instruction![[op_61, AM_61, IN_61] ADC (&LL,X) ];
    make_instruction![[op_71, AM_71, IN_71] ADC (&LL),Y ];

    make_instruction![[op_29, AM_29, IN_29] AND #&BB    ];
    make_instruction![[op_25, AM_25, IN_25] AND &LL     ];
    make_instruction![[op_35, AM_35, IN_35] AND &LL,X   ];
    make_instruction![[op_2d, AM_2D, IN_2D] AND &LLHH   ];
    make_instruction![[op_3d, AM_3D, IN_3D] AND &LLHH,X ];
    make_instruction![[op_39, AM_39, IN_39] AND &LLHH,Y ];
    make_instruction![[op_21, AM_21, IN_21] AND (&LL,X) ];
    make_instruction![[op_31, AM_31, IN_31] AND (&LL),Y ];

    make_instruction![[op_0a, AM_0A, IN_0A] ASL A       ];
    make_instruction![[op_06, AM_06, IN_06] ASL &LL     ];
    make_instruction![[op_16, AM_16, IN_16] ASL &LL,X   ];
    make_instruction![[op_0e, AM_0E, IN_0E] ASL &LLHH   ];
    make_instruction![[op_1e, AM_1E, IN_1E] ASL &LLHH,X ];

    make_instruction![[op_90, AM_90, IN_90] BCC &BB     ];

    make_instruction![[op_b0, AM_B0, IN_B0] BCS &BB     ];

    make_instruction![[op_f0, AM_F0, IN_F0] BEQ &BB     ];

    make_instruction![[op_24, AM_24, IN_24] BIT &LL     ];
    make_instruction![[op_2c, AM_2C, IN_2C] BIT &LLHH   ];

    make_instruction![[op_30, AM_30, IN_30] BMI &BB     ];

    make_instruction![[op_d0, AM_D0, IN_D0] BNE &BB     ];

    make_instruction![[op_10, AM_10, IN_10] BPL &BB     ];

    make_instruction![[op_00, AM_00, IN_00] BRK         ];

    make_instruction![[op_50, AM_50, IN_50] BVC &BB     ];

    make_instruction![[op_70, AM_70, IN_70] BVS &BB     ];

    make_instruction![[op_18, AM_18, IN_18] CLC         ];

    make_instruction![[op_d8, AM_D8, IN_D8] CLD         ];

    make_instruction![[op_58, AM_58, IN_58] CLI         ];

    make_instruction![[op_b8, AM_B8, IN_B8] CLV         ];

    make_instruction![[op_c9, AM_C9, IN_C9] CMP #&BB    ];
    make_instruction![[op_c5, AM_C5, IN_C5] CMP &LL     ];
    make_instruction![[op_d5, AM_D5, IN_D5] CMP &LL,X   ];
    make_instruction![[op_cd, AM_CD, IN_CD] CMP &LLHH   ];
    make_instruction![[op_dd, AM_DD, IN_DD] CMP &LLHH,X ];
    make_instruction![[op_d9, AM_D9, IN_D9] CMP &LLHH,Y ];
    make_instruction![[op_c1, AM_C1, IN_C1] CMP (&LL,X) ];
    make_instruction![[op_d1, AM_D1, IN_D1] CMP (&LL),Y ];

    make_instruction![[op_e0, AM_E0, IN_E0] CPX #&BB    ];
    make_instruction![[op_e4, AM_E4, IN_E4] CPX &LL     ];
    make_instruction![[op_ec, AM_EC, IN_EC] CPX &LLHH   ];

    make_instruction![[op_c0, AM_C0, IN_C0] CPY #&BB    ];
    make_instruction![[op_c4, AM_C4, IN_C4] CPY &LL     ];
    make_instruction![[op_cc, AM_CC, IN_CC] CPY &LLHH   ];

    make_instruction![[op_c6, AM_C6, IN_C6] DEC &LL     ];
    make_instruction![[op_d6, AM_D6, IN_D6] DEC &LL,X   ];
    make_instruction![[op_ce, AM_CE, IN_CE] DEC &LLHH   ];
    make_instruction![[op_de, AM_DE, IN_DE] DEC &LLHH,X ];

    make_instruction![[op_ca, AM_CA, IN_CA] DEX         ];

    make_instruction![[op_88, AM_88, IN_88] DEY         ];

    make_instruction![[op_49, AM_49, IN_49] EOR #&BB    ];
    make_instruction![[op_45, AM_45, IN_45] EOR &LL     ];
    make_instruction![[op_55, AM_55, IN_55] EOR &LL,X   ];
    make_instruction![[op_4d, AM_4D, IN_4D] EOR &LLHH   ];
    make_instruction![[op_5d, AM_5D, IN_5D] EOR &LLHH,X ];
    make_instruction![[op_59, AM_59, IN_59] EOR &LLHH,Y ];
    make_instruction![[op_41, AM_41, IN_41] EOR (&LL,X) ];
    make_instruction![[op_51, AM_51, IN_51] EOR (&LL),Y ];

    make_instruction![[op_e6, AM_E6, IN_E6] INC &LL     ];
    make_instruction![[op_f6, AM_F6, IN_F6] INC &LL,X   ];
    make_instruction![[op_ee, AM_EE, IN_EE] INC &LLHH   ];
    make_instruction![[op_fe, AM_FE, IN_FE] INC &LLHH,X ];

    make_instruction![[op_e8, AM_E8, IN_E8] INX         ];

    make_instruction![[op_c8, AM_C8, IN_C8] INY         ];

    make_instruction![[op_4c, AM_4C, IN_4C] JMP &LLHH   ];
    make_instruction![[op_6c, AM_6C, IN_6C] JMP (&LLHH) ];

    make_instruction![[op_20, AM_20, IN_20] JSR &LLHH   ];

    make_instruction![[op_a9, AM_A9, IN_A9] LDA #&BB    ];
    make_instruction![[op_a5, AM_A5, IN_A5] LDA &LL     ];
    make_instruction![[op_b5, AM_B5, IN_B5] LDA &LL,X   ];
    make_instruction![[op_ad, AM_AD, IN_AD] LDA &LLHH   ];
    make_instruction![[op_bd, AM_BD, IN_BD] LDA &LLHH,X ];
    make_instruction![[op_b9, AM_B9, IN_B9] LDA &LLHH,Y ];
    make_instruction![[op_a1, AM_A1, IN_A1] LDA (&LL,X) ];
    make_instruction![[op_b1, AM_B1, IN_B1] LDA (&LL),Y ];

    make_instruction![[op_a2, AM_A2, IN_A2] LDX #&BB    ];
    make_instruction![[op_a6, AM_A6, IN_A6] LDX &LL     ];
    make_instruction![[op_b6, AM_B6, IN_B6] LDX &LL,Y   ];
    make_instruction![[op_ae, AM_AE, IN_AE] LDX &LLHH   ];
    make_instruction![[op_be, AM_BE, IN_BE] LDX &LLHH,Y ];

    make_instruction![[op_a0, AM_A0, IN_A0] LDY #&BB    ];
    make_instruction![[op_a4, AM_A4, IN_A4] LDY &LL     ];
    make_instruction![[op_b4, AM_B4, IN_B4] LDY &LL,X   ];
    make_instruction![[op_ac, AM_AC, IN_AC] LDY &LLHH   ];
    make_instruction![[op_bc, AM_BC, IN_BC] LDY &LLHH,X ];

    make_instruction![[op_4a, AM_4A, IN_4A] LSR A       ];
    make_instruction![[op_46, AM_46, IN_46] LSR &LL     ];
    make_instruction![[op_56, AM_56, IN_56] LSR &LL,X   ];
    make_instruction![[op_4e, AM_4E, IN_4E] LSR &LLHH   ];
    make_instruction![[op_5e, AM_5E, IN_5E] LSR &LLHH,X ];

    make_instruction![[op_ea, AM_EA, IN_EA] NOP         ];

    make_instruction![[op_09, AM_09, IN_09] ORA #&BB    ];
    make_instruction![[op_05, AM_05, IN_05] ORA &LL     ];
    make_instruction![[op_15, AM_15, IN_15] ORA &LL,X   ];
    make_instruction![[op_0d, AM_0D, IN_0D] ORA &LLHH   ];
    make_instruction![[op_1d, AM_1D, IN_1D] ORA &LLHH,X ];
    make_instruction![[op_19, AM_19, IN_19] ORA &LLHH,Y ];
    make_instruction![[op_01, AM_01, IN_01] ORA (&LL,X) ];
    make_instruction![[op_11, AM_11, IN_11] ORA (&LL),Y ];

    make_instruction![[op_48, AM_48, IN_48] PHA         ];

    make_instruction![[op_08, AM_08, IN_08] PHP         ];

    make_instruction![[op_68, AM_68, IN_68] PLA         ];

    make_instruction![[op_28, AM_28, IN_28] PLP         ];

    make_instruction![[op_2a, AM_2A, IN_2A] ROL A       ];
    make_instruction![[op_26, AM_26, IN_26] ROL &LL     ];
    make_instruction![[op_36, AM_36, IN_36] ROL &LL,X   ];
    make_instruction![[op_2e, AM_2E, IN_2E] ROL &LLHH   ];
    make_instruction![[op_3e, AM_3E, IN_3E] ROL &LLHH,X ];

    make_instruction![[op_6a, AM_6A, IN_6A] ROR A       ];
    make_instruction![[op_66, AM_66, IN_66] ROR &LL     ];
    make_instruction![[op_76, AM_76, IN_76] ROR &LL,X   ];
    make_instruction![[op_6e, AM_6E, IN_6E] ROR &LLHH   ];
    make_instruction![[op_7e, AM_7E, IN_7E] ROR &LLHH,X ];

    make_instruction![[op_40, AM_40, IN_40] RTI         ];

    make_instruction![[op_60, AM_60, IN_60] RTS         ];

    make_instruction![[op_e9, AM_E9, IN_E9] SBC #&BB    ];
    make_instruction![[op_e5, AM_E5, IN_E5] SBC &LL     ];
    make_instruction![[op_f5, AM_F5, IN_F5] SBC &LL,X   ];
    make_instruction![[op_ed, AM_ED, IN_ED] SBC &LLHH   ];
    make_instruction![[op_fd, AM_FD, IN_FD] SBC &LLHH,X ];
    make_instruction![[op_f9, AM_F9, IN_F9] SBC &LLHH,Y ];
    make_instruction![[op_e1, AM_E1, IN_E1] SBC (&LL,X) ];
    make_instruction![[op_f1, AM_F1, IN_F1] SBC (&LL),Y ];

    make_instruction![[op_38, AM_38, IN_38] SEC         ];

    make_instruction![[op_f8, AM_F8, IN_F8] SED         ];

    make_instruction![[op_78, AM_78, IN_78] SEI         ];

    make_instruction![[op_85, AM_85, IN_85] STA &LL     ];
    make_instruction![[op_95, AM_95, IN_95] STA &LL,X   ];
    make_instruction![[op_8d, AM_8D, IN_8D] STA &LLHH   ];
    make_instruction![[op_9d, AM_9D, IN_9D] STA &LLHH,X ];
    make_instruction![[op_99, AM_99, IN_99] STA &LLHH,Y ];
    make_instruction![[op_81, AM_81, IN_81] STA (&LL,X) ];
    make_instruction![[op_91, AM_91, IN_91] STA (&LL),Y ];

    make_instruction![[op_86, AM_86, IN_86] STX &LL     ];
    make_instruction![[op_96, AM_96, IN_96] STX &LL,Y   ];
    make_instruction![[op_8e, AM_8E, IN_8E] STX &LLHH   ];

    make_instruction![[op_84, AM_84, IN_84] STY &LL     ];
    make_instruction![[op_94, AM_94, IN_94] STY &LL,X   ];
    make_instruction![[op_8c, AM_8C, IN_8C] STY &LLHH   ];

    make_instruction![[op_aa, AM_AA, IN_AA] TAX         ];

    make_instruction![[op_a8, AM_A8, IN_A8] TAY         ];

    make_instruction![[op_ba, AM_BA, IN_BA] TSX         ];

    make_instruction![[op_8a, AM_8A, IN_8A] TXA         ];

    make_instruction![[op_9a, AM_9A, IN_9A] TXS         ];

    make_instruction![[op_98, AM_98, IN_98] TYA         ];
}

mod illegal {
    use super::*;

    make_instruction![[op_c7, AM_C7, IN_C7] ~DCP &LL    ];
    make_instruction![[op_d7, AM_D7, IN_D7] ~DCP &LL,X  ];
    make_instruction![[op_cf, AM_CF, IN_CF] ~DCP &LLHH  ];
    make_instruction![[op_df, AM_DF, IN_DF] ~DCP &LLHH,X];
    make_instruction![[op_db, AM_DB, IN_DB] ~DCP &LLHH,Y];
    make_instruction![[op_c3, AM_C3, IN_C3] ~DCP (&LL,X)];
    make_instruction![[op_d3, AM_D3, IN_D3] ~DCP (&LL),Y];

    make_instruction![[op_e7, AM_E7, IN_E7] ~ISC &LL    ];
    make_instruction![[op_f7, AM_F7, IN_F7] ~ISC &LL,X  ];
    make_instruction![[op_ef, AM_EF, IN_EF] ~ISC &LLHH  ];
    make_instruction![[op_ff, AM_FF, IN_FF] ~ISC &LLHH,X];
    make_instruction![[op_fb, AM_FB, IN_FB] ~ISC &LLHH,Y];
    make_instruction![[op_e3, AM_E3, IN_E3] ~ISC (&LL,X)];
    make_instruction![[op_f3, AM_F3, IN_F3] ~ISC (&LL),Y];

    make_instruction![[op_a7, AM_A7, IN_A7] ~LAX &LL    ];
    make_instruction![[op_b7, AM_B7, IN_B7] ~LAX &LL,Y  ];
    make_instruction![[op_af, AM_AF, IN_AF] ~LAX &LLHH  ];
    make_instruction![[op_bf, AM_BF, IN_BF] ~LAX &LLHH,Y];
    make_instruction![[op_a3, AM_A3, IN_A3] ~LAX (&LL,X)];
    make_instruction![[op_b3, AM_B3, IN_B3] ~LAX (&LL),Y];

    make_instruction![[op_1a, AM_1A, IN_1A] ~NOP        ];
    make_instruction![[op_3a, AM_3A, IN_3A] ~NOP        ];
    make_instruction![[op_5a, AM_5A, IN_5A] ~NOP        ];
    make_instruction![[op_7a, AM_7A, IN_7A] ~NOP        ];
    make_instruction![[op_da, AM_DA, IN_DA] ~NOP        ];
    make_instruction![[op_fa, AM_FA, IN_FA] ~NOP        ];
    make_instruction![[op_80, AM_80, IN_80] ~NOP #&BB   ];
    make_instruction![[op_82, AM_82, IN_82] ~NOP #&BB   ];
    make_instruction![[op_89, AM_89, IN_89] ~NOP #&BB   ];
    make_instruction![[op_c2, AM_C2, IN_C2] ~NOP #&BB   ];
    make_instruction![[op_e2, AM_E2, IN_E2] ~NOP #&BB   ];
    make_instruction![[op_04, AM_04, IN_04] ~NOP &LL    ];
    make_instruction![[op_44, AM_44, IN_44] ~NOP &LL    ];
    make_instruction![[op_64, AM_64, IN_64] ~NOP &LL    ];
    make_instruction![[op_14, AM_14, IN_14] ~NOP &LL,X  ];
    make_instruction![[op_34, AM_34, IN_34] ~NOP &LL,X  ];
    make_instruction![[op_54, AM_54, IN_54] ~NOP &LL,X  ];
    make_instruction![[op_74, AM_74, IN_74] ~NOP &LL,X  ];
    make_instruction![[op_d4, AM_D4, IN_D4] ~NOP &LL,X  ];
    make_instruction![[op_f4, AM_F4, IN_F4] ~NOP &LL,X  ];
    make_instruction![[op_0c, AM_0C, IN_0C] ~NOP &LLHH  ];
    make_instruction![[op_1c, AM_1C, IN_1C] ~NOP &LLHH,X];
    make_instruction![[op_3c, AM_3C, IN_3C] ~NOP &LLHH,X];
    make_instruction![[op_5c, AM_5C, IN_5C] ~NOP &LLHH,X];
    make_instruction![[op_7c, AM_7C, IN_7C] ~NOP &LLHH,X];
    make_instruction![[op_dc, AM_DC, IN_DC] ~NOP &LLHH,X];
    make_instruction![[op_fc, AM_FC, IN_FC] ~NOP &LLHH,X];

    make_instruction![[op_27, AM_27, IN_27] ~RLA &LL    ];
    make_instruction![[op_37, AM_37, IN_37] ~RLA &LL,X  ];
    make_instruction![[op_2f, AM_2F, IN_2F] ~RLA &LLHH  ];
    make_instruction![[op_3f, AM_3F, IN_3F] ~RLA &LLHH,X];
    make_instruction![[op_3b, AM_3B, IN_3B] ~RLA &LLHH,Y];
    make_instruction![[op_23, AM_23, IN_23] ~RLA (&LL,X)];
    make_instruction![[op_33, AM_33, IN_33] ~RLA (&LL),Y];

    make_instruction![[op_67, AM_67, IN_67] ~RRA &LL    ];
    make_instruction![[op_77, AM_77, IN_77] ~RRA &LL,X  ];
    make_instruction![[op_6f, AM_6F, IN_6F] ~RRA &LLHH  ];
    make_instruction![[op_7f, AM_7F, IN_7F] ~RRA &LLHH,X];
    make_instruction![[op_7b, AM_7B, IN_7B] ~RRA &LLHH,Y];
    make_instruction![[op_63, AM_63, IN_63] ~RRA (&LL,X)];
    make_instruction![[op_73, AM_73, IN_73] ~RRA (&LL),Y];

    make_instruction![[op_87, AM_87, IN_87] ~SAX &LL    ];
    make_instruction![[op_97, AM_97, IN_97] ~SAX &LL,Y  ];
    make_instruction![[op_8f, AM_8F, IN_8F] ~SAX &LLHH  ];
    make_instruction![[op_83, AM_83, IN_83] ~SAX (&LL,X)];

    make_instruction![[op_eb, AM_EB, IN_EB] ~SBC #&BB   ];

    make_instruction![[op_07, AM_07, IN_07] ~SLO &LL    ];
    make_instruction![[op_17, AM_17, IN_17] ~SLO &LL,X  ];
    make_instruction![[op_0f, AM_0F, IN_0F] ~SLO &LLHH  ];
    make_instruction![[op_1f, AM_1F, IN_1F] ~SLO &LLHH,X];
    make_instruction![[op_1b, AM_1B, IN_1B] ~SLO &LLHH,Y];
    make_instruction![[op_03, AM_03, IN_03] ~SLO (&LL,X)];
    make_instruction![[op_13, AM_13, IN_13] ~SLO (&LL),Y];

    make_instruction![[op_47, AM_47, IN_47] ~SRE &LL    ];
    make_instruction![[op_57, AM_57, IN_57] ~SRE &LL,X  ];
    make_instruction![[op_4f, AM_4F, IN_4F] ~SRE &LLHH  ];
    make_instruction![[op_5f, AM_5F, IN_5F] ~SRE &LLHH,X];
    make_instruction![[op_5b, AM_5B, IN_5B] ~SRE &LLHH,Y];
    make_instruction![[op_43, AM_43, IN_43] ~SRE (&LL,X)];
    make_instruction![[op_53, AM_53, IN_53] ~SRE (&LL),Y];

    make_instruction![[op_02, AM_02, IN_02] ~JAM        ];
    make_instruction![[op_12, AM_12, IN_12] ~JAM        ];
    make_instruction![[op_22, AM_22, IN_22] ~JAM        ];
    make_instruction![[op_32, AM_32, IN_32] ~JAM        ];
    make_instruction![[op_42, AM_42, IN_42] ~JAM        ];
    make_instruction![[op_52, AM_52, IN_52] ~JAM        ];
    make_instruction![[op_62, AM_62, IN_62] ~JAM        ];
    make_instruction![[op_72, AM_72, IN_72] ~JAM        ];
    make_instruction![[op_92, AM_92, IN_92] ~JAM        ];
    make_instruction![[op_b2, AM_B2, IN_B2] ~JAM        ];
    make_instruction![[op_d2, AM_D2, IN_D2] ~JAM        ];
    make_instruction![[op_f2, AM_F2, IN_F2] ~JAM        ];
}

use legal::*;
use illegal::*;

pub fn reset() -> InstructionIterator {
    InstructionIterator::new(&addr::IMP, &act::RESET)
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, rc::Rc};

    use crate::ines::{
        cart::{Cartridge, CartridgeLoadError, Mapper},
        console::Nes,
        cpu::{Bus, Status},
        io::{ReadDevice, WriteDevice},
        ppu::{Bus as PpuBus, PpuRef},
        NesState,
    };

    use super::*;

    const NES_TEST: &[u8; 24592] = include_bytes!("nestest.nes");
    const NES_TEST_LOGS: &str = include_str!("nestest.log");

    #[test]
    fn adc_imm() {
        let mpr = Rc::new(RefCell::new(Mapper::default()));
        let ppu: PpuRef<PpuBus> = PpuRef::default();
        let mut bus = Bus::new(ppu, mpr);
        let mut reg = Registers {
            ac: 0x05,
            p: Status::U,
            pc: 0x00, // Where the next instruction will be loaded
            ..Default::default()
        };

        bus.write(0x00, 0x05); // Immediate mode value

        let mut iter = op_69();
        while let Some(_) = iter.next() {
            iter.clock(&mut reg, &mut bus);
        }

        assert_eq!(reg.ac, 0x0A);
        assert_eq!(reg.p.get(Status::N), 0);
        assert_eq!(reg.p.get(Status::C), 0);
        assert_eq!(reg.p.get(Status::Z), 0);
        assert_eq!(iter.cc, 2, "rw_count");
    }

    #[test]
    fn bne_no_branch() {
        let mpr = Rc::new(RefCell::new(Mapper::default()));
        let ppu: PpuRef<PpuBus> = PpuRef::default();
        let mut bus = Bus::new(ppu, mpr);
        let mut reg = Registers {
            p: Status::U | Status::Z,
            pc: 0x00, // Where the next instruction will be loaded
            ..Default::default()
        };

        bus.write(0x00, 0x05); // Immediate mode value

        bus.read(0x0000); // Cycle to simulate the op code read.

        let mut iter = op_d0();

        while let Some(_) = iter.next() {
            iter.clock(&mut reg, &mut bus);
        }

        assert_eq!(reg.pc, 0x01, "PC");
        assert_eq!(iter.cc, 2, "cycle count");
    }

    #[test]
    fn bne_branch_same_page() {
        let mpr = Rc::new(RefCell::new(Mapper::default()));
        let ppu: PpuRef<PpuBus> = PpuRef::default();
        let mut bus = Bus::new(ppu, mpr);
        let mut reg = Registers {
            p: Status::U,
            pc: 0x00, // Where the next instruction will be loaded
            ..Default::default()
        };
        bus.write(0x00, 0x05); // Immediate mode value

        bus.read(0x0000); // Cycle to simulate the op code read.

        let mut iter = op_d0();

        while let Some(_) = iter.next() {
            iter.clock(&mut reg, &mut bus);
        }

        assert_eq!(reg.pc, 0x06, "PC");
        assert_eq!(iter.cc, 3, "cycle count");
    }

    #[test]
    fn bne_branch_new_page() {
        let mpr = Rc::new(RefCell::new(Mapper::default()));
        let ppu: PpuRef<PpuBus> = PpuRef::default();
        let mut bus = Bus::new(ppu, mpr);
        let mut reg = Registers {
            p: Status::U,
            pc: 0xFD, // Where the next instruction will be loaded
            ..Default::default()
        };
        bus.write(0xFD, 0x05); // Immediate mode value

        bus.read(0x0000); // Cycle to simulate the op code read.

        let mut iter = op_d0();

        while let Some(_) = iter.next() {
            iter.clock(&mut reg, &mut bus);
        }

        assert_eq!(reg.pc, 0x00FD + 0x0001 + 0x0005, "PC");
        assert_eq!(iter.cc, 4, "cycle count");
    }

    #[test]
    fn nestest_log() -> Result<(), CartridgeLoadError> {
        let cart = Cartridge::try_from(NES_TEST.as_slice())?;
        let mut nes = Nes::default().with_cart(cart).with_entry(0xC000);

        let logs = NES_TEST_LOGS.split('\n').map(|s| s.trim_end());
        for (i, instruction) in logs.enumerate() {
            let next_pc = u16::from_str_radix(&instruction[0..4], 16)
                .expect("First 4 values represent the pc in hex");

            let NesState {
                ppu,
                cpu,
                tcc: _tcc,
            } = nes.run_pc(next_pc);

            let instruction_debug = format!(
                "{} {}{}",
                cpu.op,
                cpu.addr.nestest_log_addr2(),
                cpu.oper.nestest_log(ADDR_MODE[cpu.opcode as usize])
            );

            let last_instruction = format!("{:>04X}  {:>02X} {:<6}{:<33}A:{:>02X} X:{:>02X} Y:{:>02X} P:{:>02X} SP:{:>02X} PPU:{:>3},{:>3} CYC:{}",
                    cpu.reg.pc, cpu.opcode, cpu.addr.nestest_log_addr1(), instruction_debug, cpu.reg.ac, cpu.reg.x, cpu.reg.y, u8::from(cpu.reg.p), cpu.reg.sp, ppu.scanline, ppu.cycle, cpu.tcc
            );

            assert_eq!(last_instruction, instruction, "Error at line: {i}");
        }

        Ok(())
    }

    #[test]
    fn nestest() -> Result<(), CartridgeLoadError> {
        let cart = Cartridge::try_from(NES_TEST.as_slice())?;
        let mut nes = Nes::default().with_cart(cart).with_entry(0xC000);
        
        // According to nestest logs the test ends at $C66E
        nes.run_until(0xC66E);

        // Fetch error codes
        let official_opcode_result = nes.read(0x02);

        assert_eq!(
            official_opcode_result, 0,
            "Official opcodes exited with code ${:02X}",
            official_opcode_result
        );

        Ok(())
    }

    #[test]
    fn nestest_illegal() -> Result<(), CartridgeLoadError> {
        let cart = Cartridge::try_from(NES_TEST.as_slice())?;
        let mut nes = Nes::default().with_cart(cart).with_entry(0xC000);
        
        // According to nestest logs the test ends at $C66E
        nes.run_until(0xC66E);

        // Fetch error codes
        let unofficial_opcode_result = nes.read(0x03);

        assert_eq!(
            unofficial_opcode_result, 0,
            "Unofficial opcodes exited with code ${:02X}",
            unofficial_opcode_result
        );

        Ok(())
    }
}
