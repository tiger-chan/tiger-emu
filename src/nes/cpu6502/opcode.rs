mod generator;

use generator::*;

use super::{address_mode::AddrMode, instructions::Instruction, Registers, StatusReg};
use crate::{
    bus::Bus,
    nes::{Addr, HI_MASK, IRQ_HI, IRQ_LO, LO_MASK, PS},
};
use log::trace;

pub type Instruc = fn(&mut Registers, &mut dyn Bus);

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

pub const INSTRUCTION_TYPE: [Instruction; 256] = [
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
    fn init() {
        let _ = env_logger::builder()
            .filter_module("nes_ultra", log::LevelFilter::Debug)
            .target(env_logger::Target::Stdout)
            .is_test(true).try_init();
    }

    use std::{cell::RefCell, rc::Rc};

    use crate::nes::{
        board::{ClockBusContext, RangedBoardBus},
        BoardRam, Board, Cartridge,
    };

    use super::*;

    #[test]
    fn adc() {
        {
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

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
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

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
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

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
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

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
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

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
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

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
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

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
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

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

	#[test]
	fn cmp() {
        {
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x00, 0x05); // Immediate mode value

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_c9(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.p.get(StatusReg::N), 0,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 1,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 1,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  2, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x00, 0xFD); // Zeropage mode value
            bus.write(0xFD, 0x0A); // Pointed to value

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_c5(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.p.get(StatusReg::N), 1,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 0,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 0,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  3, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
            reg.x = 1;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x00, 0xFD); // Zeropage,X mode value
            bus.write(0xFE, 0x0A); // Pointed to value

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_d5(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.x,  0x01,                  "reg.X");
            assert_eq!(reg.p.get(StatusReg::N), 1,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 0,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 0,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  3, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x00;
            reg.pc = 0xFF; // Where the next instruction will be loaded
            bus.write(0x0000, 0xFE); // ABS LL mode value
            bus.write(0x0001, 0x01); // ABS HH mode value
            bus.write(0x01FE, 0x00); // Pointed to value (HHLL)

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_cd(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x00,                  "reg.A");
            assert_eq!(reg.p.get(StatusReg::N), 0,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 0,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 0,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  4, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
			reg.x = 1;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x0000, 0xFE); // ABS,X LL mode value
            bus.write(0x0001, 0x01); // ABS,X HH mode value
            bus.write(0x01FF, 0x05); // Pointed to value (HHLL)

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_dd(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.x,  0x01,                  "reg.X");
            assert_eq!(reg.p.get(StatusReg::N), 0,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 1,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 1,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  4, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
			reg.y = 2;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x0000, 0xFE); // ABS,Y LL mode value
            bus.write(0x0001, 0x01); // ABS,Y HH mode value
            bus.write(0x0200, 0x05); // Pointed to value (HHLL)

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_d9(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.y,  0x02,                  "reg.Y");
            assert_eq!(reg.p.get(StatusReg::N), 0,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 1,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 1,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  5, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x81;
			reg.x = 2;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x0000, 0xFE); // (OPER,X) IND mode value
            bus.write(0x0100, 0x01); // IND addr LL
            bus.write(0x0101, 0x04); // IND addr HH
            bus.write(0x0401, 0x01); // Pointed to value (HHLL)

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_c1(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x81,                  "reg.A");
            assert_eq!(reg.x,  0x02,                  "reg.X");
            assert_eq!(reg.p.get(StatusReg::N), 1,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 1,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 0,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  6, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x81;
			reg.y = 1;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x0000, 0xFD); // (OPER,X) IND mode value
            bus.write(0x00FD, 0x01); // IND addr LL
            bus.write(0x00FE, 0x04); // IND addr HH
            bus.write(0x0402, 0x01); // Pointed to value (HHLL)

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_d1(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x81,                  "reg.A");
            assert_eq!(reg.y,  0x01,                  "reg.Y");
            assert_eq!(reg.p.get(StatusReg::N), 1,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 1,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 0,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  5, "rw_count");
        }
	}

	#[test]
	fn cpx() {
        {
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
            reg.x = 0x05;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x00, 0x05); // Immediate mode value

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_e0(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.x,  0x05,                  "reg.X");
            assert_eq!(reg.p.get(StatusReg::N), 0,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 1,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 1,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  2, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
            reg.x = 0x05;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x00, 0xFD); // Zeropage mode value
            bus.write(0xFD, 0x0A); // Pointed to value

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_e4(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.x,  0x05,                  "reg.X");
            assert_eq!(reg.p.get(StatusReg::N), 1,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 0,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 0,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  3, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
            reg.x = 0x00;
            reg.pc = 0xFF; // Where the next instruction will be loaded
            bus.write(0x0000, 0xFE); // ABS LL mode value
            bus.write(0x0001, 0x01); // ABS HH mode value
            bus.write(0x01FE, 0x00); // Pointed to value (HHLL)

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_ec(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.x,  0x00,                  "reg.X");
            assert_eq!(reg.p.get(StatusReg::N), 0,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 0,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 0,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  4, "rw_count");
        }
	}

	#[test]
	fn cpy() {
        {
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
            reg.y = 0x05;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x00, 0x05); // Immediate mode value

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_c0(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.y,  0x05,                  "reg.Y");
            assert_eq!(reg.p.get(StatusReg::N), 0,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 1,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 1,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  2, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
            reg.y = 0x05;
            reg.pc = 0x00; // Where the next instruction will be loaded
            bus.write(0x00, 0xFD); // Zeropage mode value
            bus.write(0xFD, 0x0A); // Pointed to value

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_c4(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.y,  0x05,                  "reg.Y");
            assert_eq!(reg.p.get(StatusReg::N), 1,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 0,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 0,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  3, "rw_count");
        }

		{
            let mut reg = Registers::default();
            let ram = Rc::new(RefCell::new(BoardRam::new()));
            let mut bus = RangedBoardBus::new();
            bus.push(&ram);

            reg.p = StatusReg::U;
            reg.ac = 0x05;
            reg.y = 0x00;
            reg.pc = 0xFF; // Where the next instruction will be loaded
            bus.write(0x0000, 0xFE); // ABS LL mode value
            bus.write(0x0001, 0x01); // ABS HH mode value
            bus.write(0x01FE, 0x00); // Pointed to value (HHLL)

            let mut bus = ClockBusContext::new(&mut bus);
            bus.read(0x0000); // Cycle to simulate the op code read.
            op_cc(&mut reg, &mut bus);

            assert_eq!(reg.ac, 0x05,                  "reg.A");
            assert_eq!(reg.y,  0x00,                  "reg.Y");
            assert_eq!(reg.p.get(StatusReg::N), 0,        "N");
            assert_eq!(reg.p.get(StatusReg::C), 0,        "C");
            assert_eq!(reg.p.get(StatusReg::Z), 0,        "Z");
            assert_eq!(*bus.rw_count.borrow(),  4, "rw_count");
        }
	}

    const NES_TEST: &[u8; 24592] = include_bytes!("nestest.nes");
    //const NES_TEST_LOGS: &str = include_str!("nestest.log");

    #[test]
    fn nes_test() -> Result<(), std::io::Error> {
        init();
        
        let mut board = Board::new();
        let cart = Cartridge::new_from_bytes(NES_TEST)?;
        board.load_cart(cart);

        board.pc(0x0C000);

        // let logs: Vec<&str> = NES_TEST_LOGS.split('\n').map(|s| s.trim_end()).collect();
        // for _instruction in logs {
        //     board.step();
        // }

        // According to nestest logs the test ends at $C66E
        board.run_until(0xC66E);

        let official = board.read_only(0x02);
        let unofficial = board.read_only(0x03);

        assert_eq!(board.tcc(), 26554);
        assert_eq!(official, 0x00);
        assert_eq!(unofficial, 0x00);


        Ok(())
    }
}
