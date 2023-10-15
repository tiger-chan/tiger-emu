use nes::{
    io::{ReadDevice, RwDevice, WriteDevice},
    Byte, Cpu, CpuCtrl, Registers, Status, Word,
};

const CPU_RAM_LO: Word = 0x0000;
const MPR_IO_HI: Word = 0xFFFF;
const TEST_RAM: usize = (MPR_IO_HI - CPU_RAM_LO) as usize + 1;

#[derive(Debug)]
pub struct TestBus {
    ram: [Byte; TEST_RAM],
}

impl Default for TestBus {
    fn default() -> Self {
        Self { ram: [0; TEST_RAM] }
    }
}

impl RwDevice for TestBus {}

impl ReadDevice for TestBus {
    fn read(&self, addr: Word) -> Byte {
        self.ram[addr as usize]
    }
}

impl WriteDevice for TestBus {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        let addr = addr as usize;
        let tmp = self.ram[addr];
        self.ram[addr] = data;
        tmp
    }
}

impl CpuCtrl for TestBus {
    fn reset(&mut self) {}
}

#[derive(Clone, Debug, PartialEq, PartialOrd, serde::Deserialize)]
struct RamEntry(Word, Byte);

#[derive(Clone, Debug, PartialEq, PartialOrd, serde::Deserialize)]
struct Nes6502Cycles(Word, Byte, String);

#[derive(Clone, Debug, PartialEq, PartialOrd, serde::Deserialize)]
struct Nes6502State {
    pc: Word,
    s: Byte,
    a: Byte,
    x: Byte,
    y: Byte,
    p: Byte,
    ram: Vec<RamEntry>,
}

impl From<&Nes6502State> for Registers {
    fn from(value: &Nes6502State) -> Self {
        Self {
            ac: value.a,
            p: Status(value.p),
            pc: value.pc,
            sp: value.s,
            x: value.x,
            y: value.y,
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, serde::Deserialize)]
struct Nes6502 {
    name: String,
    initial: Nes6502State,
    #[serde(alias = "final")]
    fin: Nes6502State,
    cycles: Vec<Nes6502Cycles>,
}

// Download tests json here: https://github.com/TomHarte/ProcessorTests/tree/main/nes6502
macro_rules! cpu_test {
    ($v:literal, $test:ident) => {
        #[ignore] // Remove the ignore if the test files are downloaded
        #[test]
        fn $test() -> Result<(), Box<dyn std::error::Error>> {
            use std::fs::File;
            use std::io::Read;

            let test_file = format!("../roms/nes6502/v1/{}.json", $v.to_ascii_lowercase());

            // Open a file for reading
            let mut file = File::open(test_file)?;

            // Create a buffer to store the file contents
            let mut buffer = Vec::new();

            // Read the file contents into the buffer
            file.read_to_end(&mut buffer)?;

            let json_data = String::from_utf8(buffer)?;

            let tests: Vec<Nes6502> = serde_json::from_str(json_data.as_str())?;

            let mut cpu = Cpu::default();
            cpu.configure_bus(TestBus::default());

            for entry in tests {
                for ram in entry.initial.ram.iter() {
                    cpu.write(ram.0, ram.1);
                }

                cpu.complete_operation();
                cpu.set_reg(Registers::from(&entry.initial));

                let _ = cpu.run_pc(entry.initial.pc);
                let state = cpu.cur_state();

                let expected_reg = Registers::from(&entry.fin);
                assert_eq!(
                    state.reg, expected_reg,
                    "Test: {} -- Registers don't match",
                    entry.name
                );
                for ram in entry.fin.ram.iter() {
                    let val = cpu.read(ram.0);
                    assert_eq!(
                        val, ram.1,
                        "Test: {} -- Ram at ${:>04X} should be equal after processing",
                        entry.name, ram.0
                    );
                }
            }

            Ok(())
        }
    };
}

cpu_test!("00", cpu_op_00);
cpu_test!("01", cpu_op_01);
//cpu_test!("  ", cpu_op_  );
cpu_test!("03", cpu_op_03);
cpu_test!("04", cpu_op_04);
cpu_test!("05", cpu_op_05);
cpu_test!("06", cpu_op_06);
cpu_test!("07", cpu_op_07);
cpu_test!("08", cpu_op_08);
cpu_test!("09", cpu_op_09);
cpu_test!("0A", cpu_op_0a);
//cpu_test!("  ", cpu_op_  );
cpu_test!("0C", cpu_op_0c);
cpu_test!("0D", cpu_op_0d);
cpu_test!("0E", cpu_op_0e);
cpu_test!("0F", cpu_op_0f);
cpu_test!("10", cpu_op_10);
cpu_test!("11", cpu_op_11);
//cpu_test!("  ", cpu_op_  );
cpu_test!("13", cpu_op_13);
cpu_test!("14", cpu_op_14);
cpu_test!("15", cpu_op_15);
cpu_test!("16", cpu_op_16);
cpu_test!("17", cpu_op_17);
cpu_test!("18", cpu_op_18);
cpu_test!("19", cpu_op_19);
cpu_test!("1A", cpu_op_1a);
cpu_test!("1B", cpu_op_1b);
cpu_test!("1C", cpu_op_1c);
cpu_test!("1D", cpu_op_1d);
cpu_test!("1E", cpu_op_1e);
cpu_test!("1F", cpu_op_1f);
cpu_test!("20", cpu_op_20);
cpu_test!("21", cpu_op_21);
//cpu_test!("  ", cpu_op_  );
cpu_test!("23", cpu_op_23);
cpu_test!("24", cpu_op_24);
cpu_test!("25", cpu_op_25);
cpu_test!("26", cpu_op_26);
cpu_test!("27", cpu_op_27);
cpu_test!("28", cpu_op_28);
cpu_test!("29", cpu_op_29);
cpu_test!("2A", cpu_op_2a);
//cpu_test!("  ", cpu_op_  );
cpu_test!("2C", cpu_op_2c);
cpu_test!("2D", cpu_op_2d);
cpu_test!("2E", cpu_op_2e);
cpu_test!("2F", cpu_op_2f);
cpu_test!("30", cpu_op_30);
cpu_test!("31", cpu_op_31);
//cpu_test!("  ", cpu_op_  );
cpu_test!("33", cpu_op_33);
cpu_test!("34", cpu_op_34);
cpu_test!("35", cpu_op_35);
cpu_test!("36", cpu_op_36);
cpu_test!("37", cpu_op_37);
cpu_test!("38", cpu_op_38);
cpu_test!("39", cpu_op_39);
cpu_test!("3A", cpu_op_3a);
cpu_test!("3B", cpu_op_3b);
cpu_test!("3C", cpu_op_3c);
cpu_test!("3D", cpu_op_3d);
cpu_test!("3E", cpu_op_3e);
cpu_test!("3F", cpu_op_3f);
cpu_test!("40", cpu_op_40);
cpu_test!("41", cpu_op_41);
//cpu_test!("  ", cpu_op_  );
cpu_test!("43", cpu_op_43);
cpu_test!("44", cpu_op_44);
cpu_test!("45", cpu_op_45);
cpu_test!("46", cpu_op_46);
cpu_test!("47", cpu_op_47);
cpu_test!("48", cpu_op_48);
cpu_test!("49", cpu_op_49);
cpu_test!("4A", cpu_op_4a);
//cpu_test!("  ", cpu_op_  );
cpu_test!("4C", cpu_op_4c);
cpu_test!("4D", cpu_op_4d);
cpu_test!("4E", cpu_op_4e);
cpu_test!("4F", cpu_op_4f);
cpu_test!("50", cpu_op_50);
cpu_test!("51", cpu_op_51);
//cpu_test!("  ", cpu_op_  );
cpu_test!("53", cpu_op_53);
cpu_test!("54", cpu_op_54);
cpu_test!("55", cpu_op_55);
cpu_test!("56", cpu_op_56);
cpu_test!("57", cpu_op_57);
cpu_test!("58", cpu_op_58);
cpu_test!("59", cpu_op_59);
cpu_test!("5A", cpu_op_5a);
cpu_test!("5B", cpu_op_5b);
cpu_test!("5C", cpu_op_5c);
cpu_test!("5D", cpu_op_5d);
cpu_test!("5E", cpu_op_5e);
cpu_test!("5F", cpu_op_5f);
cpu_test!("60", cpu_op_60);
cpu_test!("61", cpu_op_61);
//cpu_test!("  ", cpu_op_  );
cpu_test!("63", cpu_op_63);
cpu_test!("64", cpu_op_64);
cpu_test!("65", cpu_op_65);
cpu_test!("66", cpu_op_66);
cpu_test!("67", cpu_op_67);
cpu_test!("68", cpu_op_68);
cpu_test!("69", cpu_op_69);
cpu_test!("6A", cpu_op_6a);
//cpu_test!("  ", cpu_op_  );
cpu_test!("6C", cpu_op_6c);
cpu_test!("6D", cpu_op_6d);
cpu_test!("6E", cpu_op_6e);
cpu_test!("6F", cpu_op_6f);
cpu_test!("70", cpu_op_70);
cpu_test!("71", cpu_op_71);
//cpu_test!("  ", cpu_op_  );
cpu_test!("73", cpu_op_73);
cpu_test!("74", cpu_op_74);
cpu_test!("75", cpu_op_75);
cpu_test!("76", cpu_op_76);
cpu_test!("77", cpu_op_77);
cpu_test!("78", cpu_op_78);
cpu_test!("79", cpu_op_79);
cpu_test!("7A", cpu_op_7a);
cpu_test!("7B", cpu_op_7b);
cpu_test!("7C", cpu_op_7c);
cpu_test!("7D", cpu_op_7d);
cpu_test!("7E", cpu_op_7e);
cpu_test!("7F", cpu_op_7f);
cpu_test!("80", cpu_op_80);
cpu_test!("81", cpu_op_81);
cpu_test!("82", cpu_op_82);
cpu_test!("83", cpu_op_83);
cpu_test!("84", cpu_op_84);
cpu_test!("85", cpu_op_85);
cpu_test!("86", cpu_op_86);
cpu_test!("87", cpu_op_87);
cpu_test!("88", cpu_op_88);
cpu_test!("89", cpu_op_89);
cpu_test!("8A", cpu_op_8a);
//cpu_test!("  ", cpu_op_  );
cpu_test!("8C", cpu_op_8c);
cpu_test!("8D", cpu_op_8d);
cpu_test!("8E", cpu_op_8e);
cpu_test!("8F", cpu_op_8f);
cpu_test!("90", cpu_op_90);
cpu_test!("91", cpu_op_91);
//cpu_test!("  ", cpu_op_  );
//cpu_test!("  ", cpu_op_  );
cpu_test!("94", cpu_op_94);
cpu_test!("95", cpu_op_95);
cpu_test!("96", cpu_op_96);
cpu_test!("97", cpu_op_97);
cpu_test!("98", cpu_op_98);
cpu_test!("99", cpu_op_99);
cpu_test!("9A", cpu_op_9a);
//cpu_test!("  ", cpu_op_  );
//cpu_test!("  ", cpu_op_  );
cpu_test!("9D", cpu_op_9d);
//cpu_test!("  ", cpu_op_  );
//cpu_test!("  ", cpu_op_  );
cpu_test!("A0", cpu_op_a0);
cpu_test!("A1", cpu_op_a1);
cpu_test!("A2", cpu_op_a2);
cpu_test!("A3", cpu_op_a3);
cpu_test!("A4", cpu_op_a4);
cpu_test!("A5", cpu_op_a5);
cpu_test!("A6", cpu_op_a6);
cpu_test!("A7", cpu_op_a7);
cpu_test!("A8", cpu_op_a8);
cpu_test!("A9", cpu_op_a9);
cpu_test!("AA", cpu_op_aa);
//cpu_test!("  ", cpu_op_  );
cpu_test!("AC", cpu_op_ac);
cpu_test!("AD", cpu_op_ad);
cpu_test!("AE", cpu_op_ae);
cpu_test!("AF", cpu_op_af);
cpu_test!("B0", cpu_op_b0);
cpu_test!("B1", cpu_op_b1);
//cpu_test!("  ", cpu_op_  );
cpu_test!("B3", cpu_op_b3);
cpu_test!("B4", cpu_op_b4);
cpu_test!("B5", cpu_op_b5);
cpu_test!("B6", cpu_op_b6);
cpu_test!("B7", cpu_op_b7);
cpu_test!("B8", cpu_op_b8);
cpu_test!("B9", cpu_op_b9);
cpu_test!("BA", cpu_op_ba);
//cpu_test!("  ", cpu_op_  );
cpu_test!("BC", cpu_op_bc);
cpu_test!("BD", cpu_op_bd);
cpu_test!("BE", cpu_op_be);
cpu_test!("BF", cpu_op_bf);
cpu_test!("C0", cpu_op_c0);
cpu_test!("C1", cpu_op_c1);
cpu_test!("C2", cpu_op_c2);
cpu_test!("C3", cpu_op_c3);
cpu_test!("C4", cpu_op_c4);
cpu_test!("C5", cpu_op_c5);
cpu_test!("C6", cpu_op_c6);
cpu_test!("C7", cpu_op_c7);
cpu_test!("C8", cpu_op_c8);
cpu_test!("C9", cpu_op_c9);
cpu_test!("CA", cpu_op_ca);
//cpu_test!("  ", cpu_op_  );
cpu_test!("CC", cpu_op_cc);
cpu_test!("CD", cpu_op_cd);
cpu_test!("CE", cpu_op_ce);
cpu_test!("CF", cpu_op_cf);
cpu_test!("D0", cpu_op_d0);
cpu_test!("D1", cpu_op_d1);
//cpu_test!("  ", cpu_op_  );
cpu_test!("D3", cpu_op_d3);
cpu_test!("D4", cpu_op_d4);
cpu_test!("D5", cpu_op_d5);
cpu_test!("D6", cpu_op_d6);
cpu_test!("D7", cpu_op_d7);
cpu_test!("D8", cpu_op_d8);
cpu_test!("D9", cpu_op_d9);
cpu_test!("DA", cpu_op_da);
cpu_test!("DB", cpu_op_db);
cpu_test!("DC", cpu_op_dc);
cpu_test!("DD", cpu_op_dd);
cpu_test!("DE", cpu_op_de);
cpu_test!("DF", cpu_op_df);
cpu_test!("E0", cpu_op_e0);
cpu_test!("E1", cpu_op_e1);
cpu_test!("E2", cpu_op_e2);
cpu_test!("E3", cpu_op_e3);
cpu_test!("E4", cpu_op_e4);
cpu_test!("E5", cpu_op_e5);
cpu_test!("E6", cpu_op_e6);
cpu_test!("E7", cpu_op_e7);
cpu_test!("E8", cpu_op_e8);
cpu_test!("E9", cpu_op_e9);
cpu_test!("EA", cpu_op_ea);
cpu_test!("EB", cpu_op_eb);
cpu_test!("EC", cpu_op_ec);
cpu_test!("ED", cpu_op_ed);
cpu_test!("EE", cpu_op_ee);
cpu_test!("EF", cpu_op_ef);
cpu_test!("F0", cpu_op_f0);
cpu_test!("F1", cpu_op_f1);
//cpu_test!("  ", cpu_op_  );
cpu_test!("F3", cpu_op_f3);
cpu_test!("F4", cpu_op_f4);
cpu_test!("F5", cpu_op_f5);
cpu_test!("F6", cpu_op_f6);
cpu_test!("F7", cpu_op_f7);
cpu_test!("F8", cpu_op_f8);
cpu_test!("F9", cpu_op_f9);
cpu_test!("FA", cpu_op_fa);
cpu_test!("FB", cpu_op_fb);
cpu_test!("FC", cpu_op_fc);
cpu_test!("FD", cpu_op_fd);
cpu_test!("FE", cpu_op_fe);
cpu_test!("FF", cpu_op_ff);
