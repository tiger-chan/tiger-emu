pub mod nestest;

#[cfg(test)]
mod test {
    use crate::{
        cart::{Cartridge, CartridgeLoadError},
        console::Nes,
        cpu::{ADDR_MODE, Registers, Status, Cpu, TestBus},
        roms, NesState, Word, Byte,
    };

    #[derive(Clone, Debug, PartialEq, PartialOrd)]
    #[derive(serde::Deserialize)]
    struct RamEntry(Word, Byte);

    #[derive(Clone, Debug, PartialEq, PartialOrd)]
    #[derive(serde::Deserialize)]
    struct Nes6502Cycles(Word, Byte, String);

    #[derive(Clone, Debug, PartialEq, PartialOrd)]
    #[derive(serde::Deserialize)]
    struct Nes6502State {
        pc: Word,
        s: Byte,
        a: Byte,
        x: Byte,
        y: Byte,
        p: Byte,
        ram: Vec<RamEntry>
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

    #[derive(Clone, Debug, PartialEq, PartialOrd)]
    #[derive(serde::Deserialize)]
    struct Nes6502 {
        name: String,
        initial: Nes6502State,
        #[serde(alias = "final")]
        fin: Nes6502State,
        cycles: Vec<Nes6502Cycles>,
    }

    #[test]
    fn nestest_log() -> Result<(), CartridgeLoadError> {
        let cart = Cartridge::try_from(roms::nestest::ROM.as_slice())?;
        let mut nes = Nes::default().with_cart(cart).with_entry(0xC000);

        let logs = roms::nestest::LOGS.split('\n').map(|s| s.trim_end());
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
        let cart = Cartridge::try_from(roms::nestest::ROM.as_slice())?;
        let mut nes = Nes::default().with_cart(cart).with_entry(0xC000);

        // According to nestest logs the test ends at $C66E
        nes.run_until(0xC66E);

        // Fetch error codes
        let official_opcode_result = nes.read(0x02);
        let unofficial_opcode_result = nes.read(0x03);

        assert_eq!(
            official_opcode_result, 0,
            "Official opcodes exited with code ${:02X} ${:02X}",
            official_opcode_result, unofficial_opcode_result
        );

        assert_eq!(
            unofficial_opcode_result, 0,
            "Unofficial opcodes exited with code ${:02X} ${:02X}",
            official_opcode_result, unofficial_opcode_result
        );

        Ok(())
    }

    #[test]
    fn nes6502_00() -> Result<(), Box<dyn std::error::Error>> {
        use std::fs::File;
        use std::io::Read;

        // Open a file for reading
        let mut file = File::open("../roms/nes6502/v1/00.json")?;

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
            assert_eq!(state.reg, expected_reg, "Test: {} -- Registers don't match", entry.name);
            for ram in entry.fin.ram.iter() {
                let val = cpu.read(ram.0);
                assert_eq!(val, ram.1, "Test: {} -- Ram at ${:>04X} should be equal after processing", entry.name, ram.0);
            }
        }

        
        Ok(())
    }
}
