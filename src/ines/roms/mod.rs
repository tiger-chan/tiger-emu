pub mod nestest;

#[cfg(test)]
mod test {
    use crate::ines::{
        cart::{Cartridge, CartridgeLoadError},
        console::Nes,
        cpu::ADDR_MODE,
        roms, NesState,
    };

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
}
