use nes::prelude::*;

pub const ROM: &[u8; 24592] = include_bytes!("nestest/nestest.nes");

pub const LOGS: &str = include_str!("nestest/nestest.log");

trait NestestAmDisplay {
    fn nestest_log_addr1(&self) -> String;

    fn nestest_log_addr2(&self) -> String;
}

trait NestestOpDisplay {   
    fn nestest_log(&self, addr: cpu::AddrMode) -> String;
}

impl NestestOpDisplay for cpu::OperData {
    fn nestest_log(&self, addr: cpu::AddrMode) -> String {
        use cpu::{OperData, AddrMode};
        match self {
            OperData::Word(w) => {
                format!("${:<04X}", w)
            }
            OperData::Byte(b) => match addr {
                AddrMode::IMM => String::from(""),
                AddrMode::IMP => String::from(""),
                _ => {
                    format!(" = {:<02X}", b)
                }
            },
            _ => "".to_owned(),
        }
    }
}


impl NestestAmDisplay for  cpu::AddrModeData {
    fn nestest_log_addr1(&self) -> String {
        use cpu::AddrModeData;
        match &self {
            AddrModeData::A | AddrModeData::Imp => {
                String::from("")
            }
            AddrModeData::Abs(lo, hi)
            | AddrModeData::Abx(lo, hi, _)
            | AddrModeData::Aby(lo, hi, _)
            | AddrModeData::Ind(lo, hi, _) => {
                format!("{:>02X} {:>02X}", lo, hi)
            }
            AddrModeData::Imm(val) => {
                format!("{:>02X}", val)
            }
            AddrModeData::Izx(lo, _, _)
            | AddrModeData::Izy(lo, _, _)
            | AddrModeData::Rel(lo, _)
            | AddrModeData::Zpx(lo, _)
            | AddrModeData::Zpy(lo, _) => {
                format!("{:>02X}", lo)
            }
            AddrModeData::Zpg(lo) => {
                format!("{:>02X}", lo)
            }
        }
    }

    fn nestest_log_addr2(&self) -> String {
        use cpu::AddrModeData;
        match &self {
            AddrModeData::A => String::from("A"),
            AddrModeData::Imp => String::from(""),
            AddrModeData::Abs(lo, hi) => {
                format!("${:>04X}", *lo as Word | (*hi as Word) << 8)
            }
            AddrModeData::Aby(lo, hi, addr) => {
                format!("${:>02X}{:>02X},Y @ {:>04X}", hi, lo, addr)
            }
            AddrModeData::Abx(lo, hi, addr) => {
                format!("${:>02X}{:>02X},X @ {:>04X}", hi, lo, addr)
            }
            AddrModeData::Imm(bb) => {
                format!("#${:>02X}", bb)
            }
            AddrModeData::Zpg(ll) => {
                format!("${:>02X}", ll)
            }
            AddrModeData::Izx(ll, ptr, addr) => {
                format!("(${:>02X},X) @ {:>02X} = {:>04X}", ll, ptr, addr)
            }
            AddrModeData::Izy(ll, ptr, addr) => {
                format!("(${:>02X}),Y = {:>04X} @ {:>04X}", ll, ptr, addr)
            }
            AddrModeData::Ind(lo, hi, addr) => {
                format!("(${:>02X}{:>02X}) = {:>04X}", hi, lo, addr)
            }
            AddrModeData::Zpx(lo, addr) => {
                format!("${:>02X},X @ {:>02X}", lo, addr)
            }
            AddrModeData::Zpy(lo, addr) => {
                format!("${:>02X},Y @ {:>02X}", lo, addr)
            }
            AddrModeData::Rel(_, _) => String::from(""),
        }
    }
}

#[cfg(test)]
mod test {
    use nes::{
        cart::{Cartridge, CartridgeLoadError},
        Nes,
        NesState,
    };

    use super::{ROM, LOGS, NestestAmDisplay, NestestOpDisplay};

    #[test]
    fn nestest_log() -> Result<(), CartridgeLoadError> {
        let cart = Cartridge::try_from(ROM.as_slice())?;
        let mut nes = Nes::default().with_cart(cart).with_entry(0xC000);

        let logs = LOGS.split('\n').map(|s| s.trim_end());
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
                cpu.oper.nestest_log(cpu.am)
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
        let cart = Cartridge::try_from(ROM.as_slice())?;
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
