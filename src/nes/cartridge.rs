use std::fs;
use std::path::Path;
use std::{io, io::Read};

type ProgramMemory = Vec<u8>;
type CharacterMemory = Vec<u8>;

enum Mapper {
    NROM = 0,
    MMC1 = 1,
    UxROM = 2,
}

impl From<u8> for Mapper {
	fn from(value: u8) -> Self {
		match value {
			0 => Self::NROM,
			1 => Self::MMC1,
			2 => Self::UxROM,
			_ => {
				unimplemented!("Mapper {} not implemented", value)
			}
		}
	}
}

struct INesHeader {
    name: [u8; 4],
    prg_chunks: u8,
    chr_chunks: u8,
    /// Flags 6 – Mapper, mirroring, battery, trainer
    /// ```
    /// 76543210
    /// ||||||||
    /// |||||||+- Mirroring: 0: horizontal (vertical arrangement) (CIRAM A10 = PPU A11)
    /// |||||||              1: vertical (horizontal arrangement) (CIRAM A10 = PPU A10)
    /// ||||||+-- 1: Cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
    /// |||||+--- 1: 512-byte trainer at $7000-$71FF (stored before PRG data)
    /// ||||+---- 1: Ignore mirroring control or above mirroring bit; instead provide four-screen VRAM
    /// ++++----- Lower nybble of mapper number
    /// ```
    mpr1: u8,
    /// Flags 7 – Mapper, VS/Playchoice, NES 2.0
    /// ```
    /// 76543210
    /// ||||||||
    /// |||||||+- VS Unisystem
    /// ||||||+-- PlayChoice-10 (8 KB of Hint Screen data stored after CHR data)
    /// ||||++--- If equal to 2, flags 8-15 are in NES 2.0 format
    /// ++++----- Upper nybble of mapper number
    /// ```
    mpr2: u8,
    /// Flags 8 – PRG-RAM size (rarely used extension)
    ///```
    /// 76543210
    /// ||||||||
    /// ++++++++- PRG RAM size
    /// ```
    prg_ram_size: u8,
    /// Flags 9 – TV system (rarely used extension)
    /// ```
    /// 76543210
    /// ||||||||
    /// |||||||+- TV system (0: NTSC; 1: PAL)
    /// +++++++-- Reserved, set to zero
    /// ```
    tv_sys1: u8,
    /// Flags 10 – TV system, PRG-RAM presence (unofficial, rarely used extension)
    /// ```
    /// 76543210
    ///   ||  ||
    ///   ||  ++- TV system (0: NTSC; 2: PAL; 1/3: dual compatible)
    ///   |+----- PRG RAM ($6000-$7FFF) (0: present; 1: not present)
    ///   +------ 0: Board has no bus conflicts; 1: Board has bus conflicts
    /// ```
    tv_sys2: u8,
    // Unused
    padding: [u8; 5],
}

impl From<&[u8]> for INesHeader {
    fn from(value: &[u8]) -> Self {
        let mut i = 0;
        let name = [value[i + 0], value[i + 1], value[i + 2], value[i + 3]];
        i += 4;
        let prg_chunks = value[i];
        i += 1;
        let chr_chunks = value[i];
        i += 1;
        let mpr1 = value[i];
        i += 1;
        let mpr2 = value[i];
        i += 1;
        let prg_ram_size = value[i];
        i += 1;
        let tv_sys1 = value[i];
        i += 1;
        let tv_sys2 = value[i];

        Self {
            name,
            prg_chunks,
            chr_chunks,
            mpr1,
            mpr2,
            prg_ram_size,
            tv_sys1,
            tv_sys2,
            padding: [0; 5],
        }
    }
}

pub struct Cartridge {
    prg: ProgramMemory,
    chr: CharacterMemory,
    mpr: Mapper,
    prg_bnk: u8,
    chr_bnk: u8,
}

impl Cartridge {
    pub fn new() -> Self {
        Self {
            prg: vec![],
            chr: vec![],
            mpr: Mapper::NROM,
            chr_bnk: 0,
            prg_bnk: 0,
        }
    }

    pub fn new_from_file(file_path: &Path) -> io::Result<Self> {
        let mut buffer = vec![];
        {
            let mut file = fs::File::open(file_path)?;
            file.read_to_end(&mut buffer)?;
        }

        let slice = buffer.as_slice();
        let header = INesHeader::from(&slice[0..16]);
        let i = 16;

        let mut i = if header.mpr1 & 0x04 == 0x04 {
            // Skip past training block
            i + 512
        } else {
            i
        };

        let mpr_id = (header.mpr2 & 0xF0) | header.mpr1 >> 4;

        let file_type: u8 = 1;

        let prg_chunks = header.prg_chunks;
        let chr_chunks = header.chr_chunks;
        let mut prg = vec![];
        let mut chr = vec![];
        match file_type {
            0 => {
                unimplemented!();
            }
            1 => {
                const PRG_CHUNK_SIZE: usize = 16384;
                const CHR_CHUNK_SIZE: usize = 8192;
                let size = prg_chunks as usize * PRG_CHUNK_SIZE;
                prg.resize(size, 0);
                prg.copy_from_slice(&buffer[i..size]);
                i = i + size;

                let size = chr_chunks as usize * CHR_CHUNK_SIZE;
                chr.resize(size, 0);
                chr.copy_from_slice(&buffer[i..size]);
                i = i + size;
            }
            2 => {
                unimplemented!();
            }
            _ => {
                unimplemented!("Unknown filetype");
            }
        }

        let result = Self {
            prg: prg,
            chr: chr,
            mpr: Mapper::from(mpr_id),
            chr_bnk: chr_chunks,
            prg_bnk: prg_chunks,
        };

        Ok(result)
    }
}

impl crate::cartridge::Cartridge for Cartridge {
    fn cpu_read(&self, addr: u16) -> Option<u8> {
        let _addr = addr & 0x0007;

        unimplemented!();
    }

    fn cpu_read_only(&self, addr: u16) -> Option<u8> {
        let _addr = addr & 0x0007;
        unimplemented!();
    }

    fn cpu_write(&mut self, addr: u16, data: u8) -> Option<()> {
        let _addr = addr & 0x0007;
        unimplemented!();
    }

    fn ppu_read(&self, addr: u16) -> Option<u8> {
        let _addr = addr & 0x3FFF;
        unimplemented!();
    }

    fn ppu_read_only(&self, addr: u16) -> Option<u8> {
        let _addr = addr & 0x3FFF;
        unimplemented!();
    }

    fn ppu_write(&mut self, addr: u16, data: u8) -> Option<()> {
        let _addr = addr & 0x3FFF;
        unimplemented!();
    }
}
