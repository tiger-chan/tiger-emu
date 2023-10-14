use crate::Byte;
use std::{io, io::Read};

use super::CartridgeLoadError;

const PRG_CHUNK_SIZE: usize = 16384;
const CHR_CHUNK_SIZE: usize = 8192;

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Format {
    #[default]
    INes,
    Nes2,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct INesHeader {
    pub fmt: Format,
    pub name: [Byte; 4],
    pub prg_chunks: Byte,
    pub chr_chunks: Byte,
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
    pub mpr1: Byte,
    /// Flags 7 – Mapper, VS/Playchoice, NES 2.0
    /// ```
    /// 76543210
    /// ||||||||
    /// |||||||+- VS Unisystem
    /// ||||||+-- PlayChoice-10 (8 KB of Hint Screen data stored after CHR data)
    /// ||||++--- If equal to 2, flags 8-15 are in NES 2.0 format
    /// ++++----- Upper nybble of mapper number
    /// ```
    pub mpr2: Byte,
    /// Flags 8 – PRG-RAM size (rarely used extension)
    ///```
    /// 76543210
    /// ||||||||
    /// ++++++++- PRG RAM size
    /// ```
    pub prg_ram_size: Byte,
    /// Flags 9 – TV system (rarely used extension)
    /// ```
    /// 76543210
    /// ||||||||
    /// |||||||+- TV system (0: NTSC; 1: PAL)
    /// +++++++-- Reserved, set to zero
    /// ```
    pub tv_sys1: Byte,
    /// Flags 10 – TV system, PRG-RAM presence (unofficial, rarely used extension)
    /// ```
    /// 76543210
    ///   ||  ||
    ///   ||  ++- TV system (0: NTSC; 2: PAL; 1/3: dual compatible)
    ///   |+----- PRG RAM ($6000-$7FFF) (0: present; 1: not present)
    ///   +------ 0: Board has no bus conflicts; 1: Board has bus conflicts
    /// ```
    pub tv_sys2: Byte,
    // Unused
    pub padding: [Byte; 5],
}

impl TryFrom<&mut io::Cursor<&[u8]>> for INesHeader {
    type Error = CartridgeLoadError;
    fn try_from(value: &mut io::Cursor<&[u8]>) -> Result<Self, Self::Error> {
        let mut result = Self {
            ..Default::default()
        };

        let _ = value.read(&mut result.name);

        let mut byte_buf = [0; 1];

        let _ = value.read(&mut byte_buf);
        result.prg_chunks = byte_buf[0];

        let _ = value.read(&mut byte_buf);
        result.chr_chunks = byte_buf[0];

        let _ = value.read(&mut byte_buf);
        result.mpr1 = byte_buf[0];

        let _ = value.read(&mut byte_buf);
        result.mpr2 = byte_buf[0];

        if result.mpr2 & 0x0C == 0x08 {
            result.fmt = Format::Nes2;
            unimplemented!();
        } else {
            result.fmt = Format::INes;

            let _ = value.read(&mut byte_buf);
            result.prg_ram_size = byte_buf[0];

            let _ = value.read(&mut byte_buf);
            result.tv_sys1 = byte_buf[0];

            let _ = value.read(&mut byte_buf);
            result.tv_sys2 = byte_buf[0];

            let mut byte_buf = [0; 5];
            let _ = value.read(&mut byte_buf);
            if byte_buf.iter().any(|x| 0 != *x) {
                Err(CartridgeLoadError::BadHeader)
            } else {
                Ok(result)
            }
        }
    }
}

impl INesHeader {
    #[allow(unused)]
    pub fn new(fmt: Format) -> Self {
        Self {
            name: [b'n', b'e', b's', 0],
            fmt,
            ..Default::default()
        }
    }

    #[allow(unused)]
    pub fn with_chr(mut self, size: u8) -> Self {
        self.chr_chunks = size;
        self
    }

    #[allow(unused)]
    pub fn with_prg(mut self, size: u8) -> Self {
        self.prg_chunks = size;
        self
    }

    pub fn chr_size(&self) -> usize {
        let size = if self.chr_chunks == 0 {
            1
        } else {
            self.chr_chunks as usize
        };
        size * CHR_CHUNK_SIZE
    }
    
    pub fn prg_size(&self) -> usize {
        self.prg_chunks as usize * PRG_CHUNK_SIZE
    }

    pub fn prg_bank(&self, cursor: &mut io::Cursor<&[u8]>) -> Result<Vec<u8>, CartridgeLoadError> {
        match self.fmt {
            Format::INes => {
                let mut prg = vec![0; self.prg_size()];
                let _ = cursor.read(prg.as_mut_slice());

                Ok(prg)
            }
            Format::Nes2 => Err(CartridgeLoadError::BadPrgChunk),
        }
    }

    pub fn chr_bank(&self, cursor: &mut io::Cursor<&[u8]>) -> Result<Vec<u8>, CartridgeLoadError> {
        match self.fmt {
            Format::INes => {
                let mut chr = vec![0; self.chr_size()];
                let _ = cursor.read(chr.as_mut_slice());

                Ok(chr)
            }
            Format::Nes2 => Err(CartridgeLoadError::BadChrChunk),
        }
    }
}
