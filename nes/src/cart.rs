mod ines_header;
mod mapper;

use std::fs;
use std::io::Seek;
use std::path::Path;
use std::{io, io::Read, io::SeekFrom};

pub use mapper::{Mapper, MapperRef};
pub use ines_header::{INesHeader, Format};

pub type ProgramMemory = Vec<u8>;
pub type CharacterMemory = Vec<u8>;

#[allow(unused)]
#[derive(Debug, Default, Clone, PartialEq, PartialOrd)]
pub struct Cartridge {
    pub header: INesHeader,
    pub prg: ProgramMemory,
    pub chr: CharacterMemory,
    pub prg_bnk: u8,
    pub chr_bnk: u8,
}

#[allow(unused)]
impl Cartridge {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn with_header(mut self, header: INesHeader) -> Self {
        self.chr = vec![0; header.chr_size()];
        self.prg = vec![0; header.prg_size()];
        self.header = header;
        self.chr_bnk = header.chr_chunks;
        self.prg_bnk = header.prg_chunks;
        self
    }
}

#[derive(Debug)]
pub enum CartridgeLoadError {
    BadHeader,
    BadPrgChunk,
    BadChrChunk,
    FileError(io::Error),
    IoError(io::Error),
}

impl TryFrom<&[u8]> for Cartridge {
    type Error = CartridgeLoadError;
    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        let mut reader = io::Cursor::new(value);

        let header = INesHeader::try_from(&mut reader)?;
        if header.mpr1 & 0x04 == 0x04 {
            // Skip past training block
            reader
                .seek(SeekFrom::Current(512))
                .map_err(CartridgeLoadError::IoError)?;
        };

        let prg = header.prg_bank(&mut reader)?;
        let chr = header.chr_bank(&mut reader)?;

        Ok(Self {
            header,
            prg,
            chr,
            chr_bnk: header.chr_chunks,
            prg_bnk: header.prg_chunks,
        })
    }
}

impl TryFrom<&Path> for Cartridge {
    type Error = CartridgeLoadError;
    fn try_from(value: &Path) -> Result<Self, Self::Error> {
        let buffer = {
            let mut buffer = vec![];
            let file = fs::File::open(value);
            match file {
                Ok(mut file) => file
                    .read_to_end(&mut buffer)
                    .map(|_| buffer)
                    .map_err(CartridgeLoadError::FileError),
                Err(x) => Err(CartridgeLoadError::FileError(x)),
            }
        };

        match buffer {
            Ok(buffer) => Cartridge::try_from(buffer.as_slice()),
            Err(x) => Err(x),
        }
    }
}
