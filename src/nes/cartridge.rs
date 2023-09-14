use std::fs;
use std::path::Path;
use std::{io, io::Read};

use super::mapper::{INesHeader, NesCpuMapper, NesPpuMapper};
use super::{Mapper, RWPpuBus, RangeRWCpuBus};

type ProgramMemory = Vec<u8>;
type CharacterMemory = Vec<u8>;

#[allow(unused)]
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
            mpr: Mapper::from(&INesHeader::default()),
            chr_bnk: 0,
            prg_bnk: 0,
        }
    }

    #[allow(unused)]
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
            mpr: Mapper::from(&header),
            chr_bnk: chr_chunks,
            prg_bnk: prg_chunks,
        };

        Ok(result)
    }
}

impl RangeRWCpuBus for Cartridge {
    fn accepted_range(&self) -> std::ops::RangeInclusive<super::Addr> {
        0x0000..=0xFFFF
    }

    fn read(&self, addr: super::Addr) -> Option<u8> {
        match <Mapper as NesCpuMapper>::read(&self.mpr, addr) {
            Some(addr) => Some(self.prg[addr as usize]),
            None => None,
        }
    }

    fn read_only(&self, addr: super::Addr) -> Option<u8> {
        match <Mapper as NesCpuMapper>::read(&self.mpr, addr) {
            Some(addr) => Some(self.prg[addr as usize]),
            None => None,
        }
    }

    fn write(&mut self, addr: super::Addr, data: u8) -> Option<()> {
        match <Mapper as NesCpuMapper>::write(&self.mpr, addr) {
            Some(addr) => {
                self.prg[addr as usize] = data;
                Some(())
            }
            None => None,
        }
    }
}

impl RWPpuBus for Cartridge {
    fn read(&self, addr: super::Addr) -> Option<u8> {
        match <Mapper as NesPpuMapper>::read(&self.mpr, addr) {
            Some(addr) => Some(self.chr[addr as usize]),
            None => None,
        }
    }

    fn read_only(&self, addr: super::Addr) -> Option<u8> {
        match <Mapper as NesPpuMapper>::read(&self.mpr, addr) {
            Some(addr) => Some(self.chr[addr as usize]),
            None => None,
        }
    }

    fn write(&mut self, addr: super::Addr, data: u8) -> Option<()> {
        match <Mapper as NesPpuMapper>::write(&self.mpr, addr) {
            Some(addr) => {
                self.chr[addr as usize] = data;
                Some(())
            }
            None => None,
        }
    }
}
