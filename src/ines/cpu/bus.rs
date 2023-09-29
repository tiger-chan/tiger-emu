// https://www.nesdev.org/wiki/CPU_memory_map

use crate::ines::{
    cart::MapperRef,
    io::{ReadDevice, RwDevice, RwMapper, WriteDevice},
    Byte, Word, CPU_RAM,
};

/// ```text
/// Address range    Size      Device
/// $0000–$07FF     $0800      2 KB internal RAM
/// $0800–$0FFF     $0800      Mirrors of $0000–$07FF
/// $1000–$17FF     $0800      Mirrors of $0000–$07FF
/// $1800–$1FFF     $0800      Mirrors of $0000–$07FF
/// ```
const CPU_RAM_LO: Word = 0x0000;

/// ```text
/// Address range    Size      Device
/// $0000–$07FF     $0800      2 KB internal RAM
/// $0800–$0FFF     $0800      Mirrors of $0000–$07FF
/// $1000–$17FF     $0800      Mirrors of $0000–$07FF
/// $1800–$1FFF     $0800      Mirrors of $0000–$07FF
/// ```
const CPU_RAM_HI: Word = 0x1FFF;

/// ```text
/// Address range    Size      Device
/// $0000–$07FF     $0800      2 KB internal RAM
/// $0800–$0FFF     $0800      Mirrors of $0000–$07FF
/// $1000–$17FF     $0800      Mirrors of $0000–$07FF
/// $1800–$1FFF     $0800      Mirrors of $0000–$07FF
/// ```
const CPU_MIRROR_MASK: Word = 0x07FF;

/// ```text
/// Address range    Size      Device
/// $2000–$2007     $0008      NES PPU registers
/// $2008–$3FFF     $1FF8      Mirrors of $2000–$2007 (repeats every 8 bytes)
/// ```
#[allow(unused)]
const PPU_REGISTER_LO: Word = 0x2000;

/// ```text
/// Address range    Size      Device
/// $2000–$2007     $0008      NES PPU registers
/// $2008–$3FFF     $1FF8      Mirrors of $2000–$2007 (repeats every 8 bytes)
/// ```
#[allow(unused)]
const PPU_REGISTER_HI: Word = 0x3FFF;

/// ```text
/// Address range    Size      Device
/// $2000–$2007     $0008      NES PPU registers
/// $2008–$3FFF     $1FF8      Mirrors of $2000–$2007 (repeats every 8 bytes)
/// ```
#[allow(unused)]
const PPU_REGISTER_MASK: Word = 0x0007;

/// ```text
/// Address range    Size      Device
/// $4000–$4017     $0018      NES APU and I/O registers
/// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
/// ```
#[allow(unused)]
const APU_IO_LO: Word = 0x4000;

/// ```text
/// Address range    Size      Device
/// $4000–$4017     $0018      NES APU and I/O registers
/// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
/// ```
#[allow(unused)]
const APU_IO_HI: Word = 0x401F;

/// ```text
/// Address range    Size      Device
/// $4020–$FFFF     $BFE0      Cartridge space: PRG ROM, PRG RAM, and mapper registers
/// ```
#[allow(unused)]
const MPR_IO_LO: Word = 0x4020;

/// ```text
/// Address range    Size      Device
/// $4020–$FFFF     $BFE0      Cartridge space: PRG ROM, PRG RAM, and mapper registers
/// ```
#[allow(unused)]
const MPR_IO_HI: Word = 0xFFFF;

/// # CPU memory map
///```text
/// Address range    Size      Device
/// $0000–$07FF     $0800      2 KB internal RAM
/// $0800–$0FFF     $0800      Mirrors of $0000–$07FF
/// $1000–$17FF     $0800      Mirrors of $0000–$07FF
/// $1800–$1FFF     $0800      Mirrors of $0000–$07FF
/// $2000–$2007     $0008      NES PPU registers
/// $2008–$3FFF     $1FF8      Mirrors of $2000–$2007 (repeats every 8 bytes)
/// $4000–$4017     $0018      NES APU and I/O registers
/// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
/// $4020–$FFFF     $BFE0      Cartridge space: PRG ROM, PRG RAM, and mapper registers
/// ```
#[derive(Debug)]
pub struct Bus {
    ram: [Byte; CPU_RAM],
    mpr: MapperRef,
}

impl Bus {
    #[allow(dead_code)]
    pub fn new(mpr: MapperRef) -> Self {
        Self {
            ram: [0; CPU_RAM],
            mpr,
        }
    }
}

impl ReadDevice for Bus {
    fn read(&self, addr: Word) -> Byte {
        match addr {
            CPU_RAM_LO..=CPU_RAM_HI => self.ram[(addr & CPU_MIRROR_MASK) as usize],
            MPR_IO_LO..=MPR_IO_HI => self.mpr.borrow().read_prg(addr),
            _ => unimplemented!(),
        }
    }
}

impl WriteDevice for Bus {
    fn write(&mut self, addr: Word, data: Byte) {
        match addr {
            CPU_RAM_LO..=CPU_RAM_HI => self.ram[(addr & CPU_MIRROR_MASK) as usize] = data,
            MPR_IO_LO..=MPR_IO_HI => self.mpr.borrow_mut().write_prg(addr, data),
            _ => unimplemented!(),
        }
    }
}

impl RwDevice for Bus {}
