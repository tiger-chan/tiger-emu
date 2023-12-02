// https://www.nesdev.org/wiki/CPU_memory_map

use std::{cell::RefCell, rc::Rc};

use crate::{
    cart::MapperRef,
    hid::HidRef,
    io::{ReadDevice, RwDevice, RwDeviceRef, WriteDevice},
    mem_map::MemoryMap,
    ppu, Byte, Word, CPU_RAM,
};

#[derive(Debug)]
pub struct CpuRam(pub [Byte; CPU_RAM]);

impl Default for CpuRam {
    fn default() -> Self {
        Self([0; CPU_RAM])
    }
}

impl RwDevice for CpuRam {}

impl ReadDevice for CpuRam {
    fn read(&self, addr: Word) -> Byte {
        let masked = (addr & CPU_MIRROR_MASK) as usize;
        self.0[masked]
    }
}

impl WriteDevice for CpuRam {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        let masked = (addr & CPU_MIRROR_MASK) as usize;
        let tmp = self.0[masked];
        self.0[masked] = data;
        tmp
    }
}

mod apu {
    use crate::{
        io::{ReadDevice, RwDevice, WriteDevice},
        Byte, Word,
    };

    /// ```text
    /// Address range    Size      Device
    /// $4000–$4017     $0018      NES APU and I/O registers
    /// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
    /// ```
    pub const IO_LO: Word = 0x4000;

    /// ```text
    /// Address range    Size      Device
    /// $4000–$4017     $0018      NES APU and I/O registers
    /// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
    /// ```
    pub const IO_HI: Word = 0x401F;

    /// ```text
    /// Address range    Size      Device
    /// $4000–$4017     $0018      NES APU and I/O registers
    /// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
    /// ```
    pub const IO_APU_LO: Word = IO_LO;

    /// ```text
    /// Address range    Size      Device
    /// $4000–$4017     $0018      NES APU and I/O registers
    /// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
    /// ```
    pub const IO_APU_HI: Word = 0x4015;

    /// ```text
    /// Address range    Size      Device
    /// $4000–$4017     $0018      NES APU and I/O registers
    /// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
    /// ```
    pub const IO_HID_LO: Word = 0x4016;

    /// ```text
    /// Address range    Size      Device
    /// $4000–$4017     $0018      NES APU and I/O registers
    /// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
    /// ```
    pub const IO_HID_HI: Word = 0x4017;

    /// ```text
    /// Address range    Size      Device
    /// $4000–$4017     $0018      NES APU and I/O registers
    /// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
    /// ```
    pub const IO_APU_TEST_LO: Word = 0x4018;

    /// ```text
    /// Address range    Size      Device
    /// $4000–$4017     $0018      NES APU and I/O registers
    /// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
    /// ```
    pub const IO_APU_TEST_HI: Word = 0x401A;

    /// ```text
    /// Address range    Size      Device
    /// $4000–$4017     $0018      NES APU and I/O registers
    /// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
    /// ```
    pub const IO_UNUSED_LO: Word = 0x401B;

    /// ```text
    /// Address range    Size      Device
    /// $4000–$4017     $0018      NES APU and I/O registers
    /// $4018–$401F     $0008      APU and I/O functionality that is normally disabled. See CPU Test Mode.
    /// ```
    pub const IO_UNUSED_HI: Word = IO_HI;

    const IO_MASK: Word = 0x001F;

    const MEM_SIZE: usize = (IO_HI - IO_LO) as usize + 1;

    #[derive(Debug)]
    pub struct Ram(pub [Byte; MEM_SIZE]);

    impl Default for Ram {
        fn default() -> Self {
            Self([0xFF; MEM_SIZE])
        }
    }

    impl RwDevice for Ram {}

    impl ReadDevice for Ram {
        fn read(&self, addr: Word) -> Byte {
            log::warn!("Unimplemented region @ {addr:<04X} is being read");
            let masked = (addr & IO_MASK) as usize;
            self.0[masked]
        }
    }

    impl WriteDevice for Ram {
        fn write(&mut self, addr: Word, data: Byte) -> Byte {
            log::warn!("Unimplemented region @ {addr:<04X} is being written");
            let masked = (addr & IO_MASK) as usize;
            let tmp = self.0[masked];
            self.0[masked] = data;
            tmp
        }
    }
}

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

pub trait CpuCtrl {
    fn reset(&mut self);
}

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
    ram: Rc<RefCell<CpuRam>>,
    apu: Rc<RefCell<apu::Ram>>,
    mem_map: MemoryMap,
}

impl Bus {
    pub fn new(ppu: RwDeviceRef, mpr: MapperRef, hid: HidRef) -> Self {
        use crate::mem_map::{Access::*, CpuMemoryMapper};

        let mut value = Self {
            ram: Rc::new(RefCell::new(CpuRam::default())),
            apu: Rc::new(RefCell::new(apu::Ram::default())),
            mem_map: MemoryMap::default(),
        };
        value.mem_map.name = "CPU Bus".to_owned();

        // https://www.nesdev.org/wiki/CPU_memory_map
        {
            value
                .mem_map
                .register(CPU_RAM_LO, CPU_RAM_HI, value.ram.clone(), ReadWrite);
            value
                .mem_map
                .register(ppu::REG_LO, ppu::REG_HI, ppu, ReadWrite);

            value
                .mem_map
                .register(apu::IO_APU_LO, apu::IO_APU_HI, value.apu.clone(), ReadWrite);

            value
                .mem_map
                .register(apu::IO_HID_HI, apu::IO_HID_HI, value.apu.clone(), Write);

            value
                .mem_map
                .register(apu::IO_HID_LO, apu::IO_HID_HI, hid.clone(), Read);

            value
                .mem_map
                .register(apu::IO_HID_LO, apu::IO_HID_LO, hid.clone(), Write);

            value.mem_map.register(
                apu::IO_APU_TEST_LO,
                apu::IO_APU_TEST_HI,
                value.apu.clone(),
                ReadWrite,
            );

            value.mem_map.register(
                apu::IO_UNUSED_LO,
                apu::IO_UNUSED_HI,
                value.apu.clone(),
                ReadWrite,
            );

            mpr.borrow().map_cpu(&mut value.mem_map);
        }
        value
    }
}

impl ReadDevice for Bus {
    fn read(&self, addr: Word) -> Byte {
        self.mem_map.read(addr)
    }
}

impl WriteDevice for Bus {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        self.mem_map.write(addr, data)
    }
}

impl RwDevice for Bus {}

impl CpuCtrl for Bus {
    fn reset(&mut self) {}
}
