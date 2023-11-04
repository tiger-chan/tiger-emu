mod nrom;

use std::{cell::RefCell, rc::Rc};

pub use nrom::Nrom;

use crate::{
    mem_map::{CpuMemoryMapper, MemoryMap, PpuMemoryMapper},
    ppu::NameTable,
};

use super::Cartridge;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Mirror {
    #[allow(unused)]
    Single,
    Vertical,
    Horizontal,
    #[allow(unused)]
    Quad,
}

pub mod name_tbl {
    use crate::Word;

    pub const NAMETABLE_0_LO: Word = 0x2000;
    pub const NAMETABLE_0_HI: Word = 0x23FF;
    pub const NAMETABLE_1_LO: Word = 0x2400;
    pub const NAMETABLE_1_HI: Word = 0x27FF;
    pub const NAMETABLE_2_LO: Word = 0x2800;
    pub const NAMETABLE_2_HI: Word = 0x2BFF;
    pub const NAMETABLE_3_LO: Word = 0x2C00;
    pub const NAMETABLE_3_HI: Word = 0x2FFF;
}

pub type MapperRef = Rc<RefCell<Mapper>>;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Mapper {
    Nrom(Nrom),
}

impl Default for Mapper {
    fn default() -> Self {
        Self::Nrom(Nrom::default())
    }
}

macro_rules! as_t_impl {
    ($n:ident, $n_mut:ident, $t: ident) => {
        fn $n(&self) -> &dyn $t {
            match self {
                Mapper::Nrom(x) => x,
            }
        }

        #[allow(unused)]
        fn $n_mut(&mut self) -> &mut dyn $t {
            match self {
                Mapper::Nrom(x) => x,
            }
        }
    };
}

impl Mapper {
    as_t_impl!(as_mm, as_mm_mut, PpuMemoryMapper);
    as_t_impl!(as_cpu_mm, as_cpu_mm_mut, CpuMemoryMapper);
}

impl PpuMemoryMapper for Mapper {
    fn map_ppu(&self, mem_map: &mut MemoryMap, nametables: &[Rc<RefCell<NameTable>>; 2]) {
        self.as_mm().map_ppu(mem_map, nametables);
    }
}

impl CpuMemoryMapper for Mapper {
    fn map_cpu(&self, mem_map: &mut MemoryMap) {
        self.as_cpu_mm().map_cpu(mem_map);
    }
}

impl From<Cartridge> for Mapper {
    fn from(value: Cartridge) -> Self {
        let header = &value.header;
        let mpr_id = (header.mpr2 & 0xF0) | header.mpr1 >> 4;
        match mpr_id {
            0 => Self::Nrom(Nrom::from(value)),
            _ => {
                unimplemented!("Mapper {} not implemented", mpr_id)
            }
        }
    }
}
