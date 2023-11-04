use std::{cell::RefCell, rc::Rc, sync::mpsc::Sender};

use crate::{
    cart::MapperRef,
    cpu::Message,
    io::{ReadDevice, ReadOnlyDevice, RwDevice, RwDeviceRef, WriteDevice},
    mem_map::{Access, MemoryMap},
    ppu::palette,
    Byte, Word,
};

use super::{nametable::NameTable, palette::PaletteTable};

pub trait CpuSignal {
    fn signal(&self) -> &Sender<Message>;
}

#[derive(Debug)]
pub struct Bus {
    #[allow(unused)]
    cpu: RwDeviceRef,
    mpr: MapperRef,
    nametables: [Rc<RefCell<NameTable>>; 2],
    palette: Rc<RefCell<PaletteTable>>,
    mem_map: MemoryMap,
    cpu_sig: Sender<Message>,
}

impl Bus {
    pub fn new(cpu: RwDeviceRef, cpu_sig: Sender<Message>, mpr: MapperRef) -> Self {
        use crate::mem_map::PpuMemoryMapper;

        let mut value = Self {
            cpu,
            mpr,
            nametables: [Rc::default(), Rc::default()],
            palette: Rc::new(RefCell::new(PaletteTable::default())),
            mem_map: MemoryMap::default(),
            cpu_sig,
        };

        value.mem_map.register(
            palette::RAM_LO,
            palette::RAM_HI,
            value.palette.clone(),
            Access::ReadWrite,
        );

        value
            .mpr
            .borrow()
            .map_ppu(&mut value.mem_map, &value.nametables);

        value
    }
}

impl CpuSignal for Bus {
    fn signal(&self) -> &Sender<Message> {
        &self.cpu_sig
    }
}

impl RwDevice for Bus {}

impl ReadDevice for Bus {
    fn read(&self, addr: Word) -> Byte {
        self.mem_map.read(addr)
    }
}

impl ReadOnlyDevice for Bus {
    fn read_only(&self, addr: Word) -> Byte {
        self.mem_map.read(addr)
    }
}

impl WriteDevice for Bus {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        self.mem_map.write(addr, data)
    }
}
