use crate::{
    io::{ReadDevice, RwDevice, RwDeviceRef, WriteDevice},
    Byte, Word,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Access {
    Read,
    Write,
    ReadWrite,
}

#[derive(Debug)]
pub struct TableEntry {
    pub lo: Word,
    pub hi: Word,
    pub device: RwDeviceRef,
    pub access: Access,
}

#[derive(Debug, Default)]
pub struct MemoryMap {
    spaces: Vec<TableEntry>,
}

impl MemoryMap {
    pub fn register(&mut self, lo: Word, hi: Word, table: RwDeviceRef, access: Access) {
        self.spaces.push(TableEntry {
            lo,
            hi,
            device: table,
            access,
        });
        self.spaces.sort_by(|a, b| a.hi.cmp(&b.lo));
    }
}

impl RwDevice for MemoryMap {}

impl ReadDevice for MemoryMap {
    fn read(&self, addr: Word) -> Byte {
        for space in self.spaces.iter() {
            if space.lo <= addr && addr <= space.hi && space.access != Access::Write {
                return space.device.borrow().read(addr);
            }
        }
        log::warn!("unmapped region is being read {addr:>04X}");
        0
    }
}

impl WriteDevice for MemoryMap {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        for space in self.spaces.iter() {
            if space.lo <= addr && addr <= space.hi && space.access != Access::Read {
                return space.device.borrow_mut().write(addr, data);
            }
        }
        log::warn!("unmapped region is being written {addr:>04X}");
        0
    }
}
