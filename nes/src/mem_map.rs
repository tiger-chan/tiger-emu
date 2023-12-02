use std::{cell::RefCell, rc::Rc};

use crate::{
    io::{ReadDevice, RwDevice, RwDeviceRef, WriteDevice},
    ppu::NameTable,
    Byte, Word,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Access {
    Read,
    Write,
    ReadWrite,
}

#[derive(Debug)]
pub struct OpenEntry {
    pub lo: Word,
    pub hi: Word,
}

#[derive(Debug)]
pub struct TableEntry {
    pub lo: Word,
    pub hi: Word,
    pub device: RwDeviceRef,
}

pub trait PpuMemoryMapper {
    fn map_ppu(&self, mem_map: &mut MemoryMap, nametables: &[Rc<RefCell<NameTable>>; 2]);
}

pub trait CpuMemoryMapper {
    fn map_cpu(&self, mem_map: &mut MemoryMap);
}

#[derive(Debug)]
pub struct MemoryMap {
    spaces_r: Vec<TableEntry>,
    spaces_w: Vec<TableEntry>,
    open_ranges: Vec<OpenEntry>,
    last_read: RefCell<Byte>,
    pub name: String,
}

impl Default for MemoryMap {
    fn default() -> Self {
        Self {
            spaces_r: vec![],
            spaces_w: vec![],
            open_ranges: vec![],
            last_read: RefCell::default(),
            name: "unknown".to_owned(),
        }
    }
}

impl MemoryMap {
    pub fn register(&mut self, lo: Word, hi: Word, table: RwDeviceRef, access: Access) {
        if access != Access::Write {
            Self::register_to(&mut self.spaces_r, lo, hi, table.clone(), &self.name);
        }

        if access != Access::Read {
            Self::register_to(&mut self.spaces_w, lo, hi, table, &self.name);
        }
    }

    fn register_to(arr: &mut Vec<TableEntry>, lo: Word, hi: Word, table: RwDeviceRef, name: &str) {
        if arr.iter().any(|s| s.lo <= lo && hi <= s.hi) {
            log::error!(
                "[[{}]] Duplicate registration of memory for {lo} to {hi}",
                name
            );
        } else {
            arr.push(TableEntry {
                lo,
                hi,
                device: table,
            });
            arr.sort_by(|a, b| a.hi.cmp(&b.lo));
        }
    }

    pub fn register_open(&mut self, lo: Word, hi: Word) {
        if self.spaces_r.iter().any(|s| s.lo <= lo && hi <= s.hi) {
            log::error!(
                "[[{}]] Duplicate registration of memory for {lo} to {hi} (Open)",
                self.name
            );
        } else if self.open_ranges.iter().any(|s| s.lo <= lo && hi <= s.hi) {
            log::error!(
                "[[{}]] Duplicate registration of memory for {lo} to {hi} (Open)",
                self.name
            );
        } else {
            self.open_ranges.push(OpenEntry { lo, hi });
            self.open_ranges.sort_by(|a, b| a.hi.cmp(&b.lo));
        }
    }
}

impl RwDevice for MemoryMap {}

impl ReadDevice for MemoryMap {
    fn read(&self, addr: Word) -> Byte {
        for space in self.spaces_r.iter() {
            if space.lo <= addr && addr <= space.hi {
                *self.last_read.borrow_mut() = space.device.borrow().read(addr);
                return *self.last_read.borrow();
            }
        }

        for space in self.open_ranges.iter() {
            if space.lo <= addr && addr <= space.hi {
                return *self.last_read.borrow();
            }
        }
        log::warn!(
            "[[{}]] unmapped region is being read ${addr:>04X}",
            self.name
        );
        0
    }

    fn read_only(&self, addr: Word) -> Byte {
        for space in self.spaces_r.iter() {
            if space.lo <= addr && addr <= space.hi {
                return space.device.borrow().read_only(addr);
            }
        }
        0
    }
}

impl WriteDevice for MemoryMap {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        for space in self.spaces_w.iter() {
            if space.lo <= addr && addr <= space.hi {
                return space.device.borrow_mut().write(addr, data);
            }
        }
        log::warn!(
            "[[{}]] unmapped region is being written ${addr:>04X}",
            self.name
        );
        0
    }
}
