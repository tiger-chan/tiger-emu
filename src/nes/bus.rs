use std::ops::RangeInclusive;

use super::Addr;

pub trait RangeRWCpuBus {
    fn read(&self, addr: Addr) -> Option<u8>;
    fn read_only(&self, addr: Addr) -> Option<u8>;
    fn write(&mut self, addr: Addr, data: u8) -> Option<()>;

    fn accepted_range(&self) -> RangeInclusive<Addr>;
}

pub trait RWPpuBus {
    fn read(&self, addr: Addr) -> Option<u8>;
    fn read_only(&self, addr: Addr) -> Option<u8>;
    fn write(&mut self, addr: Addr, data: u8) -> Option<()>;
}
