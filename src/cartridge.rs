pub trait Cartridge {
    fn cpu_read(&self, addr: u16) -> Option<u8>;
    fn cpu_read_only(&self, addr: u16) -> Option<u8>;
    fn cpu_write(&mut self, addr: u16, data: u8) -> Option<()>;

    fn ppu_read(&self, addr: u16) -> Option<u8>;
    fn ppu_read_only(&self, addr: u16) -> Option<u8>;
    fn ppu_write(&mut self, addr: u16, data: u8) -> Option<()>;
}
