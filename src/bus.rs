pub trait Bus {
    fn read(&self, addr: u16) -> u8;
    fn read_only(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, data: u8);
}
