pub trait CPU {
    fn read(&self, addr: u16) -> u8;
    fn read_only(&self, addr: u16) -> u8;
    fn write(&self, addr: u16, data: u8);

    // External event functions. In hardware these represent pins that are asserted
    // to produce a change in state.
    // Reset Interrupt - Forces CPU into known state
    fn reset(&mut self);
    // Interrupt Request - Executes an instruction at a specific location
    fn irq(&mut self);
    // Non-Maskable Interrupt Request - As above, but cannot be disabled
    fn nmi(&mut self);
    // Perform one clock cycle's worth of update
    fn clock(&mut self);
}
