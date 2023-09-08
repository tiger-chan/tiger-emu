pub trait Motherboard {
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
