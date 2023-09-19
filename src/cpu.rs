use crate::bus::Bus;

pub trait Cpu {
    // External event functions. In hardware these represent pins that are asserted
    // to produce a change in state.
    // Reset Interrupt - Forces CPU into known state
    fn reset(&mut self, bus: &mut dyn Bus);
    // Interrupt Request - Executes an instruction at a specific location
    fn irq(&mut self, bus: &mut dyn Bus);
    // Non-Maskable Interrupt Request - As above, but cannot be disabled
    fn nmi(&mut self, bus: &mut dyn Bus);
    // Perform one clock cycle's worth of update
    fn clock(&mut self, bus: &mut dyn Bus);
}
