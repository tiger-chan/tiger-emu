#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Process {
    None,
    Cpu,
    Ppu,
    Apu,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ClockCounter {
    pub proc: Process,
    step: u8,
    cycl: u8,
    curr: u8,
}

impl ClockCounter {
    pub const fn new(p: Process, cycle: u8, step: u8) -> Self {
        Self {
            proc: p,
            cycl: cycle,
            step,
            curr: cycle,
        }
    }
}

impl Iterator for ClockCounter {
    type Item = Process;
    fn next(&mut self) -> Option<Self::Item> {
        self.curr += self.step;
        match self.curr {
            x if x >= self.cycl => {
                self.curr -= self.cycl;
                Some(self.proc)
            }
            _ => Some(Process::None),
        }
    }
}

pub type ClockSeq = [ClockCounter; 3];

/// https://www.nesdev.org/wiki/Cycle_reference_chart
/// NTSC sequence is 6 ppu per 2 cpu per 1 apu
#[allow(dead_code)]
pub const NTSC_SEQ: ClockSeq = [
    ClockCounter::new(Process::Ppu, 30, 30),
    ClockCounter::new(Process::Cpu, 30, 10),
    ClockCounter::new(Process::Apu, 30, 5),
];

/// https://www.nesdev.org/wiki/Cycle_reference_chart
/// PAL sequence is 6.4 ppu per 2 cpu per 1 apu
#[allow(dead_code)]
pub const PAL_SEQ: ClockSeq = [
    ClockCounter::new(Process::Ppu, 32, 32),
    ClockCounter::new(Process::Cpu, 32, 10),
    ClockCounter::new(Process::Apu, 32, 5),
];
