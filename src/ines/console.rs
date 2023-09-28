use super::{
    cpu::{Bus as CpuBus, CpuRef},
    ClockSeq, Process, NTSC_SEQ,
};

#[derive(Debug)]
pub struct Nes {
    cpu: CpuRef<CpuBus>,
    seq: ClockSeq,
    tcc: u64,
}

impl Nes {
    #[allow(unused)]
    pub fn new() -> Self {
        Nes::default()
    }
}

impl Default for Nes {
    fn default() -> Self {
        Self {
            cpu: CpuRef::default(),
            seq: NTSC_SEQ,
            tcc: 0,
        }
    }
}

impl Iterator for Nes {
    type Item = ();
    fn next(&mut self) -> Option<Self::Item> {
        self.seq.iter_mut().for_each(|p| {
            if let Some(process) = p.next() {
                match process {
                    Process::Ppu => {
                        //self.ppu.borrow_mut().next();s
                    }
                    Process::Cpu => {
                        self.cpu.borrow_mut().next();
                    }
                    Process::Apu => {
                        //self.apu.borrow_mut().next();
                    }
                    _ => {}
                }
            }
        });

        self.tcc = self.tcc.wrapping_add(1);

        Some(())
    }
}
