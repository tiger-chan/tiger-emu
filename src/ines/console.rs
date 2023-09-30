use std::{cell::RefCell, rc::Rc};

use super::{
    cart::{Cartridge, Mapper, MapperRef},
    cpu::{Bus as CpuBus, CpuRef, CpuState, InstructionState},
    ppu::{Bus as PpuBus, PpuRef, PpuState},
    ClockSeq, Process, Word, NTSC_SEQ,
};

#[derive(Debug, Default, Clone)]
pub struct NesState {
    pub ppu: PpuState,
    pub cpu: InstructionState,
    pub tcc: u64,
}

#[derive(Debug)]
pub struct Nes {
    cpu: CpuRef<CpuBus>,
    ppu: PpuRef<PpuBus>,
    mpr: MapperRef,
    seq: ClockSeq,
    tcc: u64,
    state: NesState,
    ppu_cur: PpuState,
}

impl Nes {
    #[allow(unused)]
    pub fn new() -> Self {
        Self::default()
    }

    #[allow(unused)]
    pub fn with_cart(cart: Cartridge) -> Self {
        let mut nes = Self {
            ..Default::default()
        };
        nes.insert_cart(cart);
        nes.cpu.borrow_mut().reset();
        nes
    }

    #[allow(unused)]
    pub fn insert_cart(&mut self, cart: Cartridge) -> &mut Self {
        self.mpr = Rc::new(RefCell::new(Mapper::from(cart)));
        let cpu_bus = CpuBus::new(self.mpr.clone());
        self.cpu.borrow_mut().configure_bus(cpu_bus);
        self
    }

    #[allow(unused)]
    pub fn reset(&mut self) {
        while !self.cpu.borrow().waiting() {
            self.next();
        }
        self.cpu.borrow_mut().reset();
    }

    #[allow(unused)]
    pub fn run_pc(&mut self, addr: Word) -> NesState {
        if self.cpu.borrow().cur_pc() != addr {
            while !self.cpu.borrow().waiting() {
                self.next();
            }
            self.cpu.borrow_mut().pc(addr);
        }
        if let Some(cc) = self.count_until_process(Process::Cpu) {
            for _ in 0..=cc {
                self.next();
            }
            while !self.cpu.borrow().waiting() {
                self.next();
            }
        }

        self.state.clone()
    }

    #[allow(unused)]
    pub fn run_until(&mut self, addr: Word) -> NesState {
        let mut pc = self.cpu.borrow().cur_pc();
        while pc != addr {
            self.next();
            pc = self.cpu.borrow().cur_pc();
        }

        self.state.clone()
    }

    fn count_until_process(&self, p: Process) -> Option<u8> {
        for elem in self.seq {
            if elem.proc == p {
                let cp = elem;
                let mut i: u8 = 0;
                for x in cp {
                    if x == Process::None {
                        i += 1;
                    } else {
                        break;
                    }
                }
                return Some(i);
            }
        }
        None
    }
}

impl Default for Nes {
    fn default() -> Self {
        Self {
            cpu: CpuRef::default(),
            ppu: PpuRef::default(),
            mpr: MapperRef::default(),
            seq: NTSC_SEQ,
            tcc: 0,
            state: NesState::default(),
            ppu_cur: PpuState::default(),
        }
    }
}

impl Iterator for Nes {
    type Item = NesState;
    fn next(&mut self) -> Option<Self::Item> {
        for p in self.seq.iter_mut() {
            if let Some(process) = p.next() {
                match process {
                    Process::Ppu => {
                        if let Some(state) = self.ppu.borrow_mut().next() {
                            self.ppu_cur = state;
                        }
                    }
                    Process::Cpu => match self.cpu.borrow_mut().next() {
                        Some(CpuState::OperComplete(state)) => {
                            self.state.cpu = state;
                        }
                        None => {
                            self.state.ppu = self.ppu_cur;
                        }
                        _ => {}
                    },
                    Process::Apu => {
                        //self.apu.borrow_mut().next();
                    }
                    _ => {}
                }
            }
        }

        self.tcc = self.tcc.wrapping_add(1);

        Some(self.state.clone())
    }
}
