use std::{cell::RefCell, rc::Rc};

use crate::Clocked;

use super::{
    cart::{Cartridge, Mapper, MapperRef},
    cpu::{Bus as CpuBus, CpuRef, CpuState, InstructionState},
    ppu::{Bus as PpuBus, PpuRef, PpuState},
    Byte, ClockSeq, Process, Word, NTSC_SEQ,
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
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_cart(mut self, cart: Cartridge) -> Self {
        self.insert_cart(cart);
        self.cpu.borrow_mut().reset();
        self
    }

    #[allow(unused)]
    pub fn with_entry(mut self, addr: Word) -> Self {
        self.cpu.borrow_mut().pc(addr);
        self
    }

    #[allow(unused)]
    pub fn insert_cart(&mut self, cart: Cartridge) -> &mut Self {
        self.mpr = Rc::new(RefCell::new(Mapper::from(cart)));
        let cpu_bus = CpuBus::new(self.ppu.clone(), self.mpr.clone());
        self.cpu.borrow_mut().configure_bus(cpu_bus);
        self
    }

    pub fn reset(&mut self) {
        while !self.cpu.borrow().waiting() {
            self.clock();
        }
        self.tcc = 0;
        self.cpu.borrow_mut().reset();
    }

    pub fn irq(&mut self) {
        while !self.cpu.borrow().waiting() {
            self.clock();
        }
        self.cpu.borrow_mut().irq();
    }

    pub fn nmi(&mut self) {
        while !self.cpu.borrow().waiting() {
            self.clock();
        }
        self.cpu.borrow_mut().nmi();
    }

    #[allow(unused)]
    pub fn run_pc(&mut self, addr: Word) -> NesState {
        if self.cpu.borrow().cur_pc() != addr {
            while !self.cpu.borrow().waiting() {
                self.clock();
            }
            self.cpu.borrow_mut().pc(addr);
        }
        if let Some(cc) = self.count_until_process(Process::Cpu) {
            for _ in 0..=cc {
                self.clock();
            }
            while !self.cpu.borrow().waiting() {
                self.clock();
            }
        }

        self.state.clone()
    }

    #[allow(unused)]
    pub fn run_until(&mut self, addr: Word) -> NesState {
        let mut pc = self.cpu.borrow().cur_pc();
        while pc != addr {
            self.clock();
            pc = self.cpu.borrow().cur_pc();
        }

        self.state.clone()
    }

    #[allow(unused)]
    pub fn complete_operation(&mut self) -> NesState {
        while !self.cpu.borrow().waiting() {
            self.clock();
        }

        self.state.clone()
    }

    #[allow(unused)]
    pub fn cur_state(&self) -> NesState {
        let cpu = self.cpu.borrow().cur_state();
        let ppu = self.ppu.borrow().cur_state();
        NesState {
            ppu,
            cpu,
            tcc: self.tcc,
        }
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

    #[allow(unused)]
    pub fn read(&self, addr: Word) -> Byte {
        self.cpu.borrow().read(addr)
    }

    #[allow(unused)]
    pub fn read_slice(&self, addr: Word, out: &mut [Byte]) {
        for (i, v) in out.iter_mut().enumerate() {
            *v = self.cpu.borrow().read(addr + i as Word)
        }
    }

    pub fn is_fetching_instr(&self) -> bool {
        self.cpu.borrow().waiting()
    }

    pub fn is_vblank(&self) -> bool {
        self.ppu.borrow().is_vblank()
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

impl Clocked for Nes {
    type Item = NesState;
    fn clock(&mut self) -> Option<Self::Item> {
        for p in self.seq.iter_mut() {
            if let Some(process) = p.next() {
                match process {
                    Process::Ppu => {
                        if let Some(state) = self.ppu.borrow_mut().clock() {
                            self.ppu_cur = state;
                        }
                    }
                    Process::Cpu => match self.cpu.borrow_mut().clock() {
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
