use egui::RichText;

use super::Cpu6502;
use crate::bus::Bus;
use crate::gui::{DebugBus, DIAGNOSTIC_FONT};
use std::cell::RefCell;
use std::rc::{Rc, Weak};
pub const RAM: usize = 64 * 1024;
//pub const RAM: usize = 0x0800;
//const MAX_ADDR: u16 = 0xFFFF;

type Cpu = RefCell<Cpu6502>;

pub struct Bus6502 {
    ram: [u8; RAM],
    cpu: Option<Weak<Cpu>>,
}

impl Bus6502 {
    pub fn new() -> Self {
        Bus6502 {
            cpu: None,
            ram: [0; RAM],
        }
    }

    pub fn connect_cpu(&mut self, cpu: &Rc<Cpu>) {
        self.cpu = Some(Rc::downgrade(&cpu));
    }

    pub fn set_ram(&mut self, ram: &[u8; RAM]) {
        self.ram = *ram;
    }
}

impl Bus for Bus6502 {
    fn read(&self, addr: u16) -> u8 {
        self.ram[addr as usize]
    }

    fn read_only(&self, addr: u16) -> u8 {
        self.ram[addr as usize]
    }

    fn write(&mut self, addr: u16, data: u8) {
        self.ram[addr as usize] = data;
    }
}

impl DebugBus for Bus6502 {
    fn draw_mem(&self, ui: &mut egui::Ui, addr: u16, rows: u8, cols: u8) {
        ui.vertical(|ui| {
            let mem_block: Vec<u8> = self
                .ram
                .iter()
                .skip(addr as usize)
                .take(rows as usize * cols as usize)
                .map(|x| *x)
                .collect();
            for (i, chunk) in mem_block.chunks(cols as usize).enumerate() {
                let mut str = String::with_capacity(cols as usize * 3 + 6);
                str.push_str(format!("{:>04X} ", addr + (i as u16 * cols as u16)).as_str());
                for mem in chunk {
                    str.push_str(format!("{:>02X} ", mem).as_str());
                }

                ui.label(RichText::new(str).font(DIAGNOSTIC_FONT));
            }
        });
    }
}
