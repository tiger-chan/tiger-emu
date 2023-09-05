mod framework;
mod gui;

use egui::Ui;
pub(crate) use framework::Framework;
pub(crate) use gui::{Gui, CURSOR, DIAGNOSTIC_FONT, DISABLED, ENABLED};

pub(crate) trait DebugBus {
    fn draw_mem(&self, ui: &mut Ui, addr: u16, rows: u8, cols: u8);
}

pub(crate) trait DebugCpu {
    fn draw_cpu(&self, ui: &mut Ui);
    fn draw_code(&self, ui: &mut Ui, instruction_count: i8);
    fn step(&mut self);
}
