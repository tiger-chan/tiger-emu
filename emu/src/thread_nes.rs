mod emu_thread;
mod ui_thread;

use nes::prelude::*;

pub const FRAME_TIME: f32 = 1.0 / 60.0;
pub const BUFFER_SIZE: usize = nes::HEIGHT as usize * nes::WIDTH as usize * 3;

#[derive(Clone, Copy)]
pub struct Buffer(pub [u8; BUFFER_SIZE]);

impl Default for Buffer {
    fn default() -> Self {
        Self([0; BUFFER_SIZE])
    }
}

pub enum EmuQuery {
    CpuRegisters,
}

pub enum GuiResult {
    CpuRegister(cpu::InstructionState),
}

pub enum EmulatorMessage {
    Step,
    Query(EmuQuery),
    Quit,
}

pub enum GuiMessage {
    QueryResult(GuiResult),
}

pub use emu_thread::emu_thread;
pub use ui_thread::ui_thread;
