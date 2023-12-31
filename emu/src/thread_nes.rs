mod emu_thread;
mod ui_thread;

use nes::prelude::*;

pub const FRAME_TIME: f32 = 1.0 / 60.0;
pub const BUFFER_SIZE: usize = nes::HEIGHT as usize * nes::WIDTH as usize;

#[derive(Debug, Clone, Copy)]
pub struct Buffer(pub [Color; BUFFER_SIZE]);

impl Default for Buffer {
    fn default() -> Self {
        Self([Color::BLACK; BUFFER_SIZE])
    }
}

pub enum EmuQuery {
    CpuRegisters,
    CpuAsm(Word, Word),
    PpuPalette(Word, Word),
    PpuNametable(Word),
}

pub enum GuiResult {
    CpuRegister(cpu::InstructionState),
    CpuAsm(Vec<Byte>),
    PpuColorPalette(ppu::ColorPalette),
    PpuPalette(Word, Word, ppu::Palette),
    PpuNametable(Word, ppu::DebugNametable),
    PlayState(bool),
}

pub enum EmulatorMessage {
    Load(String),
    Play,
    Pause,
    Frame,
    Step,
    Reset,
    Irq,
    Nmi,
    Query(EmuQuery),
    Quit,
}

pub enum GuiMessage {
    Loaded,
    QueryResult(GuiResult),
}

pub use emu_thread::emu_thread;
pub use ui_thread::ui_thread;
