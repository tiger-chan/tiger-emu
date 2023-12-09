mod assembly;
mod gui;
mod thread_nes;
mod triple_buffer;

use std::error::Error;
use std::sync::{mpsc, Arc, RwLock};
use std::thread;
use triple_buffer::TripleBuffer;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::builder()
        .filter_module("nes", log::LevelFilter::Debug)
        .filter_module("emu", log::LevelFilter::Debug)
        .init();

    let ui_buffer = Arc::new(RwLock::new([0; 64]));
    let emu_buffer = ui_buffer.clone();
    let emu_handle: TripleBuffer<thread_nes::Buffer> = TripleBuffer::default();
    let ui_handle = emu_handle.clone();

    // Create channels for communication
    let (ui_sender, emulator_receiver) = mpsc::channel();
    let (emulator_sender, ui_receiver) = mpsc::channel();

    let console_thread = thread::spawn(move || {
        if let Err(err) =
            thread_nes::emu_thread(emulator_sender, emulator_receiver, emu_handle, emu_buffer)
        {
            log::error!("{}", err);
        }
    });

    thread_nes::ui_thread(ui_sender, ui_receiver, ui_handle, ui_buffer)?;

    console_thread.join().unwrap();

    Ok(())
}
