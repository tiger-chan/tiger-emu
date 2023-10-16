use nes::prelude::*;
use std::sync::mpsc;
use std::thread;

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

fn main() {
    env_logger::builder()
        .filter_module("emu", log::LevelFilter::Debug)
        .init();

    // Create channels for communication
    let (ui_sender, emulator_receiver) = mpsc::channel();
    let (emulator_sender, ui_receiver) = mpsc::channel();

    let console_thread = thread::spawn(move || {
        let mut nes = Nes::default();

        while let Ok(msg) = emulator_receiver.try_recv() {
            match msg {
                EmulatorMessage::Step => {
                    nes.next();
                }
                EmulatorMessage::Quit => {
                    log::warn!("Quiting EMU thread");
                    break;
                }
                EmulatorMessage::Query(query) => match query {
                    EmuQuery::CpuRegisters => {
                        let state = nes.cur_state().cpu;
                        let msg = GuiResult::CpuRegister(state);
                        let _ = emulator_sender.send(GuiMessage::QueryResult(msg));
                    }
                },
            }
        }
    });

    let ui_thread = thread::spawn(move || {
        let mut countdown = 5000;
        loop {
            if let Ok(msg) = ui_receiver.try_recv() {
                match msg {
                    GuiMessage::QueryResult(msg) => match msg {
                        GuiResult::CpuRegister(_reg) => {}
                    },
                }
            }

            countdown -= 1;
            if countdown == 0 {
                log::warn!("Quiting UI thread");
                ui_sender.send(EmulatorMessage::Quit).unwrap();
                break;
            } else {
                //ui_sender.send(EmulatorMessage::Step).unwrap();
                ui_sender
                    .send(EmulatorMessage::Query(EmuQuery::CpuRegisters))
                    .unwrap();
            }
        }
    });

    console_thread.join().unwrap();
    ui_thread.join().unwrap();
}
