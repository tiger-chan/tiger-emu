use std::{path::Path, sync::mpsc::*, time::Instant};

use crate::{thread_nes::FRAME_TIME, triple_buffer::TripleBuffer};
use nes::{cart::Cartridge, prelude::*, HEIGHT, WIDTH};

use super::{Buffer, EmuQuery, EmulatorMessage, GuiMessage, GuiResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum InstructionFlow {
    Waiting,
    Start,
    Finish,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum FrameFlow {
    Waiting,
    Start,
    Finish,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum EmulationStepMethod {
    None,
    Instruction(InstructionFlow),
    Frame(FrameFlow),
    Standard,
}

pub fn emu_thread(
    sender: Sender<GuiMessage>,
    receiver: Receiver<EmulatorMessage>,
    mut frame_buffer: TripleBuffer<Buffer>,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let mut nes = Nes::default();

    let mut cycles: u64 = 0;
    let mut residual_time = 0.0;
    let mut prev_instant = Instant::now();

    let mut color_time = 0.0;
    let mut emu_processing = EmulationStepMethod::None;

    'emu_loop: loop {
        let cur_instant = Instant::now();
        let mut delta_time = cur_instant.duration_since(prev_instant).as_secs_f32();
        prev_instant = cur_instant;

        let mut sent_registers = false;
        while let Ok(msg) = receiver.try_recv() {
            match msg {
                EmulatorMessage::Load(cart_location) => {
                    emu_processing = EmulationStepMethod::None;
                    let path = Path::new(&cart_location);
                    let cart = Cartridge::try_from(path)?;
                    nes = Nes::default().with_cart(cart);
                }
                EmulatorMessage::Play => {
                    emu_processing = EmulationStepMethod::Standard;
                }
                EmulatorMessage::Pause => {
                    emu_processing = EmulationStepMethod::None;
                }
                EmulatorMessage::Frame => {
                    let step = if nes.is_vblank() {
                        FrameFlow::Start
                    } else {
                        FrameFlow::Waiting
                    };

                    emu_processing = EmulationStepMethod::Frame(step);
                }
                EmulatorMessage::Step => {
                    let step = if nes.is_fetching_instr() {
                        InstructionFlow::Start
                    } else {
                        InstructionFlow::Waiting
                    };

                    emu_processing = EmulationStepMethod::Instruction(step);
                }
                EmulatorMessage::Quit => {
                    log::warn!("Quiting EMU thread");
                    break 'emu_loop;
                }
                EmulatorMessage::Query(query) => match query {
                    EmuQuery::CpuRegisters => {
                        if !sent_registers {
                            let state = nes.cur_state().cpu;
                            let msg = GuiResult::CpuRegister(state);
                            let _ = sender.send(GuiMessage::QueryResult(msg));
                        }
                        sent_registers = true;
                    }
                },
                EmulatorMessage::Irq => {
                    nes.irq();
                }
                EmulatorMessage::Nmi => {
                    nes.nmi();
                }
                EmulatorMessage::Reset => {
                    nes.reset();
                }
            }
        }

        let is_running = emu_processing == EmulationStepMethod::Standard;
        let _ = sender.send(GuiMessage::QueryResult(GuiResult::PlayState(is_running)));
        match emu_processing {
            EmulationStepMethod::None => {
                // Do Nothing no processing reset timers
                residual_time = 0.0;
            }
            EmulationStepMethod::Instruction(state) => {
                // Run one Cpu instruction
                if state == InstructionFlow::Finish {
                    let was_fetch = nes.is_fetching_instr();
                    if was_fetch {
                        emu_processing = EmulationStepMethod::None;
                    }
                } else {
                    let was_fetch = nes.is_fetching_instr();
                    nes.clock();
                    if was_fetch && was_fetch != nes.is_fetching_instr() {
                        match state {
                            InstructionFlow::Waiting => {
                                emu_processing =
                                    EmulationStepMethod::Instruction(InstructionFlow::Start);
                            }
                            InstructionFlow::Start => {
                                emu_processing =
                                    EmulationStepMethod::Instruction(InstructionFlow::Finish);
                            }
                            _ => {}
                        }
                    }
                }
            }
            EmulationStepMethod::Frame(state) => {
                // Run one v-blank period
                if state == FrameFlow::Finish {
                    let was_fetch = nes.is_vblank();
                    if was_fetch {
                        emu_processing = EmulationStepMethod::None;
                    }
                } else {
                    let was_fetch = nes.is_vblank();
                    nes.clock();
                    if was_fetch && was_fetch != nes.is_vblank() {
                        match state {
                            FrameFlow::Waiting => {
                                emu_processing = EmulationStepMethod::Frame(FrameFlow::Start);
                            }
                            FrameFlow::Start => {
                                emu_processing = EmulationStepMethod::Frame(FrameFlow::Finish);
                            }
                            _ => {}
                        }
                    }
                }
            }
            EmulationStepMethod::Standard => {
                if residual_time > 0.0 {
                    residual_time -= delta_time;
                } else {
                    if FRAME_TIME < delta_time {
                        log::warn!("Delta time is too large {delta_time}");
                        // Just ignore the time
                        delta_time = FRAME_TIME;
                    }

                    residual_time += FRAME_TIME - delta_time;

                    color_time += FRAME_TIME;
                    let t = (color_time % 5.0) / 5.0;
                    let r = (t.sin() * 127.0 + 128.0) as u8;
                    let g = ((t + 2.0 / 3.0).sin() * 127.0 + 128.0) as u8;
                    let b = ((t + 4.0 / 3.0).sin() * 127.0 + 128.0) as u8;

                    let frame = Instant::now();

                    nes.clock();

                    if let Ok(mut bck) = frame_buffer.back_mut().write() {
                        for y in 0..HEIGHT as usize {
                            let y_idx = y * WIDTH as usize;
                            for x in 0..WIDTH as usize {
                                let idx = (x + y_idx) * 3;
                                bck.0[idx] = r;
                                bck.0[idx + 1] = g;
                                bck.0[idx + 2] = b;
                            }
                        }
                    }
                    frame_buffer.submit();
                    let end_frame = Instant::now();
                    let dur = end_frame.duration_since(frame).as_secs_f32();
                    log::trace!("Frame took {dur}");
                }
            }
        }

        cycles = cycles.wrapping_add(1);
    }

    Ok(())
}
