use std::{sync::mpsc::*, time::Instant};

use crate::{thread_nes::FRAME_TIME, triple_buffer::TripleBuffer};
use nes::{prelude::*, HEIGHT, WIDTH};

use super::{Buffer, EmuQuery, EmulatorMessage, GuiMessage, GuiResult};

pub fn emu_thread(
    sender: Sender<GuiMessage>,
    receiver: Receiver<EmulatorMessage>,
    mut frame_buffer: TripleBuffer<Buffer>,
) {
    let mut nes = Nes::default();

    let mut cycles: u64 = 0;
    let mut run_emulation: bool = true;

    let mut residual_time = 0.0;
    let mut prev_instant = Instant::now();

    let mut sent_registers = false;
    'emu_loop: loop {
        let cur_instant = Instant::now();
        let mut delta_time = cur_instant.duration_since(prev_instant).as_secs_f32();
        prev_instant = cur_instant;

        if let Ok(msg) = receiver.try_recv() {
            match msg {
                EmulatorMessage::Step => {
                    nes.next();
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
            }
        }

        if run_emulation {
            if residual_time > 0.0 {
                residual_time -= delta_time;
            } else {
                if FRAME_TIME < delta_time {
                    log::warn!("Delta time is too large {delta_time}");
                    // Just ignore the time
                    delta_time = FRAME_TIME;
                }

                residual_time += FRAME_TIME - delta_time;

                let frame = Instant::now();
                if let Ok(mut bck) = frame_buffer.back_mut().write() {
                    for y in 0..HEIGHT as usize {
                        let y_idx = y * WIDTH as usize;
                        for x in 0..WIDTH as usize {
                            let idx = (x + y_idx) * 3;
                            bck.0[idx] = cycles as u8;
                            bck.0[idx + 1] = cycles as u8;
                            bck.0[idx + 2] = cycles as u8;
                        }
                    }
                }
                frame_buffer.submit();
                let end_frame = Instant::now();
                let dur = end_frame.duration_since(frame).as_secs_f32();
                log::trace!("Frame took {dur}");
            }
        } else {
            residual_time = 0.0
        }

        cycles = cycles.wrapping_add(1);
    }
}
