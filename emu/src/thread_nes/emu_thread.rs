use std::{
    path::Path,
    sync::{mpsc::*, RwLockWriteGuard},
    time::Instant,
};

use crate::{thread_nes::FRAME_TIME, triple_buffer::TripleBuffer};
use nes::{cart::Cartridge, io::DisplayDevice, prelude::*, DisplayClocked, HEIGHT, WIDTH};

use super::{Buffer, EmuQuery, EmulatorMessage, GuiMessage, GuiResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum EmulationStepMethod {
    None,
    Instruction,
    Frame,
    Standard,
}

#[derive(Debug)]
struct DisplayBuffer<'a> {
    buf: RwLockWriteGuard<'a, Buffer>,
}

impl<'a> DisplayBuffer<'a> {
    pub fn new(buf: RwLockWriteGuard<'a, Buffer>) -> Self {
        Self { buf }
    }
}

impl<'a> DisplayDevice for DisplayBuffer<'a> {
    fn write(&mut self, x: Word, y: Word, data: Color) {
        if x < WIDTH && y < HEIGHT {
            let idx = (y * WIDTH + x) as usize;
            self.buf.0[idx] = data;
        }
    }
}

pub fn emu_thread(
    sender: Sender<GuiMessage>,
    receiver: Receiver<EmulatorMessage>,
    mut frame_buffer: TripleBuffer<Buffer>,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let mut nes = Nes::default();

    let mut residual_time = 0.0;
    let mut prev_instant = Instant::now();

    let mut emu_processing = EmulationStepMethod::None;

    'emu_loop: loop {
        let last_emu_processing = emu_processing;
        let cur_instant = Instant::now();
        let mut delta_time = cur_instant.duration_since(prev_instant).as_secs_f32();
        prev_instant = cur_instant;

        let mut sent_registers = false;
        let mut sent_palettes = [false, false];
        while let Ok(msg) = receiver.try_recv() {
            match msg {
                EmulatorMessage::Load(cart_location) => {
                    emu_processing = EmulationStepMethod::None;
                    let path = Path::new(&cart_location);
                    match Cartridge::try_from(path) {
                        Ok(cart) => {
                            nes = Nes::default().with_cart(cart);
                            let _ = sender.send(GuiMessage::Loaded);
                        }
                        Err(err) => log::error!("{}", err),
                    }
                }
                EmulatorMessage::Play => {
                    emu_processing = EmulationStepMethod::Standard;
                }
                EmulatorMessage::Pause => {
                    emu_processing = EmulationStepMethod::None;
                }
                EmulatorMessage::Frame => {
                    emu_processing = EmulationStepMethod::Frame;
                }
                EmulatorMessage::Step => {
                    emu_processing = EmulationStepMethod::Instruction;
                }
                EmulatorMessage::Quit => {
                    log::warn!("Quiting EMU thread");
                    break 'emu_loop;
                }
                EmulatorMessage::Query(query) => match query {
                    EmuQuery::CpuRegisters => {
                        if !sent_registers {
                            sent_registers = true;
                            let state = nes.cur_state().cpu;
                            let msg = GuiResult::CpuRegister(state);
                            let _ = sender.send(GuiMessage::QueryResult(msg));
                        }
                    }
                    EmuQuery::PpuPalette(idx, palette) => {
                        let pal = (idx & 0x01) as usize;
                        if !sent_palettes[pal] {
                            sent_palettes[pal] = true;
                            let data = nes.read_palette(idx, palette);
                            let msg = GuiResult::PpuPalette(idx, palette, data);
                            let _ = sender.send(GuiMessage::QueryResult(msg));
                        }
                    }
                    EmuQuery::CpuAsm(start, end) => {
                        let mut data = vec![0; (end - start) as usize];
                        nes.read_only_slice(start, data.as_mut_slice());

                        let msg = GuiResult::CpuAsm(data);
                        let _ = sender.send(GuiMessage::QueryResult(msg));
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

        {
            let mut display = DisplayBuffer::new(frame_buffer.back_mut().write().unwrap());

            match emu_processing {
                EmulationStepMethod::None => {
                    // Do Nothing no processing reset timers
                    residual_time = 0.0;
                }
                EmulationStepMethod::Instruction => {
                    // Run one Cpu instruction
                    let frame = Instant::now();

                    while nes.is_fetching_instr() {
                        nes.clock(&mut display);
                    }

                    while !nes.is_fetching_instr() {
                        nes.clock(&mut display);
                    }

                    let end_frame = Instant::now();
                    let dur = end_frame.duration_since(frame).as_secs_f32();
                    log::trace!("Instruction request {dur}");

                    emu_processing = EmulationStepMethod::None;
                }
                EmulationStepMethod::Frame => {
                    // Run one v-blank period
                    let frame = Instant::now();

                    while nes.is_vblank() {
                        nes.clock(&mut display);
                    }

                    while !nes.is_vblank() {
                        nes.clock(&mut display);
                    }

                    let end_frame = Instant::now();
                    let dur = end_frame.duration_since(frame).as_secs_f32();
                    log::trace!("Frame request {dur}");

                    emu_processing = EmulationStepMethod::None;
                }
                EmulationStepMethod::Standard => {
                    if residual_time > 0.0 {
                        residual_time -= delta_time;
                    } else {
                        if FRAME_TIME < delta_time {
                            // Just ignore the time
                            log::warn!("Delta time is too large {delta_time}");
                            delta_time = FRAME_TIME;
                        }

                        residual_time += FRAME_TIME - delta_time;

                        let frame = Instant::now();

                        let mut count = 0;

                        while nes.is_vblank() {
                            count += 1;
                            nes.clock(&mut display);
                        }

                        while !nes.is_vblank() {
                            count += 1;
                            nes.clock(&mut display);
                        }

                        let end_frame = Instant::now();
                        let dur = end_frame.duration_since(frame).as_secs_f32();
                        log::trace!("Standard running took {dur} and clocked: {count}");
                    }
                }
            }
        }

        if emu_processing != last_emu_processing {
            let is_running = emu_processing == EmulationStepMethod::Standard;
            let _ = sender.send(GuiMessage::QueryResult(GuiResult::PlayState(is_running)));
        }

        if emu_processing != EmulationStepMethod::None && nes.is_vblank() {
            frame_buffer.submit();
        }
    }

    Ok(())
}
