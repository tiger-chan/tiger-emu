use std::sync::mpsc::{self, Receiver, Sender};
use std::time::Instant;

use error_iter::ErrorIter;

use pixels::{Error, Pixels, SurfaceTexture};

use winit::dpi::LogicalSize;
use winit::event_loop::ControlFlow;
use winit::window::WindowBuilder;
use winit::{
    event::{Event, VirtualKeyCode},
    event_loop::EventLoop,
};
use winit_input_helper::WinitInputHelper;

use super::{Buffer, EmulatorMessage, GuiMessage, GuiResult};
use crate::gui::{Framework, Message};
use crate::thread_nes::{EmuQuery, FRAME_TIME};
use crate::triple_buffer::TripleBuffer;

const WIDTH: u32 = 1024;
const HEIGHT: u32 = 768;
const NES_WIDTH: u32 = 256;
const NES_HEIGHT: u32 = 240;

pub fn ui_thread(
    sender: Sender<EmulatorMessage>,
    receiver: Receiver<GuiMessage>,
    mut frame_buffer: TripleBuffer<Buffer>,
) -> Result<(), Error> {
    let mut input = WinitInputHelper::new();
    let evt_loop = EventLoop::default();

    let window = {
        let size = LogicalSize::new(WIDTH as f64, HEIGHT as f64);
        WindowBuilder::default()
            .with_active(true)
            .with_title("Nes-quick")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .with_decorations(true)
            .build(&evt_loop)
            .unwrap()
    };

    let (mut pixels, mut framework) = {
        let window_size = window.inner_size();
        let scale_factor = window.scale_factor() as f32;
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        let pixels = Pixels::new(NES_WIDTH, NES_HEIGHT, surface_texture)?;

        let framework = Framework::new(
            &evt_loop,
            window_size.width,
            window_size.height,
            scale_factor,
            &pixels,
        );

        (pixels, framework)
    };

    let mut residual_time = 0.0;
    let mut prev_instant = Instant::now();
    let mut is_running = false;

    evt_loop.run(move |event, _, control_flow| {
        let (gui_sender, gui_receiver) = mpsc::channel();
        let cur_instant = Instant::now();
        let mut delta_time = cur_instant.duration_since(prev_instant).as_secs_f32();
        prev_instant = cur_instant;
        while let Ok(msg) = receiver.try_recv() {
            match msg {
                GuiMessage::QueryResult(msg) => match msg {
                    GuiResult::CpuRegister(_reg) => {}
                    GuiResult::PlayState(state) => {
                        is_running = state;
                    }
                },
            }
        }

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

            let _ = sender.send(EmulatorMessage::Query(EmuQuery::CpuRegisters));

            if let Ok(front) = frame_buffer.front().read() {
                let pixels = pixels.frame_mut();
                for (idx, p) in pixels.chunks_exact_mut(4).enumerate() {
                    let color = front.0[idx].to_array();
                    p.copy_from_slice(&color);
                }
            }
            frame_buffer.flip();
            let end_frame = Instant::now();
            let dur = end_frame.duration_since(frame).as_secs_f32();
            log::trace!("Frame took {dur}");
        }

        if residual_time < -1.0 {
            residual_time = 0.0;
            return;
        }

        // Handle input events
        if input.update(&event) {
            // Close events
            if input.key_pressed(VirtualKeyCode::Escape) || input.close_requested() {
                *control_flow = ControlFlow::Exit;

                log::warn!("Quiting UI thread");
                let _ = sender.send(EmulatorMessage::Quit);
                return;
            }

            // Update the scale factor
            if let Some(scale_factor) = input.scale_factor() {
                framework.scale_factor(scale_factor);
            }

            // Resize the window
            if let Some(size) = input.window_resized() {
                if let Err(err) = pixels.resize_surface(size.width, size.height) {
                    log_error("pixels.resize_surface", err);
                    *control_flow = ControlFlow::Exit;

                    log::warn!("Quiting UI thread");
                    let _ = sender.send(EmulatorMessage::Quit);
                    return;
                }
                framework.resize(size.width, size.height);
            }

            // Update internal state and request a redraw
            //world.update();
            window.request_redraw();
        }

        match event {
            Event::WindowEvent { event, .. } => {
                let _ = event;
                // Update egui inputs
                framework.handle_event(&event);
            }
            // Draw the current frame
            Event::RedrawRequested(_) => {
                // // Draw the world
                // board.draw(pixels.frame_mut());

                // Prepare egui
                framework.prepare(&window, &gui_sender);
                while let Ok(msg) = gui_receiver.try_recv() {
                    match msg {
                        Message::Frame => {
                            sender.send(EmulatorMessage::Frame).unwrap();
                        }
                        Message::Irq => {
                            sender.send(EmulatorMessage::Irq).unwrap();
                        }
                        Message::Load(cart) => {
                            sender.send(EmulatorMessage::Load(cart)).unwrap();
                        }
                        Message::Nmi => {
                            sender.send(EmulatorMessage::Nmi).unwrap();
                        }
                        Message::Play => {
                            sender.send(EmulatorMessage::Play).unwrap();
                        }
                        Message::PlayPause => {
                            let action = if is_running {
                                EmulatorMessage::Pause
                            } else {
                                EmulatorMessage::Play
                            };
                            sender.send(action).unwrap();
                        }
                        Message::Reset => {
                            sender.send(EmulatorMessage::Reset).unwrap();
                        }
                        Message::Step => {
                            sender.send(EmulatorMessage::Step).unwrap();
                        }
                    }
                }

                // Render everything together
                let render_result = pixels.render_with(|encoder, render_target, context| {
                    // Render the world texture
                    context.scaling_renderer.render(encoder, render_target);

                    // // Render egui
                    framework.render(encoder, render_target, context);

                    Ok(())
                });

                // Basic error handling
                if let Err(err) = render_result {
                    log_error("pixels.render", err);
                    *control_flow = ControlFlow::Exit;
                }
            }
            _ => (),
        }
    });
}

fn log_error<E: std::error::Error + 'static>(method_name: &str, err: E) {
    log::error!("{method_name}() failed: {err}");
    for source in err.sources().skip(1) {
        log::error!("  Caused by: {source}");
    }
}
