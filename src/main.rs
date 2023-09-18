mod bus;
mod cartridge;
mod cpu;
mod gui;
mod motherboard;
mod nes;
mod ppu_bus;

use std::path::Path;
use std::time::Instant;

use crate::gui::Framework;
//use crate::nes::{RAM, RES_HI, RES_LO};
use error_iter::ErrorIter;
use gui::BoardCommand;
use log::error;
use motherboard::Motherboard;
use nes::{Board, Cartridge};
use pixels::{Error, Pixels, SurfaceTexture};
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;
use winit_input_helper::WinitInputHelper;

const WIDTH: u32 = 1024;
const HEIGHT: u32 = 768;
const NES_WIDTH: u32 = 256;
const NES_HEIGHT: u32 = 240;

fn main() -> Result<(), Error> {
    const FRAME_TIME: f32 = 1.0 / 60.0;
    env_logger::builder()
        .filter_module("nes_ultra", log::LevelFilter::Debug)
        .init();

    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();
    let window = {
        let size = LogicalSize::new(WIDTH as f64, HEIGHT as f64);
        WindowBuilder::new()
            .with_title("NES Ultra")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .unwrap()
    };

    let (mut pixels, mut framework) = {
        let window_size = window.inner_size();
        let scale_factor = window.scale_factor() as f32;
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        let pixels = Pixels::new(NES_WIDTH, NES_HEIGHT, surface_texture)?;
        let framework = Framework::new(
            &event_loop,
            window_size.width,
            window_size.height,
            scale_factor,
            &pixels,
        );

        (pixels, framework)
    };

    let mut board = Board::new();

    let rom = Cartridge::new_from_file(Path::new("roms/nestest.nes"));
    match rom {
        Ok(rom) => {
            board.load_cart(rom);
            board.reset();
        }
        Err(_) => {
            panic!("Couldn't load rom file.");
        }
    }

    let mut run_emulation = false;
    let mut residual_time = 0.0;
    let mut prev_instant = Instant::now();
    event_loop.run(move |event, _, control_flow| {
        let cur_instant = Instant::now();
        let mut delta_time = cur_instant.duration_since(prev_instant).as_secs_f32();
        prev_instant = cur_instant;
        // Handle input events
        if input.update(&event) {
            // Close events
            if input.key_pressed(VirtualKeyCode::Escape) || input.close_requested() {
                *control_flow = ControlFlow::Exit;
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
                // Update egui inputs
                framework.handle_event(&event);
            }
            // Draw the current frame
            Event::RedrawRequested(_) => {
                // Draw the world
                board.draw(pixels.frame_mut());

                // Prepare egui
                framework.prepare(&window, &mut run_emulation, &mut board);

                // Render everything together
                let render_result = pixels.render_with(|encoder, render_target, context| {
                    // Render the world texture
                    context.scaling_renderer.render(encoder, render_target);

                    // Render egui
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
                board.frame();
                let end_frame = Instant::now();
                let dur = end_frame.duration_since(frame).as_secs_f32();
                log::trace!("Frame took {dur}");
            }

            if residual_time < -1.0 {
                *control_flow = ControlFlow::Exit;
                return;
            }
        } else {
            residual_time = 0.0
        }
    });
}

fn log_error<E: std::error::Error + 'static>(method_name: &str, err: E) {
    error!("{method_name}() failed: {err}");
    for source in err.sources().skip(1) {
        error!("  Caused by: {source}");
    }
}
