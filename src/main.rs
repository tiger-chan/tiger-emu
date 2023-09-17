mod bus;
mod cartridge;
mod cpu;
mod gui;
mod motherboard;
mod nes;
mod ppu_bus;

use std::path::Path;

use crate::gui::Framework;
//use crate::nes::{RAM, RES_HI, RES_LO};
use error_iter::ErrorIter;
use gui::BoardCommand;
use log::error;
use motherboard::Motherboard;
use nes::{Board, Cartridge};
use pixels::{Error, Pixels, SurfaceTexture};
use winit::dpi::LogicalSize;
use winit::event::{Event, ModifiersState, VirtualKeyCode, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;
use winit_input_helper::WinitInputHelper;

const WIDTH: u32 = 1024;
const HEIGHT: u32 = 768;

fn main() -> Result<(), Error> {
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
        let pixels = Pixels::new(WIDTH, HEIGHT, surface_texture)?;
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
        },
        Err(_) => {
            panic!("Couldn't load rom file.");
        }
    }

    event_loop.run(move |event, _, control_flow| {
        // Handle input events
        if input.update(&event) {
            // Close events
            if input.key_pressed(VirtualKeyCode::Escape) || input.close_requested() {
                *control_flow = ControlFlow::Exit;
                return;
            }

            let mut modifiers: Option<ModifiersState> = None;

            match &event {
                Event::WindowEvent { event, .. } => match &event {
                    WindowEvent::ModifiersChanged(new) => {
                        modifiers = Some(*new);
                    }
                    _ => {}
                },
                _ => (),
            }

            if input.key_pressed(VirtualKeyCode::F10) {
                board.step();
            }

            if input.key_pressed(VirtualKeyCode::F11) {
                board.frame();
            }

            match modifiers {
                Some(mods) => {
                    if mods.ctrl() && mods.shift() && input.key_pressed(VirtualKeyCode::F5) {
                        board.reset();
                    }

                    if mods.ctrl() && mods.shift() && input.key_pressed(VirtualKeyCode::I) {
                        board.irq();
                    }

                    if mods.ctrl() && mods.shift() && input.key_pressed(VirtualKeyCode::N) {
                        board.nmi();
                    }
                }
                _ => {}
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
                //world.draw(pixels.frame_mut());

                // Prepare egui
                framework.prepare(&window, &mut board);

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
    });
}

fn log_error<E: std::error::Error + 'static>(method_name: &str, err: E) {
    error!("{method_name}() failed: {err}");
    for source in err.sources().skip(1) {
        error!("  Caused by: {source}");
    }
}
