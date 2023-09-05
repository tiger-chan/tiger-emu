mod bus;
mod cpu;
mod gui;
mod nes;

use crate::gui::Framework;
use crate::{cpu::CPU, nes::RAM};
use error_iter::ErrorIter;
use gui::DebugCpu;
use log::error;
use pixels::{Error, Pixels, SurfaceTexture};
use std::{cell::RefCell, rc::Rc};
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode};
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

    let bus = Rc::new(RefCell::new(nes::Bus6502::new()));
    let cpu = Rc::new(RefCell::new(nes::Cpu6502::new()));

    bus.borrow_mut().connect_cpu(&cpu);
    cpu.borrow_mut().connect_bus(&bus);

    // Load Program (assembled at https://www.masswerk.at/6502/assembler.html)
    /*
        *=$8000
        LDX #10
        STX $0000
        LDX #3
        STX $0001
        LDY $0000
        LDA #0
        CLC
        loop
        ADC $0001
        DEY
        BNE loop
        STA $0002
        NOP
        NOP
        NOP
    */

    // Convert hex string into bytes for RAM
    let mut ram: [u8; RAM] = [0; RAM];
    let rom: Vec<u8> =
        "A2 0A 8E 00 00 A2 03 8E 01 00 AC 00 00 A9 00 18 6D 01 00 88 D0 FA 8D 02 00 EA EA EA"
            .split(" ")
            .map(|x| u8::from_str_radix(x, 16).expect("Invalid value for hex"))
            .collect();

    for (i, v) in rom.iter().enumerate() {
        ram[0x8000 + i] = *v;
    }

    // Set Reset Vector
    ram[0xFFFC] = 0x00;
    ram[0xFFFD] = 0x80;

    bus.borrow_mut().set_ram(&ram);

    cpu.borrow_mut().disssemble(0x0000, 0xFFFF);

    // Reset now that we've updated the ram
    cpu.borrow_mut().reset();

    event_loop.run(move |event, _, control_flow| {
        // Handle input events
        if input.update(&event) {
            // Close events
            if input.key_pressed(VirtualKeyCode::Escape) || input.close_requested() {
                *control_flow = ControlFlow::Exit;
                return;
            }

            if input.key_pressed(VirtualKeyCode::Space) {
                cpu.borrow_mut().step();
            }

            if input.key_pressed(VirtualKeyCode::R) {
                cpu.borrow_mut().reset();
            }

            if input.key_pressed(VirtualKeyCode::I) {
                cpu.borrow_mut().irq();
            }

            if input.key_pressed(VirtualKeyCode::N) {
                cpu.borrow_mut().nmi();
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
                let bus = &*bus.borrow();
                let cpu = &*cpu.borrow();
                framework.prepare(&window, bus, cpu);

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
