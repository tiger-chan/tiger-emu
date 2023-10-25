use std::{ops::Add, sync::mpsc::Sender};

use egui::{Color32, Context, FontId, Key, Modifiers, RichText, Ui};

use super::Message;
use nes::prelude::*;

pub const ENABLED: Color32 = Color32::GREEN;
pub const DISABLED: Color32 = Color32::RED;
//pub const CURSOR: Color32 = Color32::LIGHT_BLUE;
pub const DIAGNOSTIC_FONT: FontId = FontId::monospace(12.0);

trait DrawCpuStatus {
    fn draw(&self, ui: &mut Ui);
}

impl DrawCpuStatus for cpu::Registers {
    fn draw(&self, ui: &mut Ui) {
        ui.vertical(|ui| {
            ui.horizontal(|ui| {
                let f = |f| match self.p.get(f) == 0 {
                    true => DISABLED,
                    false => ENABLED,
                };

                ui.label(RichText::new("Status: ").font(DIAGNOSTIC_FONT));
                ui.label(
                    RichText::new("N")
                        .color(f(cpu::Status::N))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new("V")
                        .color(f(cpu::Status::V))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new("U")
                        .color(f(cpu::Status::U))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new("B")
                        .color(f(cpu::Status::B))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new("D")
                        .color(f(cpu::Status::D))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new("I")
                        .color(f(cpu::Status::I))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new("Z")
                        .color(f(cpu::Status::Z))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new("C")
                        .color(f(cpu::Status::C))
                        .font(DIAGNOSTIC_FONT),
                );
            });

            ui.vertical(|ui| {
                ui.label(
                    RichText::new(format!("PC: {:>04X} [{:>5}]", self.pc, self.pc))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new(format!("AC: {:>04X} [{:>5}]", self.ac, self.ac))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new(format!(" X: {:>04X} [{:>5}]", self.x, self.x))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new(format!(" Y: {:>04X} [{:>5}]", self.y, self.y))
                        .font(DIAGNOSTIC_FONT),
                );
                ui.label(
                    RichText::new(format!("SP: {:>04X} [{:>5}]", self.sp, self.sp))
                        .font(DIAGNOSTIC_FONT),
                );
            });
        });
    }
}

#[derive(Default, Debug)]
pub struct MainGui {
    cpu_status: bool,
    cpu_memory: bool,
    cpu_memory_page: u16,
    cpu_instructions: bool,

    ppu_palette_tbl: bool,
    ppu_palette_idx: u8,

    cpu_state: cpu::InstructionState,
}

impl MainGui {
    pub fn prepare(&mut self, ctx: &Context, sender: &Sender<Message>) {
        // General hotkeys
        {
            if ctx.input(|i| i.key_pressed(Key::F5)) {
                sender.send(Message::PlayPause).unwrap();
            }

            if ctx.input(|i| i.key_pressed(Key::F10)) {
                sender.send(Message::Step).unwrap();
            }

            if ctx.input(|i| i.key_pressed(Key::F10) && i.modifiers.matches(Modifiers::CTRL)) {
                sender.send(Message::Frame).unwrap();
            }

            if ctx.input(|i| {
                i.key_pressed(Key::F5) && i.modifiers.matches(Modifiers::CTRL | Modifiers::SHIFT)
            }) {
                sender.send(Message::Reset).unwrap();
            }

            if ctx.input(|i| {
                i.key_pressed(Key::I) && i.modifiers.matches(Modifiers::CTRL | Modifiers::SHIFT)
            }) {
                sender.send(Message::Irq).unwrap();
            }

            if ctx.input(|i| {
                i.key_pressed(Key::N) && i.modifiers.matches(Modifiers::CTRL | Modifiers::SHIFT)
            }) {
                sender.send(Message::Nmi).unwrap();
            }
        }

        egui::TopBottomPanel::top("menubar_container").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Open File...").clicked() {
                        if let Some(path) = rfd::FileDialog::new().pick_file() {
                            sender
                                .send(Message::Load(path.display().to_string()))
                                .unwrap();
                        }
                        ui.close_menu();
                    }
                });

                ui.menu_button("Diagnostics", |ui| {
                    ui.menu_button("CPU", |ui| {
                        if ui.button("Registers").clicked() {
                            self.cpu_status = !self.cpu_status;
                            ui.close_menu();
                        }

                        if ui.button("Memory").clicked() {
                            self.cpu_memory = !self.cpu_memory;
                            ui.close_menu();
                        }

                        if ui.button("Instructions").clicked() {
                            self.cpu_instructions = !self.cpu_instructions;
                            ui.close_menu();
                        }
                    });

                    if ui.button("PPU").clicked() {
                        self.ppu_palette_tbl = !self.ppu_palette_tbl;
                        ui.close_menu();
                    }

                    ui.separator();

                    if ui.button("Play (F5)").clicked() {
                        sender.send(Message::Play).unwrap();
                        ui.close_menu();
                    }

                    if ui.button("Step (F10)").clicked() {
                        sender.send(Message::Step).unwrap();
                        ui.close_menu();
                    }

                    if ui.button("Step Frame (CTRL+F10)").clicked() {
                        sender.send(Message::Frame).unwrap();
                        ui.close_menu();
                    }

                    if ui.button("Reset (CTRL + SHIFT + F5)").clicked() {
                        sender.send(Message::Reset).unwrap();
                        ui.close_menu();
                    }

                    if ui.button("IRQ (CTRL + SHIFT + I)").clicked() {
                        sender.send(Message::Irq).unwrap();
                        ui.close_menu();
                    }

                    if ui.button("NMI (CTRL + SHIFT + N)").clicked() {
                        sender.send(Message::Nmi).unwrap();
                        ui.close_menu();
                    }
                })
            });
        });

        // CPU diagnostic views
        {
            egui::Window::new("CPU: Status")
                .open(&mut self.cpu_status)
                .show(ctx, |ui| {
                    self.cpu_state.reg.draw(ui);
                });

            egui::Window::new("Instructions")
                .open(&mut self.cpu_instructions)
                .show(ctx, |_ui| {
                    //self.cpu_instructions.draw(ui);
                });

            egui::Window::new("Memory")
                .fixed_size(egui::Vec2::new(400.0, 300.0))
                .open(&mut self.cpu_memory)
                .show(ctx, |ui| {
                    ui.add(
                        egui::Slider::new(&mut self.cpu_memory_page, 0..=0xFF)
                            .step_by(0x01 as f64)
                            .hexadecimal(2, false, true)
                            .text("Page"),
                    );
                    let _addr = self.cpu_memory_page << 8;
                    //nes.draw_mem(ui, addr, 16, 16);
                });
        }

        // PPU diagnostic views
        {
            egui::Window::new("PPU: Patern Tables")
                .open(&mut self.ppu_palette_tbl)
                .show(ctx, |ui| {
                    ui.vertical(|ui| {
                        ui.horizontal(|ui| {
                            ui.label((self.ppu_palette_idx + 1).to_string());
                            if ui.button("Switch").clicked() {
                                self.ppu_palette_idx = self.ppu_palette_idx.add(1) & 0x07;
                            }
                        });
                        ui.horizontal(|_ui| {
                            // board.draw_pattern_tbl(ui, 0, self.ppu_palette_idx);
                            // board.draw_pattern_tbl(ui, 1, self.ppu_palette_idx);
                        });
                    })
                });
        }
    }

    pub fn update_cpu_status(&mut self, state: cpu::InstructionState) {
        self.cpu_state = state;
    }
}
