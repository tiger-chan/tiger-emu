use std::sync::mpsc::Sender;

use egui::{Color32, Context, FontId, Key, Modifiers, RichText, Ui};

use crate::assembly::Assembly;

use super::{ppu::PpuGui, Message};
use nes::prelude::*;

pub const ENABLED: Color32 = Color32::GREEN;
pub const DISABLED: Color32 = Color32::RED;
pub const CURSOR: Color32 = Color32::LIGHT_BLUE;
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

pub trait FromColor {
    fn from_color(value: &ppu::Color) -> Self;
}

impl FromColor for egui::Color32 {
    fn from_color(value: &ppu::Color) -> Self {
        Self::from_rgb(value.r, value.g, value.b)
    }
}

pub struct MainGui {
    cpu_status: bool,
    cpu_memory: bool,
    cpu_memory_page: u16,
    cpu_instructions: bool,

    ppu: PpuGui,

    cpu_state: cpu::InstructionState,
    cpu_asm: Assembly,
}

impl Default for MainGui {
    fn default() -> Self {
        Self {
            cpu_status: bool::default(),
            cpu_memory: bool::default(),
            cpu_memory_page: u16::default(),
            cpu_instructions: bool::default(),

            cpu_state: cpu::InstructionState::default(),
            cpu_asm: Assembly::with_capacity(25),

            ppu: PpuGui::default(),
        }
    }
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

                    self.ppu.draw_diagnostics_submenu(ui);

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
                .show(ctx, |ui| {
                    const INSTRUCTION_COUNT: i8 = 26;

                    ui.vertical(|ui| {
                        let asm = &self.cpu_asm;
                        let reg = &self.cpu_state.reg;
                        let half = INSTRUCTION_COUNT / 2;
                        let mut range = asm.get_range(reg.pc, -(half + 1));
                        for _ in 0..(((half + 1) as usize) - range.len()) {
                            range.insert(0, "Out of range");
                        }

                        for str in range.iter().skip(1) {
                            ui.label(RichText::new(*str).font(DIAGNOSTIC_FONT));
                        }

                        if let Some(str) = asm.get(reg.pc) {
                            ui.label(RichText::new(str).font(DIAGNOSTIC_FONT).color(CURSOR));
                        } else {
                            ui.label(
                                RichText::new("Out of range")
                                    .font(DIAGNOSTIC_FONT)
                                    .color(CURSOR),
                            );
                        }

                        let mut range = asm.get_range(reg.pc, half + 1);
                        for _ in 0..(((half + 1) as usize) - range.len()) {
                            range.insert(0, "Out of range");
                        }
                        for str in range.iter().skip(1) {
                            ui.label(RichText::new(*str).font(DIAGNOSTIC_FONT));
                        }
                    });
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
        self.ppu.draw_diagnostics(ctx, sender);
    }

    pub fn update_cpu_status(&mut self, state: cpu::InstructionState) {
        self.cpu_state = state;
    }

    pub fn update_ppu_palette(&mut self, tbl: Word, palette: Word, data: ppu::Palette) {
        self.ppu.update_ppu_palette(tbl, palette, data);
    }

    pub fn update_ppu_nametable(&mut self, tbl: Word, data: ppu::DebugNametable) {
        self.ppu.update_ppu_nametable(tbl, data);
    }

    pub fn update_asm(&mut self, asm: Vec<Byte>, start: Word) {
        self.cpu_asm = Assembly::from((asm.as_slice(), start as usize));
    }
}
