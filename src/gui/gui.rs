use crate::motherboard::Motherboard;

use super::{BoardCommand, CpuDisplay, MemoryDisplay, PpuDisplay};
use egui::{Color32, Context, FontId, Key, Modifiers, RichText};

pub const ENABLED: Color32 = Color32::GREEN;
pub const DISABLED: Color32 = Color32::RED;
pub const CURSOR: Color32 = Color32::LIGHT_BLUE;
pub const DIAGNOSTIC_FONT: FontId = FontId::monospace(12.0);

/// Example application state. A real application will need a lot more state than this.
pub(crate) struct Gui {
    /// Only show the egui window when true.
    full_diagnositics: bool,
    memory: bool,
    instructions: bool,
    cpu: bool,
    ppu: bool,
    memory_inspect: u16,
    picked_path: Option<String>,
    palette: u8,
}

impl Gui {
    /// Create a `Gui`.
    pub(crate) fn new() -> Self {
        Self {
            full_diagnositics: false,
            memory: false,
            instructions: false,
            cpu: false,
            ppu: false,
            memory_inspect: 0x0000,
            picked_path: None,
            palette: 0,
        }
    }

    /// Create the UI using egui.
    pub(crate) fn ui<TMotherBoard>(
        &mut self,
        ctx: &Context,
        run_emu: &mut bool,
        board: &mut TMotherBoard,
    ) where
        TMotherBoard: Motherboard + BoardCommand + MemoryDisplay + CpuDisplay + PpuDisplay,
    {
        if ctx.input(|i| i.key_pressed(egui::Key::F5)) {
            *run_emu = !*run_emu;
        }

        if ctx.input(|i| i.key_pressed(egui::Key::F10)) {
            board.step();
        }

        if ctx.input(|i| i.key_pressed(Key::F10) && i.modifiers.matches(Modifiers::CTRL)) {
            board.frame();
        }

        if ctx.input(|i| {
            i.key_pressed(Key::F5) && i.modifiers.matches(Modifiers::CTRL | Modifiers::SHIFT)
        }) {
            board.reset();
        }

        if ctx.input(|i| {
            i.key_pressed(Key::I) && i.modifiers.matches(Modifiers::CTRL | Modifiers::SHIFT)
        }) {
            board.irq();
        }

        if ctx.input(|i| {
            i.key_pressed(Key::N) && i.modifiers.matches(Modifiers::CTRL | Modifiers::SHIFT)
        }) {
            board.nmi();
        }

        egui::TopBottomPanel::top("menubar_container").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Open File...").clicked() {
                        if let Some(path) = rfd::FileDialog::new().pick_file() {
                            self.picked_path = Some(path.display().to_string());
                        }
                        ui.close_menu();
                    }
                });
                ui.menu_button("View", |ui| {
                    if ui.button("Diagnositics").clicked() {
                        self.full_diagnositics = true;
                        ui.close_menu();
                    }

                    if ui.button("Memory").clicked() {
                        self.memory = true;
                        ui.close_menu();
                    }

                    if ui.button("Instructions").clicked() {
                        self.instructions = true;
                        ui.close_menu();
                    }

                    if ui.button("CPU").clicked() {
                        self.cpu = true;
                        ui.close_menu();
                    }

                    if ui.button("PPU").clicked() {
                        self.ppu = true;
                        ui.close_menu();
                    }

                    ui.separator();

                    if ui.button("Play (F5)").clicked() {
                        //board.step();
                        ui.close_menu();
                    }

                    if ui.button("Step (F10)").clicked() {
                        board.step();
                        ui.close_menu();
                    }

                    if ui.button("Step Frame (CTRL+F10)").clicked() {
                        board.frame();
                        ui.close_menu();
                    }

                    if ui.button("Reset (CTRL + SHIFT + F5)").clicked() {
                        board.reset();
                        ui.close_menu();
                    }

                    if ui.button("IRQ (CTRL + SHIFT + I)").clicked() {
                        board.irq();
                        ui.close_menu();
                    }

                    if ui.button("NMI (CTRL + SHIFT + N)").clicked() {
                        board.nmi();
                        ui.close_menu();
                    }
                })
            });
        });

        egui::Window::new("CPU Diagnositics")
            .default_rect(ctx.available_rect())
            .open(&mut self.full_diagnositics)
            .show(ctx, |ui| {
                egui::ScrollArea::vertical().show(ui, |ui| {
                    ui.horizontal(|ui| {
                        ui.vertical(|ui| {
                            board.draw_mem(ui, 0x0000, 16, 16);
                            ui.label(RichText::new("").font(DIAGNOSTIC_FONT)); // Spacing
                            ui.label(RichText::new("").font(DIAGNOSTIC_FONT)); // Spacing
                            board.draw_mem(ui, 0x8000, 16, 16);
                        });
                        ui.vertical(|ui| {
                            board.draw_cpu(ui);
                            ui.label(RichText::new("").font(DIAGNOSTIC_FONT)); // Spacing
                            board.draw_code(ui, 26);
                        });
                    });
                });
            });

        egui::Window::new("CPU: Status")
            .open(&mut self.cpu)
            .show(ctx, |ui| {
                board.draw_cpu(ui);
            });

        egui::Window::new("PPU: Patern Tables")
            .open(&mut self.ppu)
            .show(ctx, |ui| {
                ui.vertical(|ui| {
                    ui.horizontal(|ui| {
                        ui.label((self.palette + 1).to_string());
                        if ui.button("Switch").clicked() {
                            self.palette = (self.palette + 1) & 0x07;
                        }
                    });
                    ui.horizontal(|ui| {
                        board.draw_pattern_tbl(ui, 0, self.palette);
                        board.draw_pattern_tbl(ui, 1, self.palette);
                    });
                })
            });

        egui::Window::new("Instructions")
            .open(&mut self.instructions)
            .show(ctx, |ui| {
                board.draw_code(ui, 26);
            });

        egui::Window::new("Memory")
            .fixed_size(egui::Vec2::new(400.0, 300.0))
            .open(&mut self.memory)
            .show(ctx, |ui| {
                ui.add(
                    egui::Slider::new(&mut self.memory_inspect, 0..=0xFF)
                        .step_by(0x01 as f64)
                        .hexadecimal(2, false, true)
                        .text("Page"),
                );
                let addr = self.memory_inspect << 8;
                board.draw_mem(ui, addr, 16, 16);
            });
    }
}
