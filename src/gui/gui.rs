use std::cell::RefCell;

use crate::{bus::Bus, cpu::CPU};

use super::{DebugBus, DebugCpu};
use egui::{Color32, Context, FontId, RichText};

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
    memory_inspect: u16,
}

impl Gui {
    /// Create a `Gui`.
    pub(crate) fn new() -> Self {
        Self {
            full_diagnositics: false,
            memory: false,
            instructions: false,
            cpu: false,
            memory_inspect: 0x0000,
        }
    }

    /// Create the UI using egui.
    pub(crate) fn ui<TBus, TCpu>(&mut self, ctx: &Context, bus: &RefCell<TBus>, cpu: &RefCell<TCpu>)
    where
        TBus: DebugBus + Bus,
        TCpu: DebugCpu + CPU,
    {
        egui::TopBottomPanel::top("menubar_container").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Open File...").clicked() {
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

                    ui.separator();

                    if ui.button("Step (F10)").clicked() {
                        cpu.borrow_mut().step();
                        ui.close_menu();
                    }

                    if ui.button("Reset (CTRL + SHIFT + F5)").clicked() {
                        cpu.borrow_mut().reset();
                        ui.close_menu();
                    }

                    if ui.button("IRQ (CTRL + SHIFT + I)").clicked() {
                        cpu.borrow_mut().irq();
                        ui.close_menu();
                    }

                    if ui.button("NMI (CTRL + SHIFT + N)").clicked() {
                        cpu.borrow_mut().nmi();
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
                            bus.borrow().draw_mem(ui, 0x0000, 16, 16);
                            ui.label(RichText::new("").font(DIAGNOSTIC_FONT)); // Spacing
                            ui.label(RichText::new("").font(DIAGNOSTIC_FONT)); // Spacing
                            bus.borrow().draw_mem(ui, 0x8000, 16, 16);
                        });
                        ui.vertical(|ui| {
                            cpu.borrow().draw_cpu(ui);
                            ui.label(RichText::new("").font(DIAGNOSTIC_FONT)); // Spacing
                            cpu.borrow().draw_code(ui, 26);
                        });
                    });
                });
            });

        egui::Window::new("CPU: Status")
            .open(&mut self.cpu)
            .show(ctx, |ui| {
                cpu.borrow().draw_cpu(ui);
            });

        egui::Window::new("Instructions")
            .open(&mut self.instructions)
            .show(ctx, |ui| {
                cpu.borrow().draw_code(ui, 26);
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
                bus.borrow().draw_mem(ui, addr, 16, 16);
            });
    }
}
