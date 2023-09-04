use super::{DebugBus, DebugCpu};
use egui::{Color32, Context, FontId, RichText};

pub const ENABLED: Color32 = Color32::GREEN;
pub const DISABLED: Color32 = Color32::RED;
pub const CURSOR: Color32 = Color32::LIGHT_BLUE;
pub const DIAGNOSTIC_FONT: FontId = FontId::monospace(12.0);

/// Example application state. A real application will need a lot more state than this.
pub(crate) struct Gui {
    /// Only show the egui window when true.
    window_open: bool,
    instruct: bool,
}

impl Gui {
    /// Create a `Gui`.
    pub(crate) fn new() -> Self {
        Self {
            window_open: true,
            instruct: true,
        }
    }

    /// Create the UI using egui.
    pub(crate) fn ui(&mut self, ctx: &Context, bus: &dyn DebugBus, cpu: &dyn DebugCpu) {
        egui::TopBottomPanel::top("menubar_container").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Open File...").clicked() {
                        self.window_open = true;
                        ui.close_menu();
                    }
                });
                ui.menu_button("View", |ui| {
                    if ui.button("Diagnositics").clicked() {
                        self.instruct = true;
                        ui.close_menu();
                    }
                })
            });
        });

        egui::Window::new("CPU Diagnositics")
            .default_rect(ctx.available_rect())
            .open(&mut self.instruct)
            .show(ctx, |ui| {
                egui::ScrollArea::vertical().show(ui, |ui| {
                    ui.horizontal(|ui| {
                        ui.vertical(|ui| {
                            bus.draw_mem(ui, 0x0000, 16, 16);
                            ui.label(RichText::new("").font(DIAGNOSTIC_FONT)); // Spacing
                            ui.label(RichText::new("").font(DIAGNOSTIC_FONT)); // Spacing
                            bus.draw_mem(ui, 0x8000, 16, 16);
                        });
                        ui.vertical(|ui| {
                            cpu.draw_cpu(ui);
                            ui.label(RichText::new("").font(DIAGNOSTIC_FONT)); // Spacing
                            cpu.draw_code(ui, 26);
                        });
                    });
                });
            });
    }
}
