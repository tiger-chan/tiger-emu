use std::sync::mpsc::Sender;

use egui::{Context, Key, Modifiers};

use super::Message;

#[derive(Default, Debug)]
pub struct MainGui {
    cpu_status: bool,
    cpu_memory: bool,
    cpu_instructions: bool,
}

impl MainGui {
    pub fn prepare(&mut self, ctx: &Context, sender: &Sender<Message>) {
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
                            self.cpu_status = true;
                            ui.close_menu();
                        }

                        if ui.button("Memory").clicked() {
                            self.cpu_memory = true;
                            ui.close_menu();
                        }

                        if ui.button("Instructions").clicked() {
                            self.cpu_instructions = true;
                            ui.close_menu();
                        }
                    });

                    if ui.button("PPU").clicked() {
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
    }
}
