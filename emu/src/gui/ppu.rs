use egui::{Context, TextureHandle, TextureOptions, Ui};
use nes::prelude::*;
use std::{
    ops::Add,
    sync::mpsc::Sender,
    time::{Duration, Instant},
};

use super::{ui::FromColor, Message};

pub const PATTERN_UPDATE_INTERVAL: Duration = Duration::from_secs(1);

pub trait DrawPpuPattern {
    fn draw(&self, ui: &mut Ui, img: &mut egui::ColorImage, texture: &mut egui::TextureHandle);
}

pub trait DrawPpuNametable {
    fn draw(
        &self,
        ui: &mut Ui,
        img: &mut egui::ColorImage,
        texture: &mut egui::TextureHandle,
        colors: &ppu::ColorPalette,
    );
}

impl DrawPpuPattern for ppu::Palette {
    fn draw(&self, ui: &mut Ui, img: &mut egui::ColorImage, texture: &mut egui::TextureHandle) {
        for (i, v) in self.0.iter().enumerate() {
            img.pixels[i] = egui::Color32::from_color(v);
        }

        texture.set(egui::ImageData::Color(img.clone()), TextureOptions::LINEAR);

        let size = texture.size_vec2() * 1.5;
        ui.image(texture, size);
    }
}

impl DrawPpuNametable for ppu::DebugNametable {
    fn draw(
        &self,
        ui: &mut Ui,
        img: &mut egui::ColorImage,
        texture: &mut egui::TextureHandle,
        colors: &ppu::ColorPalette,
    ) {
        for (i, v) in self.0.iter().enumerate() {
            img.pixels[i] = egui::Color32::from_color(&colors.0[(*v) as usize]);
        }

        texture.set(egui::ImageData::Color(img.clone()), TextureOptions::LINEAR);

        let size = texture.size_vec2();
        ui.image(texture, size);
    }
}

pub struct PpuGui {
    palette_tbl: bool,
    palette_idx: u16,
    nametable: bool,

    palette_last: u16,
    palettes: [[ppu::Palette; 8]; 2],
    pattern_imgs: [egui::ColorImage; 2],
    pattern_textures: [Option<egui::TextureHandle>; 2],
    last_pattern_req: Instant,

    nametables: [ppu::DebugNametable; 4],
    nametable_imgs: [egui::ColorImage; 4],
    nametable_textures: [Option<egui::TextureHandle>; 4],

    color_palette: ppu::ColorPalette,
}

impl Default for PpuGui {
    fn default() -> Self {
        Self {
            palette_tbl: bool::default(),
            palette_idx: u16::default(),
            nametable: bool::default(),

            palette_last: u16::MAX,
            palettes: <[[ppu::Palette; 8]; 2]>::default(),
            pattern_imgs: [
                egui::ColorImage::new([128, 128], egui::Color32::GREEN),
                egui::ColorImage::new([128, 128], egui::Color32::GREEN),
            ],
            pattern_textures: <[Option<egui::TextureHandle>; 2]>::default(),
            last_pattern_req: Instant::now(),

            nametables: <[ppu::DebugNametable; 4]>::default(),
            nametable_imgs: [
                egui::ColorImage::new([256, 240], egui::Color32::GREEN),
                egui::ColorImage::new([256, 240], egui::Color32::GREEN),
                egui::ColorImage::new([256, 240], egui::Color32::GREEN),
                egui::ColorImage::new([256, 240], egui::Color32::GREEN),
            ],
            nametable_textures: <[Option<egui::TextureHandle>; 4]>::default(),

            color_palette: ppu::ColorPalette::default(),
        }
    }
}

impl PpuGui {
    pub fn draw_diagnostics_submenu(&mut self, ui: &mut egui::Ui) {
        ui.menu_button("PPU", |ui| {
            if ui.button("Palette").clicked() {
                self.palette_tbl = !self.palette_tbl;
                ui.close_menu();
            }

            if ui.button("Nametables").clicked() {
                self.nametable = !self.nametable;
                ui.close_menu();
            }
        });
    }

    pub fn draw_diagnostics(&mut self, ctx: &Context, sender: &Sender<Message>) {
        egui::Window::new("PPU: Patern Tables")
            .open(&mut self.palette_tbl)
            .show(ctx, |ui| {
                let cur = Instant::now();
                if self.palette_idx != self.palette_last
                    || cur.duration_since(self.last_pattern_req) >= PATTERN_UPDATE_INTERVAL
                {
                    self.last_pattern_req = cur;
                    self.palette_last = self.palette_idx;
                    let _ = sender.send(Message::QueryPalette(0, self.palette_idx));
                    let _ = sender.send(Message::QueryPalette(1, self.palette_idx));

                    if self.pattern_textures[0].is_none() {
                        let name = "ppu_pattern_0".to_owned();
                        self.pattern_textures[0] = Some(ui.ctx().load_texture(
                            name,
                            egui::ColorImage::new([256, 240], egui::Color32::GRAY),
                            TextureOptions::LINEAR,
                        ));
                    }

                    if self.pattern_textures[1].is_none() {
                        let name = "ppu_pattern_1".to_owned();
                        self.pattern_textures[1] = Some(ui.ctx().load_texture(
                            name,
                            egui::ColorImage::new([256, 240], egui::Color32::GRAY),
                            TextureOptions::LINEAR,
                        ));
                    }
                }
                ui.vertical(|ui| {
                    ui.horizontal(|ui| {
                        ui.label((self.palette_idx + 1).to_string());
                        if ui.button("Switch").clicked() {
                            self.palette_idx = self.palette_idx.add(1) & 0x07;
                        }
                    });
                    ui.horizontal(|ui| {
                        let left_img = &mut self.pattern_imgs[0];
                        let left_texture = self.pattern_textures[0].as_mut().unwrap();
                        let left = &self.palettes[0][self.palette_idx as usize];
                        left.draw(ui, left_img, left_texture);

                        let right_img = &mut self.pattern_imgs[1];
                        let right_texture = self.pattern_textures[1].as_mut().unwrap();
                        let right = &self.palettes[1][self.palette_idx as usize];
                        right.draw(ui, right_img, right_texture);
                    });
                })
            });

        egui::Window::new("PPU: Nametables")
            .open(&mut self.nametable)
            .show(ctx, |ui| {
                {
                    let _ = sender.send(Message::QueryNametable(0));
                    let _ = sender.send(Message::QueryNametable(1));
                    let _ = sender.send(Message::QueryNametable(2));
                    let _ = sender.send(Message::QueryNametable(3));

                    let create_nametable_texture =
                        |tbls: &mut [Option<TextureHandle>; 4], ui: &mut Ui, idx: usize| {
                            if tbls[idx].is_none() {
                                let name = format!("ppu_nametable_{idx}");
                                tbls[idx] = Some(ui.ctx().load_texture(
                                    name,
                                    egui::ColorImage::new([256, 240], egui::Color32::GRAY),
                                    TextureOptions::LINEAR,
                                ));
                            }
                        };

                    create_nametable_texture(&mut self.nametable_textures, ui, 0);
                    create_nametable_texture(&mut self.nametable_textures, ui, 1);
                    create_nametable_texture(&mut self.nametable_textures, ui, 2);
                    create_nametable_texture(&mut self.nametable_textures, ui, 3);
                }

                ui.vertical(|ui| {
                    ui.horizontal(|ui| {
                        let left_img = &mut self.nametable_imgs[0];
                        let left_texture = self.nametable_textures[0].as_mut().unwrap();
                        let left = &self.nametables[0];
                        left.draw(ui, left_img, left_texture, &self.color_palette);

                        let right_img = &mut self.nametable_imgs[1];
                        let right_texture = self.nametable_textures[1].as_mut().unwrap();
                        let right = &self.nametables[1];

                        right.draw(ui, right_img, right_texture, &self.color_palette);
                    });

                    ui.horizontal(|ui| {
                        let left_img = &mut self.nametable_imgs[2];
                        let left_texture = self.nametable_textures[2].as_mut().unwrap();
                        let left = &self.nametables[2];
                        left.draw(ui, left_img, left_texture, &self.color_palette);

                        let right_img = &mut self.nametable_imgs[3];
                        let right_texture = self.nametable_textures[3].as_mut().unwrap();
                        let right = &self.nametables[3];

                        right.draw(ui, right_img, right_texture, &self.color_palette)
                    });
                })
            });
    }

    pub fn update_ppu_col_palette(&mut self, data: ppu::ColorPalette) {
        self.color_palette = data;
    }

    pub fn update_ppu_palette(&mut self, tbl: Word, palette: Word, data: ppu::Palette) {
        let tbl = (tbl & 0x01) as usize;
        let palette = (palette & 0x07) as usize;
        self.palettes[tbl][palette] = data;
    }

    pub fn update_ppu_nametable(&mut self, tbl: Word, data: ppu::DebugNametable) {
        let tbl = (tbl & 0x03) as usize;
        self.nametables[tbl] = data;
    }
}
