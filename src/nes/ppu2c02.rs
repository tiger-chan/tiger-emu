mod color;
mod palette;
mod ppu_bus;
mod ppu_memory;

pub use color::*;
pub use palette::*;
pub use ppu_bus::*;
pub use ppu_memory::*;

use std::cell::RefCell;
use std::rc::Rc;

use egui::TextureOptions;

use crate::bus::Bus;
use crate::nes::board::PPU_RAM_MASK;

use super::{Addr, RangeRWCpuBus};

pub struct DebugPpuData {
    pattern: [egui::ColorImage; 2],
    pattern_texture: [RefCell<Option<egui::TextureHandle>>; 2],
}

#[allow(unused)]
pub struct Ppu2C02 {
    memory: Rc<RefCell<PpuMemory>>,
    screen: [Color; 256 * 240],
    cycle: u16,
    scanline: u16,

    debug_data: DebugPpuData,
}

impl Ppu2C02 {
    pub fn new(ntsc: bool) -> Self {
        Self {
            memory: Rc::new(RefCell::new(PpuMemory::new(ntsc))),
            screen: [Color::new(50, 50, 50); 256 * 240],
            cycle: 0,
            scanline: 0,

            debug_data: DebugPpuData {
                pattern: [
                    egui::ColorImage::new([128, 128], egui::Color32::GREEN),
                    egui::ColorImage::new([128, 128], egui::Color32::GREEN),
                ],
                pattern_texture: [RefCell::new(None), RefCell::new(None)],
            },
        }
    }

    pub fn clock(&mut self, _bus: &mut dyn Bus, _ppu_bus: &mut dyn crate::ppu_bus::PpuBus) {
        self.cycle = self.cycle.wrapping_add(1);

        let mut set_pixel = |x, y, v| {
            let x = x as usize;
            let y = y as usize;
            let (w, h) = (256, 240);
            if x < w && y < h {
                self.screen[(y * w) + x] = v;
            }
        };

        // This could be random but whatever...
        set_pixel(
            self.cycle.wrapping_sub(1),
            self.scanline,
            self.memory.borrow().col_palette[if rand::random() { 0x3F } else { 0x30 }],
        );

        match self.cycle {
            341 => {
                self.cycle = 0;
                self.scanline = self.scanline.wrapping_add(1);
            }
            _ => {}
        }

        match self.scanline {
            261 => {
                self.scanline = u16::MAX;
            }
            _ => {}
        }
    }

    pub fn frame_complete(&self) -> bool {
        self.scanline == u16::MAX
    }

    pub fn debug_reset_complete(&mut self) {
        self.scanline = 0;
    }

    pub fn draw(&self, pixels: &mut [u8]) {
        for (i, pixel) in pixels.chunks_exact_mut(4).enumerate() {
            let colors = self.screen[i];
            pixel.copy_from_slice(&colors.to_array());
        }
    }

    fn debug_color_from_palette(
        &self,
        ppu_bus: &dyn crate::ppu_bus::PpuBus,
        palette: Addr,
        pixel: Addr,
    ) -> Color {
        let addr = 0x3F00 + palette.overflowing_shl(2).0 + pixel;
        let addr = ppu_bus.read_only(addr);
        self.memory.borrow().col_palette[addr as usize]
    }

    pub fn draw_pattern_tbl(
        &mut self,
        ppu_bus: &dyn crate::ppu_bus::PpuBus,
        ui: &mut egui::Ui,
        tbl: Addr,
        palette: Addr,
    ) {
        let mut borrowed = self.debug_data.pattern_texture[tbl as usize].borrow_mut();
        let texture = borrowed.get_or_insert_with(|| {
            let name = format!("ppu2c02_{tbl}");
            ui.ctx().load_texture(
                name,
                egui::ColorImage::new([256, 240], egui::Color32::GRAY),
                TextureOptions::LINEAR,
            )
        });

        for y in 0..16 as Addr {
            for x in 0..16 as Addr {
                let offset = y * 256 + x * 16;

                for r in 0..8 {
                    let mut lsb = ppu_bus.read_only(tbl * 0x1000 + offset + r + 0);
                    let mut msb = ppu_bus.read_only(tbl * 0x1000 + offset + r + 8);
                    for c in 0..8 {
                        let pixel = (lsb & 0x01 + msb & 0x01) as Addr;
                        lsb = lsb >> 1;
                        msb = msb >> 1;

                        let x = x * 8 + (7 - c);
                        let y = y * 8 + r;

                        let color = self.debug_color_from_palette(ppu_bus, palette, pixel);

                        let mut set_pixel = |x, y, v| {
                            let x = x as usize;
                            let y = y as usize;
                            let (w, h) = (
                                self.debug_data.pattern[tbl as usize].width(),
                                self.debug_data.pattern[tbl as usize].height(),
                            );
                            if x < w && y < h {
                                self.debug_data.pattern[tbl as usize].pixels[(y * w) + x] =
                                    egui::Color32::from(v);
                            }
                        };
                        set_pixel(x, y, &color);
                    }
                }
            }
        }

        texture.set(
            egui::ImageData::Color(self.debug_data.pattern[tbl as usize].clone()),
            TextureOptions::LINEAR,
        );

        let size = texture.size_vec2();
        ui.image(texture, size);
    }
}

impl RangeRWCpuBus for Ppu2C02 {
    fn accepted_range(&self) -> std::ops::RangeInclusive<Addr> {
        0x2000..=0x3FFF
    }

    fn read(&self, addr: Addr) -> Option<u8> {
        let _addr = addr & PPU_RAM_MASK;
        None
    }

    fn read_only(&self, addr: Addr) -> Option<u8> {
        let _addr = addr & PPU_RAM_MASK;
        None
    }

    fn write(&mut self, addr: Addr, _data: u8) -> Option<()> {
        let _addr = addr & PPU_RAM_MASK;
        None
    }
}
