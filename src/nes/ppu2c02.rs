use std::rc::Rc;
use std::{cell::RefCell, rc::Weak};

use egui::TextureOptions;

use crate::bus::Bus;
use crate::gui::PpuDisplay;
use crate::nes::board::PPU_RAM_MASK;

use super::{Addr, RWPpuBus, RangeRWCpuBus};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Color {
    pub const fn new(r: u8, g: u8, b: u8) -> Self {
        Self { b, g, r }
    }
}

impl From<&Color> for egui::Color32 {
    fn from(value: &Color) -> Self {
        Self::from_rgb(value.r, value.g, value.b)
    }
}

fn create_palette(bytes: &[u8; 64 * 3]) -> [Color; 64] {
    let mut colors = [Color::new(0, 0, 0); 64];
    for i in 0..64 {
        let r = bytes[i + 0];
        let g = bytes[i + 1];
        let b = bytes[i + 2];
        colors[i] = Color::new(r, g, b);
    }

    colors
}

const X2C02: &[u8; 64 * 3] = include_bytes!("2C02.pal");
const X2C07: &[u8; 64 * 3] = include_bytes!("2C07.pal");

const INTERN_PPU_MASK: Addr = 0x3FFF;

const TBL_NAME: usize = 0x0400;
const TBL_NAME_COUNT: usize = 2;
const TBL_PALETTE: usize = 0x0020;

type NameTable = [[u8; TBL_NAME]; TBL_NAME_COUNT];
type PaletteTable = [u8; TBL_PALETTE];

macro_rules! name_arr {
    ($value:literal) => {
        [[$value; TBL_NAME]; TBL_NAME_COUNT]
    };
}

pub struct PpuBus {
    devices: [Weak<RefCell<dyn RWPpuBus>>; 2],
}

impl PpuBus {
    pub fn new(ppu: &Rc<RefCell<Ppu2C02>>) -> Self {
        let tmp = Rc::downgrade(&ppu);
        Self {
            devices: [tmp.clone(), tmp],
        }
    }

    pub fn cartridge<T>(&mut self, device: &Rc<RefCell<T>>)
    where
        T: RWPpuBus + 'static,
    {
        let tmp = Rc::downgrade(device);
        self.devices[1] = tmp;
    }
}

impl crate::ppu_bus::PpuBus for PpuBus {
    fn read(&self, addr: u16) -> u8 {
        for device in &self.devices {
            let result = match device.upgrade() {
                Some(device) => device.borrow().read(addr),
                _ => None,
            };

            match result {
                Some(r) => {
                    return r;
                }
                _ => continue,
            }
        }

        0
    }

    fn read_only(&self, addr: u16) -> u8 {
        for device in &self.devices {
            let result = match device.upgrade() {
                Some(device) => device.borrow().read_only(addr),
                _ => None,
            };

            match result {
                Some(r) => {
                    return r;
                }
                _ => continue,
            }
        }

        0
    }

    fn write(&mut self, addr: u16, data: u8) {
        for device in &self.devices {
            let result = match device.upgrade() {
                Some(device) => device.borrow_mut().write(addr, data),
                _ => None,
            };

            match result {
                Some(_) => {
                    return;
                }
                _ => continue,
            }
        }
    }
}

pub struct DebugPpuData {
    image: egui::ColorImage,
    texture: RefCell<Option<egui::TextureHandle>>,
}

#[allow(unused)]
pub struct Ppu2C02 {
    vram: Rc<RefCell<NameTable>>,
    palette: Rc<RefCell<PaletteTable>>,
    col_palette: [Color; 64],

    cycle: u16,
    scanline: u16,

    debug_data: DebugPpuData,
}

impl Ppu2C02 {
    pub fn new(ntsc: bool) -> Self {
        let vram = Rc::new(RefCell::new(name_arr![0]));
        let palette = Rc::new(RefCell::new([0; TBL_PALETTE]));
        Self {
            vram: vram,
            palette: palette,
            // I would prefer there a better way to do this
            col_palette: if ntsc { create_palette(X2C02) } else { create_palette(X2C07) },
            cycle: 0,
            scanline: 0,

            debug_data: DebugPpuData {
                image: egui::ColorImage::new([256, 240], egui::Color32::GREEN),
                texture: RefCell::new(None),
            },
        }
    }

    pub fn clock(&mut self, _bus: &mut dyn Bus, _cart: &mut dyn crate::ppu_bus::PpuBus) {
        self.cycle = self.cycle.wrapping_add(1);

        let mut set_pixel = |x, y, v| {
            let x = x as usize;
            let y = y as usize;
            let (w, h) = (
                self.debug_data.image.width(),
                self.debug_data.image.height(),
            );
            if x < w && y < h {
                self.debug_data.image.pixels[(y * w) + x] = egui::Color32::from(v);
            }
        };

        // This could be random but whatever...
        set_pixel(
            self.cycle.wrapping_sub(1),
            self.scanline,
            &self.col_palette[if rand::random() { 0x3F } else { 0x30 } ],
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

impl RWPpuBus for Ppu2C02 {
    fn read(&self, addr: Addr) -> Option<u8> {
        let _addr = addr & INTERN_PPU_MASK;
        None
    }

    fn read_only(&self, addr: Addr) -> Option<u8> {
        let _addr = addr & INTERN_PPU_MASK;
        None
    }

    fn write(&mut self, addr: Addr, _data: u8) -> Option<()> {
        let _addr = addr & INTERN_PPU_MASK;
        None
    }
}

impl PpuDisplay for Ppu2C02 {
    fn draw_palette(&self, ui: &mut egui::Ui) {
        let mut borrowed = self.debug_data.texture.borrow_mut();
        let texture = borrowed.get_or_insert_with(|| {
            ui.ctx().load_texture(
                "ppu2c02",
                egui::ColorImage::new([256, 240], egui::Color32::GRAY),
                TextureOptions::LINEAR,
            )
        });

        texture.set(
            egui::ImageData::Color(self.debug_data.image.clone()),
            TextureOptions::LINEAR,
        );

        let size = texture.size_vec2();
        ui.image(texture, size);
    }
}
