use std::rc::Rc;
use std::{cell::RefCell, rc::Weak};

use egui::TextureOptions;

use crate::bus::Bus;
use crate::nes::board::PPU_RAM_MASK;

use super::{Addr, RWPpuBus, RangeRWCpuBus};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Color {
    pub const fn new(r: u8, g: u8, b: u8) -> Self {
        Self { b, g, r }
    }

    pub fn to_array(&self) -> [u8; 4] {
        [self.r, self.g, self.b, 0xFF]
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
const TBL_PATTERN: usize = 0x1000;
const TBL_PATTERN_COUNT: usize = 2;
const TBL_PALETTE: usize = 0x0020;

type NameTable = [[u8; TBL_NAME]; TBL_NAME_COUNT];
type PatternTable = [[u8; TBL_PATTERN]; TBL_PATTERN_COUNT];
type PaletteTable = [u8; TBL_PALETTE];

macro_rules! name_arr {
    ($value:literal) => {
        [[$value; TBL_NAME]; TBL_NAME_COUNT]
    };
}

macro_rules! pattern_arr {
    ($value:literal) => {
        [[$value; TBL_PATTERN]; TBL_PATTERN_COUNT]
    };
}

pub struct PpuBus {
    devices: [Weak<RefCell<dyn RWPpuBus>>; 2],
}

impl PpuBus {
    pub fn new(ppu: &Ppu2C02) -> Self {
        let tmp = Rc::downgrade(&ppu.memory);
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
    pattern: [egui::ColorImage; 2],
    pattern_texture: [RefCell<Option<egui::TextureHandle>>; 2],
}

pub struct PpuMemory {
    pub vram: RefCell<NameTable>,
    pub pattern: RefCell<PatternTable>,
    pub palette: RefCell<PaletteTable>,
    pub col_palette: [Color; 64],
}

impl PpuMemory {
    pub fn new(ntsc: bool) -> Self {
        Self {
            vram: RefCell::new(name_arr![0]),
            pattern: RefCell::new(pattern_arr![0]),
            palette: RefCell::new([0; TBL_PALETTE]),
            // I would prefer there a better way to do this
            col_palette: if ntsc {
                create_palette(X2C02)
            } else {
                create_palette(X2C07)
            },
        }
    }
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

impl RWPpuBus for PpuMemory {
    fn read(&self, addr: Addr) -> Option<u8> {
        let addr = addr & INTERN_PPU_MASK;
        match &addr {
            0x0000..=0x1FFF => {
                let tbl = ((addr & 0x1000) >> 12) as usize;
                let addr = (addr & 0x0FFF) as usize;
                let tbl = &self.pattern.borrow()[tbl];
                Some(tbl[addr])
            }
            0x2000..=0x3EFF => None,
            0x3F00..=0x3FFF => {
                let addr = addr & 0x001F;
                let addr = match addr {
                    0x0010 => 0x0000,
                    0x0014 => 0x0004,
                    0x0018 => 0x0008,
                    0x001C => 0x000C,
                    x => x,
                };

                let data = self.palette.borrow()[addr as usize]; //& (mask.grayscale ? 0x30 : 0x3F);
                Some(data)
            }
            _ => None,
        }
    }

    fn read_only(&self, addr: Addr) -> Option<u8> {
        let addr = addr & INTERN_PPU_MASK;
        match &addr {
            0x0000..=0x1FFF => {
                let tbl = ((addr & 0x1000) >> 12) as usize;
                let addr = (addr & 0x0FFF) as usize;
                let tbl = &self.pattern.borrow()[tbl];
                Some(tbl[addr])
            }
            0x2000..=0x3EFF => None,
            0x3F00..=0x3FFF => {
                let addr = addr & 0x001F;
                let addr = match addr {
                    0x0010 => 0x0000,
                    0x0014 => 0x0004,
                    0x0018 => 0x0008,
                    0x001C => 0x000C,
                    x => x,
                };

                let data = self.palette.borrow()[addr as usize]; //& (mask.grayscale ? 0x30 : 0x3F);
                Some(data)
            }
            _ => None,
        }
    }

    fn write(&mut self, addr: Addr, data: u8) -> Option<()> {
        let addr = addr & INTERN_PPU_MASK;
        match &addr {
            0x0000..=0x1FFF => {
                let tbl = ((addr & 0x1000) >> 12) as usize;
                let addr = (addr & 0x0FFF) as usize;
                let tbl = &mut self.pattern.borrow_mut()[tbl];
                tbl[addr] = data;
                Some(())
            }
            0x2000..=0x3EFF => None,
            0x3F00..=0x3FFF => {
                let addr = addr & 0x001F;
                let addr = match addr {
                    0x0010 => 0x0000,
                    0x0014 => 0x0004,
                    0x0018 => 0x0008,
                    0x001C => 0x000C,
                    x => x,
                };

                self.palette.borrow_mut()[addr as usize] = data;
                Some(())
            }
            _ => None,
        }
    }
}
