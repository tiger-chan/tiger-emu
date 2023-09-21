mod color;
mod palette;
mod bus;
mod memory;
mod registers;

pub use color::*;
pub use palette::*;
pub use bus::*;
pub use memory::*;
pub use registers::*;

use std::cell::RefCell;
use std::rc::Rc;

use egui::TextureOptions;

use crate::{bus::Bus, ppu_bus::PpuBus};
use crate::nes::board::PPU_RAM_MASK;

use super::{Addr, RangeRWCpuBus, HI_MASK, LO_MASK};

pub struct DebugPpuData {
    pattern: [egui::ColorImage; 2],
    pattern_texture: [RefCell<Option<egui::TextureHandle>>; 2],
}

impl DebugPpuData {
    fn new() -> Self {
        Self {
            pattern: [
                egui::ColorImage::new([128, 128], egui::Color32::GREEN),
                egui::ColorImage::new([128, 128], egui::Color32::GREEN),
            ],
            pattern_texture: [RefCell::new(None), RefCell::new(None)],
        }
    }
}

#[allow(unused)]
pub struct Ppu2C02 {
    memory: Rc<RefCell<PpuMemory>>,
    reg: RefCell<Registers>,
    screen: [Color; 256 * 240],
    cycle: u16,
    scanline: u16,
    pub bus: bus::PpuBus,

    debug_data: DebugPpuData,
}

impl Ppu2C02 {
    pub fn new(ntsc: bool) -> Self {
        let mem = Rc::new(RefCell::new(PpuMemory::new(ntsc)));
        let bus = bus::PpuBus::new(&mem);
        Self {
            reg: RefCell::new(Registers::default()),
            bus,
            memory: mem,
            screen: [Color::new(50, 50, 50); 256 * 240],
            cycle: 0,
            scanline: 0,

            debug_data: DebugPpuData::new(),
        }
    }

    pub fn nmi(&self) -> bool {
        self.reg.borrow().ctrl & Ctrl::V == Ctrl::V && self.reg.borrow().status & Status::V == Status::V
    }

    pub fn clock(&mut self, _bus: &mut dyn Bus) {
        if self.scanline == Addr::MAX && self.cycle == 1 {
            self.reg.borrow_mut().status.set(Status::V, false);
        }

        if self.scanline == 241 && self.cycle == 1 {
            self.reg.borrow_mut().status.set(Status::V, true);
            if self.reg.borrow().ctrl & Ctrl::V == Ctrl::V {

            }
        }

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

        self.cycle = self.cycle.wrapping_add(1);

        if self.cycle == 341 {
            self.cycle = 0;
            self.scanline = self.scanline.wrapping_add(1);
        }

        if self.scanline == 261 {
            self.scanline = u16::MAX;
        }
    }

    pub fn reset(&mut self) {
        self.cycle = 0;
        self.scanline = 0;
        self.reg.borrow_mut().addr = 0x00;
        self.reg.borrow_mut().addr_latch = 0x00;
        self.reg.borrow_mut().data_buffer = 0x00;
        self.reg.borrow_mut().ctrl = Ctrl::new(0x00);
        self.reg.borrow_mut().mask = Mask::new(0x00);
        self.reg.borrow_mut().status = Status::new(0x00);
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
        ppu_bus: &dyn PpuBus,
        palette: Addr,
        pixel: Addr,
    ) -> Color {
        let addr = 0x3F00 + palette.overflowing_shl(2).0 + pixel;
        let addr = ppu_bus.read_only(addr) & 0x3F;
        self.memory.borrow().col_palette[addr as usize]
    }

    pub fn draw_pattern_tbl(
        &mut self,
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
                    let mut lsb = self.bus.read_only(tbl * 0x1000 + offset + r);
                    let mut msb = self.bus.read_only(tbl * 0x1000 + offset + r + 8);
                    for c in 0..8 {
                        let pixel = ((lsb & 0x01) + (msb & 0x01)) as Addr;
                        lsb >>= 1;
                        msb >>= 1;

                        let x = x * 8 + (7 - c);
                        let y = y * 8 + r;

                        let color = self.debug_color_from_palette(&self.bus, palette, pixel);

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
        let addr = addr & PPU_RAM_MASK;
        match addr {
            0x0002 => {
                let tmp = u8::from(self.reg.borrow().status.get(Status::V | Status::S | Status::O));
                let tmp = tmp | self.reg.borrow().data_buffer & 0x1f;
                self.reg.borrow_mut().status.set(Status::V, false);
                self.reg.borrow_mut().addr_latch = 0;

                Some(tmp)
            }
            0x0007 => { 
                let mut tmp = self.reg.borrow().data_buffer;
                let addr = self.reg.borrow().addr;
                self.reg.borrow_mut().data_buffer = self.bus.read(addr);

                if addr > 0x3F00 {
                    tmp = self.reg.borrow().data_buffer;
                }
                self.reg.borrow_mut().addr = addr.wrapping_add(1);
                Some(tmp)
            },
            _ => None
        }
    }

    fn read_only(&self, addr: Addr) -> Option<u8> {
        let addr = addr & PPU_RAM_MASK;
        match addr {
            0x0000 => Some(self.reg.borrow().ctrl.into()),
            0x0001 => Some(self.reg.borrow().mask.into()),
            0x0002 => Some(self.reg.borrow().status.into()),
            _ => None
        }
    }

    fn write(&mut self, addr: Addr, data: u8) -> Option<()> {
        match addr & PPU_RAM_MASK {
            0x0000 => {
                self.reg.borrow_mut().ctrl = Ctrl::new(data);
                Some(())
            },
            0x0001 => {
                self.reg.borrow_mut().mask = Mask::new(data);
                Some(())
            },
            0x0006 => {
                if self.reg.borrow().addr_latch == 0 {
                    let addr = self.reg.borrow().addr;
                    self.reg.borrow_mut().addr = (addr & LO_MASK) | (data as Addr) << 8;
                    self.reg.borrow_mut().addr_latch = 1;
                } else {
                    let addr = self.reg.borrow().addr;
                    self.reg.borrow_mut().addr = (addr & HI_MASK) | data as Addr;
                    self.reg.borrow_mut().addr_latch = 0;
                }
                Some(())
            }
            0x0007 => {
                let ppu_addr = self.reg.borrow().addr;
                self.bus.write(ppu_addr, data);
                self.reg.borrow_mut().addr = ppu_addr.wrapping_add(1);
                Some(())
            }
            _ => {
                None
            }
        }
    }
}
