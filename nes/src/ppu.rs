mod bus;
mod color;
mod registers;

use std::{cell::RefCell, rc::Rc};

use crate::{
    io::{DisplayDevice, ReadOnlyDevice},
    DisplayClocked,
};

use super::{
    io::{ReadDevice, RwDevice, WriteDevice},
    Byte, Word,
};

pub use bus::Bus;
pub use color::Color;
pub use registers::*;

pub const WIDTH: Word = 256;
pub const HEIGHT: Word = 240;

#[allow(unused)]
mod cycles {
    use crate::Word;

    /// # Cycle 0
    ///
    /// This is an idle cycle. The value on the PPU address bus during this
    /// cycle appears to be the same CHR address that is later used to fetch the
    /// low background tile byte starting at dot 5 (possibly calculated during
    /// the two unused NT fetches at the end of the previous scanline).
    pub const IDLE: Word = 0;

    /// # Cycles 1-256
    ///
    /// The data for each tile is fetched during this phase. Each memory access
    /// takes 2 PPU cycles to complete, and 4 must be performed per tile:
    /// 1. Nametable byte
    /// 2. Attribute table byte
    /// 3. Pattern table tile low
    /// 4. Pattern table tile high (+8 bytes from pattern table tile low)
    ///
    /// The data fetched from these accesses is placed into internal latches,
    /// and then fed to the appropriate shift registers when it's time to do so
    /// (every 8 cycles). Because the PPU can only fetch an attribute byte every
    /// 8 cycles, each sequential string of 8 pixels is forced to have the same
    /// palette attribute.
    ///
    /// Sprite 0 hit acts as if the image starts at cycle 2 (which is the same
    /// cycle that the shifters shift for the first time), so the sprite 0 flag
    /// will be raised at this point at the earliest. Actual pixel output is
    /// delayed further due to internal render pipelining, and the first pixel
    /// is output during cycle 4.
    ///
    /// The shifters are reloaded during ticks 9, 17, 25, ..., 257.
    ///
    /// > *Note*: At the beginning of each scanline, the data for the first two
    /// > tiles is already loaded into the shift registers (and ready to be
    /// > rendered), so the first tile that gets fetched is Tile 3.
    /// >
    /// > While all of this is going on, sprite evaluation for the next scanline
    /// > is taking place as a seperate process, independent to what's happening
    /// > here.
    pub const VISABLE: Word = 255;

    /// # Cycles 1-256
    ///
    /// The data for each tile is fetched during this phase. Each memory access
    /// takes 2 PPU cycles to complete, and 4 must be performed per tile:
    /// 1. Nametable byte
    /// 2. Attribute table byte
    /// 3. Pattern table tile low
    /// 4. Pattern table tile high (+8 bytes from pattern table tile low)
    ///
    /// The data fetched from these accesses is placed into internal latches,
    /// and then fed to the appropriate shift registers when it's time to do so
    /// (every 8 cycles). Because the PPU can only fetch an attribute byte every
    /// 8 cycles, each sequential string of 8 pixels is forced to have the same
    /// palette attribute.
    ///
    /// Sprite 0 hit acts as if the image starts at cycle 2 (which is the same
    /// cycle that the shifters shift for the first time), so the sprite 0 flag
    /// will be raised at this point at the earliest. Actual pixel output is
    /// delayed further due to internal render pipelining, and the first pixel
    /// is output during cycle 4.
    ///
    /// The shifters are reloaded during ticks 9, 17, 25, ..., 257.
    ///
    /// > *Note*: At the beginning of each scanline, the data for the first two
    /// > tiles is already loaded into the shift registers (and ready to be
    /// > rendered), so the first tile that gets fetched is Tile 3.
    /// >
    /// > While all of this is going on, sprite evaluation for the next scanline
    /// > is taking place as a seperate process, independent to what's happening
    /// > here.
    pub const VIS_LO: Word = 1;

    /// # Cycles 1-256
    ///
    /// The data for each tile is fetched during this phase. Each memory access
    /// takes 2 PPU cycles to complete, and 4 must be performed per tile:
    /// 1. Nametable byte
    /// 2. Attribute table byte
    /// 3. Pattern table tile low
    /// 4. Pattern table tile high (+8 bytes from pattern table tile low)
    ///
    /// The data fetched from these accesses is placed into internal latches,
    /// and then fed to the appropriate shift registers when it's time to do so
    /// (every 8 cycles). Because the PPU can only fetch an attribute byte every
    /// 8 cycles, each sequential string of 8 pixels is forced to have the same
    /// palette attribute.
    ///
    /// Sprite 0 hit acts as if the image starts at cycle 2 (which is the same
    /// cycle that the shifters shift for the first time), so the sprite 0 flag
    /// will be raised at this point at the earliest. Actual pixel output is
    /// delayed further due to internal render pipelining, and the first pixel
    /// is output during cycle 4.
    ///
    /// The shifters are reloaded during ticks 9, 17, 25, ..., 257.
    ///
    /// > *Note*: At the beginning of each scanline, the data for the first two
    /// > tiles is already loaded into the shift registers (and ready to be
    /// > rendered), so the first tile that gets fetched is Tile 3.
    /// >
    /// > While all of this is going on, sprite evaluation for the next scanline
    /// > is taking place as a seperate process, independent to what's happening
    /// > here.
    pub const VIS_HI: Word = VIS_LO + VISABLE;

    /// # Cycles 257-320
    ///
    /// The tile data for the sprites on the next scanline are fetched here.
    /// Again, each memory access takes 2 PPU cycles to complete, and 4 are
    /// performed for each of the 8 sprites:
    ///
    /// 1. Garbage nametable byte
    /// 2. Garbage nametable byte
    /// 3. Pattern table tile low
    /// 4. Pattern table tile high (+8 bytes from pattern table tile low)
    ///
    /// The garbage fetches occur so that the same circuitry that performs the
    /// BG tile fetches could be reused for the sprite tile fetches.
    ///
    /// If there are less than 8 sprites on the next scanline, then dummy
    /// fetches to tile $FF occur for the left-over sprites, because of the
    /// dummy sprite data in the secondary OAM (see sprite evaluation). This
    /// data is then discarded, and the sprites are loaded with a transparent
    /// set of values instead.
    ///
    /// In addition to this, the X positions and attributes for each sprite are
    /// loaded from the secondary OAM into their respective counters/latches.
    /// This happens during the second garbage nametable fetch, with the
    /// attribute byte loaded during the first tick and the X coordinate during
    /// the second.
    pub const HBLANK_GC: Word = 63;

    /// # Cycles 257-320
    ///
    /// The tile data for the sprites on the next scanline are fetched here.
    /// Again, each memory access takes 2 PPU cycles to complete, and 4 are
    /// performed for each of the 8 sprites:
    ///
    /// 1. Garbage nametable byte
    /// 2. Garbage nametable byte
    /// 3. Pattern table tile low
    /// 4. Pattern table tile high (+8 bytes from pattern table tile low)
    ///
    /// The garbage fetches occur so that the same circuitry that performs the
    /// BG tile fetches could be reused for the sprite tile fetches.
    ///
    /// If there are less than 8 sprites on the next scanline, then dummy
    /// fetches to tile $FF occur for the left-over sprites, because of the
    /// dummy sprite data in the secondary OAM (see sprite evaluation). This
    /// data is then discarded, and the sprites are loaded with a transparent
    /// set of values instead.
    ///
    /// In addition to this, the X positions and attributes for each sprite are
    /// loaded from the secondary OAM into their respective counters/latches.
    /// This happens during the second garbage nametable fetch, with the
    /// attribute byte loaded during the first tick and the X coordinate during
    /// the second.
    pub const HB_GC_LO: Word = 257;

    /// # Cycles 257-320
    ///
    /// The tile data for the sprites on the next scanline are fetched here.
    /// Again, each memory access takes 2 PPU cycles to complete, and 4 are
    /// performed for each of the 8 sprites:
    ///
    /// 1. Garbage nametable byte
    /// 2. Garbage nametable byte
    /// 3. Pattern table tile low
    /// 4. Pattern table tile high (+8 bytes from pattern table tile low)
    ///
    /// The garbage fetches occur so that the same circuitry that performs the
    /// BG tile fetches could be reused for the sprite tile fetches.
    ///
    /// If there are less than 8 sprites on the next scanline, then dummy
    /// fetches to tile $FF occur for the left-over sprites, because of the
    /// dummy sprite data in the secondary OAM (see sprite evaluation). This
    /// data is then discarded, and the sprites are loaded with a transparent
    /// set of values instead.
    ///
    /// In addition to this, the X positions and attributes for each sprite are
    /// loaded from the secondary OAM into their respective counters/latches.
    /// This happens during the second garbage nametable fetch, with the
    /// attribute byte loaded during the first tick and the X coordinate during
    /// the second.
    pub const HB_GC_HI: Word = HB_GC_LO + HBLANK_GC;

    /// # Cycles 321-336
    ///
    /// This is where the first two tiles for the next scanline are fetched, and
    /// loaded into the shift registers. Again, each memory access takes 2 PPU
    /// cycles to complete, and 4 are performed for the two tiles:
    ///
    /// 1. Nametable byte
    /// 2. Attribute table byte
    /// 3. Pattern table tile low
    /// 4. Pattern table tile high (+8 bytes from pattern table tile low)
    pub const HBLANK_FETCH: Word = 15;

    /// # Cycles 321-336
    ///
    /// This is where the first two tiles for the next scanline are fetched, and
    /// loaded into the shift registers. Again, each memory access takes 2 PPU
    /// cycles to complete, and 4 are performed for the two tiles:
    ///
    /// 1. Nametable byte
    /// 2. Attribute table byte
    /// 3. Pattern table tile low
    /// 4. Pattern table tile high (+8 bytes from pattern table tile low)
    pub const HB_FETCH_LO: Word = 321;

    /// # Cycles 321-336
    ///
    /// This is where the first two tiles for the next scanline are fetched, and
    /// loaded into the shift registers. Again, each memory access takes 2 PPU
    /// cycles to complete, and 4 are performed for the two tiles:
    ///
    /// 1. Nametable byte
    /// 2. Attribute table byte
    /// 3. Pattern table tile low
    /// 4. Pattern table tile high (+8 bytes from pattern table tile low)
    pub const HB_FETCH_HI: Word = HB_FETCH_LO + HBLANK_FETCH;

    /// # Cycles 337-340
    ///
    /// Two bytes are fetched, but the purpose for this is unknown. These
    /// fetches are 2 PPU cycles each.
    ///
    /// 1. Nametable byte
    /// 2. Nametable byte
    ///
    /// Both of the bytes fetched here are the same nametable byte that will be
    /// fetched at the beginning of the next scanline (tile 3, in other words).
    /// At least one mapper -- MMC5 -- is known to use this string of three
    /// consecutive nametable fetches to clock a scanline counter.
    pub const HBLANK_FINAL: Word = 3;

    /// # Cycles 337-340
    ///
    /// Two bytes are fetched, but the purpose for this is unknown. These
    /// fetches are 2 PPU cycles each.
    ///
    /// 1. Nametable byte
    /// 2. Nametable byte
    ///
    /// Both of the bytes fetched here are the same nametable byte that will be
    /// fetched at the beginning of the next scanline (tile 3, in other words).
    /// At least one mapper -- MMC5 -- is known to use this string of three
    /// consecutive nametable fetches to clock a scanline counter.
    pub const HB_FINAL_LO: Word = 337;

    /// # Cycles 337-340
    ///
    /// Two bytes are fetched, but the purpose for this is unknown. These
    /// fetches are 2 PPU cycles each.
    ///
    /// 1. Nametable byte
    /// 2. Nametable byte
    ///
    /// Both of the bytes fetched here are the same nametable byte that will be
    /// fetched at the beginning of the next scanline (tile 3, in other words).
    /// At least one mapper -- MMC5 -- is known to use this string of three
    /// consecutive nametable fetches to clock a scanline counter.
    pub const HB_FINAL_HI: Word = HB_FINAL_LO + HBLANK_FINAL;
}

#[allow(unused)]
mod scanlines {
    use crate::Word;

    /// # Pre-render scanline (-1 or 261)
    ///
    /// This is a dummy scanline, whose sole purpose is to fill the shift
    /// registers with the data for the first two tiles of the next scanline.
    /// Although no pixels are rendered for this scanline, the PPU still makes
    /// the same memory accesses it would for a regular scanline, using whatever
    /// the current value of the PPU's V register is, and for the sprite
    /// fetches, whatever data is currently in secondary OAM (e.g., the results
    /// from scanline 239's sprite evaluation from the previous frame).
    ///
    /// This scanline varies in length, depending on whether an even or an odd
    /// frame is being rendered. For odd frames, the cycle at the end of the
    /// scanline is skipped (this is done internally by jumping directly from
    /// (339,261) to (0,0), replacing the idle tick at the beginning of the
    /// first visible scanline with the last tick of the last dummy nametable
    /// fetch). For even frames, the last cycle occurs normally. This is done to
    /// compensate for some shortcomings with the way the PPU physically outputs
    /// its video signal, the end result being a crisper image when the screen
    /// isn't scrolling. However, this behavior can be bypassed by keeping
    /// rendering disabled until after this scanline has passed, which results
    /// in an image with a "dot crawl" effect similar to, but not exactly like,
    /// what's seen in interlaced video.
    ///
    /// During pixels 280 through 304 of this scanline, the vertical scroll bits
    /// are reloaded if rendering is enabled.
    pub const PRE: Word = 261;

    /// # Visible scanlines (0-239)
    ///
    /// These are the visible scanlines, which contain the graphics to be
    /// displayed on the screen. This includes the rendering of both the
    /// background and the sprites. During these scanlines, the PPU is busy
    /// fetching data, so the program should not access PPU memory during this
    /// time, unless rendering is turned off.
    pub const VIS_LO: Word = 0;

    /// # Visible scanlines (0-239)
    ///
    /// These are the visible scanlines, which contain the graphics to be
    /// displayed on the screen. This includes the rendering of both the
    /// background and the sprites. During these scanlines, the PPU is busy
    /// fetching data, so the program should not access PPU memory during this
    /// time, unless rendering is turned off.
    pub const VIS_HI: Word = 239;

    /// # Post-render scanline (240)
    ///
    /// The PPU just idles during this scanline. Even though accessing PPU
    /// memory from the program would be safe here, the VBlank flag isn't set
    /// until after this scanline.
    pub const POST: Word = 240;

    /// Vertical blanking lines (241-260)
    ///
    /// The VBlank flag of the PPU is set at tick 1 (the second tick) of
    /// scanline 241, where the VBlank NMI also occurs. The PPU makes no memory
    /// accesses during these scanlines, so PPU memory can be freely accessed by
    /// the program.
    pub const VBLANK: Word = 19;

    /// Vertical blanking lines (241-260)
    ///
    /// The VBlank flag of the PPU is set at tick 1 (the second tick) of
    /// scanline 241, where the VBlank NMI also occurs. The PPU makes no memory
    /// accesses during these scanlines, so PPU memory can be freely accessed by
    /// the program.
    pub const VB_LO: Word = 241;

    /// Vertical blanking lines (241-260)
    ///
    /// The VBlank flag of the PPU is set at tick 1 (the second tick) of
    /// scanline 241, where the VBlank NMI also occurs. The PPU makes no memory
    /// accesses during these scanlines, so PPU memory can be freely accessed by
    /// the program.
    pub const VB_HI: Word = VB_LO + VBLANK;
}

#[derive(Debug, Clone)]
pub struct Palette(pub Box<[Color; 128 * 128]>);

impl Default for Palette {
    fn default() -> Self {
        Self(Box::new([Color::default(); 128 * 128]))
    }
}

impl Palette {
    pub const WIDTH: usize = 128;
    pub const HEIGHT: usize = 128;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct PpuState {
    pub scanline: u16,
    pub cycle: u16,
}

pub type PpuRef<PpuBus> = Rc<RefCell<Ppu<PpuBus>>>;

const STATIC_COLORS: [Color; 2] = [Color::BLACK, Color::WHITE];

#[derive(Debug)]
pub struct Ppu<PpuBus: RwDevice> {
    bus: Option<PpuBus>,
    reg: Registers,
    state: PpuState,
}

impl<PpuBus: RwDevice> Ppu<PpuBus> {
    pub fn configure_bus(&mut self, bus: PpuBus) {
        self.bus = Some(bus);
    }

    pub fn cur_state(&self) -> PpuState {
        PpuState {
            scanline: self.state.scanline,
            cycle: self.state.cycle,
        }
    }

    pub fn is_vblank(&self) -> bool {
        self.reg.status & Status::V == Status::V
    }
}

impl<PpuBus: RwDevice + ReadOnlyDevice> Ppu<PpuBus> {
    pub fn read_palette(&self, tbl: Word, palette: Word) -> Palette {
        let mut pixels = Palette::default();

        let color_from_palette = |bus: &PpuBus, palette: Word, pixel: Word| -> Color {
            let addr = 0x3F00 + palette.overflowing_shl(2).0 + pixel;
            let _addr = bus.read_only(addr) & 0x3F;
            //self.memory.borrow().col_palette[addr as usize]
            Color::WHITE
        };

        for y in 0..16 as Word {
            for x in 0..16 as Word {
                let offset = y * 256 + x * 16;

                if let Some(bus) = self.bus.as_ref() {
                    for r in 0..8 {
                        let mut lsb = bus.read_only(tbl * 0x1000 + offset + r);
                        let mut msb = bus.read_only(tbl * 0x1000 + offset + r + 8);
                        for c in 0..8 {
                            let pixel = ((lsb & 0x01) + (msb & 0x01)) as Word;
                            lsb >>= 1;
                            msb >>= 1;

                            let x = x * 8 + (7 - c);
                            let y = y * 8 + r;

                            let color = color_from_palette(bus, palette, pixel);

                            let mut set_pixel = |x, y, v| {
                                let x = x as usize;
                                let y = y as usize;
                                let (w, h) = (Palette::WIDTH, Palette::HEIGHT);
                                if x < w && y < h {
                                    pixels.0[(y * w) + x] = v;
                                }
                            };
                            set_pixel(x, y, color);
                        }
                    }
                }
            }
        }

        pixels
    }
}

impl<PpuBus: RwDevice> Default for Ppu<PpuBus> {
    fn default() -> Self {
        Self {
            bus: None,
            reg: Registers::default(),
            state: PpuState::default(),
        }
    }
}

impl<PpuBus: RwDevice> DisplayClocked for Ppu<PpuBus> {
    type Item = PpuState;
    fn clock(&mut self, display: &mut dyn DisplayDevice) -> Option<Self::Item> {
        if self.state.cycle > cycles::HB_FINAL_HI {
            self.state.cycle = 0;
            self.state.scanline += 1;

            if self.state.scanline == scanlines::VB_LO {
                self.reg.status |= Status::V;
                if self.reg.ctrl & Ctrl::V == Ctrl::V {
                    todo!("Should trigger NMI interrupt")
                }
            }

            if self.state.scanline > scanlines::PRE {
                self.state.scanline = 0;
                self.reg.status &= !Status::V;
            }
        }

        if self.state.scanline <= scanlines::VIS_HI {
            let scanline = self.state.scanline;
            let cycle = self.state.cycle;
            // let idx =
            //     (((scanline * WIDTH + cycle) as f32).sin().signum() == -1.0) as usize;
            let idx = scanline & 0x01;
            let idx = ((scanline * cycles::VIS_HI + cycle + idx) & 0x01) as usize;
            display.write(cycle, scanline, STATIC_COLORS[idx]);
        }

        self.state.cycle += 1;

        Some(self.state)
    }
}

impl<PpuBus: RwDevice> RwDevice for Ppu<PpuBus> {}

impl<PpuBus: RwDevice> ReadDevice for Ppu<PpuBus> {
    fn read(&self, addr: Word) -> Byte {
        if let Some(bus) = &self.bus {
            bus.read(addr)
        } else {
            0
        }
    }
}

impl<PpuBus: RwDevice> WriteDevice for Ppu<PpuBus> {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        if let Some(bus) = &mut self.bus {
            bus.write(addr, data)
        } else {
            0
        }
    }
}
