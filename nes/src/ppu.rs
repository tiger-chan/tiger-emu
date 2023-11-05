mod bus;
mod color;
mod color_palette;
mod nametable;
mod palette;
mod pattern;
mod registers;

use std::{cell::RefCell, rc::Rc};

use crate::{
    cpu::Message,
    io::{DisplayDevice, ReadDevice, RwDevice, WriteDevice},
    Byte, DisplayClocked, Word,
};

use self::{
    bus::CpuSignal,
    color_palette::{create_palette, X2C02, X2C07},
};

pub use bus::Bus;
pub use color::Color;
pub use nametable::NameTable;
pub use registers::*;

/// ```text
/// Address range    Size      Device
/// $2000–$2007     $0008      NES PPU registers
/// $2008–$3FFF     $1FF8      Mirrors of $2000–$2007 (repeats every 8 bytes)
/// ```
pub const REG_LO: Word = 0x2000;

/// ```text
/// Address range    Size      Device
/// $2000–$2007     $0008      NES PPU registers
/// $2008–$3FFF     $1FF8      Mirrors of $2000–$2007 (repeats every 8 bytes)
/// ```
pub const REG_HI: Word = 0x3FFF;

/// ```text
/// Address range    Size      Device
/// $2000–$2007     $0008      NES PPU registers
/// $2008–$3FFF     $1FF8      Mirrors of $2000–$2007 (repeats every 8 bytes)
/// ```
pub const REG_MASK: Word = 0x0007;

pub const WIDTH: Word = 256;
pub const HEIGHT: Word = 240;

#[allow(unused)]
mod cycles {
    use crate::Word;

    /// Vertical blanking lines (241-260)
    ///
    /// The VBlank flag of the PPU is set at tick 1 (the second tick) of
    /// scanline 241, where the VBlank NMI also occurs. The PPU makes no memory
    /// accesses during these scanlines, so PPU memory can be freely accessed by
    /// the program.
    pub const VB_SIG: Word = 1;

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

#[derive(Debug, Clone, Copy)]
pub struct PpuState {
    pub scanline: u16,
    pub cycle: u16,
    pub even_frame: bool,
}

impl Default for PpuState {
    fn default() -> Self {
        Self {
            scanline: 0,
            cycle: 0,
            even_frame: true,
        }
    }
}

pub type PpuRef<PpuBus> = Rc<RefCell<Ppu<PpuBus>>>;

#[derive(Debug)]
pub struct Ppu<PpuBus: RwDevice> {
    bus: Option<PpuBus>,
    reg: RefCell<Registers>,
    state: PpuState,
    col_palette: [Color; 64],
}

impl<PpuBus: RwDevice> Ppu<PpuBus> {
    pub fn configure_bus(&mut self, bus: PpuBus) {
        self.bus = Some(bus);
    }

    pub fn cur_state(&self) -> PpuState {
        self.state
    }

    pub fn is_vblank(&self) -> bool {
        self.reg.borrow().status & Status::V == Status::V
    }

    #[allow(unused)]
    pub fn set_palette_ntsc(&mut self) {
        self.col_palette = create_palette(X2C02);
    }

    #[allow(unused)]
    pub fn set_palette_pal(&mut self) {
        self.col_palette = create_palette(X2C07);
    }

    pub fn read_palette(&self, tbl: Word, palette: Word) -> Palette {
        let mut pixels = Palette::default();

        let color_from_palette = |bus: &PpuBus, palette: Word, pixel: Word| -> Color {
            let addr = 0x3F00 + palette.overflowing_shl(2).0 + pixel;
            let col_addr = bus.read_only(addr) & 0x3F;
            self.col_palette[col_addr as usize]
        };

        let mut set_pixel = |x, y, v| {
            let x = x as usize;
            let y = y as usize;
            let (w, h) = (Palette::WIDTH, Palette::HEIGHT);
            if x < w && y < h {
                pixels.0[(y * w) + x] = v;
            }
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
            reg: RefCell::new(Registers::default()),
            state: PpuState::default(),
            col_palette: create_palette(X2C02),
        }
    }
}

impl<PpuBus: RwDevice + CpuSignal> DisplayClocked for Ppu<PpuBus> {
    type Item = PpuState;
    fn clock(&mut self, display: &mut dyn DisplayDevice) -> Option<Self::Item> {
        let PpuState {
            scanline,
            cycle,
            even_frame,
        } = &mut self.state;

        if let Some(bus) = self.bus.as_mut() {
            if *scanline == scanlines::VB_LO && *cycle == cycles::VB_SIG {
                self.reg.borrow_mut().status |= Status::V;
                if self.reg.borrow().ctrl & Ctrl::V == Ctrl::V {
                    let _ = bus.signal().send(Message::Nmi);
                }
            }

            if *scanline <= scanlines::VIS_HI {
                // let idx =
                //     (((scanline * WIDTH + cycle) as f32).sin().signum() == -1.0) as usize;

                // let idx = *scanline & 0x01;
                // let idx = ((*scanline * cycles::VIS_HI + *cycle + idx) & 0x01) as usize;

                let idx = if rand::random() { 0x3F } else { 0x30 };
                display.write(*cycle, *scanline, self.col_palette[idx]);
            }
        }

        *cycle += 1;
        if *cycle > cycles::HB_FINAL_HI {
            *cycle = 0;
            *scanline += 1;
        }

        if *scanline > scanlines::PRE {
            *even_frame = !*even_frame;
            *scanline = 0;
            self.reg.borrow_mut().status &= !Status::V;
        }

        Some(self.state)
    }
}

impl<PpuBus: RwDevice> RwDevice for Ppu<PpuBus> {}

impl<PpuBus: RwDevice> ReadDevice for Ppu<PpuBus> {
    fn read(&self, addr: Word) -> Byte {
        if let Some(bus) = &self.bus {
            let masked = addr & REG_MASK;
            match masked {
                0x0002 => {
                    let tmp =
                        Byte::from(self.reg.borrow().status & (Status::V | Status::S | Status::O));

                    let tmp = tmp | self.reg.borrow().data_buffer & 0x1F;
                    self.reg.borrow_mut().status.set(Status::V, false);
                    self.reg.borrow_mut().addr_latch = 0;

                    tmp
                }
                0x0007 => {
                    let mut tmp = self.reg.borrow().data_buffer;
                    let addr = self.reg.borrow().addr_lo | self.reg.borrow().addr_hi;
                    self.reg.borrow_mut().data_buffer = bus.read(addr);

                    if addr > 0x3F00 {
                        tmp = self.reg.borrow().data_buffer;
                    }
                    let new_addr = addr.wrapping_add(1);
                    self.reg.borrow_mut().addr_lo = new_addr & 0x00FF;
                    self.reg.borrow_mut().addr_hi = new_addr & 0xFF00;

                    tmp
                }
                _ => 0,
            }
        } else {
            0
        }
    }

    fn read_only(&self, addr: Word) -> Byte {
        let masked = addr & REG_MASK;
        match masked {
            0x0000 => self.reg.borrow().ctrl.into(),
            0x0001 => self.reg.borrow().mask.into(),
            0x0002 => self.reg.borrow().status.into(),
            _ => 0,
        }
    }
}

impl<PpuBus: RwDevice> WriteDevice for Ppu<PpuBus> {
    fn write(&mut self, addr: Word, data: Byte) -> Byte {
        let masked = addr & REG_MASK;
        if let Some(bus) = &mut self.bus {
            match masked {
                0x0000 => {
                    let tmp = self.reg.borrow().ctrl.into();
                    self.reg.borrow_mut().ctrl = Ctrl::new(data);
                    tmp
                }
                0x0001 => {
                    let tmp = self.reg.borrow().mask.into();
                    self.reg.borrow_mut().mask = Mask::new(data);
                    tmp
                }
                0x0006 => {
                    if self.reg.borrow().addr_latch == 0 {
                        self.reg.borrow_mut().addr_hi = (data as Word) << 8;
                        self.reg.borrow_mut().addr_latch = 1;
                    } else {
                        self.reg.borrow_mut().addr_lo = data as Word;
                        self.reg.borrow_mut().addr_latch = 0;
                    }

                    0
                }
                0x0007 => {
                    let ppu_addr = self.reg.borrow().addr_lo | self.reg.borrow().addr_hi;
                    let tmp = bus.write(ppu_addr, data);

                    let new_addr = ppu_addr.wrapping_add(1);
                    self.reg.borrow_mut().addr_hi = new_addr & 0xFF00;
                    self.reg.borrow_mut().addr_lo = new_addr & 0x00FF;
                    tmp
                }
                _ => 0,
            }
        } else {
            0
        }
    }
}
