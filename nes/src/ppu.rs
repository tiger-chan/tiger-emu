mod bus;
mod color;
mod color_palette;
mod nametable;
mod palette;
mod pattern;
mod registers;

use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use crate::{
    cpu::Message,
    io::{DisplayDevice, ReadDevice, RwDevice, WriteDevice},
    Byte, DisplayClocked, Word, HI_MASK,
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

fn color_idx<PpuBus: RwDevice>(bus: &PpuBus, palette: Word, pixel: Word) -> usize {
    let addr = 0x3F00 + palette.overflowing_shl(2).0 + pixel;
    (bus.read_only(addr) & 0x3F) as usize
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

#[derive(Debug, Clone)]
pub struct DebugNametable(pub Box<[Byte; 256 * 240]>);

impl Default for DebugNametable {
    fn default() -> Self {
        Self(Box::new([Byte::default(); 256 * 240]))
    }
}

impl DebugNametable {
    pub const WIDTH: usize = 256;
    pub const HEIGHT: usize = 240;
}

#[derive(Debug, Clone)]
pub struct ColorPalette(pub Box<[Color; 64]>);

impl Default for ColorPalette {
    fn default() -> Self {
        Self(Box::new([Color::default(); 64]))
    }
}

impl ColorPalette {
    pub const WIDTH: usize = 4;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct PpuState {
    pub scanline: u16,
    pub cycle: u16,
    pub frame: u64,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct PpuInternalState {
    pub v_ram: Loopy,
    pub tmp_ram: Loopy,
    pub w_fine_x: Byte,

    pub tile_id: Byte,
    pub tile_attr: Byte,
    pub tile_lsb: Byte,
    pub tile_msb: Byte,

    pub attr_lo: Word,
    pub attr_hi: Word,

    pub ptrn_lo: Word,
    pub ptrn_hi: Word,
}

impl PpuInternalState {
    pub fn shift(&mut self, mask: Mask) {
        if mask & Mask::BG != 0 {
            // Shifting background pattern row
            self.ptrn_lo <<= 1;
            self.ptrn_hi <<= 1;

            // Shifting palette attributes by 1
            self.attr_lo <<= 1;
            self.attr_hi <<= 1;
        }
    }

    pub fn load_shifter(&mut self) {
        self.ptrn_lo = (self.ptrn_lo & HI_MASK) | self.tile_lsb as Word;
        self.ptrn_hi = (self.ptrn_hi & HI_MASK) | self.tile_msb as Word;

        self.attr_lo = (self.attr_lo & HI_MASK) | ((self.tile_attr as Word & 0b01) * 0xFF);
        self.attr_hi = (self.attr_hi & HI_MASK) | (((self.tile_attr as Word & 0b10) >> 1) * 0xFF);
    }
}

pub type PpuRef<PpuBus> = Rc<RefCell<Ppu<PpuBus>>>;

#[derive(Debug)]
pub struct Ppu<PpuBus: RwDevice> {
    bus: Option<PpuBus>,
    reg: RefCell<Registers>,
    state: PpuState,
    internal: RefCell<PpuInternalState>,
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

    pub fn reset(&mut self) {
        *self.internal.borrow_mut() = PpuInternalState::default();
        *self.reg.borrow_mut() = Registers::default();
        self.state = PpuState::default();
    }

    pub fn read_palette(&self, tbl: Word, palette: Word) -> Palette {
        let mut pixels = Palette::default();

        let color_from_palette = |bus: &PpuBus, palette: Word, pixel: Word| -> Color {
            self.col_palette[color_idx(bus, palette, pixel)]
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

    pub fn read_nametable(&self, tbl: Word) -> DebugNametable {
        let mut nametable = DebugNametable::default();

        let start = nametable::LO + tbl * nametable::SIZE;
        let start_attr = nametable::ATTR_LO + tbl * nametable::SIZE;

        let get_pixel_color = |bus: &PpuBus, id: Byte, attr: Byte, x: Byte, y: Byte| -> u8 {
            let attr = attr as Word;
            let tile_addr = id as Word * 16;
            let tx = x as Word;
            let ty = y as Word;

            let d_lo = bus.read_only(tile_addr + ty * 2) as Word;
            let d_hi = bus.read_only(tile_addr + ty * 2 + 1) as Word;

            let bit_idx = 7 - tx;

            let p_bits = (((d_hi >> bit_idx) & 0x01) << 1) | ((d_lo >> bit_idx) & 0x01);
            let y_idx = ((y >> 2) << 1) + (x >> 2);
            let y_idx = y_idx & 0x03;
            let p_select = (attr >> y_idx) << 2;

            bus.read_only(0x3F00 + p_select + p_bits) & 0x3F
        };

        if let Some(bus) = self.bus.as_ref() {
            for y in 0..30 {
                for x in 0..32 {
                    let ay = y >> 2;
                    let ax = x >> 2;
                    let cell_ns_addr = start + y * 32 + x;
                    let cell_attr_addr = start_attr + (ay * 8) + ax;

                    let cell_tile_id = bus.read_only(cell_ns_addr); // Read tile ID
                    let cell_attributes = bus.read_only(cell_attr_addr); // Read attribute byte
                    for ty in 0..8 {
                        for tx in 0..8 {
                            let pixel_color =
                                get_pixel_color(bus, cell_tile_id, cell_attributes, tx, ty);
                            let screen_x = x * 8 + tx as Word;
                            let screen_y = y * 8 + ty as Word;
                            let idx = ((screen_y * 256) + screen_x) as usize;
                            nametable.0[idx] = pixel_color;
                        }
                    }
                }
            }
        }

        nametable
    }

    pub fn read_col_palette(&self) -> ColorPalette {
        ColorPalette(Box::new(self.col_palette))
    }
}

impl<PpuBus: RwDevice> Default for Ppu<PpuBus> {
    fn default() -> Self {
        Self {
            bus: None,
            reg: RefCell::new(Registers::default()),
            state: PpuState::default(),
            internal: RefCell::default(),
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
            frame,
        } = &mut self.state;

        if let Some(bus) = self.bus.as_mut() {
            if *scanline == scanlines::VB_LO && *cycle == cycles::VB_SIG {
                self.reg.borrow_mut().status |= Status::V;

                if self.reg.borrow().ctrl & Ctrl::V == Ctrl::V {
                    let _ = bus.signal().send(Message::Nmi);
                }
            }

            let update_tiles = |cycle: u16,
                                bus: &mut PpuBus,
                                mut internal: RefMut<PpuInternalState>,
                                reg: Ref<Registers>| {
                match cycle & 0x07 {
                    0 => internal.v_ram.inc_x(reg.mask),
                    1 => {
                        internal.load_shifter();

                        let addr = internal.v_ram.nt();
                        internal.tile_id = bus.read(addr);
                    }
                    3 => {
                        let addr = internal.v_ram.attr();
                        internal.tile_attr = bus.read(addr);

                        let v_ram = internal.v_ram;
                        if (v_ram.coarse_y() & 0x02) > 0 {
                            internal.tile_attr >>= 4
                        };
                        if (v_ram.coarse_x() & 0x02) > 0 {
                            internal.tile_attr >>= 2
                        };
                        internal.tile_attr &= 0x03;
                    }
                    5 => {
                        let ctrl = reg.ctrl;
                        let bg = ((ctrl & Ctrl::B) == Ctrl::B) as Word * 0x1000;
                        let tile = (internal.tile_id as Word) << 4;
                        let fine_y = internal.v_ram.fine_y();

                        internal.tile_lsb = bus.read(bg + tile + fine_y);
                    }
                    7 => {
                        let ctrl = reg.ctrl;
                        let bg = ((ctrl & Ctrl::B) == Ctrl::B) as Word * 0x1000;
                        let tile = (internal.tile_id as Word) << 4;
                        let fine_y = internal.v_ram.fine_y() + 8;

                        internal.tile_lsb = bus.read(bg + tile + fine_y);
                    }

                    _ => {}
                }
            };

            let calc_pixel = |internal: Ref<PpuInternalState>| {
                let mux = 0x8000 >> (internal.w_fine_x & !Loopy::W);
                let p0 = ((internal.ptrn_lo & mux) > 0) as Word;
                let p1 = ((internal.ptrn_hi & mux) > 0) as Word;
                let bg_pixel = (p1 << 1) | p0;

                let p0 = ((internal.attr_lo & mux) > 0) as Word;
                let p1 = ((internal.attr_hi & mux) > 0) as Word;
                let bg_palette = (p1 << 1) | p0;

                (bg_pixel, bg_palette)
            };

            match *scanline {
                scanlines::VIS_LO..=scanlines::VIS_HI => {
                    let cycle = *cycle;
                    match cycle {
                        cycles::VIS_LO..=cycles::VIS_HI => {
                            self.internal.borrow_mut().shift(self.reg.borrow().mask);

                            // Draw to display
                            let mask = self.reg.borrow().mask;
                            if mask & Mask::BG == Mask::BG {
                                let (pixel, palette) = calc_pixel(self.internal.borrow());

                                let idx = color_idx(bus, palette, pixel);
                                let mask = if (mask & Mask::GRAY) == Mask::GRAY {
                                    0x30
                                } else {
                                    0x3F
                                };

                                let color = self.col_palette[idx & mask];

                                display.write(cycle, *scanline, color);
                            }

                            // load tiles
                            update_tiles(cycle, bus, self.internal.borrow_mut(), self.reg.borrow());

                            if cycle == cycles::VIS_HI {
                                self.internal
                                    .borrow_mut()
                                    .v_ram
                                    .inc_y(self.reg.borrow().mask);
                            }
                        }
                        cycles::HB_GC_LO => {
                            self.internal.borrow_mut().load_shifter();

                            let tmp_ram = self.internal.borrow().tmp_ram;
                            self.internal
                                .borrow_mut()
                                .v_ram
                                .copy_x(tmp_ram, self.reg.borrow().mask);

                            // TODO Evaluate sprites
                        }
                        cycles::HB_FETCH_LO..=cycles::HB_FETCH_HI => {
                            // Fetch next 2 scanline tiles

                            update_tiles(cycle, bus, self.internal.borrow_mut(), self.reg.borrow());

                            if cycle & 0x07 == 0 {
                                self.internal
                                    .borrow_mut()
                                    .v_ram
                                    .inc_x(self.reg.borrow().mask);
                            }
                        }
                        cycles::HB_FINAL_LO..=cycles::HB_FINAL_HI => {
                            if cycle & 0x01 == 0x01 {
                                // Unused nametable fetches
                                let nt = self.internal.borrow().v_ram.nt();
                                self.internal.borrow_mut().tile_id = bus.read(nt);
                            }
                        }
                        _ => {}
                    }
                }
                scanlines::PRE => {
                    let cycle = *cycle;
                    match cycle {
                        cycles::VB_SIG => {
                            self.reg.borrow_mut().status &= !Status::V;
                        }
                        cycles::VIS_LO..=cycles::VIS_HI => {
                            update_tiles(cycle, bus, self.internal.borrow_mut(), self.reg.borrow());

                            if cycle & 0x07 == 0 {
                                self.internal
                                    .borrow_mut()
                                    .v_ram
                                    .inc_x(self.reg.borrow().mask);
                            }

                            if cycle == cycles::VIS_HI {
                                self.internal
                                    .borrow_mut()
                                    .v_ram
                                    .inc_y(self.reg.borrow().mask);
                            }
                        }
                        cycles::HB_GC_LO => {
                            self.internal.borrow_mut().load_shifter();

                            let tmp_ram = self.internal.borrow().tmp_ram;
                            self.internal
                                .borrow_mut()
                                .v_ram
                                .copy_x(tmp_ram, self.reg.borrow().mask);
                        }
                        280..=304 => {
                            let tmp_ram = self.internal.borrow().tmp_ram;
                            self.internal
                                .borrow_mut()
                                .v_ram
                                .copy_y(tmp_ram, self.reg.borrow().mask);
                        }
                        cycles::HB_FETCH_LO..=cycles::HB_FETCH_HI => {
                            // Fetch next 2 scanline tiles

                            update_tiles(cycle, bus, self.internal.borrow_mut(), self.reg.borrow());

                            if cycle & 0x07 == 0 {
                                self.internal
                                    .borrow_mut()
                                    .v_ram
                                    .inc_x(self.reg.borrow().mask);
                            }
                        }
                        cycles::HB_FINAL_LO..=cycles::HB_FINAL_HI => {
                            if cycle & 0x01 == 0x01 {
                                // Unused nametable fetches
                                let nt = self.internal.borrow().v_ram.nt();
                                self.internal.borrow_mut().tile_id = bus.read(nt);
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        *cycle += 1;
        if *cycle > cycles::HB_FINAL_HI {
            *cycle = 0;
            *scanline += 1;
        }

        if *scanline > scanlines::PRE {
            *frame = frame.wrapping_add(1);
            *scanline = 0;
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
                    self.internal.borrow_mut().w_fine_x &= !Loopy::W;

                    tmp
                }
                0x0007 => {
                    let mut tmp = self.reg.borrow().data_buffer;
                    let addr = self.internal.borrow().v_ram.into();
                    self.reg.borrow_mut().data_buffer = bus.read(addr);

                    if addr > 0x3F00 {
                        tmp = self.reg.borrow().data_buffer;
                    }

                    let new_addr = self.reg.borrow().ctrl.increment(addr);
                    self.internal.borrow_mut().v_ram = new_addr.into();

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
                    let new = Ctrl::new(data);
                    self.reg.borrow_mut().ctrl = new;

                    let mut tmp_ram = self.internal.borrow().tmp_ram;
                    tmp_ram.set_nt_select(data as Word);
                    self.internal.borrow_mut().tmp_ram = tmp_ram;
                    tmp
                }
                0x0001 => {
                    let tmp = self.reg.borrow().mask.into();
                    self.reg.borrow_mut().mask = Mask::new(data);
                    tmp
                }
                0x0005 => {
                    let mut tmp_ram = self.internal.borrow().tmp_ram;
                    let mut w_fine_x = self.internal.borrow().w_fine_x;
                    if w_fine_x & Loopy::W == 0 {
                        tmp_ram = (tmp_ram & !Loopy::COARSE_X) | Loopy::from((data >> 3) as Word);
                        w_fine_x = Loopy::W | (data & Loopy::FINE_X);
                    } else {
                        let d = data as Word;
                        let (fine_y, course_y): (Word, Word) =
                            (d & Loopy::FINE_Y, (d & Loopy::COARSE_Y) >> 3);
                        tmp_ram.set_fine_y(fine_y);
                        tmp_ram.set_coarse_y(course_y);
                        w_fine_x &= !Loopy::W;
                    }

                    self.internal.borrow_mut().tmp_ram = tmp_ram;
                    self.internal.borrow_mut().w_fine_x = w_fine_x;

                    0
                }
                0x0006 => {
                    let mut tmp_ram = self.internal.borrow().tmp_ram;
                    let mut w_fine_x = self.internal.borrow().w_fine_x;
                    if w_fine_x & Loopy::W == 0 {
                        let d = ((data & 0x3F) as Word) << 8;
                        tmp_ram = (tmp_ram & !Loopy::from(0x3F00)) | Loopy::from(d);
                        w_fine_x |= Loopy::W;
                    } else {
                        tmp_ram = (tmp_ram & Loopy::from(0xFF00)) | Loopy::from(data as Word);
                        w_fine_x &= !Loopy::W;
                        self.internal.borrow_mut().v_ram = tmp_ram;
                    }

                    self.internal.borrow_mut().tmp_ram = tmp_ram;
                    self.internal.borrow_mut().w_fine_x = w_fine_x;

                    0
                }
                0x0007 => {
                    let ppu_addr = self.internal.borrow().v_ram.into();
                    let tmp = bus.write(ppu_addr, data);

                    let new_addr = self.reg.borrow().ctrl.increment(ppu_addr);
                    self.internal.borrow_mut().v_ram = new_addr.into();
                    tmp
                }
                _ => 0,
            }
        } else {
            0
        }
    }
}
