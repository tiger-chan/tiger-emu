use crate::{
    registers::{bit_and, bit_or, bit_xor, display, not, partial_eq, reg_add_impl, reg_from_impl},
    Byte, Word, HI_MASK, LO_MASK,
};
use core::fmt;
use std::ops::{Add, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

use super::nametable;

/// # Summary
/// ```text
/// Common Name   Address   Bits         Notes
/// PPUCTRL       $2000     VPHB SINN    NMI enable (V), PPU master/slave (P),
///                                      sprite height (H), background tile
///                                      select (B), sprite tile select (S),
///                                      increment mode (I), nametable select (NN)
/// PPUMASK       $2001     BGRs bMmG    color emphasis (BGR), sprite enable (s),
///                                      background enable (b), sprite left column
///                                      enable (M), background left column enable
///                                      (m), greyscale (G)
/// PPUSTATUS     $2002     VSO- ----    vblank (V), sprite 0 hit (S), sprite
///                                      overflow (O); read resets write pair
///                                      for $2005/$2006
/// OAMADDR       $2003     aaaa aaaa    OAM read/write address
/// OAMDATA       $2004     dddd dddd    OAM data read/write
/// PPUSCROLL     $2005     xxxx xxxx    fine scroll position (two writes: X
///                                      scroll, Y scroll)
/// PPUADDR       $2006     aaaa aaaa    PPU read/write address (two writes: most
///                                      significant byte, least significant byte)
/// PPUDATA       $2007     dddd dddd    PPU data read/write
/// OAMDMA        $4014     aaaa aaaa    OAM DMA high address
///```
#[derive(Default, Debug)]
pub struct Registers {
    /// # PPUCTRL
    ///
    /// ## Address
    /// $2000
    ///
    /// ## Bits
    /// VPHB SINN
    ///
    /// > NMI enable (V), PPU master/slave (P), sprite height (H), background
    /// tile select (B), sprite tile select (S), increment mode (I), nametable
    /// select (NN)
    pub ctrl: Ctrl,
    /// # PPUMASK
    ///
    /// ## Address
    /// $2001
    ///
    /// ## Bits
    /// BGRs bMmG
    ///
    /// > color emphasis (BGR), sprite enable (s), background enable (b), sprite
    /// left column enable (M), background left column enable (m), greyscale (G)
    pub mask: Mask,
    /// # PPUSTATUS
    ///
    /// ## Address
    /// $2002
    ///
    /// ## Bits
    /// VSO- ----
    ///
    /// > vblank (V), sprite 0 hit (S), sprite overflow (O); read resets write
    /// pair for $2005/$2006
    pub status: Status,

    pub data_buffer: Byte,
}

/// # Controller ($2000) > write
/// - Common name: PPUCTRL
/// - Description: PPU control register
/// - Access: write
///
/// Various flags controlling PPU operation
/// ```text
/// 7  bit  0
/// ---- ----
/// VPHB SINN
/// |||| ||||
/// |||| ||++- Base nametable address
/// |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
/// |||| |+--- VRAM address increment per CPU read/write of PPUDATA
/// |||| |     (0: add 1, going across; 1: add 32, going down)
/// |||| +---- Sprite pattern table address for 8x8 sprites
/// ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
/// |||+------ Background pattern table address (0: $0000; 1: $1000)
/// ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
/// |+-------- PPU master/slave select
/// |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
/// +--------- Generate an NMI at the start of the
///            vertical blanking interval (0: off; 1: on)
/// ```
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Ctrl(Byte);

impl Ctrl {
    pub const fn new(val: Byte) -> Self {
        Self(val)
    }

    pub fn set(&mut self, f: Self, v: bool) -> &mut Self {
        match v {
            true => self.bitor_assign(f),
            false => self.bitand_assign(!f),
        }
        self
    }

    pub fn get(&self, f: Self) -> Self {
        Self(self.0 & f.0)
    }

    pub fn increment(&self, val: Word) -> Word {
        if *self & Self::I == Self::I {
            val.wrapping_add(32)
        } else {
            val.wrapping_add(1)
        }
    }

    /// Add 256 to the X scroll position
    pub const X: Ctrl = Ctrl::new(1 << 0);

    /// Add 240 to the Y scroll position
    pub const Y: Ctrl = Ctrl::new(1 << 1);

    /// VRAM address increment per CPU read/write of PPUDATA
    /// (0: add 1, going across; 1: add 32, going down)
    pub const I: Ctrl = Ctrl::new(1 << 2);

    /// Sprite pattern table address for 8x8 sprites
    /// (0: $0000; 1: $1000; ignored in 8x16 mode)
    pub const S: Ctrl = Ctrl::new(1 << 3);

    /// Background pattern table address (0: $0000; 1: $1000)
    pub const B: Ctrl = Ctrl::new(1 << 4);

    /// Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
    pub const H: Ctrl = Ctrl::new(1 << 5);

    /// PPU master/slave select
    /// (0: read backdrop from EXT pins; 1: output color on EXT pins)
    pub const P: Ctrl = Ctrl::new(1 << 6);

    /// Generate an NMI at the start of the
    /// vertical blanking interval (0: off; 1: on)
    pub const V: Ctrl = Ctrl::new(1 << 7);
}

not!(Ctrl);
reg_from_impl!(Ctrl, Byte);
reg_add_impl!(Ctrl, Byte);
bit_or!(Ctrl, Byte);
bit_and!(Ctrl, Byte);
bit_xor!(Ctrl, Byte);
partial_eq!(Ctrl, Byte);

reg_from_impl!(Ctrl, u16);
reg_add_impl!(Ctrl, u16);
display!(Ctrl [V, P, H, B, S, I, Y, X]);

/// # Mask ($2001) > write
/// - Common name: PPUMASK
/// - Description: PPU mask register
/// - Access: write
///
/// This register controls the rendering of sprites and backgrounds, as well as colour effects.
/// ```text
/// 7  bit  0
/// ---- ----
/// BGRs bMmG
/// |||| ||||
/// |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
/// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
/// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
/// |||| +---- 1: Show background
/// |||+------ 1: Show sprites
/// ||+------- Emphasize red (green on PAL/Dendy)
/// |+-------- Emphasize green (red on PAL/Dendy)
/// +--------- Emphasize blue
/// ```
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Mask(Byte);

impl Mask {
    pub const fn new(val: Byte) -> Self {
        Self(val)
    }

    pub fn set(&mut self, f: Self, v: bool) -> &mut Self {
        match v {
            true => self.bitor_assign(f),
            false => self.bitand_assign(!f),
        }
        self
    }

    pub fn get(&self, f: Self) -> Self {
        Self(self.0 & f.0)
    }

    /// Greyscale (0: normal color, 1: produce a greyscale display)
    pub const GRAY: Self = Self::new(1 << 0);

    /// 1: Show background in leftmost 8 pixels of screen, 0: Hide
    pub const LBG: Self = Self::new(1 << 1);

    /// 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    pub const LSPR: Self = Self::new(1 << 2);

    /// 1: Show background
    pub const BG: Self = Self::new(1 << 3);

    /// 1: Show sprites
    pub const SPR: Self = Self::new(1 << 4);

    /// Emphasize red (green on PAL/Dendy)
    pub const R: Self = Self::new(1 << 5);

    /// Emphasize green (red on PAL/Dendy)
    pub const G: Self = Self::new(1 << 6);

    /// Emphasize blue
    pub const B: Self = Self::new(1 << 7);
}

not!(Mask);
reg_from_impl!(Mask, Byte);
reg_add_impl!(Mask, Byte);
bit_or!(Mask, Byte);
bit_and!(Mask, Byte);
bit_xor!(Mask, Byte);
partial_eq!(Mask, Byte);

reg_from_impl!(Mask, u16);
reg_add_impl!(Mask, u16);
display!(Mask [B, G, R, SPR, BG, LSPR, LBG, GRAY], [B, G, R, s, b, M, m, G]);

/// # Status ($2002) < read
/// - Common name: PPUSTATUS
/// - Description: PPU status register
/// - Access: read
///
/// This register reflects the state of various functions inside the PPU. It is
/// often used for determining timing. To determine when the PPU has reached a
/// given pixel of the screen, put an opaque (non-transparent) pixel of sprite
/// 0 there.
///
/// ```text
/// 7  bit  0
/// ---- ----
/// VSO. ....
/// |||| ||||
/// |||+-++++- PPU open bus. Returns stale PPU bus contents.
/// ||+------- Sprite overflow. The intent was for this flag to be set
/// ||         whenever more than eight sprites appear on a scanline, but a
/// ||         hardware bug causes the actual behavior to be more complicated
/// ||         and generate false positives as well as false negatives; see
/// ||         PPU sprite evaluation. This flag is set during sprite
/// ||         evaluation and cleared at dot 1 (the second dot) of the
/// ||         pre-render line.
/// |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
/// |          a nonzero background pixel; cleared at dot 1 of the pre-render
/// |          line.  Used for raster timing.
/// +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
///            Set at dot 1 of line 241 (the line *after* the post-render
///            line); cleared after reading $2002 and at dot 1 of the
///            pre-render line.
/// ```
///
/// ## Notes:
/// - Reading the status register will clear bit 7 mentioned above and also the
/// address latch used by PPUSCROLL and PPUADDR. It does not clear the sprite 0
/// hit or overflow bit.
/// - Once the sprite 0 hit flag is set, it will not be cleared until the end of
/// the next vertical blank. If attempting to use this flag for raster timing,
/// it is important to ensure that the sprite 0 hit check happens outside of
/// vertical blank, otherwise the CPU will "leak" through and the check will
/// fail. The easiest way to do this is to place an earlier check for bit 6 = 0,
/// which will wait for the pre-render scanline to begin.
/// - If using sprite 0 hit to make a bottom scroll bar below a vertically
/// scrolling or freely scrolling playfield, be careful to ensure that the tile
/// in the playfield behind sprite 0 is opaque.
/// - Sprite 0 hit is not detected at x=255, nor is it detected at x=0 through 7
/// if the background or sprites are hidden in this area.
/// - Race Condition Warning: Reading PPUSTATUS within two cycles of the start
/// of vertical blank will return 0 in bit 7 but clear the latch anyway, causing
/// NMI to not occur that frame. See NMI and PPU_frame_timing for details.
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Status(Byte);

impl Status {
    pub const fn new(val: Byte) -> Self {
        Self(val)
    }

    pub fn set(&mut self, f: Self, v: bool) -> &mut Self {
        match v {
            true => self.bitor_assign(f),
            false => self.bitand_assign(!f),
        }
        self
    }

    pub fn get(&self, f: Self) -> Self {
        Self(self.0 & f.0)
    }

    /// PPU open bus. Returns stale PPU bus contents.
    pub const U: Self = Self::new(0b00011111);
    /// Sprite overflow. The intent was for this flag to be set
    /// whenever more than eight sprites appear on a scanline, but a
    /// hardware bug causes the actual behavior to be more complicated
    /// and generate false positives as well as false negatives; see
    /// PPU sprite evaluation. This flag is set during sprite
    /// evaluation and cleared at dot 1 (the second dot) of the
    /// pre-render line.
    pub const O: Self = Self::new(1 << 5);
    /// Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
    /// a nonzero background pixel; cleared at dot 1 of the pre-render
    /// line.  Used for raster timing.
    pub const S: Self = Self::new(1 << 6);
    /// Vertical blank has started (0: not in vblank; 1: in vblank).
    /// Set at dot 1 of line 241 (the line *after* the post-render
    /// line); cleared after reading $2002 and at dot 1 of the
    /// pre-render line.
    pub const V: Self = Self::new(1 << 7);
}

not!(Status);
reg_from_impl!(Status, Byte);
reg_add_impl!(Status, Byte);
bit_or!(Status, Byte);
bit_and!(Status, Byte);
bit_xor!(Status, Byte);
partial_eq!(Status, Byte);

reg_from_impl!(Status, u16);
reg_add_impl!(Status, u16);
display!(Status [V, S, O, U, U, U, U, U], [V, S, O, -, -, -, -, -]);

/// https://www.nesdev.org/wiki/PPU_scrolling
///
///```text
/// yyy NN YYYYY XXXXX
/// ||| || ||||| +++++-- coarse X scroll
/// ||| || +++++-------- coarse Y scroll
/// ||| ++-------------- nametable select
/// +++----------------- fine Y scroll
/// ```
#[derive(Debug, Default, Clone, Copy)]
pub struct Loopy(Word);

impl Loopy {
    pub fn set_nt_select(&mut self, val: Word) {
        let val = (val << Self::NT_LSH) & Self::NT_SELECT;
        self.0 = self.0 & !Self::NT_SELECT | val;
    }

    pub fn set_coarse_x(&mut self, val: Word) {
        self.0 = (self.0 & !Self::COARSE_X) | val & Self::COARSE_X;
    }

    pub fn coarse_x(&self) -> Word {
        self.0 & Self::COARSE_X
    }

    pub fn set_coarse_y(&mut self, val: Word) {
        let val = (val << Self::COARSE_Y_LSH) & Self::COARSE_Y;
        self.0 = (self.0 & !Self::COARSE_Y) | val;
    }

    pub fn coarse_y(&self) -> Word {
        (self.0 & Self::COARSE_Y) >> Self::COARSE_Y_LSH
    }

    pub fn set_fine_y(&mut self, val: Word) {
        let val = (val << Self::FINE_Y_LSH) & Self::FINE_Y;
        self.0 = (self.0 & !Self::FINE_Y) | val;
    }

    pub fn fine_y(&self) -> Word {
        (self.0 & Self::FINE_Y) >> Self::FINE_Y_LSH
    }

    pub fn inc_x(&mut self, mask: Mask) {
        if mask & (Mask::BG | Mask::SPR) != 0 {
            let coarse_x = self.0 & Self::COARSE_X;
            if coarse_x == 31 {
                self.0 &= !Self::COARSE_X;
                self.0 ^= Self::NT_X;
            } else {
                // Incrementing is fine since we know we aren't going to
                // overflow in to coarse x
                self.0 += 1;
            }
        }
    }

    pub fn inc_y(&mut self, mask: Mask) {
        if mask & (Mask::BG | Mask::SPR) != 0 {
            if self.0 & Self::FINE_Y != Self::FINE_Y {
                // Reset fine y (since we are reseting or incrementing)
                self.0 += 0x1000;
            } else {
                self.0 &= !Self::FINE_Y;
                let mut coarse_y = (self.0 & Self::COARSE_Y) >> Self::COARSE_Y_LSH;
                match coarse_y {
                    29 => {
                        coarse_y = 0;
                        self.0 ^= Self::NT_Y;
                    }
                    31 => {
                        // In case the pointer is in the attribute memory, we
                        // just wrap around the current nametable
                        coarse_y = 0;
                    }
                    _ => {
                        // We can just increment since there is nothing special
                        // going on
                        coarse_y += 1;
                    }
                }
                self.0 &= !Self::COARSE_Y;
                coarse_y <<= Self::COARSE_Y_LSH;
                self.0 |= coarse_y;
            }
        }
    }

    /// https://www.nesdev.org/wiki/PPU_scrolling#At_dot_257_of_each_scanline
    pub fn copy_x(&mut self, tmp: Self, mask: Mask) {
        if mask & (Mask::BG | Mask::SPR) != 0 {
            // NT_X | COARSE_X;
            const X_MASK: Word = 0x0400 | 0x001F;
            self.0 &= !X_MASK;
            self.0 |= tmp.0 & X_MASK;
        }
    }

    /// https://www.nesdev.org/wiki/PPU_scrolling#During_dots_280_to_304_of_the_pre-render_scanline_(end_of_vblank)
    pub fn copy_y(&mut self, tmp: Self, mask: Mask) {
        if mask & (Mask::BG | Mask::SPR) != 0 {
            // FINE_Y | NT_Y | COARSE_Y;
            const Y_MASK: Word = 0x7000 | 0x0800 | 0x03E0;
            self.0 &= !Y_MASK;
            self.0 |= tmp.0 & Y_MASK;
        }
    }

    /// # $2006 first write (w is 0)
    ///```text
    /// t: .CDEFGH ........ <- d: ..CDEFGH
    ///        <unused>     <- d: AB......
    /// t: Z...... ........ <- 0 (bit Z is cleared)
    /// w:                  <- 1
    /// ```
    pub fn write_addr_lo(&mut self, data: Byte) {
        let d = data & 0x3F;
        let tmp = (d as Word) << 8;
        self.0 &= LO_MASK;
        self.0 |= tmp;
    }

    /// # $2006 second write (w is 1)
    ///```text
    /// t: ....... ABCDEFGH <- d: ABCDEFGH
    /// v: <...all bits...> <- t: <...all bits...>
    /// w:                  <- 0
    /// ```
    pub fn write_addr_hi(&mut self, data: Byte) {
        self.0 &= HI_MASK;
        self.0 |= data as Word;
    }

    /// https://www.nesdev.org/wiki/PPU_scrolling#Tile_and_attribute_fetching
    pub fn nt(&self) -> Word {
        nametable::LO | (self.0 & 0x0FFF)
    }

    /// https://www.nesdev.org/wiki/PPU_scrolling#Tile_and_attribute_fetching
    pub fn attr(&self) -> Word {
        const COARSE_Y: Word = 0x38;
        nametable::ATTR_LO
            | (self.0 & Self::NT_SELECT)
            | ((self.0 >> 4) & COARSE_Y)
            | ((self.0 >> 2) & Self::FINE_X as Word)
    }

    pub const COARSE_X: Word = 0x001F;

    pub const COARSE_Y: Word = 0x03E0;

    pub const COARSE_Y_LSH: Word = 5;

    pub const NT_X: Word = 0x0400;

    pub const NT_Y: Word = 0x0800;

    pub const NT_SELECT: Word = 0x0C00;

    pub const NT_LSH: Word = 10;

    pub const FINE_Y: Word = 0x7000;

    pub const FINE_Y_LSH: Word = 12;

    pub const W: Byte = 0x80;

    pub const FINE_X: Byte = 0x07;

    pub const FINE_N: Byte = 0x07;
}

not!(Loopy);
reg_from_impl!(Loopy, Word, Word);
reg_add_impl!(Loopy, Word);
bit_or!(Loopy, Word);
bit_and!(Loopy, Word);
bit_xor!(Loopy, Word);
partial_eq!(Loopy, Word);
