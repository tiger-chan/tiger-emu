use core::fmt;
use std::ops::{Add, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

use crate::nes::Addr;

macro_rules! reg_from_impl {
    ($to:ty, $from:ty) => {
        reg_from_impl!($to, $from, u8);
    };

    ($to:ty, $from:ty, $cast:ty) => {
        impl From<$from> for $to {
            fn from(value: $from) -> Self {
                Self(value as $cast)
            }
        }

        impl From<$to> for $from {
            fn from(value: $to) -> Self {
                value.0 as $from
            }
        }
    };
}

macro_rules! reg_add_impl {
    ($cls:ty, $plus:ty) => {
        impl Add<$plus> for $cls {
            type Output = $plus;
            fn add(self, rhs: $plus) -> Self::Output {
                self.0 as $plus + rhs
            }
        }

        impl Add<$cls> for $plus {
            type Output = $plus;
            fn add(self, rhs: $cls) -> Self::Output {
                self + rhs.0 as $plus
            }
        }
    };
}

macro_rules! bit_or {
    ($cls:ty, $v:ty) => {
        impl BitOr for $cls {
            type Output = $cls;
            fn bitor(self, rhs: Self) -> Self::Output {
                Self(self.0 | rhs.0)
            }
        }

        impl BitOrAssign for $cls {
            fn bitor_assign(&mut self, rhs: Self) {
                self.0 |= rhs.0
            }
        }

        impl BitOr<$v> for $cls {
            type Output = $cls;
            fn bitor(self, rhs: $v) -> Self::Output {
                Self(self.0 | rhs)
            }
        }

        impl BitOrAssign<$v> for $cls {
            fn bitor_assign(&mut self, rhs: $v) {
                self.0 |= rhs
            }
        }
    };
}

macro_rules! bit_and {
    ($cls:ty, $v:ty) => {
        impl BitAnd for $cls {
            type Output = $cls;
            fn bitand(self, rhs: Self) -> Self::Output {
                Self(self.0 & rhs.0)
            }
        }

        impl BitAndAssign for $cls {
            fn bitand_assign(&mut self, rhs: Self) {
                self.0 &= rhs.0
            }
        }

        impl BitAnd<$v> for $cls {
            type Output = $cls;
            fn bitand(self, rhs: $v) -> Self::Output {
                Self(self.0 & rhs)
            }
        }

        impl BitAndAssign<$v> for $cls {
            fn bitand_assign(&mut self, rhs: $v) {
                self.0 &= rhs
            }
        }
    };
}

macro_rules! bit_xor {
    ($cls:ty, $v:ty) => {
        impl BitXor for $cls {
            type Output = $cls;
            fn bitxor(self, rhs: Self) -> Self::Output {
                Self(self.0 ^ rhs.0)
            }
        }

        impl BitXorAssign for $cls {
            fn bitxor_assign(&mut self, rhs: Self) {
                self.0 ^= rhs.0
            }
        }

        impl BitXor<$v> for $cls {
            type Output = $cls;
            fn bitxor(self, rhs: $v) -> Self::Output {
                Self(self.0 ^ rhs)
            }
        }

        impl BitXorAssign<$v> for $cls {
            fn bitxor_assign(&mut self, rhs: $v) {
                self.0 ^= rhs
            }
        }
    };
}

macro_rules! not {
    ($cls:ty) => {
        impl Not for $cls {
            type Output = $cls;
            fn not(self) -> Self::Output {
                Self(!self.0)
            }
        }
    };
}

macro_rules! partial_eq {
    ($cls:ty, $with:ty) => {
        impl PartialEq<$with> for $cls {
            fn eq(&self, other: &$with) -> bool {
                &self.0 == other
            }
        }
    };
}

macro_rules! display {
    ($cls:ty [$a:tt, $b:tt, $c:tt, $d:tt, $e:tt, $f:tt, $g:tt, $h:tt]) => {
        impl fmt::Display for $cls {
            fn fmt(&self, fout: &mut fmt::Formatter<'_>) -> fmt::Result {
                let func = |f, i, e| match self.get(f) == 1 {
                    true => i,
                    false => e,
                };

                let a = func(<$cls>::$a, stringify!($a), ".");
                let b = func(<$cls>::$b, stringify!($b), ".");
                let c = func(<$cls>::$c, stringify!($c), ".");
                let d = func(<$cls>::$d, stringify!($d), ".");
                let e = func(<$cls>::$e, stringify!($e), ".");
                let f = func(<$cls>::$f, stringify!($f), ".");
                let g = func(<$cls>::$g, stringify!($g), ".");
                let h = func(<$cls>::$h, stringify!($h), ".");

                write!(
                    fout,
                    "{:?}{:?}{:?}{:?}{:?}{:?}{:?}{:?}",
                    a, b, c, d, e, f, g, h
                )
            }
        }
    };

    ($cls:ty [$a:tt, $b:tt, $c:tt, $d:tt, $e:tt, $f:tt, $g:tt, $h:tt], [$aa:tt, $bb:tt, $cc:tt, $dd:tt, $ee:tt, $ff:tt, $gg:tt, $hh:tt]) => {
        impl fmt::Display for $cls {
            fn fmt(&self, fout: &mut fmt::Formatter<'_>) -> fmt::Result {
                let func = |f, i, e| match self.get(f) == 1 {
                    true => i,
                    false => e,
                };

                let a = func(<$cls>::$a, stringify!($aa), ".");
                let b = func(<$cls>::$b, stringify!($bb), ".");
                let c = func(<$cls>::$c, stringify!($cc), ".");
                let d = func(<$cls>::$d, stringify!($dd), ".");
                let e = func(<$cls>::$e, stringify!($ee), ".");
                let f = func(<$cls>::$f, stringify!($ff), ".");
                let g = func(<$cls>::$g, stringify!($gg), ".");
                let h = func(<$cls>::$h, stringify!($hh), ".");

                write!(
                    fout,
                    "{:?}{:?}{:?}{:?}{:?}{:?}{:?}{:?}",
                    a, b, c, d, e, f, g, h
                )
            }
        }
    };
}

/// # Summary
/// ```
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
#[derive(Default)]
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

    pub addr_latch: u8,
    pub data_buffer: u8,
    pub addr: Addr,
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
pub struct Ctrl(u8);

impl Ctrl {
    pub const fn new(val: u8) -> Self {
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
reg_from_impl!(Ctrl, u8);
reg_add_impl!(Ctrl, u8);
bit_or!(Ctrl, u8);
bit_and!(Ctrl, u8);
bit_xor!(Ctrl, u8);
partial_eq!(Ctrl, u8);

reg_from_impl!(Ctrl, u16);
reg_add_impl!(Ctrl, u16);
display!(Ctrl [V, P, H, B, S, I, Y, X]);

/// # Mask ($2001) > write
/// - Common name: PPUMASK
/// - Description: PPU mask register
/// - Access: write
///
/// This register controls the rendering of sprites and backgrounds, as well as colour effects.
/// ```
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
pub struct Mask(u8);

impl Mask {
    pub const fn new(val: u8) -> Self {
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
reg_from_impl!(Mask, u8);
reg_add_impl!(Mask, u8);
bit_or!(Mask, u8);
bit_and!(Mask, u8);
bit_xor!(Mask, u8);
partial_eq!(Mask, u8);

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
/// ```
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
pub struct Status(u8);

impl Status {
    pub const fn new(val: u8) -> Self {
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
reg_from_impl!(Status, u8);
reg_add_impl!(Status, u8);
bit_or!(Status, u8);
bit_and!(Status, u8);
bit_xor!(Status, u8);
partial_eq!(Status, u8);

reg_from_impl!(Status, u16);
reg_add_impl!(Status, u16);
display!(Status [V, S, O, U, U, U, U, U], [V, S, O, -, -, -, -, -]);
