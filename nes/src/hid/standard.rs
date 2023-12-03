use crate::Byte;
use std::ops::{BitAnd, BitOr, Not, BitOrAssign, BitAndAssign};

use super::Joypad;

#[derive(Debug, Default, Clone, Copy)]
pub enum StandardButton {
    #[default]
    None = 0,
    A = 1 << 0,
    B = 1 << 1,
    Select = 1 << 2,
    Start = 1 << 3,
    Up = 1 << 4,
    Down = 1 << 5,
    Left = 1 << 6,
    Right = 1 << 7,
}

impl Not for StandardButton {
    type Output = Byte;
    fn not(self) -> Self::Output {
        !(self as Byte)
    }
}

impl BitOr<StandardButton> for Byte {
    type Output = Byte;
    fn bitor(self, rhs: StandardButton) -> Self::Output {
        self | (rhs as Self)
    }
}

impl BitOrAssign<StandardButton> for Byte {
    fn bitor_assign(&mut self, rhs: StandardButton) {
        *self |= rhs as Self
    }
}

impl BitAnd<StandardButton> for Byte {
    type Output = Byte;
    fn bitand(self, rhs: StandardButton) -> Self::Output {
        self & (rhs as Self)
    }
}

impl BitAndAssign<StandardButton> for Byte {
    fn bitand_assign(&mut self, rhs: StandardButton) {
        *self &= rhs as Self
    }
}

/// # Standard Controller
///
/// https://www.nesdev.org/wiki/Standard_controller
///
///```text
/// 0 - A
/// 1 - B
/// 2 - Select
/// 3 - Start
/// 4 - Up
/// 5 - Down
/// 6 - Left
/// 7 - Right
/// ```
#[derive(Debug, Default)]
pub struct Standard {
    pub btns: Byte,
    shft: Byte,
}

impl Standard {}

impl Joypad for Standard {
    fn reset(&mut self) {
        self.shft = self.btns;
    }

    fn read(&self) -> Byte {
        self.shft & 0x01
    }

    fn read_mut(&mut self) -> Byte {
        let val = self.shft & 0x01;
        self.shft >>= 1;
        val
    }

    fn fill(&mut self, data: Byte) {
        self.btns = data;
    }
}
