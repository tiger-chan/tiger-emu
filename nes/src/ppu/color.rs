use crate::Byte;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Color {
    pub r: Byte,
    pub g: Byte,
    pub b: Byte,
}

impl Default for Color {
    fn default() -> Self {
        Color::new(0x00, 0x00, 0x00)
    }
}

impl Color {
    pub const fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }

    pub fn to_array(self) -> [u8; 4] {
        [self.r, self.g, self.b, 0xFF]
    }

    pub const BLACK: Color = Color::new(0x00, 0x00, 0x00);
    pub const WHITE: Color = Color::new(0xFF, 0xFF, 0xFF);
}
