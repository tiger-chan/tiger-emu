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

    pub fn to_array(self) -> [u8; 4] {
        [self.r, self.g, self.b, 0xFF]
    }
}

impl From<&Color> for egui::Color32 {
    fn from(value: &Color) -> Self {
        Self::from_rgb(value.r, value.g, value.b)
    }
}
