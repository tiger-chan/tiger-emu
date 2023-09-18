use super::Color;

pub fn create_palette(bytes: &[u8; 64 * 3]) -> [Color; 64] {
    let mut colors = [Color::new(0, 0, 0); 64];
    for i in 0..64 {
        let r = bytes[i + 0];
        let g = bytes[i + 1];
        let b = bytes[i + 2];
        colors[i] = Color::new(r, g, b);
    }

    colors
}

pub const X2C02: &[u8; 64 * 3] = include_bytes!("2C02.pal");
pub const X2C07: &[u8; 64 * 3] = include_bytes!("2C07.pal");
