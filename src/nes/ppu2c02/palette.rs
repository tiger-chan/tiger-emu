use super::Color;

pub fn create_palette(bytes: &[u8; 64 * 3]) -> [Color; 64] {
    let mut colors = [Color::new(0, 0, 0); 64];
    for (i, col) in colors.iter_mut().enumerate() {
        let offset = i * 3;
        #[allow(clippy::identity_op)]
        let r = bytes[offset + 0];
        let g = bytes[offset + 1];
        let b = bytes[offset + 2];
        *col = Color::new(r, g, b);
    }

    colors
}

pub const X2C02: &[u8; 64 * 3] = include_bytes!("2C02.pal");
pub const X2C07: &[u8; 64 * 3] = include_bytes!("2C07.pal");
