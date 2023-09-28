mod nrom;

pub use nrom::Nrom;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Mapper {
    Nrom(Nrom),
}

impl Default for Mapper {
    fn default() -> Self {
        Self::Nrom(Nrom::default())
    }
}
