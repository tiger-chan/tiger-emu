#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct INesHeader {
    pub name: [u8; 4],
    pub prg_chunks: u8,
    pub chr_chunks: u8,
    /// Flags 6 – Mapper, mirroring, battery, trainer
    /// ```
    /// 76543210
    /// ||||||||
    /// |||||||+- Mirroring: 0: horizontal (vertical arrangement) (CIRAM A10 = PPU A11)
    /// |||||||              1: vertical (horizontal arrangement) (CIRAM A10 = PPU A10)
    /// ||||||+-- 1: Cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
    /// |||||+--- 1: 512-byte trainer at $7000-$71FF (stored before PRG data)
    /// ||||+---- 1: Ignore mirroring control or above mirroring bit; instead provide four-screen VRAM
    /// ++++----- Lower nybble of mapper number
    /// ```
    pub mpr1: u8,
    /// Flags 7 – Mapper, VS/Playchoice, NES 2.0
    /// ```
    /// 76543210
    /// ||||||||
    /// |||||||+- VS Unisystem
    /// ||||||+-- PlayChoice-10 (8 KB of Hint Screen data stored after CHR data)
    /// ||||++--- If equal to 2, flags 8-15 are in NES 2.0 format
    /// ++++----- Upper nybble of mapper number
    /// ```
    pub mpr2: u8,
    /// Flags 8 – PRG-RAM size (rarely used extension)
    ///```
    /// 76543210
    /// ||||||||
    /// ++++++++- PRG RAM size
    /// ```
    pub prg_ram_size: u8,
    /// Flags 9 – TV system (rarely used extension)
    /// ```
    /// 76543210
    /// ||||||||
    /// |||||||+- TV system (0: NTSC; 1: PAL)
    /// +++++++-- Reserved, set to zero
    /// ```
    pub tv_sys1: u8,
    /// Flags 10 – TV system, PRG-RAM presence (unofficial, rarely used extension)
    /// ```
    /// 76543210
    ///   ||  ||
    ///   ||  ++- TV system (0: NTSC; 2: PAL; 1/3: dual compatible)
    ///   |+----- PRG RAM ($6000-$7FFF) (0: present; 1: not present)
    ///   +------ 0: Board has no bus conflicts; 1: Board has bus conflicts
    /// ```
    pub tv_sys2: u8,
    // Unused
    pub padding: [u8; 5],
}

impl From<&[u8]> for INesHeader {
    fn from(value: &[u8]) -> Self {
        let mut i = 0;
        let name = [value[i], value[i + 1], value[i + 2], value[i + 3]];
        i += 4;
        let prg_chunks = value[i];
        i += 1;
        let chr_chunks = value[i];
        i += 1;
        let mpr1 = value[i];
        i += 1;
        let mpr2 = value[i];
        i += 1;
        let prg_ram_size = value[i];
        i += 1;
        let tv_sys1 = value[i];
        i += 1;
        let tv_sys2 = value[i];

        Self {
            name,
            prg_chunks,
            chr_chunks,
            mpr1,
            mpr2,
            prg_ram_size,
            tv_sys1,
            tv_sys2,
            padding: [0; 5],
        }
    }
}
