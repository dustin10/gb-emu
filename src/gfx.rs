#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Color {
    Transparent,
    RGB { r: u8, g: u8, b: u8 },
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ColorIndex {
    Zero,
    One,
    Two,
    Three,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Pallette {
    one: Color,
    two: Color,
    three: Color,
}

impl Pallette {
    pub fn new(one: Color, two: Color, three: Color) -> Self {
        Self { one, two, three }
    }
    pub fn get_color(&self, index: ColorIndex) -> Color {
        match index {
            ColorIndex::Zero => Color::Transparent,
            ColorIndex::One => self.one,
            ColorIndex::Two => self.two,
            ColorIndex::Three => self.three,
        }
    }
}

#[derive(Debug)]
pub struct Tile {
    pub pixels: [ColorIndex; 64],
}

impl Tile {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for Tile {
    fn default() -> Self {
        Self {
            pixels: [ColorIndex::Zero; 64],
        }
    }
}

/// The [`Gpu`] is responsible for managing the VRAM and drawing the graphics to the emulator
/// screen.
pub struct Gpu {
    /// Video memory of the emulator.
    vram: [u8; 8192],
}

impl Gpu {
    /// Creates a new default [`Gpu`].
    pub fn new() -> Self {
        Self::default()
    }
    /// Reads a single byte from VRAM. The address given should be based at 0 rather than the start
    /// address of VRAM in the documentation which is 0x8000.
    pub fn read_u8(&self, address: u16) -> u8 {
        tracing::debug!("read VRAM address: {:#06x}", address);
        self.vram[address as usize]
    }
    /// Writes a single byte to VRAM. The address given should be based at 0 rather than the start
    /// address of VRAM in the documentation which is 0x8000.
    pub fn write_u8(&mut self, address: u16, byte: u8) {
        tracing::debug!("write VRAM address: {:#06x} = {:#04x}", address, byte);
        self.vram[address as usize] = byte;
    }
}

impl Default for Gpu {
    /// Creates a default [`Gpu`].
    fn default() -> Self {
        Self { vram: [0; 8192] }
    }
}
