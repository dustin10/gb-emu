use std::fmt::Display;

/// The [`Color`] struct defines a color in the RGBA format with values of each component ranging
/// from 0 to 255.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Color {
    // Red component.
    pub r: u8,
    // Green component.
    pub g: u8,
    // Blue component.
    pub b: u8,
    // Alpha component.
    pub a: u8,
}

/// Enumerates the values which can be used to reference a [`Color`] in the current
/// active [`Pallette`].
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ColorIndex {
    /// Background color correponding to 0.
    Background,
    /// The color correponding to 1.
    A,
    /// The color correponding to 2.
    B,
    /// The color correponding to 3.
    C,
}

impl From<u8> for ColorIndex {
    /// Creates a [`ColorIndex`] from a [`u8`] value.
    fn from(value: u8) -> Self {
        match value {
            0 => ColorIndex::Background,
            1 => ColorIndex::A,
            2 => ColorIndex::B,
            3 => ColorIndex::C,
            _ => {
                tracing::warn!("invalid u8 converted to ColorIndex: {}", value);
                ColorIndex::C
            }
        }
    }
}

impl Display for ColorIndex {
    /// Writes a string representation of the [`ColorIndex`] value to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

/// The [`Pallette`] struct contains the set of [`Color`]s that will be used to render the frame
/// onto the emulator screen.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Pallette {
    /// Background color correponding to 0.
    bg: Color,
    /// The color correponding to 1.
    a: Color,
    /// The color correponding to 2.
    b: Color,
    /// The color correponding to 3.
    c: Color,
}

impl Pallette {
    /// Creates a new [`Pallette`] with the given four [`Color`]s.
    pub fn new(bg: Color, a: Color, b: Color, c: Color) -> Self {
        Self { bg, a, b, c }
    }
    /// Gets a reference to the [`Color`] which correponds to the specified [`ColorIndex`].
    pub fn get_color_by_index(&self, index: ColorIndex) -> &Color {
        match index {
            ColorIndex::Background => &self.bg,
            ColorIndex::A => &self.a,
            ColorIndex::B => &self.b,
            ColorIndex::C => &self.c,
        }
    }
}

impl Default for Pallette {
    /// Creates a [`Pallette`] with the default GameBoy color scheme.
    fn default() -> Self {
        Self::new(
            Color {
                r: 155,
                g: 188,
                b: 15,
                a: 1,
            },
            Color {
                r: 139,
                g: 172,
                b: 15,
                a: 1,
            },
            Color {
                r: 48,
                g: 98,
                b: 48,
                a: 1,
            },
            Color {
                r: 15,
                g: 56,
                b: 15,
                a: 1,
            },
        )
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
