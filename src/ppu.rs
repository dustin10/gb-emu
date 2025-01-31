use crate::mem::Mapper;

use std::fmt::Display;

/// The [`Color`] struct defines a color in the RGBA format with values of each component ranging
/// from 0 to 255.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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

/// Number of bytes in a [`Tile`].
const NUM_TILE_BYTES: usize = 64;

/// A [`Tile`] is a square consisting of an 8 x 8 grouping of pixels. Each pixel value contains an
/// index into the [`Pallette`] that represents the color that should be used to render it.
#[derive(Copy, Clone, Debug)]
pub struct Tile {
    /// Raw pixel data of the tile containing the index into the color pallette for each pixel.
    pixels: [u8; NUM_TILE_BYTES],
}

impl Tile {
    /// Creates a new default [`Tile`].
    pub fn new() -> Self {
        Self::default()
    }
    /// Creates a new [`Tile`] with the specified pixel data.
    pub fn with_pixels(pixels: &[u8]) -> Self {
        let mut tile = Self::default();
        tile.write_block_u8(0, 0, pixels);

        tile
    }
    /// Writes a single byte to the 8 x 8 pixel data of the [`Tile`]. The `row` and `col` are
    /// zero-based.
    pub fn write_u8(&mut self, row: usize, col: usize, value: u8) {
        assert!(row < 7);
        assert!(col < 7);

        let idx = (row * 7) + col;

        self.pixels[idx] = value;
    }
    /// Writes a block of bytes to the 8 x 8 pixel data of the [`Tile`]. The `row` and `col`
    /// are zero-based and specify the starting position to write the block of bytes.
    pub fn write_block_u8(&mut self, row: usize, col: usize, bytes: &[u8]) {
        assert!(row < 7);
        assert!(col < 7);

        let num_bytes = bytes.len();
        let dest_start = (row * 7) + col;
        let dest_end = dest_start + num_bytes;

        self.pixels[dest_start..dest_end].copy_from_slice(&bytes[0..num_bytes]);
    }
}

impl Default for Tile {
    /// Creates a new [`Tile`] whose pixel data consists of all zeros.
    fn default() -> Self {
        Self {
            pixels: [0; NUM_TILE_BYTES],
        }
    }
}

/// Enumerates the different methods that are available to address tile data for a [`Layer`].
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TileAddressingStrategy {
    /// The “$8000 method” uses $8000 as its base pointer and uses an unsigned addressing, meaning that tiles 0-127
    /// are in block 0, and tiles 128-255 are in block 1.
    Unsigned,
    /// The “$8800 method” uses $9000 as its base pointer and uses a signed addressing, meaning that tiles 0-127 are
    /// in block 2, and tiles -128 to -1 are in block 1; or, to put it differently, “$8800 addressing” takes tiles
    /// 0-127 from block 2 and tiles 128-255 from block 1.
    Signed,
}

impl From<u8> for TileAddressingStrategy {
    /// Convertes a [`u8`] to the appropriate [`TileAddressingStrategy`] value.
    fn from(value: u8) -> Self {
        match value {
            0 => TileAddressingStrategy::Signed,
            _ => TileAddressingStrategy::Unsigned,
        }
    }
}

/// Number of bytes for the VRAM data excluding the two tile maps.
const VRAM_NUM_BYTES: usize = 0x97FF - 0x8000 + 1;

/// Number of bytes for the OAM data.
const OAM_NUM_BYTES: usize = 0xFE9F - 0xFE00 + 1;

/// Number of bytes for the tile map data.
const TILE_MAP_NUM_BYTES: usize = 0x9BFF - 0x9800 + 1;

/// The [`Ppu`] is responsible for managing the VRAM and drawing the graphics to the emulator
/// screen.
#[derive(Debug)]
pub struct Ppu {
    /// Video memory of the emulator. This is where the tile data is stored.
    vram: [u8; VRAM_NUM_BYTES],
    /// Object Attribute Memory (OAM) of the emulator.
    oam: [u8; OAM_NUM_BYTES],
    /// Tile map whose memory resides between 0x9800 and 0x9BFF.
    tile_map_one: [u8; TILE_MAP_NUM_BYTES],
    /// Tile map whose memory resides between 0x9C00 and 0x9FFF.
    tile_map_two: [u8; TILE_MAP_NUM_BYTES],
}

impl Ppu {
    /// Creates a new default [`Ppu`].
    pub fn new() -> Self {
        Self::default()
    }
}

impl Mapper for Ppu {
    /// Reads a single byte from VRAM. The address given should be based at 0 rather than the start
    /// address of VRAM in the documentation which is 0x8000.
    fn read_u8(&self, address: u16) -> u8 {
        match address {
            0x9800..=0x9BFF => {
                tracing::debug!("read PPU tile map one address: {:#06x}", address);

                let idx = address as usize - 0x9800;
                assert!(idx < self.tile_map_one.len());

                tracing::trace!("read tile map one index: {}", idx);

                self.tile_map_one[idx]
            }
            0x9C00..=0x9FFF => {
                tracing::debug!("read PPU tile map two address: {:#06x}", address);

                let idx = address as usize - 0x9C00;
                assert!(idx < self.tile_map_two.len());

                tracing::trace!("read tile map two index: {}", idx);

                self.tile_map_two[idx]
            }
            0xFE00..=0xFE9F => {
                tracing::debug!("read PPU OAM address: {:#06x}", address);

                let idx = address as usize - 0xFE00;
                assert!(idx < self.oam.len());

                tracing::trace!("read OAM index: {}", idx);

                self.oam[idx]
            }
            _ => {
                tracing::debug!("read PPU VRAM address: {:#06x}", address);

                let idx = address as usize - 0x8000;
                assert!(idx < self.vram.len());

                tracing::trace!("read VRAM index: {}", idx);

                self.vram[address as usize]
            }
        }
    }
    /// Writes a single byte to VRAM. The address given should be based at 0 rather than the start
    /// address of VRAM in the documentation which is 0x8000.
    fn write_u8(&mut self, address: u16, byte: u8) {
        match address {
            0x9800..=0x9BFF => {
                tracing::debug!(
                    "write PPU tile map A address: {:#06x} = {:#04x}",
                    address,
                    byte
                );

                let idx = address as usize - 0x9800;
                assert!(idx < self.tile_map_one.len());

                tracing::trace!("write tile map one index: {}", idx);

                self.tile_map_one[idx] = byte;
            }
            0x9C00..=0x9FFF => {
                tracing::debug!(
                    "write PPU tile map B address: {:#06x} = {:#04x}",
                    address,
                    byte
                );

                let idx = address as usize - 0x9C00;
                assert!(idx < self.tile_map_two.len());

                tracing::trace!("write tile map two index: {}", idx);

                self.tile_map_two[idx] = byte;
            }
            0xFE00..=0xFE9F => {
                tracing::debug!("write PPU OAM address: {:#06x} = {:#04x}", address, byte);

                let idx = address as usize - 0xFE00;
                assert!(idx < self.oam.len());

                tracing::trace!("write OAM index: {}", idx);

                self.oam[idx] = byte;
            }
            _ => {
                tracing::debug!("write PPU VRAM address: {:#06x} = {:#04x}", address, byte);

                let idx = address as usize - 0x8000;
                assert!(idx < self.vram.len());

                tracing::trace!("write VRAM index: {}", idx);

                self.vram[idx] = byte;
            }
        }
    }
}

impl Default for Ppu {
    /// Creates a default [`Ppu`] with all bytes of it's memory set to zero.
    fn default() -> Self {
        Self {
            vram: [0; VRAM_NUM_BYTES],
            oam: [0; OAM_NUM_BYTES],
            tile_map_one: [0; TILE_MAP_NUM_BYTES],
            tile_map_two: [0; TILE_MAP_NUM_BYTES],
        }
    }
}
