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

/// A [`Tile`] is a square consisting of an 8 x 8 grouping of pixels. Each pixel value contains an
/// index into the [`Pallette`] that represents the color that should be used to render it.
#[derive(Copy, Clone, Debug)]
pub struct Tile {
    /// Raw pixel data of the tile containing the index into the color pallette for each pixel.
    pixels: [u8; 64],
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
        Self { pixels: [0; 64] }
    }
}

/// Enumerates the different types of [`Layer`]s which are used to render the image onto the
/// emulator screen.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LayerKind {
    /// The background is composed of a tilemap. A tilemap is a large grid of tiles. However,
    /// tiles aren’t directly written to tilemaps, they merely contain references to the tiles.
    /// This makes reusing tiles very cheap, both in CPU time and in required memory space, and
    /// it is the main mechanism that helps work around the paltry 8 KiB of video RAM.
    ///
    /// The background can be made to scroll as a whole, writing to two hardware registers. This
    /// makes scrolling very cheap.
    Background,
    /// The window is sort of a second background layer on top of the background. It is fairly limited:
    /// it has no transparency, it’s always a rectangle and only the position of the top-left pixel can
    /// be controlled.
    ///
    /// Possible usage include a fixed status bar in an otherwise scrolling game (e.g. Super Mario Land 2).
    Window,
    /// The background layer is useful for elements scrolling as a whole, but it’s impractical for objects
    /// that need to move separately, such as the player.
    ///
    /// The objects layer is designed to fill this gap: objects are made of 1 or 2 stacked tiles
    /// (8×8 or 8×16 pixels) and can be displayed anywhere on the screen.
    Objects,
}

/// Number of tiles per layer. Each tile is an 8 x 8 group of pixels so divide the screen width and
/// height by 8 and then multiply then together to get the total number of tiles to store for each
/// layer.
const TILES_PER_LAYER: usize = (160 / 8) * (144 / 8);

/// A [`Layer`] is a grid of [`Tile`]s which represent a layer of pixels in the final scene
/// rendered to the emulator display.
#[derive(Debug)]
pub struct Layer {
    /// [`LayerKind`] for the layer.
    _kind: LayerKind,
    /// Array of [`Tile`]s that contain the pixel data for the layer.
    _tiles: [Tile; TILES_PER_LAYER],
    /// Strategy used to address the tile data of the layer.
    addressing_strategy: TileAddressingStrategy,
}

impl Layer {
    /// Creates a new [`Layer`] of the specified [`LayerKind`].
    fn with_kind(kind: LayerKind) -> Self {
        Self {
            _kind: kind,
            _tiles: [Tile::default(); TILES_PER_LAYER],
            addressing_strategy: TileAddressingStrategy::Unsigned,
        }
    }
    /// Creates a new background [`Layer`].
    pub fn background() -> Self {
        Self::with_kind(LayerKind::Background)
    }
    /// Creates a new window [`Layer`].
    pub fn window() -> Self {
        Self::with_kind(LayerKind::Window)
    }
    /// Creates a new objects [`Layer`].
    pub fn objects() -> Self {
        Self::with_kind(LayerKind::Objects)
    }
    /// Sets the active [`TileAddressingStrategy`] for the layer.
    pub fn set_addressing_strategy(&mut self, strategy: TileAddressingStrategy) {
        self.addressing_strategy = strategy;
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

/// The [`Ppu`] is responsible for managing the VRAM and drawing the graphics to the emulator
/// screen.
pub struct Ppu {
    /// Video memory of the emulator.
    vram: [u8; 8192],
    /// Background layer tiles.
    _background: Layer,
    /// Window layer tiles.
    _window: Layer,
    /// Objects layer tiles.
    _objects: Layer,
    /// Tile map whose memory resides between 0x9800 and 0x9BFF.
    tile_map_a: [u8; 1024],
    /// Tile map whose memory resides between 0x9C00 and 0x9FFF.
    tile_map_b: [u8; 1024],
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
                tracing::debug!("read PPU tile map A address: {:#06x}", address);

                let idx = address as usize - 0x9800;
                assert!(idx < self.tile_map_a.len());

                tracing::trace!("read tile map A index: {}", idx);

                self.tile_map_a[idx]
            }
            0x9C00..=0x9FFF => {
                tracing::debug!("read PPU tile map B address: {:#06x}", address);

                let idx = address as usize - 0x9C00;
                assert!(idx < self.tile_map_b.len());

                tracing::trace!("read tile map B index: {}", idx);

                self.tile_map_b[idx]
            }
            _ => {
                tracing::debug!("read PPU VRAM address: {:#06x}", address);
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
                assert!(idx < self.tile_map_b.len());

                tracing::trace!("write tile map A index: {}", idx);

                self.tile_map_a[idx] = byte;
            }
            0x9C00..=0x9FFF => {
                tracing::debug!(
                    "write PPU tile map B address: {:#06x} = {:#04x}",
                    address,
                    byte
                );

                let idx = address as usize - 0x9C00;
                assert!(idx < self.tile_map_b.len());

                tracing::trace!("write tile map B index: {}", idx);

                self.tile_map_b[idx] = byte;
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
    /// Creates a default [`Ppu`] with all VRAM bytes set to zero.
    fn default() -> Self {
        Self {
            vram: [0; 8192],
            _background: Layer::background(),
            _window: Layer::window(),
            _objects: Layer::objects(),
            tile_map_a: [0; 1024],
            tile_map_b: [0; 1024],
        }
    }
}
