use crate::mem::Mapper;

use anyhow::Context;
use std::{cell::RefCell, fmt::Display, path::Path, rc::Rc};

/// Defines the maximum size of the addressable memory for the emulator.
const ADDRESSABLE_MEMORY: usize = 65536;

/// Address in emulator memory where the cartridge data is loaded.
const CARTRIDGE_START_ADDR: u16 = 0x0000;

/// Total number of bytes that make up the Nintendo logo stored on the cartridge.
const NINTENDO_LOGO_LENGTH: usize = 48;

/// Address of the byte to start at when reading the Nintendo logo.
const NINTENDO_LOGO_START: usize = 0x0104;

/// One past the address of the byte to end at when reading the Nintendo logo.
const NINTENDO_LOGO_END: usize = 0x0133;

/// Value set for the old licensee in the header when the new licensee value should be read
/// instead.
const OLD_LICENSEE_REDIRECT_VALUE: u8 = 0x33;

/// Address of the first byte of the title.
const TITLE_START_ADDR: usize = 0x0134;

/// Address of the last byte of the title.
const TITLE_END_ADDR: usize = 0x0143;

/// Address of the first byte of the manufacturer code.
const MANUFACTURER_START_ADDR: usize = 0x013F;

/// Address of the last byte of the manufacturer code.
const MANUFACTURER_END_ADDR: usize = 0x0142;

/// Address of the CGB flag.
const CGB_FLAG_ADDR: usize = 0x0143;

/// Address of the low byte of the new licensee code.
const NEW_LICENSEE_CODE_LOW_ADDR: usize = 0x0145;

/// Address of the high byte of the new licensee code.
const NEW_LICENSEE_CODE_HIGH_ADDR: usize = 0x0144;

/// Address of the SGB flag.
const SGB_FLAG_ADDR: usize = 0x0146;

/// Address of the cartridge type byte.
const TYPE_ADDR: usize = 0x0147;

/// Address of the ROM size byte.
const ROM_SIZE_ADDR: usize = 0x0148;

/// Address of the RAM size byte.
const RAM_SIZE_ADDR: usize = 0x0149;

/// Address of the destination byte.
const DESTINATION_ADDR: usize = 0x014A;

/// Address of the old licensee code.
const OLD_LICENSEE_CODE_ADDR: usize = 0x014B;

/// Address of the cartridge version.
const VERSION_ADDR: usize = 0x014C;

/// Address of the expected header checksum value.
const HEADER_CHECKSUM_ADDR: usize = 0x014D;

/// Address of the low byte of the expected global checksum value.
const GLOBAL_CHECKSUM_LOW_ADDR: usize = 0x014F;

/// Address of the high byte of the expected global checksum value.
const GLOBAL_CHECKSUM_HIGH_ADDR: usize = 0x014E;

/// Address of the byte to start at when computing the header checksum.
const COMPUTE_HEADER_CHECKSUM_START: usize = 0x0134;

/// One past the address of the byte to end at when computing the header checksum.
const COMPUTE_HEADER_CHECKSUM_END: usize = 0x014D;

/// Enumerates various types of GameBoy cartridges supported by the emulator.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CartridgeKind {
    /// No mapping required.
    RomOnly,
    /// Type 1 memory bank controller.
    MBC1 { ram: bool, battery: bool },
}

impl<T> From<T> for CartridgeKind
where
    T: Into<u8>,
{
    /// Creates the appropriate [`CartridgeKind`] that maps to the given [`u8`] which represents
    /// the memory bank controller that a [`crate::cartridge::Cartridge`] requires. This value is
    /// read from the cartridge header.
    ///
    /// # Panic
    ///
    /// This function will panic when the cartridge type which the byte maps to has not yet been
    /// implemented.
    fn from(value: T) -> Self {
        let v = value.into();

        match v {
            0x00 => CartridgeKind::RomOnly,
            0x01 => CartridgeKind::MBC1 {
                ram: true,
                battery: true,
            },
            0x02 => CartridgeKind::MBC1 {
                ram: true,
                battery: false,
            },
            0x03 => CartridgeKind::MBC1 {
                ram: true,
                battery: true,
            },
            _ => panic!("unsupported cartridge type: {:#02x}", v),
        }
    }
}

impl Display for CartridgeKind {
    /// Writes a string representation of the [`CartridgeKind`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CartridgeKind::RomOnly => f.write_str("ROM Only"),
            CartridgeKind::MBC1 { ram, battery } => {
                f.write_fmt(format_args!("MBC1 RAM:{} Battery:{}", ram, battery))
            }
        }
    }
}

/// The [`Mbc`] trait defines the common behavior required for the various memory bank controllers
/// that can exist on a GameBoy cartridge.
pub trait Mbc: Mapper {
    /// Writes a block of bytes to memory at the given start address.
    fn write_block(&mut self, start_addr: u16, bytes: &[u8]);
}

/// [`RomOnly`] is an implementation of the [`MBC`] trait which does not do any special handling of
/// memory addresses and simply reads and writes at the provided address.
#[derive(Debug)]
pub struct RomOnly {
    /// Raw bytes of data contained in memory.
    data: [u8; ADDRESSABLE_MEMORY],
}

impl RomOnly {
    /// Creates a new default [`RomOnly`].
    pub fn new() -> Self {
        Self::default()
    }
}

impl Mapper for RomOnly {
    /// Reads a single byte from memory at the given address.
    fn read_u8(&self, address: u16) -> u8 {
        tracing::debug!("read RomOnly MBC address: {:#06x}", address);
        self.data[address as usize]
    }
    /// Writes a single byte to memory at the given address.
    fn write_u8(&mut self, address: u16, byte: u8) {
        tracing::debug!(
            "write RomOnly MBC address: {:#06x} = {:#04x}",
            address,
            byte
        );
        self.data[address as usize] = byte;
    }
}

impl Mbc for RomOnly {
    /// Writes a block of bytes to memory at the given start address.
    fn write_block(&mut self, start_addr: u16, bytes: &[u8]) {
        tracing::debug!(
            "write {} bytes to RomOnly MBC memory starting at {:#06x}",
            bytes.len(),
            start_addr
        );

        let num_bytes = bytes.len();
        let dest_start = start_addr as usize;
        let dest_end = dest_start + num_bytes;

        self.data[dest_start..dest_end].copy_from_slice(&bytes[0..num_bytes]);
    }
}

impl Default for RomOnly {
    /// Creates a default [`RomOnly`] which has all data set to zero.
    fn default() -> Self {
        Self {
            data: [0; ADDRESSABLE_MEMORY],
        }
    }
}

/// Holds the data that makes up the header of the [`Cartridge`]. The header contains data such as
/// the [`CartridgeKind`], the title of the game, the company that made it, etc.
#[derive(Debug)]
pub struct Header {
    /// Byte array containing the Nintendo logo that is displayed when the GameBoy is turned on. If
    /// these bytes do not match a well-known value then the game will not be allowed to run.
    pub nintendo_logo: [u8; 48],
    /// Title of the game in uppercase ASCII.
    pub title: String,
    /// In older cartridges these bytes were part of the Title (see above). In newer cartridges they
    /// contain a 4-character manufacturer code (in uppercase ASCII). The purpose of the manufacturer
    /// code is unknown.
    pub manufacturer_code: String,
    /// Indicates what kind of hardware is present on the cartridge including the memory bank
    /// controller implementation.
    pub kind: CartridgeKind,
    /// Indicates how much ROM is present on the cartridge. In most cases, the ROM size is given by
    /// 32 KiB × (1 << <value>).
    pub rom_size: u8,
    /// Indicates how much RAM is present on the cartridge, if any. If the cartridge type does not
    /// include “RAM” in its name, this should be set to 0.
    pub ram_size: u8,
    /// This byte specifies whether this version of the game is intended to be sold in Japan or
    /// elsewhere.
    pub destination: u8,
    /// Specifies the version number of the game. It is usually 0x00.
    pub version: u8,
    /// Contains an 8-bit checksum computed from the cartridge header bytes $0134–014C.
    pub expected_header_checksum: u8,
    /// Contains the computed header checksum.
    pub computed_header_checksum: u8,
    /// Contains a 16-bit (big-endian) checksum simply computed as the sum of all the bytes of the
    /// cartridge ROM.
    pub expected_global_checksum: u16,
    /// Contains the computed global checksum.
    pub computed_global_checksum: u16,
    /// A two-character ASCII “licensee code” indicating the game’s publisher. It is only meaningful
    /// if the old licensee code is 0x33.
    pub new_licensee_code: u16,
    /// Used in older (pre-SGB) cartridges to specify the game’s publisher. However, the value 0x33
    /// indicates that the new licensee codes must be considered instead.
    pub old_licensee_code: u8,
    /// In older cartridges this byte was part of the title. The CGB and later models interpret this
    /// byte to decide whether to enable Color mode (“CGB Mode”) or to fall back to monochrome
    /// compatibility mode (“Non-CGB Mode”).
    pub cgb_flag: u8,
    /// Specifies whether the game supports SGB functions.
    pub sgb_flag: u8,
}

impl<T> From<T> for Header
where
    T: AsRef<[u8]>,
{
    /// Creates a new [`Header`] by parsing the relevant bytes of the [`Cartridge`] header.
    fn from(value: T) -> Self {
        let data = value.as_ref();

        let mut nintendo_logo = [0; NINTENDO_LOGO_LENGTH];
        nintendo_logo[0..NINTENDO_LOGO_LENGTH]
            .copy_from_slice(&data[NINTENDO_LOGO_START..=NINTENDO_LOGO_END]);

        let mut title_bytes = [0; 16];
        title_bytes[0..16].copy_from_slice(&data[TITLE_START_ADDR..=TITLE_END_ADDR]);

        let title = String::from_utf8_lossy(&title_bytes)
            .to_string()
            .replace('\0', "");

        let mut manufacturer_code_bytes = [0; 4];
        manufacturer_code_bytes[0..4]
            .copy_from_slice(&data[MANUFACTURER_START_ADDR..=MANUFACTURER_END_ADDR]);

        let manufacturer_code = String::from_utf8_lossy(&manufacturer_code_bytes)
            .to_string()
            .replace('\0', "");

        let new_licensee_code: u16 = (data[NEW_LICENSEE_CODE_HIGH_ADDR] as u16)
            | ((data[NEW_LICENSEE_CODE_LOW_ADDR] as u16) >> 8);

        let kind = data[TYPE_ADDR].into();

        let ram_size = match kind {
            CartridgeKind::MBC1 { ram, .. } if ram => 0,
            _ => data[RAM_SIZE_ADDR],
        };

        let mut computed_header_checksum: u8 = 0;
        for value in data
            .iter()
            .take(COMPUTE_HEADER_CHECKSUM_END)
            .skip(COMPUTE_HEADER_CHECKSUM_START)
        {
            computed_header_checksum = computed_header_checksum
                .wrapping_sub(*value)
                .wrapping_sub(1);
        }

        let mut computed_global_checksum: u16 = 0;
        for (address, value) in data.iter().enumerate() {
            if address == GLOBAL_CHECKSUM_LOW_ADDR || address == GLOBAL_CHECKSUM_HIGH_ADDR {
                continue;
            }

            computed_global_checksum = computed_global_checksum.wrapping_add(*value as u16);
        }

        let expected_global_checksum: u16 = (data[GLOBAL_CHECKSUM_LOW_ADDR] as u16)
            | ((data[GLOBAL_CHECKSUM_HIGH_ADDR] as u16) << 8);

        Self {
            nintendo_logo,
            title,
            manufacturer_code,
            cgb_flag: data[CGB_FLAG_ADDR],
            new_licensee_code,
            sgb_flag: data[SGB_FLAG_ADDR],
            kind,
            rom_size: data[ROM_SIZE_ADDR],
            ram_size,
            destination: data[DESTINATION_ADDR],
            old_licensee_code: data[OLD_LICENSEE_CODE_ADDR],
            version: data[VERSION_ADDR],
            expected_header_checksum: data[HEADER_CHECKSUM_ADDR],
            computed_header_checksum,
            expected_global_checksum,
            computed_global_checksum,
        }
    }
}

impl Header {
    /// Creates a new [`Header`] by parsing the relevant bytes from of the specified [`Cartridge`]
    /// data. Once the header is parsed, the checksum is calculated and compared with the expected
    /// value in order to validate the cartridge contents.
    pub fn parse_and_validate(data: &[u8]) -> anyhow::Result<Self> {
        let header = Self::from(data);
        if !header.is_valid() {
            anyhow::bail!("invalid cartridge header checksum");
        }

        Ok(header)
    }
    /// Determines if the header is valid by verifying the expected checksum value.
    fn is_valid(&self) -> bool {
        self.expected_header_checksum == self.computed_header_checksum
    }
    /// Returns the licensee code as a string by inspecting the old license value from the header
    /// and returning the new value if necessary.
    pub fn licensee_code(&self) -> String {
        if self.old_licensee_code == OLD_LICENSEE_REDIRECT_VALUE {
            let bytes = vec![
                (self.new_licensee_code << 8) as u8,
                self.new_licensee_code as u8,
            ];

            let code = String::from_utf8(bytes).expect("new licenses code is valid");

            format!("{} (New)", code)
        } else {
            format!("{}", self.old_licensee_code).to_uppercase()
        }
    }
}

/// The [`Cartridge`] struct represents a game cartridge which is loaded into the Game Boy.
pub struct Cartridge {
    /// Name of the game cartridge.
    pub name: String,
    /// Header data of the game cartridge.
    pub header: Header,
    /// [`Mbc`] implementation for the cartridge type.
    pub mbc: Rc<RefCell<dyn Mbc>>,
}

impl Cartridge {
    /// Creates a new [`Cartridge`] from the GameBoy ROM file at the specified path.
    pub fn from_rom_file(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        tracing::debug!("load cartridge from rom file: {:?}", path.as_ref());

        let name = path
            .as_ref()
            .file_name()
            .and_then(|s| s.to_str().map(String::from))
            .unwrap_or_else(|| String::from("Unknown"));

        let data = std::fs::read(path.as_ref())
            .context(format!("read file: {}", path.as_ref().to_string_lossy()))?;

        let header = Header::parse_and_validate(&data)?;

        let mut mbc = match header.kind {
            CartridgeKind::RomOnly => RomOnly::new(),
            _ => anyhow::bail!("unsupported cartridge type: {}", header.kind),
        };

        mbc.write_block(CARTRIDGE_START_ADDR, &data);

        Ok(Self {
            name,
            header,
            mbc: Rc::new(RefCell::new(mbc)),
        })
    }
}
