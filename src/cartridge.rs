use crate::mem::{Memory, MBC};

use anyhow::Context;
use std::path::Path;

/// Address in emulator memory where the cartridge data is loaded.
const CARTRIDGE_START_ADDR: u16 = 0x0000;

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

/// Holds the data that makes up the header of the [`Cartridge`]. The header contains data such as
/// what [`MBC`] type to use, the title of the game, who made it, etc.
#[derive(Debug)]
pub struct Header {
    /// Title of the game in uppercase ASCII.
    pub title: String,
    /// In older cartridges these bytes were part of the Title (see above). In newer cartridges they
    /// contain a 4-character manufacturer code (in uppercase ASCII). The purpose of the manufacturer
    /// code is unknown.
    pub manufacturer_code: String,
    /// Indicates what kind of hardware is present on the cartridge including the memory bank
    /// controller implementation.
    pub mbc: MBC,
    /// Indicates how much ROM is present on the cartridge. In most cases, the ROM size is given by
    /// 32 KiB × (1 << <value>).
    pub rom_size: u8,
    /// Indicates how much RAM is present on the cartridge, if any. If the cartridge type does not include
    /// “RAM” in its name, this should be set to 0.
    pub ram_size: u8,
    /// This byte specifies whether this version of the game is intended to be sold in Japan or elsewhere.
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
    /// A two-character ASCII “licensee code” indicating the game’s publisher. It is only meaningful if
    /// the old licensee code is 0x33.
    pub new_licensee_code: u16,
    /// Used in older (pre-SGB) cartridges to specify the game’s publisher. However, the value 0x33
    /// indicates that the new licensee codes must be considered instead.
    pub old_licensee_code: u8,
    /// In older cartridges this byte was part of the title. The CGB and later models interpret this byte
    /// to decide whether to enable Color mode (“CGB Mode”) or to fall back to monochrome compatibility
    /// mode (“Non-CGB Mode”).
    pub cgb_flag: u8,
    /// Specifies whether the game supports SGB functions.
    pub sgb_flag: u8,
}

impl Header {
    /// Creates a new [`Header`] by parsing the relevant bytes from of the [`Cartridge`] data.
    fn new(data: &[u8]) -> Self {
        let mut title_bytes = [0; 16];
        title_bytes[0..16].copy_from_slice(&data[TITLE_START_ADDR..=TITLE_END_ADDR]);

        let title = String::from_utf8_lossy(&title_bytes).to_string();

        let mut manufacturer_code_bytes = [0; 4];
        manufacturer_code_bytes[0..4]
            .copy_from_slice(&data[MANUFACTURER_START_ADDR..=MANUFACTURER_END_ADDR]);

        let manufacturer_code = String::from_utf8_lossy(&manufacturer_code_bytes).to_string();

        let new_licensee_code: u16 = (data[NEW_LICENSEE_CODE_HIGH_ADDR] as u16)
            | ((data[NEW_LICENSEE_CODE_LOW_ADDR] as u16) >> 8);

        let mbc = data[TYPE_ADDR].into();

        let ram_size = match mbc {
            MBC::One { ram, .. } if ram => 0,
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
            title,
            manufacturer_code,
            cgb_flag: data[CGB_FLAG_ADDR],
            new_licensee_code,
            sgb_flag: data[SGB_FLAG_ADDR],
            mbc,
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
    /// Determines if the header is valid by verifying the expected checksum value.
    pub fn is_valid(&self) -> bool {
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
            format!("{:2x}", self.old_licensee_code).to_uppercase()
        }
    }
}

/// The [`Cartridge`] struct represents a game cartridge which is loaded into the Game Boy.
#[derive(Debug)]
pub struct Cartridge {
    /// Name of the game catridge.
    pub name: String,
    /// Raw data contained in the cartridge.
    data: Vec<u8>,
    /// Header data of the game cartridge.
    pub header: Header,
}

impl Cartridge {
    /// Creates a new [`Cartridge`] with the given values.
    pub fn new(name: String, data: Vec<u8>) -> Self {
        let header = Header::new(&data);

        Self { name, data, header }
    }
    /// Creates a new [`Cartridge`] by loading a ROM file from the given file path on disk.
    pub fn from_rom(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        tracing::debug!("load cartridge from rom file: {:?}", path.as_ref());

        let name = path
            .as_ref()
            .file_name()
            .and_then(|s| s.to_str().map(String::from))
            .unwrap_or_else(|| String::from("Unknown"));

        let data = std::fs::read(path.as_ref())
            .context(format!("read file: {}", path.as_ref().to_string_lossy()))?;

        Ok(Self::new(name, data))
    }
    /// Loads the cartridge data into emulator memory.
    pub fn load<M>(&self, memory: &mut M)
    where
        M: Memory,
    {
        memory.write_block(CARTRIDGE_START_ADDR, &self.data);
    }
}
