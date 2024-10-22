use crate::mem::{Memory, MBC};

use anyhow::Context;
use std::path::Path;

/// Address in emulator memory where the cartridge data is loaded.
const CARTRIDGE_START_ADDRESS: u16 = 0x0000;

/// Value set for the old licensee in the header when the new licensee value should be read
/// instead.
const OLD_LICENSEE_REDIRECT_VALUE: u8 = 0x33;

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
        title_bytes[0..16].copy_from_slice(&data[0x0134..=0x0143]);

        let title = String::from_utf8_lossy(&title_bytes).to_string();

        let mut manufacturer_code_bytes = [0; 4];
        manufacturer_code_bytes[0..4].copy_from_slice(&data[0x013F..=0x0142]);

        let manufacturer_code = String::from_utf8_lossy(&manufacturer_code_bytes).to_string();

        let new_licensee_code: u16 = (data[0x0144] as u16) | ((data[0x0145] as u16) >> 8);

        let mbc = data[0x0147].into();

        let ram_size = match mbc {
            MBC::One { ram, .. } if ram => 0,
            _ => data[0x0149],
        };

        let mut computed_header_checksum: u8 = 0;
        for value in data.iter().take(0x014D).skip(0x0134) {
            computed_header_checksum = computed_header_checksum
                .wrapping_sub(*value)
                .wrapping_sub(1);
        }

        let mut computed_global_checksum: u16 = 0;
        for (address, value) in data.iter().enumerate() {
            if address == 0x014E || address == 0x014F {
                continue;
            }

            computed_global_checksum = computed_global_checksum.wrapping_add(*value as u16);
        }

        let expected_global_checksum: u16 = (data[0x014F] as u16) | ((data[0x014E] as u16) << 8);

        Self {
            title,
            manufacturer_code,
            cgb_flag: data[0x0143],
            new_licensee_code,
            sgb_flag: data[0x0146],
            mbc,
            rom_size: data[0x0148],
            ram_size,
            destination: data[0x014A],
            old_licensee_code: data[0x014B],
            version: data[0x014C],
            expected_header_checksum: data[0x014D],
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
        println!("{:?}", header);
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
    pub fn load(&self, memory: &mut Memory) {
        memory.write_block(CARTRIDGE_START_ADDRESS, &self.data);
    }
}
