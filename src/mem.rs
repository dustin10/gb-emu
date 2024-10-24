use std::fmt::Display;

/// Defines the maximum size of the addressable memory for the emulator.
const ADDRESSABLE_MEMORY: usize = 65536;

/// Enumerates the supported memory bank controller (MBC) implementations.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum MBC {
    /// No mapping required.
    NoMapping,
    /// MBC1
    One { ram: bool, battery: bool },
}

pub trait Memory {
    /// Reads a single byte from memory at the given address.
    fn read_u8(&self, address: u16) -> u8;
    /// Reads the next two bytes from the given address in memory and combines them into a single
    /// u16 value.
    fn read_u16(&self, address: u16) -> u16;
    /// Reads a single 8 bit signed integer from memory at the given address.
    fn read_i8(&self, address: u16) -> i8;
    /// Writes a single byte to memory at the given address.
    fn write_u8(&mut self, address: u16, byte: u8);
    /// Writes a block of bytes to memory at the given start address.
    fn write_block(&mut self, start_addr: u16, bytes: &[u8]);
}

impl From<u8> for MBC {
    /// Creates the appropraite [`MBC`] that maps to the given [`u8`] which represents the memory
    /// bank controller that a [`crate::cartridge::Cartridge`] requires. This value is read from
    /// the cartridge header.
    ///
    /// # Panic
    ///
    /// This function will panic when the MBC which the byte maps to has not yet been implemented.
    fn from(value: u8) -> Self {
        match value {
            0x00 => MBC::NoMapping,
            0x01 => MBC::One {
                ram: true,
                battery: true,
            },
            0x02 => MBC::One {
                ram: true,
                battery: false,
            },
            0x03 => MBC::One {
                ram: true,
                battery: true,
            },
            _ => panic!("unsupported MBC type: {:#2x}", value),
        }
    }
}

impl Display for MBC {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MBC::NoMapping => f.write_str("ROM Only"),
            MBC::One { ram, battery } => {
                f.write_fmt(format_args!("MBC1 RAM:{} Battery:{}", ram, battery))
            }
        }
    }
}

/// [`RomOnly`] represents the emulator memory bus
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

impl Memory for RomOnly {
    /// Reads a single byte from memory at the given address.
    fn read_u8(&self, address: u16) -> u8 {
        self.data[address as usize]
    }
    /// Reads the next two bytes from the given address in memory and combines them into a single
    /// u16 value.
    fn read_u16(&self, address: u16) -> u16 {
        let low = self.read_u8(address);
        let high = self.read_u8(address + 1);

        (high as u16) << 8 | low as u16
    }
    /// Reads a single 8 bit signed integer from memory at the given address.
    fn read_i8(&self, address: u16) -> i8 {
        self.read_u8(address) as i8
    }
    /// Writes a single byte to memory at the given address.
    fn write_u8(&mut self, address: u16, byte: u8) {
        self.data[address as usize] = byte;
    }
    /// Writes a block of bytes to memory at the given start address.
    fn write_block(&mut self, start_addr: u16, bytes: &[u8]) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rom_only_default() {
        let memory = RomOnly::default();
        assert_eq!(ADDRESSABLE_MEMORY, memory.data.len());

        let non_zero = memory.data.iter().any(|b| *b != 0);
        assert!(!non_zero);
    }

    #[test]
    fn test_rom_only_read_u8() {
        let mut memory = RomOnly::new();
        memory.data[0x1010] = 200;
        let byte = memory.read_u8(0x1010);
        assert_eq!(200, byte);
    }

    #[test]
    fn test_rom_only_read_u16() {
        let mut memory = RomOnly::new();
        memory.data[0x1010] = 0x01;
        memory.data[0x1011] = 0x01;
        let value = memory.read_u16(0x1010);
        assert_eq!(0x0101, value);
    }

    #[test]
    fn test_rom_only_read_i8() {
        {
            let mut memory = RomOnly::new();
            memory.data[0x1010] = 1;
            let value = memory.read_i8(0x1010);
            assert_eq!(1, value);
        }
        {
            let mut memory = RomOnly::new();
            memory.data[0x1010] = 0xFF;
            let value = memory.read_i8(0x1010);
            assert_eq!(-1, value);
        }
    }

    #[test]
    fn test_rom_only_write_byte() {
        let mut memory = RomOnly::new();
        memory.write_u8(0x1010, 200);
        assert_eq!(200, memory.data[0x1010]);
    }

    #[test]
    fn test_rom_only_write_block() {
        let block = [200, 201, 202];

        let mut memory = RomOnly::new();
        memory.write_block(0x1010, &block);

        assert_eq!(200, memory.data[0x1010]);
        assert_eq!(201, memory.data[0x1011]);
        assert_eq!(202, memory.data[0x1012]);
    }
}
