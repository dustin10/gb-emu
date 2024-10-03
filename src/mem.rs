/// Defines the maximum size of the addressable memory for the emulator.
const ADDRESSABLE_MEMORY: usize = 0xFFFF;

/// [`Memory`] represents the emulator memory bus
#[derive(Debug)]
pub struct Memory {
    /// Raw bytes of data contained in memory.
    data: [u8; ADDRESSABLE_MEMORY],
}

impl Memory {
    /// Creates a new default [`Memory`].
    pub fn new() -> Self {
        Self::default()
    }
    /// Reads a single byte to memory at the given address.
    pub fn read_byte(&self, address: u16) -> u8 {
        self.data[address as usize]
    }
    /// Writes a single byte to memory at the given address.
    pub fn write_byte(&mut self, address: u16, byte: u8) {
        self.data[address as usize] = byte;
    }
    /// Writes a block of bytes to memory at the given start address.
    pub fn write_block(&mut self, start_addr: u16, bytes: &[u8]) {
        let num_bytes = bytes.len();
        let dest_start = start_addr as usize;
        let dest_end = dest_start + num_bytes;

        self.data[dest_start..dest_end].copy_from_slice(&bytes[0..num_bytes]);
    }
}

impl Default for Memory {
    /// Creates a default [`Memory`] which has all data set to zero.
    fn default() -> Self {
        Self {
            data: [0; ADDRESSABLE_MEMORY],
        }
    }
}
