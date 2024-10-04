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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory_default() {
        let memory = Memory::default();
        assert_eq!(ADDRESSABLE_MEMORY, memory.data.len());

        let non_zero = memory.data.iter().any(|b| *b != 0);
        assert!(!non_zero);
    }

    #[test]
    fn test_memory_read_byte() {
        let mut memory = Memory::new();
        memory.data[0x1010] = 200;
        let byte = memory.read_byte(0x1010);
        assert_eq!(200, byte);
    }

    #[test]
    fn test_memory_write_byte() {
        let mut memory = Memory::new();
        memory.write_byte(0x1010, 200);
        assert_eq!(200, memory.data[0x1010]);
    }

    #[test]
    fn test_memory_write_block() {
        let block = [200, 201, 202];

        let mut memory = Memory::new();
        memory.write_block(0x1010, &block);

        assert_eq!(200, memory.data[0x1010]);
        assert_eq!(201, memory.data[0x1011]);
        assert_eq!(202, memory.data[0x1012]);
    }
}
