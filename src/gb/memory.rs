#[derive(Debug)]
pub struct MemoryBus {
    data: [u8; 0xFFFF],
}

impl MemoryBus {
    pub fn read_byte(&self, address: u16) -> u8 {
        self.data[address as usize]
    }
    pub fn write_byte(&mut self, address: u16, byte: u8) {
        self.data[address as usize] = byte;
    }
}

impl Default for MemoryBus {
    fn default() -> Self {
        Self { data: [0; 0xFFFF] }
    }
}
