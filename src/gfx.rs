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
