use crate::{cartridge::Cartridge, cpu::Cpu, mem::Memory};

pub mod cartridge;
pub mod cpu;
pub mod mem;

/// The [`Emulator`] struct is the container that is responsible for managing all of the subsystems
/// required to emulate the Game Boy and play a game cartridge.
#[derive(Debug, Default)]
pub struct Emulator {
    /// [`Cpu`] that is responsible for reading, decoding and executing instructions.
    cpu: Cpu,
    /// [`Memory`] that is used to store data to and load data from by the [`Cpu`].
    memory: Memory,
    /// [`Cartridge`] that is currently loaded into the emulator.
    cartridge: Option<Cartridge>,
}

impl Emulator {
    /// Creates a new default [`Emulator`].
    pub fn new() -> Self {
        Self::default()
    }
    /// Loads the given [`Cartridge`] into the emulator so that it can be played by the user.
    pub fn load_cartridge(&mut self, cartridge: Cartridge) {
        cartridge.load(&mut self.memory);
        self.cartridge = Some(cartridge);
    }
    /// Runs the emulator. Currently, must be called after [`Emulator::load_cartridge`].
    pub fn run(&mut self) -> anyhow::Result<()> {
        match &self.cartridge {
            None => anyhow::bail!("no Cartridge loaded"),
            Some(cartridge) => {
                if !cartridge.header.is_valid() {
                    anyhow::bail!("Cartridge header checksum mismatch");
                }

                loop {
                    self.cpu.step(&mut self.memory);
                }
            }
        }
    }
}
