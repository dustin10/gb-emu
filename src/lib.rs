#![allow(unused)]

use crate::{cpu::Cpu, mem::Memory};

use anyhow::Context;
use std::path::Path;

pub mod cpu;
pub mod mem;

/// Address in emulator memory where the cartridge data is loaded.
const CARTRIDGE_START_ADDRESS: u16 = 0x0000;

/// The [`Cartridge`] struct represents a game cartridge which is loaded into the Game Boy.
#[derive(Debug, Default)]
pub struct Cartridge {
    /// Name of the game catridge.
    pub name: String,
    /// Raw data contained in the cartridge.
    data: Vec<u8>,
}

impl Cartridge {
    /// Creates a new [`Cartridge`] with the given values.
    pub fn new(name: String, data: Vec<u8>) -> Self {
        Self { name, data }
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
        memory.write_bytes(CARTRIDGE_START_ADDRESS, &self.data);
    }
}

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
        if self.cartridge.is_none() {
            anyhow::bail!("no Cartridge loaded");
        }

        loop {
            self.cpu.step(&mut self.memory);
        }
    }
}
