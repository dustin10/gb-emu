use crate::{cartridge::Cartridge, cpu::Cpu, mem::Memory};

pub mod cartridge;
pub mod cpu;
pub mod mem;

/// The [`Emulator`] struct is the container that is responsible for managing all of the subsystems
/// required to emulate the Game Boy and play a game cartridge.
#[derive(Debug)]
pub struct Emulator<M>
where
    M: Memory,
{
    /// [`Cpu`] that is responsible for reading, decoding and executing instructions.
    cpu: Cpu,
    /// [`Memory`] that is used to store data to and load data from by the [`Cpu`].
    memory: M,
    /// [`Cartridge`] that is currently loaded into the emulator.
    cartridge: Cartridge,
}

impl<M> Emulator<M>
where
    M: Memory,
{
    /// Creates a new [`Emulator`] with the given values.
    pub fn new(cpu: Cpu, memory: M, cartridge: Cartridge) -> Self {
        Self {
            cpu,
            cartridge,
            memory,
        }
    }
    /// Runs the emulator. Currently, must be called after [`Emulator::load_cartridge`].
    pub fn run(&mut self) -> anyhow::Result<()> {
        if !self.cartridge.header.is_valid() {
            anyhow::bail!("Cartridge header checksum mismatch");
        }

        self.cartridge.load(&mut self.memory);

        loop {
            self.cpu.step(&mut self.memory);
        }
    }
}
