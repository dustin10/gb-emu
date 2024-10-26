pub mod cart;
pub mod cpu;
pub mod mem;

use crate::{cart::Cartridge, cpu::Cpu, mem::Mmu};

use std::rc::Rc;

/// Contains the memory address the program counter should be set to after the boot screen is
/// displayed in order to start executing the cartridge instructions.
const START_INSTRUCTION: u16 = 0x0100;

/// The [`Emulator`] struct is the container that is responsible for managing all of the subsystems
/// required to emulate the Game Boy and play a game cartridge.
pub struct Emulator {
    /// [`Cpu`] that is responsible for reading, decoding and executing instructions.
    cpu: Cpu,
    /// [`Mmu`] that is used to store data to and load various types of data.
    mmu: Mmu,
    /// [`Cartridge`] that is currently loaded into the emulator.
    _cartridge: Cartridge,
}

impl Emulator {
    /// Creates a new [`Emulator`] which is capable of playing the specified [`Cartridge`].
    pub fn load(cartridge: Cartridge) -> Self {
        let cpu = Cpu::new();
        let mmu = Mmu::new(Rc::clone(&cartridge.mbc));

        Self {
            cpu,
            _cartridge: cartridge,
            mmu,
        }
    }
    /// Runs the emulator. Currently, must be called after [`Emulator::load_cartridge`].
    pub fn run(&mut self) -> anyhow::Result<()> {
        // TODO: render boot screen

        self.cpu.registers.pc = START_INSTRUCTION;

        loop {
            self.cpu.step(&mut self.mmu);
        }
    }
}
