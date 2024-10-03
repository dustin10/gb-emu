use crate::gb::{cartridge::Cartridge, cpu::Cpu, mem::Memory};

pub mod cartridge;
pub mod cpu;
pub mod mem;

#[derive(Debug, Default)]
pub struct Emulator {
    _cpu: Cpu,
    memory: Memory,
    cartridge: Option<Cartridge>,
}

impl Emulator {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn load_cartridge(&mut self, cartridge: Cartridge) {
        cartridge.load(&mut self.memory);
        self.cartridge = Some(cartridge);
    }
    pub fn run(&mut self) -> anyhow::Result<()> {
        todo!()
    }
}
