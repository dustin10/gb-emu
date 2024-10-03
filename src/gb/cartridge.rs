use crate::gb::mem::Memory;

use anyhow::Context;
use std::path::PathBuf;

const PROGRAM_START_ADDRESS: u16 = 0x0000;

#[derive(Debug, Default)]
pub struct Cartridge {
    pub name: String,
    data: Vec<u8>,
}

impl Cartridge {
    pub fn new(name: String, data: Vec<u8>) -> Self {
        Self { name, data }
    }
    pub fn from_rom(path: impl AsRef<PathBuf>) -> anyhow::Result<Self> {
        tracing::debug!("loading cartridge from rom file: {:?}", path.as_ref());

        let name = path
            .as_ref()
            .file_name()
            .and_then(|s| s.to_str().map(String::from))
            .unwrap_or_else(|| String::from("Unknown"));

        let data = std::fs::read(path.as_ref()).context(format!(
            "read rom file: {}",
            path.as_ref().to_string_lossy()
        ))?;

        Ok(Self::new(name, data))
    }
    pub fn load(&self, memory: &mut Memory) {
        memory.write_block(PROGRAM_START_ADDRESS, &self.data);
    }
}
