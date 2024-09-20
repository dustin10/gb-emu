use std::path::PathBuf;

pub struct Cartridge {
    data: Vec<u8>,
}

impl Cartridge {
    pub fn load_from_file(path: impl AsRef<PathBuf>) -> anyhow::Result<Self> {
        let data = std::fs::read(path.as_ref())?;

        Ok(Self { data })
    }
}
