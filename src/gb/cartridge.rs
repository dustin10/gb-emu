use std::path::PathBuf;

#[derive(Default)]
pub struct Cartridge {
    _data: Vec<u8>,
}

impl Cartridge {
    pub fn load_from_file(path: impl AsRef<PathBuf>) -> anyhow::Result<Self> {
        let data = std::fs::read(path.as_ref())?;

        Ok(Self { _data: data })
    }
}
