use anyhow::Context;
use clap::Parser;
use gb_emu::{
    cartridge::Cartridge,
    cpu::Cpu,
    mem::{RomOnly, MBC},
    Emulator,
};
use tracing::metadata::LevelFilter;
use tracing_subscriber::EnvFilter;

/// A Game Boy emulator.
#[derive(Debug, Parser)]
struct Args {
    /// Specifies the path to the ROM file that the emulator will run.
    #[arg(
        short,
        long,
        help = "Required. Specifies the path to the ROM file to run."
    )]
    rom: String,
}

/// Main entry point into the emulator application.
fn main() -> anyhow::Result<()> {
    // Default to INFO logs but allow the RUST_LOG env variable to override.
    tracing_subscriber::fmt()
        .pretty()
        .with_level(true)
        .with_target(true)
        .with_file(true)
        .with_line_number(true)
        .with_thread_ids(true)
        .with_thread_names(true)
        .with_env_filter(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();

    let args = Args::parse();

    let cpu = Cpu::new();

    let cartridge = Cartridge::from_rom(args.rom).context("create Cartridge from ROM file")?;

    let memory = match cartridge.header.mbc {
        MBC::NoMapping => RomOnly::new(),
        _ => panic!("unsupported MBC type: {}", cartridge.header.mbc),
    };

    Emulator::new(cpu, memory, cartridge).run()
}
