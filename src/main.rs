use anyhow::Context;
use clap::Parser;
use gb_emu::{
    DebugMode,
    {cart::Cartridge, Emulator},
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
    /// Flag that if set causes the emulator to start up with debug mode set to [`DebugMode::Pause`].
    #[arg(
        short,
        long,
        help = "Optional. If set, starts the emulator in debug mode."
    )]
    debug: bool,
}

/// Main entry point into the emulator application.
fn main() -> anyhow::Result<()> {
    let (non_blocking, _guard) = tracing_appender::non_blocking(std::io::stdout());

    // Default to INFO logs but allow the RUST_LOG env variable to override.
    tracing_subscriber::fmt()
        .with_writer(non_blocking)
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

    let cartridge = Cartridge::from_rom(args.rom).context("create Cartridge from ROM file")?;

    let debug_mode = if args.debug {
        DebugMode::Pause
    } else {
        DebugMode::Disabled
    };

    Emulator::new(cartridge, debug_mode).run()
}
