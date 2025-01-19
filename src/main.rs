use anyhow::Context;
use bounded_vec_deque::BoundedVecDeque;
use clap::Parser;
use gb_emu::{cart::Cartridge, util::MessageCaptureLayer, DebugMode, Emulator};
use std::sync::{Arc, Mutex};
use tracing::metadata::LevelFilter;
use tracing_subscriber::{prelude::*, EnvFilter, Registry};

/// Maximum size of the log history to retain in memory for display in the debug UI.
const MAX_LOG_HISTORY: usize = 100;

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
    /// Flag that if set causes the emulator to start up in debug mode.
    #[arg(
        short,
        long,
        help = "Optional. If set, starts the emulator in debug mode."
    )]
    debug: bool,
    /// Flag that if set causes the emulator to start in a paused state when debug mode is enabled.
    #[arg(
        short,
        long,
        help = "Optional. If set and in debug mode, starts the emulator in a paused state."
    )]
    paused: bool,
}

impl Args {
    /// Returns the [`DebugMode`] that the [`Emulator`] should be configured with based on the
    /// flags passed in when executing the application.
    fn debug_mode(&self) -> DebugMode {
        if self.debug {
            if self.paused {
                DebugMode::Pause
            } else {
                DebugMode::Continue
            }
        } else {
            DebugMode::Disabled
        }
    }
}

/// Main entry point into the emulator application.
fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let stdout_layer = tracing_subscriber::fmt::Layer::default()
        .pretty()
        .with_level(true)
        .with_target(true)
        .with_file(true)
        .with_line_number(true)
        .with_thread_ids(true)
        .with_thread_names(true);

    let logs = Arc::new(Mutex::new(BoundedVecDeque::new(MAX_LOG_HISTORY)));

    let debug_ui_layer = MessageCaptureLayer::new(Arc::clone(&logs));

    // Default to INFO level logs but allow the RUST_LOG env variable to override.
    let global_filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .from_env_lossy();

    Registry::default()
        .with(stdout_layer)
        .with(debug_ui_layer)
        .with(global_filter)
        .init();

    let cartridge = Cartridge::from_rom_file(&args.rom).context("create Cartridge from ROM file")?;

    Emulator::new(cartridge, args.debug_mode(), logs).run()
}
