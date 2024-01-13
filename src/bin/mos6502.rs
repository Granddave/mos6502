use anyhow::Result;
use clap::{Parser, Subcommand};
use tracing_chrome::{ChromeLayerBuilder, FlushGuard};
use tracing_subscriber::prelude::*;

use mos6502::{
    assembler::{assemble, AssemblyArgs},
    disassembler::{disassemble, DisassemblyArgs},
    emulator::{emulate, EmulationArgs},
};

#[derive(Parser)]
#[command(version)]
#[command(propagate_version = true)]
struct Cli {
    #[clap(long)]
    trace: bool,
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Assemble(AssemblyArgs),
    Disassemble(DisassemblyArgs),
    Emulate(EmulationArgs),
}

pub fn trace() -> FlushGuard {
    let (chrome_layer, _guard) = ChromeLayerBuilder::new().build();
    tracing_subscriber::registry().with(chrome_layer).init();
    return _guard;
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let _trace_guard = if cli.trace { Some(trace()) } else { None };

    match &cli.command {
        Command::Assemble(args) => assemble(args),
        Command::Disassemble(args) => disassemble(args),
        Command::Emulate(args) => emulate(args),
    }
}
