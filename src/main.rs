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
    #[clap(help = "Enable chrome tracing")]
    #[clap(long_help = "Enable chrome tracing which on program exit will generate
a json file to be opened with a chrome tracing compatible
viewer.")]
    trace: bool,
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    #[clap(about = "Assemble a program")]
    #[clap(aliases = &["a", "asm"])]
    Assemble(AssemblyArgs),
    #[clap(about = "Disassemble a binary file")]
    #[clap(aliases = &["d", "dis"])]
    Disassemble(DisassemblyArgs),
    #[clap(about = "Run a program in the emulator")]
    #[clap(aliases = &["e", "emu"])]
    Emulate(EmulationArgs),
}

pub fn trace() -> FlushGuard {
    let (chrome_layer, guard) = ChromeLayerBuilder::new().build();
    tracing_subscriber::registry().with(chrome_layer).init();

    guard
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
