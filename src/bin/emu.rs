use anyhow::{Context, Result};

use clap::Parser;
use mos6502::{
    assembler::compile_code,
    emulator::{self, tui},
    instrumentation::trace,
};

// TODO: Either pass this as an argument, or check 0xfffc...?
const PROGRAM_START: u16 = 0x8000;

#[derive(Parser)]
struct Cli {
    #[clap(long)]
    trace: bool,
    #[clap(long)]
    tui: bool,
    input: String,
}

fn get_program_bytes(filename: &str) -> anyhow::Result<Vec<u8>> {
    if filename.ends_with(".bin") {
        // Read binary file
        std::fs::read(filename).with_context(|| "Unable to read file")
    } else if filename.ends_with(".asm") {
        // Read assembly file
        let input_source =
            std::fs::read_to_string(filename).with_context(|| "Unable to read file")?;
        compile_code(&input_source, PROGRAM_START).with_context(|| "Compilation failed")
    } else {
        return Err(anyhow::anyhow!("Unknown file type. Allowed: .bin, .asm"));
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let _trace_guard = if cli.trace { Some(trace()) } else { None };

    let program_bytes = get_program_bytes(&cli.input)?;

    if cli.tui {
        tui::exec(&program_bytes, PROGRAM_START)?;
    } else {
        emulator::run(&program_bytes, PROGRAM_START)?;
    };

    Ok(())
}
