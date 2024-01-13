use anyhow::{Context, Result};
use clap::Parser;

use mos6502::instrumentation::trace;

const PROGRAM_START: u16 = 0x8000;

#[derive(Parser)]
struct Cli {
    #[clap(short, value_name = "FILENAME", default_value = "a.bin")]
    output: String,
    #[clap(long)]
    trace: bool,
    input: String,
}

#[tracing::instrument]
fn main() -> Result<()> {
    let cli = Cli::parse();
    let _trace_guard = if cli.trace { Some(trace()) } else { None };

    let input_source = std::fs::read_to_string(cli.input).with_context(|| "Unable to read file")?;
    let bytes = mos6502::assembler::compile_code(&input_source, PROGRAM_START)
        .with_context(|| "Compilation failed")?;
    std::fs::write(&cli.output, bytes).with_context(|| "Unable to write file")?;

    Ok(())
}
