use anyhow::{Context, Result};
use clap::Parser;

use mos6502::{
    ast::Node,
    disassembler::{disassemble_code, listing},
    instrumentation::trace,
};

#[derive(Parser)]
struct Cli {
    #[clap(long)]
    trace: bool,
    input: String,
}

#[tracing::instrument]
fn main() -> Result<()> {
    let cli = Cli::parse();
    let _trace_guard = if cli.trace { Some(trace()) } else { None };

    let bytes = std::fs::read(cli.input).with_context(|| "Unable to read file")?;
    let ast = disassemble_code(&bytes)
        .into_iter()
        .map(Node::Instruction)
        .collect();
    println!("{}", listing::generate(0x8000, ast));

    Ok(())
}
