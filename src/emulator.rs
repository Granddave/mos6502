use anyhow::{Context, Result};
use clap::Args;

use crate::{
    assembler::compile_code,
    emulator::{
        cpu::{Cpu, RunOption},
        memory::{Bus, Memory},
    },
};

pub mod cpu;
pub mod memory;
pub mod tui;

#[derive(Args, Debug)]
pub struct EmulationArgs {
    input: String,
    #[clap(long)]
    tui: bool,
}

/// Simple utility function to run the emulator without the terminal user interface.
/// Runs the emulator with the given program bytes and program start address.
#[tracing::instrument]
pub fn run(program_bytes: &[u8], program_start: u16) -> Result<()> {
    let mut memory = Memory::new();
    memory.write_word(cpu::RESET_VECTOR, program_start); // TODO: Include in the program
    memory.load(0x0000, program_bytes);
    let mut cpu = Cpu::new();
    cpu.reset();
    cpu.run(&mut memory, RunOption::StopOnBreakInstruction);
    println!("Program finished");

    Ok(())
}

#[tracing::instrument]
pub fn emulate(args: &EmulationArgs) -> Result<()> {
    const PROGRAM_START: u16 = 0x8000;
    let program_bytes = if args.input.ends_with(".bin") {
        // Read binary file
        std::fs::read(&args.input).with_context(|| "Unable to read file")?
    } else if args.input.ends_with(".asm") {
        // Read assembly file
        let input_source =
            std::fs::read_to_string(&args.input).with_context(|| "Unable to read file")?;
        compile_code(&input_source, PROGRAM_START).with_context(|| "Compilation failed")?
    } else {
        return Err(anyhow::anyhow!("Unknown file type. Allowed: .bin, .asm"));
    };
    if args.tui {
        tui::exec(&program_bytes, PROGRAM_START)?;
    } else {
        run(&program_bytes, PROGRAM_START)?;
    };
    Ok(())
}
