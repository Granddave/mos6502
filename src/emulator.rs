use crate::emulator::{
    cpu::{Cpu, RunOption},
    memory::{Bus, Memory},
};

pub mod cpu;
pub mod memory;
pub mod tui;

/// Simple utility function to run the emulator without the terminal user interface.
/// Runs the emulator with the given program bytes and program start address.
pub fn run(program_bytes: &[u8], program_start: u16) -> anyhow::Result<()> {
    let mut memory = Memory::new();
    memory.write_word(cpu::RESET_VECTOR, program_start); // TODO: Include in the program
    memory.load(0x0000, program_bytes);
    let mut cpu = Cpu::new();
    cpu.reset();
    cpu.run(&mut memory, RunOption::StopOnBreakInstruction);
    println!("Program finished");

    Ok(())
}
