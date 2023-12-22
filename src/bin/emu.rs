use anyhow::Result;

use mos6502::emulator::{cpu::Cpu, memory::Memory};

fn main() -> Result<()> {
    let mut cpu = Cpu::new();
    let mut memory = Memory::new();
    cpu.run(&mut memory, 1000);

    Ok(())
}
