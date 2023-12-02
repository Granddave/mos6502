use mos6502::emulator::{cpu::Cpu, memory::Memory};

fn main() {
    let mut cpu = Cpu::new();
    let mut memory = Memory::new();
    cpu.run(&mut memory, 1000);
}
