use mos6502::emulator::cpu::RunOption;
use mos6502::{
    assembler::assemble_code,
    emulator::{
        cpu,
        cpu::Cpu,
        memory::{Bus, Memory},
    },
};

#[test]
fn test_loop_program() {
    let program_start = 0x8000;

    let source = include_str!("../examples/loop.asm");
    let bytes = assemble_code(source, 0).unwrap();

    let mut cpu = Cpu::new();
    let mut memory = Memory::new();
    memory.load(0x0000, &bytes);
    memory.write_word(cpu::RESET_VECTOR, program_start); // TODO: Include in the program

    cpu.reset();
    cpu.run(&mut memory, RunOption::UntilCycles(200));

    assert_eq!(memory.read_byte(0x0000), 0x0a);
}
