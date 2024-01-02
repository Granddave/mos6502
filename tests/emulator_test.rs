use mos6502::emulator::cpu::RunOption;
use mos6502::{
    assembler::compile_code,
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
    let bytes = compile_code(source, program_start).unwrap();

    let mut cpu = Cpu::new();
    let mut memory = Memory::new();
    memory.load(program_start, &bytes);
    memory.write_word(cpu::RESET_VECTOR, program_start); // TODO: Include in the program

    cpu.reset();
    cpu.run(&mut memory, RunOption::StopOnBreakInstruction);

    assert_eq!(memory.read_byte(0x0000), 0x0a);
}
