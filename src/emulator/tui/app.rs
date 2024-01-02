use crate::emulator::{
    cpu::{self, Cpu, Register, RunOption},
    memory::{Bus, Memory},
};

pub struct App {
    cpu: Cpu,
    memory: Memory,

    /// The program to run.
    program: Vec<u8>,
    /// The start address of the program in memory.
    program_start: u16,

    should_quit: bool,
}

impl App {
    pub fn new(program: &[u8], program_start: u16) -> Self {
        let mut app = Self {
            cpu: Cpu::new(),
            memory: Memory::new(),
            program: program.to_vec(),
            program_start,
            should_quit: false,
        };
        app.reset();
        app
    }

    pub fn should_quit(&self) -> bool {
        self.should_quit
    }

    /// Resets the application with the provided program.
    pub fn reset(&mut self) {
        self.memory = Memory::new();
        self.memory
            .write_word(cpu::RESET_VECTOR, self.program_start);
        self.memory.load(self.program_start, &self.program);

        self.cpu = Cpu::new();
        self.cpu.reset();
    }

    /// Quits the application.
    pub fn quit(&mut self) {
        self.should_quit = true;
    }

    /// Steps the CPU by one instruction.
    pub fn step_cpu(&mut self) {
        self.cpu.step(&mut self.memory);
    }

    /// Run CPU execution until a break instruction is reached.
    pub fn continue_execution(&mut self) {
        self.cpu
            .run(&mut self.memory, RunOption::StopOnBreakInstruction);
    }

    /// Get the status of the CPU in a string format.
    pub fn status(&self) -> String {
        let mut output = String::new();

        output.push_str("Registers:\n");
        output.push_str(&format!("A:  0x{:02x}\n", self.cpu.register(Register::A)));
        output.push_str(&format!("X:  0x{:02x}\n", self.cpu.register(Register::X)));
        output.push_str(&format!("Y:  0x{:02x}\n", self.cpu.register(Register::Y)));
        output.push_str(&format!("PC: 0x{:04x}\n", self.cpu.program_counter()));
        output.push_str(&format!("SP: 0x{:02x}\n", self.cpu.stack_pointer()));
        output.push_str(&format!("S:  {:08b}\n", u8::from(self.cpu.status())));
        output.push_str("    NV-BDIZC\n");

        // output.push_str("------------");
        // output.push_str("Stack:");
        // self.memory.dump(
        //     STACK_BASE + self.cpu.stack_pointer() as u16,
        //     STACK_BASE + STACK_POINTER_START as u16,
        // );

        output
    }
}
