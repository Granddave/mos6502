use crate::{
    ast::AST,
    disassembler::disassemble_code,
    emulator::{
        cpu::{self, Cpu, RunOption},
        memory::{Bus, Memory},
    },
};

#[derive(Debug, Default, Clone)]
pub struct StateValue<T> {
    value: T,
    did_change: bool,
}

impl<T: std::cmp::PartialEq> StateValue<T> {
    pub fn set(&mut self, value: T) {
        if self.value == value {
            return;
        }
        self.value = value;
        self.did_change = true;
    }

    pub fn get(&self) -> T
    where
        T: Copy,
    {
        self.value
    }

    pub fn has_changed(&self) -> bool {
        self.did_change
    }

    pub fn invalidate(&mut self) {
        self.did_change = false;
    }
}

#[derive(Debug, Default, Clone)]
pub struct EmulationState {
    pub a: StateValue<u8>,
    pub x: StateValue<u8>,
    pub y: StateValue<u8>,
    pub pc: StateValue<u16>,
    pub sp: StateValue<u8>,

    // Status register
    pub carry: StateValue<bool>,
    pub zero: StateValue<bool>,
    pub interrupt_disable: StateValue<bool>,
    pub decimal: StateValue<bool>,
    pub break_command: StateValue<bool>,
    pub overflow: StateValue<bool>,
    pub negative: StateValue<bool>,
}

impl EmulationState {
    fn invalidate(&mut self) {
        self.a.invalidate();
        self.x.invalidate();
        self.y.invalidate();
        self.pc.invalidate();
        self.sp.invalidate();

        self.carry.invalidate();
        self.zero.invalidate();
        self.interrupt_disable.invalidate();
        self.decimal.invalidate();
        self.break_command.invalidate();
        self.overflow.invalidate();
        self.negative.invalidate();
    }
}

pub struct App {
    /// Emulated CPU
    cpu: Cpu,
    /// Emulated memory
    memory: Memory,

    /// The program to run.
    program: Vec<u8>,
    /// The start address of the program in memory.
    pub program_start: u16,
    /// A cached disassembled AST of the loaded program
    pub disassembled_program: AST,

    /// State of the CPU
    state: EmulationState,

    /// If the app should quit
    should_quit: bool,
}

impl App {
    pub fn new(program: &[u8], program_start: u16) -> Self {
        let mut app = Self {
            cpu: Cpu::new(),
            memory: Memory::new(),
            program: program.to_vec(),
            program_start,
            disassembled_program: disassemble_code(program),
            state: EmulationState::default(),
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
        self.state.invalidate();
    }

    /// Quits the application.
    pub fn quit(&mut self) {
        self.should_quit = true;
    }

    /// Steps the CPU by one instruction.
    pub fn step_cpu(&mut self) {
        self.cpu.step(&mut self.memory);
        self.state.invalidate();
    }

    /// Run CPU execution until a break instruction is reached.
    pub fn continue_execution(&mut self) {
        self.cpu
            .run(&mut self.memory, RunOption::StopOnBreakInstruction);
    }

    /// Get the last and current state of the emulation
    pub fn state(&mut self) -> EmulationState {
        let regs = self.cpu.registers();
        self.state.a.set(regs.a);
        self.state.x.set(regs.x);
        self.state.y.set(regs.y);
        self.state.pc.set(regs.pc);
        self.state.sp.set(regs.sp);

        let status = regs.status;
        self.state.carry.set(status.carry);
        self.state.zero.set(status.zero);
        self.state.interrupt_disable.set(status.interrupt_disable);
        self.state.decimal.set(status.decimal);
        self.state.break_command.set(status.break_command);
        self.state.overflow.set(status.overflow);
        self.state.negative.set(status.negative);

        self.state.clone()
    }

    pub fn memory_slice(&self, start: u16, end: u16) -> &[u8] {
        &self.memory.slice(start, end)
    }
}
