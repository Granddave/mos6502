use std::io::Write;

use crate::emulator::cpu::Register;

use super::{
    cpu::{Cpu, RunOption, STACK_BASE, STACK_POINTER_START},
    memory::Memory,
};

pub struct Tui<'a> {
    cpu: &'a mut Cpu,
    memory: &'a mut Memory,

    last_command: Option<Command>,
    exit: bool,
}

enum Command {
    Step,
    Continue,
    Quit,
}

impl<'a> Tui<'a> {
    pub fn new(cpu: &'a mut Cpu, memory: &'a mut Memory) -> Self {
        Self {
            cpu,
            memory,
            last_command: None,
            exit: false,
        }
    }

    fn execute_command(&mut self, cmd: Command) {
        match cmd {
            Command::Step => {
                self.cpu.step(&mut self.memory);
                self.draw();
            }
            Command::Continue => {
                self.cpu
                    .run(&mut self.memory, RunOption::StopOnBreakInstruction);
                println!("Program finished");
                self.draw();
            }
            Command::Quit => {
                self.exit = true;
            }
        }

        self.last_command = Some(cmd);
    }

    pub fn exec(&mut self) {
        println!("TUI");
        println!("Press 's' to step, 'q' to quit");

        while !self.exit {
            let mut input = String::new();
            print!("> ");
            std::io::stdout().flush().unwrap();
            std::io::stdin().read_line(&mut input).unwrap();
            println!("");

            match input.trim() {
                "q" | "quit" => self.execute_command(Command::Quit),
                "s" | "step" => self.execute_command(Command::Step),
                "c" | "continue" => self.execute_command(Command::Continue),
                "" => {
                    if let Some(command) = self.last_command.take() {
                        self.execute_command(command);
                    }
                }
                command => println!("Unknown command '{}'", command),
            }
        }
    }

    fn draw(&self) {
        let mut output = String::new();
        println!("============");
        println!("Registers:");
        output.push_str(&format!("A:  0x{:02x}\n", self.cpu.register(Register::A)));
        output.push_str(&format!("X:  0x{:02x}\n", self.cpu.register(Register::X)));
        output.push_str(&format!("Y:  0x{:02x}\n", self.cpu.register(Register::Y)));
        output.push_str(&format!("PC: 0x{:04x}\n", self.cpu.program_counter()));
        output.push_str(&format!("SP: 0x{:02x}\n", self.cpu.stack_pointer()));
        output.push_str(&format!("S:  {:08b}\n", u8::from(self.cpu.status())));
        output.push_str("    NV-BDIZC\n");
        println!("{}", output);

        println!("------------");
        println!("Stack:");
        self.memory.dump(
            STACK_BASE + self.cpu.stack_pointer() as u16,
            STACK_BASE + STACK_POINTER_START as u16,
        );
    }
}
