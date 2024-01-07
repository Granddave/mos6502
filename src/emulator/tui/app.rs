use crate::{
    disassembler::{disassemble_code, listing},
    emulator::{
        cpu::{self, Cpu, RunOption, STACK_BASE, STACK_PAGE},
        memory::{Bus, Memory},
    },
};

use self::{state::EmulationState, widget::AppWidget};

pub mod state;
pub mod widget;

const MEMORY_SCROLL_PAGE: usize = 0x10;
const MEMORY_SCROLL_MAX: usize = 0xff;

#[derive(Default)]
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
    /// with memory addresses as keys.
    pub disassembled_program: Vec<(usize, String)>,
    pub disassembly_widget_scroll: usize,
    pub disassembly_frame_height: usize,

    pub memory_page_to_display: usize,
    pub memory_frame_height: usize,

    /// State of the CPU
    state: EmulationState,

    pub selected_widget: AppWidget,

    /// If the app should quit
    should_quit: bool,
}

impl App {
    pub fn new(program: &[u8], program_start: u16) -> Self {
        // turn a Vec<Instruction> into a Vec<(usize, String)>
        // where the usize is the memory address of the instruction.
        // TODO: Refactor this into a function
        let disassembly: Vec<(usize, String)> =
            disassemble_code(&program[program_start as usize..])
                .iter()
                .scan(0, |acc, ins| {
                    let addr = *acc;
                    *acc += ins.size();
                    Some((addr, ins.clone()))
                })
                .map(|(addr, node)| {
                    let memory_addr = program_start as usize + addr;
                    let line = listing::generate_line(memory_addr, &node);
                    (memory_addr, line)
                })
                .collect();

        let mut app = Self {
            program: program.to_vec(),
            program_start,
            disassembled_program: disassembly,
            selected_widget: AppWidget::Disassembly,
            ..Default::default()
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
            .write_word(cpu::RESET_VECTOR, self.program_start); // TODO: Include in the program
        self.memory.load(0x0000, &self.program);

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

    pub fn memory_slice(&self, start: usize, end: usize) -> &[u8] {
        self.memory.slice(start, end)
    }

    pub fn stack_memory(&self) -> &[u8] {
        let sp_addr = STACK_PAGE + self.state.sp.get() as u16 + 1;
        self.memory_slice(sp_addr as usize, STACK_BASE as usize + 1)
    }

    pub fn scroll_up(&mut self) {
        match self.selected_widget {
            AppWidget::Disassembly => {
                if self.disassembly_widget_scroll > 0 {
                    self.disassembly_widget_scroll -= 1;
                }
            }
            AppWidget::Memory => {
                if self.memory_page_to_display > 0 {
                    self.memory_page_to_display -= 1;
                }
            }
            _ => {}
        }
    }

    pub fn scroll_up_page(&mut self) {
        match self.selected_widget {
            AppWidget::Disassembly => {
                if self.disassembly_widget_scroll > self.disassembly_frame_height {
                    self.disassembly_widget_scroll -= self.disassembly_frame_height;
                } else {
                    self.disassembly_widget_scroll = 0;
                }
            }
            AppWidget::Memory => {
                if self.memory_page_to_display > MEMORY_SCROLL_PAGE {
                    self.memory_page_to_display -= MEMORY_SCROLL_PAGE;
                } else {
                    self.memory_page_to_display = 0;
                }
            }
            _ => {}
        }
    }

    pub fn scroll_down(&mut self) {
        match self.selected_widget {
            AppWidget::Disassembly => {
                if self.disassembly_widget_scroll < self.disassembled_program.len() - 1 {
                    self.disassembly_widget_scroll += 1;
                }
            }
            AppWidget::Memory => {
                if self.memory_page_to_display < MEMORY_SCROLL_MAX {
                    self.memory_page_to_display += 1;
                }
            }
            _ => {}
        }
    }

    pub fn scroll_down_page(&mut self) {
        match self.selected_widget {
            AppWidget::Disassembly => {
                let max_scroll = self.disassembled_program.len() - 1;
                if self.disassembly_widget_scroll < max_scroll - self.disassembly_frame_height {
                    self.disassembly_widget_scroll += self.disassembly_frame_height;
                } else {
                    self.disassembly_widget_scroll = max_scroll;
                }
            }
            AppWidget::Memory => {
                if self.memory_page_to_display < MEMORY_SCROLL_MAX - MEMORY_SCROLL_PAGE {
                    self.memory_page_to_display += MEMORY_SCROLL_PAGE;
                } else {
                    self.memory_page_to_display = MEMORY_SCROLL_MAX;
                }
            }
            _ => {}
        }
    }
}
