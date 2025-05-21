use crate::{
    disassembler::{disassemble_code, listing},
    emulator::{
        bus::{Bus, Readable, Writeable},
        cpu::{self, Cpu, STACK_BASE, STACK_PAGE},
    },
};
use anyhow::Result;

use self::{state::EmulationState, widget::AppWidget};

pub mod state;
pub mod widget;

const MEMORY_SCROLL_PAGE: usize = 0x10;
const MEMORY_SCROLL_MAX: usize = 0xff;

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub enum RunMode {
    #[default]
    Step,
    Run,
}

#[derive(Default)]
pub struct App {
    /// Emulated CPU
    cpu: Cpu,
    /// Emulated memory
    memory: Bus,

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
    pub _memory_frame_height: usize,

    /// State of the CPU
    state: EmulationState,

    pub selected_widget: AppWidget,

    pub run_mode: RunMode,

    /// If the app should quit
    should_quit: bool,
}

impl App {
    pub fn new(program: &[u8], program_start: u16) -> Result<Self> {
        // turn a Vec<Instruction> into a Vec<(usize, String)>
        // where the usize is the memory address of the instruction.
        // TODO: Refactor this into a function
        let disassembly: Vec<(usize, String)> =
            disassemble_code(&program[program_start as usize..])?
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
            memory: Bus::new(),
            ..Default::default()
        };

        app.reset();
        Ok(app)
    }

    pub fn should_quit(&self) -> bool {
        self.should_quit
    }

    /// Resets the application with the provided program.
    pub fn reset(&mut self) {
        self.memory = Bus::new();
        self.memory
            .write_word(cpu::RESET_VECTOR, self.program_start); // TODO: Include in the program
        self.memory.load(0x0000, &self.program);

        self.cpu = Cpu::new();
        self.cpu.reset();
        self.state.invalidate();

        self.run_mode = RunMode::Step;
    }

    /// Quits the application.
    pub fn quit(&mut self) {
        self.should_quit = true;
    }

    /// Clock the CPU by one cycle.
    pub fn clock(&mut self) {
        match self.run_mode {
            RunMode::Run => self.step_cpu(),
            RunMode::Step => (),
        }
    }

    /// Steps the CPU by one instruction.
    pub fn step_cpu(&mut self) {
        match self.run_mode {
            RunMode::Run => self.cpu.clock(&mut self.memory),
            RunMode::Step => self.cpu.step(&mut self.memory),
        }
        self.state.invalidate();
    }

    /// Run CPU execution until a break instruction is reached.
    pub fn toggle_run_step_mode(&mut self) {
        match self.run_mode {
            RunMode::Run => self.run_mode = RunMode::Step,
            RunMode::Step => self.run_mode = RunMode::Run,
        }
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
        self.memory.data()[start..end].as_ref()
    }

    pub fn stack_memory(&self) -> &[u8] {
        let sp_addr = STACK_PAGE + self.state.sp.get() as u16 + 1;
        self.memory_slice(sp_addr as usize, STACK_BASE as usize + 1)
    }

    pub fn scroll_up(&mut self) {
        match self.selected_widget {
            AppWidget::Disassembly => {
                self.disassembly_widget_scroll = self.disassembly_widget_scroll.saturating_sub(1);
            }
            AppWidget::Memory => {
                self.memory_page_to_display = self.memory_page_to_display.saturating_sub(1)
            }
            _ => {}
        }
    }

    pub fn scroll_up_page(&mut self) {
        match self.selected_widget {
            AppWidget::Disassembly => {
                self.disassembly_widget_scroll = self
                    .disassembly_widget_scroll
                    .saturating_sub(self.disassembly_frame_height)
            }
            AppWidget::Memory => {
                self.memory_page_to_display = self
                    .memory_page_to_display
                    .saturating_sub(MEMORY_SCROLL_PAGE)
            }
            _ => {}
        }
    }

    pub fn scroll_down(&mut self) {
        match self.selected_widget {
            AppWidget::Disassembly => {
                if self.disassembly_widget_scroll
                    < self.disassembled_program.len().saturating_sub(1)
                {
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
                let max_scroll = self.disassembled_program.len().saturating_sub(1);
                if self.disassembly_widget_scroll
                    < max_scroll.saturating_sub(self.disassembly_frame_height)
                {
                    self.disassembly_widget_scroll += self.disassembly_frame_height;
                } else {
                    self.disassembly_widget_scroll = max_scroll;
                }
            }
            AppWidget::Memory => {
                if self.memory_page_to_display
                    < MEMORY_SCROLL_MAX.saturating_sub(MEMORY_SCROLL_PAGE)
                {
                    self.memory_page_to_display += MEMORY_SCROLL_PAGE;
                } else {
                    self.memory_page_to_display = MEMORY_SCROLL_MAX;
                }
            }
            _ => {}
        }
    }
}
