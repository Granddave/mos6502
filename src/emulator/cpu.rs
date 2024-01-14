use self::registers::{Register, Registers, Status};
use super::memory::Memory;
use crate::{
    assembler::codegen::opcode::OPCODE_MAPPING,
    ast::{AddressingMode, Instruction, Mnemonic, Operand},
    emulator::memory::Bus,
};

pub mod registers;

// Stack offsets
/// The stack is located at the end of the page address.
pub const STACK_PAGE: u16 = 0x0100;
/// Where on the stack page the stack pointer starts.
pub const STACK_POINTER_START: u8 = 0xff;
/// Where in memory the base of the stack resides
/// This is where in memory the stack pointer starts.
pub const STACK_BASE: u16 = STACK_PAGE + STACK_POINTER_START as u16;

// Interrupt vectors
/// NMI (Non-maskable interrupt) vector
pub const NMI_VECTOR: u16 = 0xfffa;
/// Reset vector (power-on and hardware reset)
pub const RESET_VECTOR: u16 = 0xfffc;
/// IRQ (Interrupt request) maskable interrupt
pub const INTERRUPT_VECTOR: u16 = 0xfffe;

#[derive(Debug)]
pub enum RunOption {
    /// Run until the given number of cycles has been reached.
    UntilCycles(usize),
    /// Run until the program executes a BRK instruction.
    StopOnBreakInstruction,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Cpu {
    /// CPU registers
    regs: Registers,

    // Emulation variables
    /// If a reset interrupt is pending
    reset_interrupt_pending: bool,
    /// If an NMI interrupt is pending
    nmi_interrupt_pending: bool,
    /// If an IRQ interrupt is pending
    irq_interrupt_pending: bool,

    /// This is used to keep track of the current cycle in the current instruction.
    /// A new instruction is fetched and decoded once the current cycle reaches zero.
    cycles_left_for_instruction: usize,

    last_instruction: Option<Instruction>,
}

// CPU implementation
impl Cpu {
    #[tracing::instrument]
    pub fn new() -> Self {
        Self::default()
    }

    /// Fetches and decodes the next instruction from memory.
    #[tracing::instrument]
    fn fetch_and_decode(&mut self, memory: &mut Memory) -> Instruction {
        let opcode = memory.read_byte(self.regs.pc);
        let (mnemonic, addr_mode) = OPCODE_MAPPING
            .find_instruction(opcode)
            .unwrap_or_else(|| panic!("Invalid opcode: '{:#02x}'", opcode));

        let operand = match addr_mode {
            AddressingMode::Absolute
            | AddressingMode::AbsoluteX
            | AddressingMode::AbsoluteY
            | AddressingMode::Indirect => Operand::Absolute(memory.read_word(self.regs.pc + 1)),
            AddressingMode::ZeroPage
            | AddressingMode::ZeroPageX
            | AddressingMode::ZeroPageY
            | AddressingMode::IndirectIndexedX
            | AddressingMode::IndirectIndexedY => {
                Operand::ZeroPage(memory.read_byte(self.regs.pc + 1))
            }
            AddressingMode::Relative => Operand::Relative(memory.read_byte(self.regs.pc + 1) as i8),
            AddressingMode::Immediate => Operand::Immediate(memory.read_byte(self.regs.pc + 1)),
            AddressingMode::Accumulator | AddressingMode::Implied => Operand::Implied,
            _ => panic!("Invalid addressing mode: '{:#?}'", addr_mode),
        };

        Instruction::new(mnemonic, addr_mode, operand)
    }

    #[tracing::instrument]
    fn execute_instruction(&mut self, ins: &Instruction, memory: &mut Memory) -> usize {
        match (&ins.mnemonic, &ins.addr_mode, &ins.operand) {
            (Mnemonic::ADC, _, Operand::Immediate(value)) => {
                self.add_with_carry(*value);
                2
            }
            (Mnemonic::ADC, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.add_with_carry(memory.read_byte(*addr as u16));
                3
            }
            (Mnemonic::ADC, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                self.add_with_carry(memory.read_byte((*addr + self.regs.x) as u16));
                4
            }
            (Mnemonic::ADC, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.add_with_carry(memory.read_byte(*addr));
                4
            }
            (Mnemonic::ADC, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.add_with_carry(memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::ADC, AddressingMode::AbsoluteY, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.add_with_carry(memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::ADC, AddressingMode::IndirectIndexedX, Operand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.add_with_carry(memory.read_byte(indirect_addr));
                6
            }
            (Mnemonic::ADC, AddressingMode::IndirectIndexedY, Operand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.add_with_carry(memory.read_byte(indexed_addr & 0xff));
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // AND
            (Mnemonic::AND, _, Operand::Immediate(value)) => {
                self.regs.a &= *value;
                self.set_zero_and_negative_flags(self.regs.a);
                2
            }
            (Mnemonic::AND, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.regs.a &= memory.read_byte(*addr as u16);
                self.set_zero_and_negative_flags(self.regs.a);
                3
            }
            (Mnemonic::AND, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                self.regs.a &= memory.read_byte((*addr + self.regs.x) as u16);
                self.set_zero_and_negative_flags(self.regs.a);
                4
            }
            (Mnemonic::AND, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.regs.a &= memory.read_byte(*addr);
                self.set_zero_and_negative_flags(self.regs.a);
                4
            }
            (Mnemonic::AND, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.regs.a &= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.regs.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::AND, AddressingMode::AbsoluteY, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.regs.a &= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.regs.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::AND, AddressingMode::IndirectIndexedX, Operand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.regs.a &= memory.read_byte(indirect_addr);
                self.set_zero_and_negative_flags(self.regs.a);
                6
            }
            (Mnemonic::AND, AddressingMode::IndirectIndexedY, Operand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.regs.a &= memory.read_byte(indexed_addr & 0xff);
                self.set_zero_and_negative_flags(self.regs.a);
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // ASL
            (Mnemonic::ASL, AddressingMode::Accumulator, Operand::Implied) => {
                self.regs.a = self.shift_left(self.regs.a);
                2
            }
            (Mnemonic::ASL, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_left(value));
                5
            }
            (Mnemonic::ASL, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                let addr = (*addr + self.regs.x) as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_left(value));
                6
            }
            (Mnemonic::ASL, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                let value = memory.read_byte(*addr);
                memory.write_byte(*addr, self.shift_left(value));
                6
            }
            (Mnemonic::ASL, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.regs.x as u16);
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_left(value));
                7
            }
            // Branch instructions
            (Mnemonic::BCC, _, Operand::Relative(offset)) => {
                self.branch_on_condition(!self.regs.status.carry, *offset)
            }
            (Mnemonic::BCS, _, Operand::Relative(offset)) => {
                self.branch_on_condition(self.regs.status.carry, *offset)
            }
            (Mnemonic::BEQ, _, Operand::Relative(offset)) => {
                self.branch_on_condition(self.regs.status.zero, *offset)
            }
            (Mnemonic::BMI, _, Operand::Relative(offset)) => {
                self.branch_on_condition(self.regs.status.negative, *offset)
            }
            (Mnemonic::BNE, _, Operand::Relative(offset)) => {
                self.branch_on_condition(!self.regs.status.zero, *offset)
            }
            (Mnemonic::BPL, _, Operand::Relative(offset)) => {
                self.branch_on_condition(!self.regs.status.negative, *offset)
            }
            (Mnemonic::BVC, _, Operand::Relative(offset)) => {
                self.branch_on_condition(!self.regs.status.overflow, *offset)
            }
            (Mnemonic::BVS, _, Operand::Relative(offset)) => {
                self.branch_on_condition(self.regs.status.overflow, *offset)
            }
            // BIT
            (Mnemonic::BIT, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                let value = memory.read_byte(*addr as u16);
                self.bit_test(value);
                3
            }
            (Mnemonic::BIT, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                let value = memory.read_byte(*addr);
                self.bit_test(value);
                4
            }
            // BRK
            (Mnemonic::BRK, _, Operand::Implied) => {
                // Causes a non-maskable interrupt
                self.regs.pc += 2;
                self.push_to_stack(memory, (self.regs.pc >> 8) as u8);
                self.push_to_stack(memory, self.regs.pc as u8);
                self.regs.status.break_command = true;
                self.push_to_stack(memory, self.regs.status.into());
                self.regs.status.interrupt_disable = true;
                self.regs.pc = memory.read_word(INTERRUPT_VECTOR);
                7
            }
            // CLC
            (Mnemonic::CLC, _, Operand::Implied) => {
                self.regs.status.carry = false;
                2
            }
            // CLI
            (Mnemonic::CLI, _, Operand::Implied) => {
                self.regs.status.interrupt_disable = false;
                2
            }
            // CLV
            (Mnemonic::CLV, _, Operand::Implied) => {
                self.regs.status.overflow = false;
                2
            }
            // CMP
            (Mnemonic::CMP, _, Operand::Immediate(value)) => {
                self.compare(Register::A, *value);
                2
            }
            (Mnemonic::CMP, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.compare(Register::A, memory.read_byte(*addr as u16));
                3
            }
            (Mnemonic::CMP, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                self.compare(Register::A, memory.read_byte((*addr + self.regs.x) as u16));
                4
            }
            (Mnemonic::CMP, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.compare(Register::A, memory.read_byte(*addr));
                4
            }
            (Mnemonic::CMP, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.compare(Register::A, memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::CMP, AddressingMode::AbsoluteY, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.compare(Register::A, memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::CMP, AddressingMode::IndirectIndexedX, Operand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.compare(Register::A, memory.read_byte(indirect_addr));
                6
            }
            (Mnemonic::CMP, AddressingMode::IndirectIndexedY, Operand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.compare(Register::A, memory.read_byte(indexed_addr & 0xff));
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // CPX
            (Mnemonic::CPX, _, Operand::Immediate(value)) => {
                self.compare(Register::X, *value);
                2
            }
            (Mnemonic::CPX, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.compare(Register::X, memory.read_byte(*addr as u16));
                3
            }
            (Mnemonic::CPX, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.compare(Register::X, memory.read_byte(*addr));
                4
            }
            // CPY
            (Mnemonic::CPY, _, Operand::Immediate(value)) => {
                self.compare(Register::Y, *value);
                2
            }
            (Mnemonic::CPY, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.compare(Register::Y, memory.read_byte(*addr as u16));
                3
            }
            (Mnemonic::CPY, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.compare(Register::Y, memory.read_byte(*addr));
                4
            }
            // DEC
            (Mnemonic::DEC, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr).wrapping_sub(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                5
            }
            (Mnemonic::DEC, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                let addr = (*addr + self.regs.x) as u16;
                let value = memory.read_byte(addr).wrapping_sub(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                6
            }
            (Mnemonic::DEC, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                let value = memory.read_byte(*addr).wrapping_sub(1);
                memory.write_byte(*addr, value);
                self.set_zero_and_negative_flags(value);
                6
            }
            (Mnemonic::DEC, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.regs.x as u16);
                let value = memory.read_byte(addr).wrapping_sub(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                7
            }
            // DEX
            (Mnemonic::DEX, _, Operand::Implied) => {
                self.regs.x = self.regs.x.wrapping_sub(1);
                self.set_zero_and_negative_flags(self.regs.x);
                2
            }
            // DEY
            (Mnemonic::DEY, _, Operand::Implied) => {
                self.regs.y = self.regs.y.wrapping_sub(1);
                self.set_zero_and_negative_flags(self.regs.y);
                2
            }
            // EOR
            (Mnemonic::EOR, _, Operand::Immediate(value)) => {
                self.regs.a ^= *value;
                self.set_zero_and_negative_flags(self.regs.a);
                2
            }
            (Mnemonic::EOR, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.regs.a ^= memory.read_byte(*addr as u16);
                self.set_zero_and_negative_flags(self.regs.a);
                3
            }
            (Mnemonic::EOR, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                self.regs.a ^= memory.read_byte((*addr + self.regs.x) as u16);
                self.set_zero_and_negative_flags(self.regs.a);
                4
            }
            (Mnemonic::EOR, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.regs.a ^= memory.read_byte(*addr);
                self.set_zero_and_negative_flags(self.regs.a);
                4
            }
            (Mnemonic::EOR, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.regs.a ^= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.regs.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::EOR, AddressingMode::AbsoluteY, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.regs.a ^= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.regs.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::EOR, AddressingMode::IndirectIndexedX, Operand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.regs.a ^= memory.read_byte(indirect_addr);
                self.set_zero_and_negative_flags(self.regs.a);
                6
            }
            (Mnemonic::EOR, AddressingMode::IndirectIndexedY, Operand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.regs.a ^= memory.read_byte(indexed_addr & 0xff);
                self.set_zero_and_negative_flags(self.regs.a);
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // INC
            (Mnemonic::INC, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr).wrapping_add(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                5
            }
            (Mnemonic::INC, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                let addr = (*addr + self.regs.x) as u16;
                let value = memory.read_byte(addr).wrapping_add(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                6
            }
            (Mnemonic::INC, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                let value = memory.read_byte(*addr).wrapping_add(1);
                memory.write_byte(*addr, value);
                self.set_zero_and_negative_flags(value);
                6
            }
            (Mnemonic::INC, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.regs.x as u16);
                let value = memory.read_byte(addr).wrapping_add(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                7
            }
            // INX
            (Mnemonic::INX, _, Operand::Implied) => {
                self.regs.x = self.regs.x.wrapping_add(1);
                self.set_zero_and_negative_flags(self.regs.x);
                2
            }
            // INY
            (Mnemonic::INY, _, Operand::Implied) => {
                self.regs.y = self.regs.y.wrapping_add(1);
                self.set_zero_and_negative_flags(self.regs.y);
                2
            }
            // JMP
            (Mnemonic::JMP, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.regs.pc = *addr;
                3
            }
            (Mnemonic::JMP, AddressingMode::Indirect, Operand::Absolute(addr)) => {
                let indirect_addr = memory.read_word(*addr);
                self.regs.pc = indirect_addr;
                5
            }
            // JSR
            (Mnemonic::JSR, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                let return_addr = self.regs.pc - 1;
                self.push_to_stack(memory, (return_addr >> 8) as u8);
                self.push_to_stack(memory, return_addr as u8);
                self.regs.pc = *addr;
                6
            }
            // LDA
            (Mnemonic::LDA, _, Operand::Immediate(value)) => {
                self.load_register(Register::A, *value);
                2
            }
            (Mnemonic::LDA, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.load_register(Register::A, memory.read_byte(*addr as u16));
                3
            }
            (Mnemonic::LDA, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                self.load_register(Register::A, memory.read_byte((*addr + self.regs.x) as u16));
                4
            }
            (Mnemonic::LDA, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.load_register(Register::A, memory.read_byte(*addr));
                4
            }
            (Mnemonic::LDA, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.load_register(Register::A, memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::LDA, AddressingMode::AbsoluteY, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.load_register(Register::A, memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::LDA, AddressingMode::IndirectIndexedX, Operand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.load_register(Register::A, memory.read_byte(indirect_addr));
                6
            }
            (Mnemonic::LDA, AddressingMode::IndirectIndexedY, Operand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.load_register(Register::A, memory.read_byte(indexed_addr & 0xff));
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // LDX
            (Mnemonic::LDX, _, Operand::Immediate(value)) => {
                self.load_register(Register::X, *value);
                2
            }
            (Mnemonic::LDX, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.load_register(Register::X, memory.read_byte(*addr as u16));
                3
            }
            (Mnemonic::LDX, AddressingMode::ZeroPageY, Operand::ZeroPage(addr)) => {
                self.load_register(Register::X, memory.read_byte((*addr + self.regs.y) as u16));
                4
            }
            (Mnemonic::LDX, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.load_register(Register::X, memory.read_byte(*addr));
                4
            }
            (Mnemonic::LDX, AddressingMode::AbsoluteY, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.load_register(Register::X, memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            // LDY
            (Mnemonic::LDY, _, Operand::Immediate(value)) => {
                self.load_register(Register::Y, *value);
                2
            }
            (Mnemonic::LDY, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.load_register(Register::Y, memory.read_byte(*addr as u16));
                3
            }
            (Mnemonic::LDY, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                self.load_register(Register::Y, memory.read_byte((*addr + self.regs.x) as u16));
                4
            }
            (Mnemonic::LDY, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.load_register(Register::Y, memory.read_byte(*addr));
                4
            }
            (Mnemonic::LDY, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.load_register(Register::Y, memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            // LSR
            (Mnemonic::LSR, AddressingMode::Accumulator, Operand::Implied) => {
                self.regs.a = self.shift_right(self.regs.a);
                2
            }
            (Mnemonic::LSR, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_right(value));
                5
            }
            (Mnemonic::LSR, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                let addr = (*addr + self.regs.x) as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_right(value));
                6
            }
            (Mnemonic::LSR, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                let value = memory.read_byte(*addr);
                memory.write_byte(*addr, self.shift_right(value));
                6
            }
            (Mnemonic::LSR, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.regs.x as u16);
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_right(value));
                7
            }
            // NOP
            (Mnemonic::NOP, _, Operand::Implied) => 2,
            // ORA
            (Mnemonic::ORA, _, Operand::Immediate(value)) => {
                self.regs.a |= *value;
                self.set_zero_and_negative_flags(self.regs.a);
                2
            }
            (Mnemonic::ORA, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.regs.a |= memory.read_byte(*addr as u16);
                self.set_zero_and_negative_flags(self.regs.a);
                3
            }
            (Mnemonic::ORA, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                self.regs.a |= memory.read_byte((*addr + self.regs.x) as u16);
                self.set_zero_and_negative_flags(self.regs.a);
                4
            }
            (Mnemonic::ORA, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.regs.a |= memory.read_byte(*addr);
                self.set_zero_and_negative_flags(self.regs.a);
                4
            }
            (Mnemonic::ORA, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.regs.a |= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.regs.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::ORA, AddressingMode::AbsoluteY, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.regs.a |= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.regs.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::ORA, AddressingMode::IndirectIndexedX, Operand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.regs.a |= memory.read_byte(indirect_addr);
                self.set_zero_and_negative_flags(self.regs.a);
                6
            }
            (Mnemonic::ORA, AddressingMode::IndirectIndexedY, Operand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.regs.a |= memory.read_byte(indexed_addr & 0xff);
                self.set_zero_and_negative_flags(self.regs.a);
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // PHA
            (Mnemonic::PHA, _, Operand::Implied) => {
                self.push_to_stack(memory, self.regs.a);
                3
            }
            // PHP
            (Mnemonic::PHP, _, Operand::Implied) => {
                self.push_to_stack(memory, self.regs.status.into());
                3
            }
            // PLA
            (Mnemonic::PLA, _, Operand::Implied) => {
                self.regs.a = self.pop_from_stack(memory);
                self.set_zero_and_negative_flags(self.regs.a);
                4
            }
            // PLP
            (Mnemonic::PLP, _, Operand::Implied) => {
                self.regs.status = self.pop_from_stack(memory).into();
                4
            }
            // ROL
            (Mnemonic::ROL, AddressingMode::Accumulator, Operand::Implied) => {
                self.regs.a = self.rotate_left(self.regs.a);
                2
            }
            (Mnemonic::ROL, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_left(value));
                5
            }
            (Mnemonic::ROL, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                let addr = (*addr + self.regs.x) as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_left(value));
                6
            }
            (Mnemonic::ROL, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                let value = memory.read_byte(*addr);
                memory.write_byte(*addr, self.rotate_left(value));
                6
            }
            (Mnemonic::ROL, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.regs.x as u16);
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_left(value));
                7
            }
            // ROR
            (Mnemonic::ROR, AddressingMode::Accumulator, Operand::Implied) => {
                self.regs.a = self.rotate_right(self.regs.a);
                2
            }
            (Mnemonic::ROR, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_right(value));
                5
            }
            (Mnemonic::ROR, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                let addr = (*addr + self.regs.x) as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_right(value));
                6
            }
            (Mnemonic::ROR, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                let value = memory.read_byte(*addr);
                memory.write_byte(*addr, self.rotate_right(value));
                6
            }
            (Mnemonic::ROR, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.regs.x as u16);
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_right(value));
                7
            }
            // RTI
            (Mnemonic::RTI, _, Operand::Implied) => {
                self.regs.status = self.pop_from_stack(memory).into();
                self.regs.status.break_command = false;
                self.regs.status.interrupt_disable = false;
                self.regs.pc = self.pop_from_stack(memory) as u16;
                self.regs.pc |= (self.pop_from_stack(memory) as u16) << 8;
                6
            }
            // RTS
            (Mnemonic::RTS, _, Operand::Implied) => {
                self.regs.pc = self.pop_from_stack(memory) as u16;
                self.regs.pc |= (self.pop_from_stack(memory) as u16) << 8;
                self.regs.pc += 1;
                6
            }
            // SBC
            (Mnemonic::SBC, _, Operand::Immediate(value)) => {
                self.subtract_with_carry(*value);
                2
            }
            (Mnemonic::SBC, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.subtract_with_carry(memory.read_byte(*addr as u16));
                3
            }
            (Mnemonic::SBC, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                self.subtract_with_carry(memory.read_byte((*addr + self.regs.x) as u16));
                4
            }
            (Mnemonic::SBC, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.subtract_with_carry(memory.read_byte(*addr));
                4
            }
            (Mnemonic::SBC, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.subtract_with_carry(memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::SBC, AddressingMode::AbsoluteY, Operand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.subtract_with_carry(memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (Mnemonic::SBC, AddressingMode::IndirectIndexedX, Operand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.subtract_with_carry(memory.read_byte(indirect_addr));
                6
            }
            (Mnemonic::SBC, AddressingMode::IndirectIndexedY, Operand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.subtract_with_carry(memory.read_byte(indexed_addr & 0xff));
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // SEC
            (Mnemonic::SEC, _, Operand::Implied) => {
                self.regs.status.carry = true;
                2
            }
            // SEI
            (Mnemonic::SEI, _, Operand::Implied) => {
                self.regs.status.interrupt_disable = true;
                2
            }
            // STA
            (Mnemonic::STA, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.store_register(Register::A, *addr as u16, memory);
                3
            }
            (Mnemonic::STA, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                self.store_register(Register::A, (*addr + self.regs.x) as u16, memory);
                4
            }
            (Mnemonic::STA, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.store_register(Register::A, *addr, memory);
                4
            }
            (Mnemonic::STA, AddressingMode::AbsoluteX, Operand::Absolute(addr)) => {
                let (_, indexed_addr) = self.indexed_indirect(Register::X, *addr);
                self.store_register(Register::A, indexed_addr, memory);
                5
            }
            (Mnemonic::STA, AddressingMode::AbsoluteY, Operand::Absolute(addr)) => {
                let (_, indexed_addr) = self.indexed_indirect(Register::Y, *addr);
                self.store_register(Register::A, indexed_addr, memory);
                5
            }
            (Mnemonic::STA, AddressingMode::IndirectIndexedX, Operand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.store_register(Register::A, indirect_addr, memory);
                6
            }
            (Mnemonic::STA, AddressingMode::IndirectIndexedY, Operand::ZeroPage(addr)) => {
                let (_, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.store_register(Register::A, indexed_addr, memory);
                6
            }
            // STX
            (Mnemonic::STX, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.store_register(Register::X, *addr as u16, memory);
                3
            }
            (Mnemonic::STX, AddressingMode::ZeroPageY, Operand::ZeroPage(addr)) => {
                self.store_register(Register::X, (*addr + self.regs.y) as u16, memory);
                4
            }
            (Mnemonic::STX, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.store_register(Register::X, *addr, memory);
                4
            }
            // STY
            (Mnemonic::STY, AddressingMode::ZeroPage, Operand::ZeroPage(addr)) => {
                self.store_register(Register::Y, *addr as u16, memory);
                3
            }
            (Mnemonic::STY, AddressingMode::ZeroPageX, Operand::ZeroPage(addr)) => {
                self.store_register(Register::Y, (*addr + self.regs.x) as u16, memory);
                4
            }
            (Mnemonic::STY, AddressingMode::Absolute, Operand::Absolute(addr)) => {
                self.store_register(Register::Y, *addr, memory);
                4
            }
            // TAX
            (Mnemonic::TAX, _, Operand::Implied) => {
                self.load_register(Register::X, self.regs.a);
                2
            }
            // TAY
            (Mnemonic::TAY, _, Operand::Implied) => {
                self.load_register(Register::Y, self.regs.a);
                2
            }
            // TSX
            (Mnemonic::TSX, _, Operand::Implied) => {
                self.load_register(Register::X, self.regs.sp);
                2
            }
            // TXA
            (Mnemonic::TXA, _, Operand::Implied) => {
                self.load_register(Register::A, self.regs.x);
                2
            }
            // TXS
            (Mnemonic::TXS, _, Operand::Implied) => {
                self.regs.sp = self.regs.x;
                2
            }
            // TYA
            (Mnemonic::TYA, _, Operand::Implied) => {
                self.load_register(Register::A, self.regs.y);
                2
            }
            _ => todo!("Invalid instruction: '{:#?}'", &ins),
        }
    }

    #[tracing::instrument]
    fn indexed_indirect(&self, register: Register, addr: u16) -> (bool, u16) {
        let indexed_addr = addr.wrapping_add(match register {
            Register::X => self.regs.x,
            Register::Y => self.regs.y,
            _ => panic!("Invalid register: '{:#?}'", register),
        } as u16);
        let page_boundary_crossed = (addr & 0xFF00) != (indexed_addr & 0xFF00);

        (page_boundary_crossed, indexed_addr)
    }

    #[tracing::instrument]
    fn indexed_indirect_x(&self, memory: &mut Memory, zp_addr: u8) -> u16 {
        memory.read_word((zp_addr + self.regs.x) as u16) & 0xff
    }

    #[tracing::instrument]
    fn indexed_indirect_y(&self, memory: &mut Memory, zp_addr: u8) -> (bool, u16) {
        let indirect_addr = memory.read_word(zp_addr as u16);
        self.indexed_indirect(Register::Y, indirect_addr)
    }

    /// Branch on condition.
    /// Returns the number of cycles taken.
    #[tracing::instrument]
    fn branch_on_condition(&mut self, should_branch: bool, offset: i8) -> usize {
        if should_branch {
            let old_pc = self.regs.pc;
            self.regs.pc = self.regs.pc.wrapping_add_signed(offset as i16);

            let boundery_crossed = (self.regs.pc & 0xFF00) != (old_pc & 0xFF00);
            if boundery_crossed {
                4
            } else {
                3
            }
        } else {
            2
        }
    }

    #[tracing::instrument]
    fn shift_left(&mut self, value: u8) -> u8 {
        self.regs.status.carry = value & 0x80 != 0;
        let result = value << 1;
        self.set_zero_and_negative_flags(result);
        result
    }

    #[tracing::instrument]
    fn shift_right(&mut self, value: u8) -> u8 {
        self.regs.status.carry = value & 0x01 != 0;
        let result = value >> 1;
        self.set_zero_and_negative_flags(result);
        result
    }

    #[tracing::instrument]
    fn rotate_left(&mut self, value: u8) -> u8 {
        let carry = self.regs.status.carry as u8;
        self.regs.status.carry = value & 0x80 != 0;
        let result = value << 1 | carry;
        self.set_zero_and_negative_flags(result);
        result
    }

    #[tracing::instrument]
    fn rotate_right(&mut self, value: u8) -> u8 {
        let carry = self.regs.status.carry as u8;
        self.regs.status.carry = value & 0x01 != 0;
        let result = value >> 1 | carry << 7;
        self.set_zero_and_negative_flags(result);
        result
    }

    #[tracing::instrument]
    fn add_with_carry(&mut self, value: u8) {
        // TODO: Handle BCD mode

        let result: u16 = value as u16 + self.regs.a as u16 + self.regs.status.carry as u16;
        self.regs.status.zero = result & 0xff == 0;
        self.regs.status.negative = result & 0x80 != 0;
        self.regs.status.overflow =
            (self.regs.a ^ result as u8) & (value ^ result as u8) & 0x80 != 0;
        self.regs.status.carry = result > 0xff;
        self.regs.a = result as u8;
    }

    #[tracing::instrument]
    fn subtract_with_carry(&mut self, value: u8) {
        // TODO: Handle BCD mode

        let borrow = if self.regs.status.carry { 0 } else { 1 };
        let result: u16 = (self.regs.a as u16)
            .wrapping_sub(value as u16)
            .wrapping_sub(borrow as u16);

        self.regs.status.negative = result & 0x80 != 0;
        self.regs.status.zero = result & 0xff == 0;
        self.regs.status.overflow =
            ((self.regs.a ^ value) & 0x80) != 0 && ((self.regs.a ^ result as u8) & 0x80) != 0;
        self.regs.status.carry = result < 0x100;
        self.regs.a = result as u8;
    }

    #[tracing::instrument]
    fn bit_test(&mut self, value: u8) {
        self.set_zero_and_negative_flags(value);
        self.regs.status.overflow = value & 0x40 != 0;
    }

    #[tracing::instrument]
    fn compare(&mut self, register: Register, value: u8) {
        let register_value = match register {
            Register::A => self.regs.a,
            Register::X => self.regs.x,
            Register::Y => self.regs.y,
        };
        self.set_zero_and_negative_flags(register_value.wrapping_sub(value));
        self.regs.status.carry = register_value >= value;
    }

    #[tracing::instrument]
    fn set_zero_and_negative_flags(&mut self, value: u8) {
        self.regs.status.zero = value == 0;
        self.regs.status.negative = value & 0x80 != 0;
    }

    #[tracing::instrument]
    fn load_register(&mut self, register: Register, value: u8) {
        match register {
            Register::A => self.regs.a = value,
            Register::X => self.regs.x = value,
            Register::Y => self.regs.y = value,
        }
        self.set_zero_and_negative_flags(value);
    }

    #[tracing::instrument]
    fn store_register(&mut self, register: Register, addr: u16, memory: &mut Memory) {
        let value = match register {
            Register::A => self.regs.a,
            Register::X => self.regs.x,
            Register::Y => self.regs.y,
        };
        memory.write_byte(addr, value);
    }

    #[tracing::instrument]
    fn push_to_stack(&mut self, memory: &mut Memory, value: u8) {
        memory.write_byte(STACK_PAGE + self.regs.sp as u16, value);
        self.regs.sp = self.regs.sp.wrapping_sub(1);
    }

    #[tracing::instrument]
    fn pop_from_stack(&mut self, memory: &mut Memory) -> u8 {
        self.regs.sp = self.regs.sp.wrapping_add(1);
        memory.read_byte(STACK_PAGE + self.regs.sp as u16)
    }

    #[tracing::instrument]
    fn set_program_counter(&mut self, addr: u16) {
        self.regs.pc = addr;
    }

    #[tracing::instrument]
    fn handle_interrupt(&mut self, memory: &mut Memory, vector: u16) -> usize {
        let return_addr = self.regs.pc;
        self.push_to_stack(memory, (return_addr >> 8) as u8);
        self.push_to_stack(memory, return_addr as u8);
        self.push_to_stack(memory, self.regs.status.into());
        self.regs.status.interrupt_disable = true;
        self.regs.pc = memory.read_word(vector);

        7
    }

    #[tracing::instrument]
    fn handle_reset(&mut self, memory: &mut Memory) -> usize {
        self.regs.a = 0;
        self.regs.x = 0;
        self.regs.y = 0;
        self.regs.sp = 0xFD;
        self.regs.status = Status {
            interrupt_disable: true,
            ..Default::default()
        };
        self.regs.pc = memory.read_word(RESET_VECTOR);

        8
    }

    #[tracing::instrument]
    fn handle_interrupt_request(&mut self, memory: &mut Memory) {
        if self.reset_interrupt_pending {
            self.reset_interrupt_pending = false;
            self.cycles_left_for_instruction = self.handle_reset(memory);
        } else if self.nmi_interrupt_pending {
            self.nmi_interrupt_pending = false;
            self.cycles_left_for_instruction = self.handle_interrupt(memory, NMI_VECTOR);
        } else if self.irq_interrupt_pending {
            self.irq_interrupt_pending = false;
            if !self.regs.status.interrupt_disable {
                self.cycles_left_for_instruction = self.handle_interrupt(memory, INTERRUPT_VECTOR);
            }
        }
    }
}

// Control functions
impl Cpu {
    /// Reset interrupt request
    #[tracing::instrument]
    pub fn reset(&mut self) {
        self.reset_interrupt_pending = true;
    }

    /// IRQ - Hardware interrupt request
    #[tracing::instrument]
    pub fn irq(&mut self) {
        self.irq_interrupt_pending = true;
    }

    /// NMI - Non-maskable hardware interrupt request
    #[tracing::instrument]
    pub fn nmi(&mut self) {
        self.nmi_interrupt_pending = true;
    }

    /// Ticks the clock of the CPU.
    #[tracing::instrument]
    pub fn clock(&mut self, memory: &mut Memory) {
        if self.cycles_left_for_instruction > 0 {
            // Processing current instruction
            self.cycles_left_for_instruction -= 1;
            return;
        }

        self.handle_interrupt_request(memory);
        if self.cycles_left_for_instruction > 0 {
            // Processing interrupt
            self.cycles_left_for_instruction -= 1;
            return;
        }

        // Ok, we're ready to process the next instruction
        let instruction = self.fetch_and_decode(memory);
        self.regs.pc += instruction.size() as u16;
        self.cycles_left_for_instruction = self.execute_instruction(&instruction, memory);
        self.last_instruction = Some(instruction);
        self.cycles_left_for_instruction -= 1;
    }

    /// Steps the CPU by one instruction.
    #[tracing::instrument]
    pub fn step(&mut self, memory: &mut Memory) {
        // Fetch and decode the instruction
        self.clock(memory);

        // Execute the instruction
        for _ in 0..self.cycles_left_for_instruction {
            self.clock(memory);
        }
    }

    /// Runs the CPU until the given number of cycles has been reached.
    #[tracing::instrument]
    pub fn run(&mut self, memory: &mut Memory, run_option: RunOption) {
        match run_option {
            RunOption::UntilCycles(cycles_to_run) => {
                for _ in 0..cycles_to_run {
                    self.clock(memory);
                }
            }
            RunOption::StopOnBreakInstruction => loop {
                if let Some(ins) = &self.last_instruction {
                    if ins.mnemonic == Mnemonic::BRK {
                        break;
                    }
                }
                self.clock(memory);
            },
        }
    }

    pub fn registers(&self) -> Registers {
        self.regs.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{assembler::assemble_code, emulator::memory::Memory};

    use pretty_assertions::assert_eq;

    const PROGRAM_START: u16 = 0x8000;

    /// A test case for the CPU.
    ///
    /// The test case consists of a code snippet, the expected CPU state after
    /// executing the code snippet, and the expected number of cycles.
    ///
    /// Optionally, the test case can also specify an initialization function
    /// for the memory and an assertion function for the memory after executing
    /// the code snippet.
    struct TestCase {
        /// Code to run on a reset CPU.
        code: &'static str,
        /// Expected CPU state after executing the code snippet.
        expected_cpu: Cpu,
        /// Expected number of cpu cycles to run.
        expected_cycles: usize,
        /// Optional function to initialize the memory before running the test.
        init_memory_fn: Option<fn(&mut Memory)>,
        /// Optional function to assert the memory after running the test.
        expected_memory_fn: Option<fn(&Memory)>,
    }

    impl Default for TestCase {
        fn default() -> Self {
            Self {
                code: "",
                expected_cpu: Cpu::new(),
                expected_cycles: 0,
                init_memory_fn: None,
                expected_memory_fn: None,
            }
        }
    }

    impl TestCase {
        pub fn run_test(&self) {
            // Arrange
            let mut cpu = Cpu::new();
            let mut memory = Memory::new();
            let bytes = assemble_code(self.code, PROGRAM_START).expect("Failed to compile code");
            memory.load(PROGRAM_START, &bytes);
            if let Some(init_memory_fn) = self.init_memory_fn {
                init_memory_fn(&mut memory);
            }
            cpu.set_program_counter(PROGRAM_START);

            // Act
            cpu.run(&mut memory, RunOption::UntilCycles(self.expected_cycles));

            // Assert
            assert_eq!(cpu.regs.a, self.expected_cpu.regs.a);
            assert_eq!(cpu.regs.x, self.expected_cpu.regs.x);
            assert_eq!(cpu.regs.y, self.expected_cpu.regs.y);
            assert_eq!(cpu.regs.pc, self.expected_cpu.regs.pc);
            assert_eq!(cpu.regs.sp, self.expected_cpu.regs.sp);
            assert_eq!(cpu.regs.status, self.expected_cpu.regs.status);

            if let Some(expected_memory_fn) = self.expected_memory_fn {
                expected_memory_fn(&memory);
            }
        }
    }

    #[test]
    fn test_arithmetic() {
        vec![
            // ADC
            TestCase {
                // Simple addition
                code: "LDA #$10\nADC #$10",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x20,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: false,
                            overflow: false,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Addition with carry
                code: "LDA #$ff\nADC #$01",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: true,
                            overflow: false,
                            zero: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Overflow
                code: "LDA #$7f\nADC #$01",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x80,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: false,
                            overflow: true,
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff\nADC #$ff",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xfe,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: true,
                            overflow: false,
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff\nADC #$ff\nADC #$ff",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xfe,
                        pc: PROGRAM_START + 2 + 2 + 2,
                        status: Status {
                            carry: true,
                            overflow: false,
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff\nADC #$ff\nADC #$ff\nADC #$ff",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xfe,
                        pc: PROGRAM_START + 2 + 2 + 2 + 2,
                        status: Status {
                            carry: true,
                            overflow: false,
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2 + 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "ADC #$80\nADC #$80",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: true,
                            overflow: true,
                            zero: true,
                            negative: false,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // SBC
            TestCase {
                code: "SEC\nSBC #$01",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xff,
                        pc: PROGRAM_START + 1 + 2,
                        status: Status {
                            carry: false,
                            overflow: false,
                            zero: false,
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$10\nSEC\nSBC #$10",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        pc: PROGRAM_START + 2 + 1 + 2,
                        status: Status {
                            carry: true,
                            overflow: false,
                            zero: true,
                            negative: false,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$10\nSEC\nSBC #$11",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xff,
                        pc: PROGRAM_START + 2 + 1 + 2,
                        status: Status {
                            carry: false,
                            overflow: false,
                            zero: false,
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$10\nSEC\nSBC #$ff",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x11,
                        pc: PROGRAM_START + 2 + 1 + 2,
                        status: Status {
                            carry: false,
                            overflow: false,
                            zero: false,
                            negative: false,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_set_and_clear() {
        vec![
            // CLC (clear carry flag)
            TestCase {
                // First set carry flag and then clear it
                code: "SEC\nCLC",
                expected_cpu: Cpu {
                    regs: Registers {
                        status: Status {
                            carry: false,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 1 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // SEC (set carry flag)
            TestCase {
                code: "SEC",
                expected_cpu: Cpu {
                    regs: Registers {
                        status: Status {
                            carry: true,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            // CLV (clear overflow flag)
            TestCase {
                // First cause an overflow with ADC and then clear it
                code: "LDA #$7f\nADC #$01\nCLV",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x80,
                        status: Status {
                            overflow: false,
                            zero: false,
                            carry: false,
                            negative: true,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 2 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_branches() {
        vec![
            // BCC
            TestCase {
                // Branch taken forward 0x10 bytes
                code: "BCC $10\nLDA #$01",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        status: Status {
                            carry: false,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 0x10,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 3, // LDA is not executed
                ..Default::default()
            },
            TestCase {
                // Branch taken, backwards on other page
                code: "BCC $80",
                expected_cpu: Cpu {
                    regs: Registers {
                        status: Status {
                            carry: false,
                            ..Default::default()
                        },
                        pc: PROGRAM_START - 126,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 4,
                ..Default::default()
            },
            TestCase {
                // Branch not taken
                code: "SEC\nBCC $01\nLDA #$01",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x01,
                        status: Status {
                            carry: true,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 1 + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
            // TODO: Test other branching instructions
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_jump() {
        vec![
            // JMP
            TestCase {
                // Absolute jump
                code: "JMP $ff00",
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: 0xff00,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 3,
                ..Default::default()
            },
            TestCase {
                // Indirect jump
                code: "JMP ($ff00)",
                init_memory_fn: Some(|memory| memory.write_word(0xff00, 0x1234)),
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: 0x1234,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 5,
                ..Default::default()
            },
            // JSR
            TestCase {
                // Jump to subroutine
                code: "JSR $ff00",
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: 0xff00,
                        sp: 0xfd,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 6,
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x01fe), 0x02);
                    assert_eq!(memory.read_byte(0x01ff), 0x80);
                }),
                ..Default::default()
            },
            // RTS
            TestCase {
                // Return from subroutine
                code: "JSR $ff00\nLDA #$01",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0xff00, 0x60); // RTS
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x01,
                        pc: PROGRAM_START + 3 + 2, // JSR + LDA
                        sp: 0xff,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 6 + 6 + 2,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_bit_shifts_and_rotates() {
        vec![
            // ASL
            TestCase {
                // Simple shift
                code: "LDA #$08\nASL",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        status: Status {
                            zero: false,
                            carry: false,
                            negative: false,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Shift with carry
                code: "LDA #$80\nASL",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        status: Status {
                            zero: true,
                            carry: true,
                            negative: false,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // LSR
            TestCase {
                // Simple shift
                code: "LDA #$10\nLSR",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x08,
                        status: Status {
                            zero: false,
                            carry: false,
                            negative: false,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Shift with carry
                code: "LDA #$01\nLSR",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        status: Status {
                            zero: true,
                            carry: true,
                            negative: false,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // ROL
            TestCase {
                // Simple rotate
                code: "LDA #$80\nROL\nROL",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x01,
                        status: Status {
                            zero: false,
                            carry: false,
                            negative: false,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 1 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
            // ROR
            TestCase {
                // Simple rotate
                code: "LDA #$01\nROR\nROR",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x80,
                        status: Status {
                            zero: false,
                            carry: false,
                            negative: true,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 1 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_bit_test() {
        vec![
            TestCase {
                // Test BIT with zero
                code: "BIT $00",
                expected_cpu: Cpu {
                    regs: Registers {
                        status: Status {
                            zero: true,
                            overflow: false,
                            negative: false,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 3,
                ..Default::default()
            },
            TestCase {
                // Test BIT with negative
                code: "LDA #$80\nBIT $00",
                init_memory_fn: Some(|memory| memory.write_byte(0x00, 0x80)),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x80,
                        status: Status {
                            zero: false,
                            overflow: false,
                            negative: true,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 3,
                ..Default::default()
            },
            TestCase {
                // Test BIT with overflow
                code: "LDA #$40\nBIT $00",
                init_memory_fn: Some(|memory| memory.write_byte(0x00, 0x40)),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x40,
                        status: Status {
                            zero: false,
                            overflow: true,
                            negative: false,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 3,
                ..Default::default()
            },
            TestCase {
                // Test BIT with overflow and negative
                code: "LDA #%11000000\nBIT $00",
                init_memory_fn: Some(|memory| memory.write_byte(0x00, 0b11000000)),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0b11000000,
                        status: Status {
                            zero: false,
                            overflow: true,
                            negative: true,
                            ..Default::default()
                        },
                        pc: PROGRAM_START + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 3,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_bitwise_operations() {
        // AND
        vec![
            TestCase {
                code: "LDA #$10\nAND #$10",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        pc: PROGRAM_START + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #%10101010\nAND #$0f",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0b1010,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "AND #$ff",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        pc: PROGRAM_START + 2,
                        status: Status {
                            zero: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff\nAND #$00",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            zero: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // EOR
            TestCase {
                code: "LDA #$10\nEOR #$10",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            zero: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$f0\nEOR #$0f",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xff,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$0f\nEOR #$ff",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xf0,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // OR
            TestCase {
                code: "LDA #$10\nORA #$10",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        pc: PROGRAM_START + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$f0\nORA #$0f",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xff,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #%01010101\nORA #$0f",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0b01011111,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "ORA #$ff",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xff,
                        pc: PROGRAM_START + 2,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_comparisons() {
        vec![
            // CMP
            TestCase {
                // Register = Operand
                code: "LDA #$30\nCMP #$30",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x30,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: true,
                            zero: true,
                            negative: false,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Register > Operand
                code: "LDA #$80\nCMP #$40",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x80,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: true,
                            zero: false,
                            negative: false,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Register < Operand
                code: "LDA #$20\nCMP #$40",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x20,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: false,
                            zero: false,
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Register < Operand
                code: "LDA #$f0\nCMP #$30",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xf0,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: true,
                            zero: false,
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$30\nCMP #$f0",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x30,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: false,
                            zero: false,
                            negative: false,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff\nCMP #$00",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xff,
                        pc: PROGRAM_START + 2 + 2,
                        status: Status {
                            carry: true,
                            zero: false,
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_decrement() {
        vec![
            // DEC
            TestCase {
                code: "DEC $10",
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: PROGRAM_START + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 5,
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x10, 0x10);
                }),
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x10), 0x0f);
                }),
            },
            TestCase {
                code: "DEC $10",
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: PROGRAM_START + 2,
                        status: Status {
                            zero: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 5,
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x10, 0x01);
                }),
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x10), 0x00);
                }),
            },
            TestCase {
                code: "DEC $10",
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: PROGRAM_START + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 5,
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x10, 0x80);
                }),
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x10), 0x7f);
                }),
            },
            // DEX
            TestCase {
                code: "DEX",
                expected_cpu: Cpu {
                    regs: Registers {
                        x: 0xff,
                        pc: PROGRAM_START + 1,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            // DEY
            TestCase {
                code: "DEY",
                expected_cpu: Cpu {
                    regs: Registers {
                        y: 0xff,
                        pc: PROGRAM_START + 1,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_increment() {
        vec![
            // INC
            TestCase {
                code: "INC $10",
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: PROGRAM_START + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 5,
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x10, 0x01);
                }),
                ..Default::default()
            },
            TestCase {
                code: "INC $10",
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: PROGRAM_START + 2,
                        status: Status {
                            zero: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 5,
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x10, 0xff);
                }),
                ..Default::default()
            },
            // INC test negative flag
            TestCase {
                code: "INC $10",
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: PROGRAM_START + 2,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 5,
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x10, 0x7f);
                }),
                ..Default::default()
            },
            // INX
            TestCase {
                code: "INX",
                expected_cpu: Cpu {
                    regs: Registers {
                        x: 0x01,
                        pc: PROGRAM_START + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            // INY
            TestCase {
                code: "INY",
                expected_cpu: Cpu {
                    regs: Registers {
                        y: 0x01,
                        pc: PROGRAM_START + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_lda_imm() {
        vec![
            TestCase {
                code: "LDA #$10",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        pc: PROGRAM_START + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0xff,
                        pc: PROGRAM_START + 2,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$00",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        pc: PROGRAM_START + 2,
                        status: Status {
                            zero: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_lda() {
        vec![
            // Zero page
            TestCase {
                code: "LDA $10",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x10, 0x10);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        pc: PROGRAM_START + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 3,
                ..Default::default()
            },
            // Zero page, X
            TestCase {
                code: "LDX #$01\nLDA $10,X",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x11, 0x10);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        x: 0x01,
                        pc: PROGRAM_START + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 6,
                ..Default::default()
            },
            // Absolute
            TestCase {
                code: "LDA $1234",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x1234, 0x10);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        pc: PROGRAM_START + 3,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 4,
                ..Default::default()
            },
            // Absolute, X
            TestCase {
                code: "LDX #$01\nLDA $1234,X",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x1235, 0x10);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        x: 0x01,
                        pc: PROGRAM_START + 2 + 3,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 6,
                ..Default::default()
            },
            // Absolute, X, page boundary crossed
            TestCase {
                code: "LDX #$01\nLDA $12ff,X",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x1300, 0x10);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        x: 0x01,
                        pc: PROGRAM_START + 2 + 3,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 7, // An extra cycle
                ..Default::default()
            },
            // Absolute, Y
            TestCase {
                code: "LDY #$01\nLDA $1234,Y",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x1235, 0x10);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        y: 0x01,
                        pc: PROGRAM_START + 2 + 3,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 6,
                ..Default::default()
            },
            // Absolute, Y, page boundary crossed
            TestCase {
                code: "LDY #$01\nLDA $12ff,Y",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x1300, 0x10);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        y: 0x01,
                        pc: PROGRAM_START + 2 + 3,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 7, // An extra cycle due to page boundary crossing
                ..Default::default()
            },
            // Indirect, X
            TestCase {
                code: "LDX #$01\nLDA ($10,X)",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x11, 0x34);
                    memory.write_byte(0x34, 0x10);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        x: 0x01,
                        pc: PROGRAM_START + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 8,
                ..Default::default()
            },
            // Indirect, Y
            TestCase {
                code: "LDY #$01\nLDA ($10),Y",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x10, 0x34);
                    memory.write_byte(0x35, 0x10);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        y: 0x01,
                        pc: PROGRAM_START + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 7,
                ..Default::default()
            },
            // Indirect, Y, page boundary crossed
            TestCase {
                code: "LDY #$01\nLDA ($34),Y",
                init_memory_fn: Some(|memory| {
                    memory.write_byte(0x0034, 0xff);
                    memory.write_byte(0x0000, 0x10);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x10,
                        y: 0x01,
                        pc: PROGRAM_START + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 8, // An extra cycle due to page boundary crossing
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_nop() {
        vec![
            TestCase {
                code: "NOP",
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: PROGRAM_START + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            TestCase {
                code: "NOP\nNOP",
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: PROGRAM_START + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },

                expected_cycles: 4,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_sta() {
        vec![
            // Zero page
            TestCase {
                code: "LDA #$34\nSTA $10",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x34,
                        pc: PROGRAM_START + 2 + 2,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 3,
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x10), 0x34);
                }),
                ..Default::default()
            },
            // Absolute
            TestCase {
                code: "LDA #$34\nSTA $1234",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x34,
                        pc: PROGRAM_START + 2 + 3,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 4,
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x1234), 0x34);
                }),
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_register_transfers() {
        vec![
            // TAX
            TestCase {
                code: "LDA #$34\nTAX",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x34,
                        x: 0x34,
                        pc: PROGRAM_START + 2 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // TAY
            TestCase {
                code: "LDA #$34\nTAY",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x34,
                        y: 0x34,
                        pc: PROGRAM_START + 2 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // TXA
            TestCase {
                code: "LDX #$34\nTXA",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x34,
                        x: 0x34,
                        pc: PROGRAM_START + 2 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // TYA
            TestCase {
                code: "LDY #$34\nTYA",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x34,
                        y: 0x34,
                        pc: PROGRAM_START + 2 + 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // TSX
            TestCase {
                code: "TSX",
                expected_cpu: Cpu {
                    regs: Registers {
                        x: 0xff,
                        sp: 0xff,
                        pc: PROGRAM_START + 1,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            // TXS
            TestCase {
                code: "LDX #$fc\nTXS",
                expected_cpu: Cpu {
                    regs: Registers {
                        x: 0xfc,
                        sp: 0xfc,
                        pc: PROGRAM_START + 2 + 1,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_stack_operations() {
        vec![
            // PHA
            TestCase {
                code: "LDA #$34\nPHA",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x34,
                        pc: PROGRAM_START + 2 + 1,
                        sp: 0xff - 1,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 3,
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x01ff), 0x34);
                }),
                ..Default::default()
            },
            // PLA
            TestCase {
                code: "LDA #$34\nPHA\nLDA #$00\nPLA",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x34,
                        pc: PROGRAM_START + 2 + 1 + 2 + 1,
                        sp: 0xff,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 3 + 2 + 4,
                ..Default::default()
            },
            // PHP
            TestCase {
                code: "LDA #$80\nPHP",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x80,
                        pc: PROGRAM_START + 2 + 1,
                        sp: 0xff - 1,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 3,
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x01ff), 0b1000_0000);
                }),
                ..Default::default()
            },
            // PLP
            TestCase {
                code: "LDA #$80\nPHA\nLDA #$00\nPLP",
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x00,
                        pc: PROGRAM_START + 2 + 1 + 2 + 1,
                        sp: 0xff,
                        status: Status {
                            negative: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2 + 3 + 2 + 4,
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x01ff), 0b1000_0000); // Old stack value
                }),
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }

    #[test]
    fn test_interrupts() {
        vec![
            // BRK
            TestCase {
                code: "BRK",
                init_memory_fn: Some(|memory| {
                    memory.write_word(INTERRUPT_VECTOR, 0x1200);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        pc: 0x1200,
                        sp: 0xff - 3,
                        status: Status {
                            break_command: true,
                            interrupt_disable: true,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 7,
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_word(0x01fe), PROGRAM_START + 1 + 2);
                }),
            },
            // RTI
            TestCase {
                code: "BRK",
                init_memory_fn: Some(|memory| {
                    memory.write_word(INTERRUPT_VECTOR, 0x1200);
                    // LDA #$01
                    memory.write_byte(0x1200, 0xa9);
                    memory.write_byte(0x1201, 0x01);
                    // RTI
                    memory.write_byte(0x1202, 0x40);
                }),
                expected_cpu: Cpu {
                    regs: Registers {
                        a: 0x01,
                        pc: PROGRAM_START + 1 + 2,
                        sp: 0xff,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 7 + 2 + 6,
                ..Default::default()
            },
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }
}
