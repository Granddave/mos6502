use super::memory::Memory;
use crate::{
    assembler::compiler::opcode::OPCODE_MAPPING,
    ast::{ASTAddressingMode, ASTInstructionNode, ASTMnemonic, ASTOperand},
    emulator::memory::Bus,
};

// Stack offsets
pub const STACK_BASE: u16 = 0x0100;
pub const STACK_POINTER_START: u8 = 0xff;

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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Status {
    /// (C) Carry flag, set if the last operation caused an overflow from bit 7 or an
    /// underflow from bit 0.
    carry: bool,
    /// (Z) Zero flag, set if the result of the last operation was zero.
    zero: bool,
    /// (I) Interrupt disable flag, set if the CPU is not to respond to maskable
    /// interrupts (IRQ).
    interrupt_disable: bool,
    /// (D) Decimal mode flag, set if the CPU is in decimal mode.
    decimal: bool,
    /// (B) Break command flag, set if a software interrupt (BRK) instruction has
    /// been executed.
    break_command: bool,
    /// (V) Overflow flag, set if the last operation caused an overflow from bit 6 or an
    /// underflow from bit 1.
    overflow: bool,
    /// (S) Negative/Sign flag, set if the result of the last operation had bit 7 set.
    negative: bool,
}

impl From<Status> for u8 {
    fn from(status: Status) -> Self {
        let mut status_byte = 0;
        if status.carry {
            status_byte |= 0b0000_0001;
        }
        if status.zero {
            status_byte |= 0b0000_0010;
        }
        if status.interrupt_disable {
            status_byte |= 0b0000_0100;
        }
        if status.decimal {
            status_byte |= 0b0000_1000;
        }
        if status.break_command {
            status_byte |= 0b0001_0000;
        }
        if status.overflow {
            status_byte |= 0b0100_0000;
        }
        if status.negative {
            status_byte |= 0b1000_0000;
        }
        status_byte
    }
}

impl From<u8> for Status {
    fn from(status_byte: u8) -> Self {
        Self {
            carry: status_byte & 0b0000_0001 != 0,
            zero: status_byte & 0b0000_0010 != 0,
            interrupt_disable: status_byte & 0b0000_0100 != 0,
            decimal: status_byte & 0b0000_1000 != 0,
            break_command: status_byte & 0b0001_0000 != 0,
            overflow: status_byte & 0b0100_0000 != 0,
            negative: status_byte & 0b1000_0000 != 0,
        }
    }
}

#[derive(Debug)]
pub enum Register {
    A,
    X,
    Y,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cpu {
    // CPU registers
    /// Accumulator
    a: u8,
    /// X register
    x: u8,
    /// Y register
    y: u8,
    /// Program counter
    pc: u16,
    /// Stack pointer
    sp: u8,
    /// Status register
    status: Status,

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

    last_instruction: Option<ASTInstructionNode>,
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: STACK_POINTER_START,
            status: Status::default(),

            reset_interrupt_pending: false,
            nmi_interrupt_pending: false,
            irq_interrupt_pending: false,
            cycles_left_for_instruction: 0,
            last_instruction: None,
        }
    }
}

impl Cpu {
    #[tracing::instrument]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&self, register: Register) -> u8 {
        match register {
            Register::A => self.a,
            Register::X => self.x,
            Register::Y => self.y,
        }
    }

    pub fn status(&self) -> Status {
        self.status
    }

    pub fn program_counter(&self) -> u16 {
        self.pc
    }

    pub fn stack_pointer(&self) -> u8 {
        self.sp
    }

    /// Fetches and decodes the next instruction from memory.
    #[tracing::instrument]
    fn fetch_and_decode(&mut self, memory: &mut Memory) -> ASTInstructionNode {
        let opcode = memory.read_byte(self.pc);
        let ins = OPCODE_MAPPING
            .find_instruction(opcode)
            .unwrap_or_else(|| panic!("Invalid opcode: '{:#02x}'", opcode));

        let operand = match ins.addr_mode {
            ASTAddressingMode::Absolute
            | ASTAddressingMode::AbsoluteX
            | ASTAddressingMode::AbsoluteY
            | ASTAddressingMode::Indirect => ASTOperand::Absolute(memory.read_word(self.pc + 1)),
            ASTAddressingMode::ZeroPage
            | ASTAddressingMode::ZeroPageX
            | ASTAddressingMode::ZeroPageY
            | ASTAddressingMode::IndirectIndexedX
            | ASTAddressingMode::IndirectIndexedY => {
                ASTOperand::ZeroPage(memory.read_byte(self.pc + 1))
            }
            ASTAddressingMode::Relative => {
                ASTOperand::Relative(memory.read_byte(self.pc + 1) as i8)
            }
            ASTAddressingMode::Immediate => ASTOperand::Immediate(memory.read_byte(self.pc + 1)),
            ASTAddressingMode::Accumulator | ASTAddressingMode::Implied => ASTOperand::Implied,
            _ => panic!("Invalid addressing mode: '{:#?}'", ins.addr_mode),
        };

        ASTInstructionNode { ins, operand }
    }

    #[tracing::instrument]
    fn execute_instruction(&mut self, ins: &ASTInstructionNode, memory: &mut Memory) -> usize {
        match (&ins.ins.mnemonic, &ins.ins.addr_mode, &ins.operand) {
            (ASTMnemonic::ADC, _, ASTOperand::Immediate(value)) => {
                self.add_with_carry(*value);
                2
            }
            (ASTMnemonic::ADC, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.add_with_carry(memory.read_byte(*addr as u16));
                3
            }
            (ASTMnemonic::ADC, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.add_with_carry(memory.read_byte((*addr + self.x) as u16));
                4
            }
            (ASTMnemonic::ADC, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.add_with_carry(memory.read_byte(*addr));
                4
            }
            (ASTMnemonic::ADC, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.add_with_carry(memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::ADC, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.add_with_carry(memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::ADC, ASTAddressingMode::IndirectIndexedX, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.add_with_carry(memory.read_byte(indirect_addr));
                6
            }
            (ASTMnemonic::ADC, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.add_with_carry(memory.read_byte(indexed_addr & 0xff));
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // AND
            (ASTMnemonic::AND, _, ASTOperand::Immediate(value)) => {
                self.a &= *value;
                self.set_zero_and_negative_flags(self.a);
                2
            }
            (ASTMnemonic::AND, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.a &= memory.read_byte(*addr as u16);
                self.set_zero_and_negative_flags(self.a);
                3
            }
            (ASTMnemonic::AND, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.a &= memory.read_byte((*addr + self.x) as u16);
                self.set_zero_and_negative_flags(self.a);
                4
            }
            (ASTMnemonic::AND, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.a &= memory.read_byte(*addr);
                self.set_zero_and_negative_flags(self.a);
                4
            }
            (ASTMnemonic::AND, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.a &= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::AND, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.a &= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::AND, ASTAddressingMode::IndirectIndexedX, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.a &= memory.read_byte(indirect_addr);
                self.set_zero_and_negative_flags(self.a);
                6
            }
            (ASTMnemonic::AND, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.a &= memory.read_byte(indexed_addr & 0xff);
                self.set_zero_and_negative_flags(self.a);
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // ASL
            (ASTMnemonic::ASL, ASTAddressingMode::Accumulator, ASTOperand::Implied) => {
                self.a = self.shift_left(self.a);
                2
            }
            (ASTMnemonic::ASL, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_left(value));
                5
            }
            (ASTMnemonic::ASL, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                let addr = (*addr + self.x) as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_left(value));
                6
            }
            (ASTMnemonic::ASL, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                let value = memory.read_byte(*addr);
                memory.write_byte(*addr, self.shift_left(value));
                6
            }
            (ASTMnemonic::ASL, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.x as u16);
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_left(value));
                7
            }
            // Branch instructions
            (ASTMnemonic::BCC, _, ASTOperand::Relative(offset)) => {
                self.branch_on_condition(!self.status.carry, *offset)
            }
            (ASTMnemonic::BCS, _, ASTOperand::Relative(offset)) => {
                self.branch_on_condition(self.status.carry, *offset)
            }
            (ASTMnemonic::BEQ, _, ASTOperand::Relative(offset)) => {
                self.branch_on_condition(self.status.zero, *offset)
            }
            (ASTMnemonic::BMI, _, ASTOperand::Relative(offset)) => {
                self.branch_on_condition(self.status.negative, *offset)
            }
            (ASTMnemonic::BNE, _, ASTOperand::Relative(offset)) => {
                self.branch_on_condition(!self.status.zero, *offset)
            }
            (ASTMnemonic::BPL, _, ASTOperand::Relative(offset)) => {
                self.branch_on_condition(!self.status.negative, *offset)
            }
            (ASTMnemonic::BVC, _, ASTOperand::Relative(offset)) => {
                self.branch_on_condition(!self.status.overflow, *offset)
            }
            (ASTMnemonic::BVS, _, ASTOperand::Relative(offset)) => {
                self.branch_on_condition(self.status.overflow, *offset)
            }
            // BIT
            (ASTMnemonic::BIT, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                let value = memory.read_byte(*addr as u16);
                self.bit_test(value);
                3
            }
            (ASTMnemonic::BIT, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                let value = memory.read_byte(*addr);
                self.bit_test(value);
                4
            }
            // BRK
            (ASTMnemonic::BRK, _, ASTOperand::Implied) => {
                // Causes a non-maskable interrupt
                self.pc += 2;
                self.push_to_stack(memory, (self.pc >> 8) as u8);
                self.push_to_stack(memory, self.pc as u8);
                self.status.break_command = true;
                self.push_to_stack(memory, self.status.into());
                self.status.interrupt_disable = true;
                self.pc = memory.read_word(INTERRUPT_VECTOR);
                7
            }
            // CLC
            (ASTMnemonic::CLC, _, ASTOperand::Implied) => {
                self.status.carry = false;
                2
            }
            // CLI
            (ASTMnemonic::CLI, _, ASTOperand::Implied) => {
                self.status.interrupt_disable = false;
                2
            }
            // CLV
            (ASTMnemonic::CLV, _, ASTOperand::Implied) => {
                self.status.overflow = false;
                2
            }
            // CMP
            (ASTMnemonic::CMP, _, ASTOperand::Immediate(value)) => {
                self.compare(Register::A, *value);
                2
            }
            (ASTMnemonic::CMP, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.compare(Register::A, memory.read_byte(*addr as u16));
                3
            }
            (ASTMnemonic::CMP, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.compare(Register::A, memory.read_byte((*addr + self.x) as u16));
                4
            }
            (ASTMnemonic::CMP, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.compare(Register::A, memory.read_byte(*addr));
                4
            }
            (ASTMnemonic::CMP, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.compare(Register::A, memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::CMP, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.compare(Register::A, memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::CMP, ASTAddressingMode::IndirectIndexedX, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.compare(Register::A, memory.read_byte(indirect_addr));
                6
            }
            (ASTMnemonic::CMP, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.compare(Register::A, memory.read_byte(indexed_addr & 0xff));
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // CPX
            (ASTMnemonic::CPX, _, ASTOperand::Immediate(value)) => {
                self.compare(Register::X, *value);
                2
            }
            (ASTMnemonic::CPX, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.compare(Register::X, memory.read_byte(*addr as u16));
                3
            }
            (ASTMnemonic::CPX, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.compare(Register::X, memory.read_byte(*addr));
                4
            }
            // CPY
            (ASTMnemonic::CPY, _, ASTOperand::Immediate(value)) => {
                self.compare(Register::Y, *value);
                2
            }
            (ASTMnemonic::CPY, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.compare(Register::Y, memory.read_byte(*addr as u16));
                3
            }
            (ASTMnemonic::CPY, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.compare(Register::Y, memory.read_byte(*addr));
                4
            }
            // DEC
            (ASTMnemonic::DEC, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr).wrapping_sub(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                5
            }
            (ASTMnemonic::DEC, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                let addr = (*addr + self.x) as u16;
                let value = memory.read_byte(addr).wrapping_sub(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                6
            }
            (ASTMnemonic::DEC, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                let value = memory.read_byte(*addr).wrapping_sub(1);
                memory.write_byte(*addr, value);
                self.set_zero_and_negative_flags(value);
                6
            }
            (ASTMnemonic::DEC, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.x as u16);
                let value = memory.read_byte(addr).wrapping_sub(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                7
            }
            // DEX
            (ASTMnemonic::DEX, _, ASTOperand::Implied) => {
                self.x = self.x.wrapping_sub(1);
                self.set_zero_and_negative_flags(self.x);
                2
            }
            // DEY
            (ASTMnemonic::DEY, _, ASTOperand::Implied) => {
                self.y = self.y.wrapping_sub(1);
                self.set_zero_and_negative_flags(self.y);
                2
            }
            // EOR
            (ASTMnemonic::EOR, _, ASTOperand::Immediate(value)) => {
                self.a ^= *value;
                self.set_zero_and_negative_flags(self.a);
                2
            }
            (ASTMnemonic::EOR, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.a ^= memory.read_byte(*addr as u16);
                self.set_zero_and_negative_flags(self.a);
                3
            }
            (ASTMnemonic::EOR, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.a ^= memory.read_byte((*addr + self.x) as u16);
                self.set_zero_and_negative_flags(self.a);
                4
            }
            (ASTMnemonic::EOR, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.a ^= memory.read_byte(*addr);
                self.set_zero_and_negative_flags(self.a);
                4
            }
            (ASTMnemonic::EOR, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.a ^= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::EOR, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.a ^= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::EOR, ASTAddressingMode::IndirectIndexedX, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.a ^= memory.read_byte(indirect_addr);
                self.set_zero_and_negative_flags(self.a);
                6
            }
            (ASTMnemonic::EOR, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.a ^= memory.read_byte(indexed_addr & 0xff);
                self.set_zero_and_negative_flags(self.a);
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // INC
            (ASTMnemonic::INC, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr).wrapping_add(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                5
            }
            (ASTMnemonic::INC, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                let addr = (*addr + self.x) as u16;
                let value = memory.read_byte(addr).wrapping_add(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                6
            }
            (ASTMnemonic::INC, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                let value = memory.read_byte(*addr).wrapping_add(1);
                memory.write_byte(*addr, value);
                self.set_zero_and_negative_flags(value);
                6
            }
            (ASTMnemonic::INC, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.x as u16);
                let value = memory.read_byte(addr).wrapping_add(1);
                memory.write_byte(addr, value);
                self.set_zero_and_negative_flags(value);
                7
            }
            // INX
            (ASTMnemonic::INX, _, ASTOperand::Implied) => {
                self.x = self.x.wrapping_add(1);
                self.set_zero_and_negative_flags(self.x);
                2
            }
            // INY
            (ASTMnemonic::INY, _, ASTOperand::Implied) => {
                self.y = self.y.wrapping_add(1);
                self.set_zero_and_negative_flags(self.y);
                2
            }
            // JMP
            (ASTMnemonic::JMP, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.pc = *addr;
                3
            }
            (ASTMnemonic::JMP, ASTAddressingMode::Indirect, ASTOperand::Absolute(addr)) => {
                let indirect_addr = memory.read_word(*addr);
                self.pc = indirect_addr;
                5
            }
            // JSR
            (ASTMnemonic::JSR, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                let return_addr = self.pc - 1;
                self.push_to_stack(memory, (return_addr >> 8) as u8);
                self.push_to_stack(memory, return_addr as u8);
                self.pc = *addr;
                6
            }
            // LDA
            (ASTMnemonic::LDA, _, ASTOperand::Immediate(value)) => {
                self.load_register(Register::A, *value);
                2
            }
            (ASTMnemonic::LDA, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::A, memory.read_byte(*addr as u16));
                3
            }
            (ASTMnemonic::LDA, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::A, memory.read_byte((*addr + self.x) as u16));
                4
            }
            (ASTMnemonic::LDA, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.load_register(Register::A, memory.read_byte(*addr));
                4
            }
            (ASTMnemonic::LDA, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.load_register(Register::A, memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::LDA, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.load_register(Register::A, memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::LDA, ASTAddressingMode::IndirectIndexedX, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.load_register(Register::A, memory.read_byte(indirect_addr));
                6
            }
            (ASTMnemonic::LDA, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.load_register(Register::A, memory.read_byte(indexed_addr & 0xff));
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // LDX
            (ASTMnemonic::LDX, _, ASTOperand::Immediate(value)) => {
                self.load_register(Register::X, *value);
                2
            }
            (ASTMnemonic::LDX, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::X, memory.read_byte(*addr as u16));
                3
            }
            (ASTMnemonic::LDX, ASTAddressingMode::ZeroPageY, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::X, memory.read_byte((*addr + self.y) as u16));
                4
            }
            (ASTMnemonic::LDX, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.load_register(Register::X, memory.read_byte(*addr));
                4
            }
            (ASTMnemonic::LDX, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
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
            (ASTMnemonic::LDY, _, ASTOperand::Immediate(value)) => {
                self.load_register(Register::Y, *value);
                2
            }
            (ASTMnemonic::LDY, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::Y, memory.read_byte(*addr as u16));
                3
            }
            (ASTMnemonic::LDY, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::Y, memory.read_byte((*addr + self.x) as u16));
                4
            }
            (ASTMnemonic::LDY, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.load_register(Register::Y, memory.read_byte(*addr));
                4
            }
            (ASTMnemonic::LDY, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
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
            (ASTMnemonic::LSR, ASTAddressingMode::Accumulator, ASTOperand::Implied) => {
                self.a = self.shift_right(self.a);
                2
            }
            (ASTMnemonic::LSR, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_right(value));
                5
            }
            (ASTMnemonic::LSR, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                let addr = (*addr + self.x) as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_right(value));
                6
            }
            (ASTMnemonic::LSR, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                let value = memory.read_byte(*addr);
                memory.write_byte(*addr, self.shift_right(value));
                6
            }
            (ASTMnemonic::LSR, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.x as u16);
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.shift_right(value));
                7
            }
            // NOP
            (ASTMnemonic::NOP, _, ASTOperand::Implied) => 2,
            // ORA
            (ASTMnemonic::ORA, _, ASTOperand::Immediate(value)) => {
                self.a |= *value;
                self.set_zero_and_negative_flags(self.a);
                2
            }
            (ASTMnemonic::ORA, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.a |= memory.read_byte(*addr as u16);
                self.set_zero_and_negative_flags(self.a);
                3
            }
            (ASTMnemonic::ORA, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.a |= memory.read_byte((*addr + self.x) as u16);
                self.set_zero_and_negative_flags(self.a);
                4
            }
            (ASTMnemonic::ORA, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.a |= memory.read_byte(*addr);
                self.set_zero_and_negative_flags(self.a);
                4
            }
            (ASTMnemonic::ORA, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.a |= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::ORA, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.a |= memory.read_byte(indexed_addr);
                self.set_zero_and_negative_flags(self.a);
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::ORA, ASTAddressingMode::IndirectIndexedX, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.a |= memory.read_byte(indirect_addr);
                self.set_zero_and_negative_flags(self.a);
                6
            }
            (ASTMnemonic::ORA, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.a |= memory.read_byte(indexed_addr & 0xff);
                self.set_zero_and_negative_flags(self.a);
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // PHA
            (ASTMnemonic::PHA, _, ASTOperand::Implied) => {
                self.push_to_stack(memory, self.a);
                3
            }
            // PHP
            (ASTMnemonic::PHP, _, ASTOperand::Implied) => {
                self.push_to_stack(memory, self.status.into());
                3
            }
            // PLA
            (ASTMnemonic::PLA, _, ASTOperand::Implied) => {
                self.a = self.pop_from_stack(memory);
                self.set_zero_and_negative_flags(self.a);
                4
            }
            // PLP
            (ASTMnemonic::PLP, _, ASTOperand::Implied) => {
                self.status = self.pop_from_stack(memory).into();
                4
            }
            // ROL
            (ASTMnemonic::ROL, ASTAddressingMode::Accumulator, ASTOperand::Implied) => {
                self.a = self.rotate_left(self.a);
                2
            }
            (ASTMnemonic::ROL, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_left(value));
                5
            }
            (ASTMnemonic::ROL, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                let addr = (*addr + self.x) as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_left(value));
                6
            }
            (ASTMnemonic::ROL, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                let value = memory.read_byte(*addr);
                memory.write_byte(*addr, self.rotate_left(value));
                6
            }
            (ASTMnemonic::ROL, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.x as u16);
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_left(value));
                7
            }
            // ROR
            (ASTMnemonic::ROR, ASTAddressingMode::Accumulator, ASTOperand::Implied) => {
                self.a = self.rotate_right(self.a);
                2
            }
            (ASTMnemonic::ROR, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_right(value));
                5
            }
            (ASTMnemonic::ROR, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                let addr = (*addr + self.x) as u16;
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_right(value));
                6
            }
            (ASTMnemonic::ROR, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                let value = memory.read_byte(*addr);
                memory.write_byte(*addr, self.rotate_right(value));
                6
            }
            (ASTMnemonic::ROR, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.x as u16);
                let value = memory.read_byte(addr);
                memory.write_byte(addr, self.rotate_right(value));
                7
            }
            // RTI
            (ASTMnemonic::RTI, _, ASTOperand::Implied) => {
                self.status = self.pop_from_stack(memory).into();
                self.status.break_command = false;
                self.status.interrupt_disable = false;
                self.pc = self.pop_from_stack(memory) as u16;
                self.pc |= (self.pop_from_stack(memory) as u16) << 8;
                6
            }
            // RTS
            (ASTMnemonic::RTS, _, ASTOperand::Implied) => {
                self.pc = self.pop_from_stack(memory) as u16;
                self.pc |= (self.pop_from_stack(memory) as u16) << 8;
                self.pc += 1;
                6
            }
            // SBC
            (ASTMnemonic::SBC, _, ASTOperand::Immediate(value)) => {
                self.subtract_with_carry(*value);
                2
            }
            (ASTMnemonic::SBC, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.subtract_with_carry(memory.read_byte(*addr as u16));
                3
            }
            (ASTMnemonic::SBC, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.subtract_with_carry(memory.read_byte((*addr + self.x) as u16));
                4
            }
            (ASTMnemonic::SBC, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.subtract_with_carry(memory.read_byte(*addr));
                4
            }
            (ASTMnemonic::SBC, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.subtract_with_carry(memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::SBC, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.subtract_with_carry(memory.read_byte(indexed_addr));
                if page_boundary_crossed {
                    5
                } else {
                    4
                }
            }
            (ASTMnemonic::SBC, ASTAddressingMode::IndirectIndexedX, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.subtract_with_carry(memory.read_byte(indirect_addr));
                6
            }
            (ASTMnemonic::SBC, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.subtract_with_carry(memory.read_byte(indexed_addr & 0xff));
                if page_boundary_crossed {
                    6
                } else {
                    5
                }
            }
            // SEC
            (ASTMnemonic::SEC, _, ASTOperand::Implied) => {
                self.status.carry = true;
                2
            }
            // SEI
            (ASTMnemonic::SEI, _, ASTOperand::Implied) => {
                self.status.interrupt_disable = true;
                2
            }
            // STA
            (ASTMnemonic::STA, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::A, *addr as u16, memory);
                3
            }
            (ASTMnemonic::STA, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::A, (*addr + self.x) as u16, memory);
                4
            }
            (ASTMnemonic::STA, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.store_register(Register::A, *addr, memory);
                4
            }
            (ASTMnemonic::STA, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (_, indexed_addr) = self.indexed_indirect(Register::X, *addr);
                self.store_register(Register::A, indexed_addr, memory);
                5
            }
            (ASTMnemonic::STA, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (_, indexed_addr) = self.indexed_indirect(Register::Y, *addr);
                self.store_register(Register::A, indexed_addr, memory);
                5
            }
            (ASTMnemonic::STA, ASTAddressingMode::IndirectIndexedX, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.store_register(Register::A, indirect_addr, memory);
                6
            }
            (ASTMnemonic::STA, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let (_, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.store_register(Register::A, indexed_addr, memory);
                6
            }
            // STX
            (ASTMnemonic::STX, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::X, *addr as u16, memory);
                3
            }
            (ASTMnemonic::STX, ASTAddressingMode::ZeroPageY, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::X, (*addr + self.y) as u16, memory);
                4
            }
            (ASTMnemonic::STX, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.store_register(Register::X, *addr, memory);
                4
            }
            // STY
            (ASTMnemonic::STY, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::Y, *addr as u16, memory);
                3
            }
            (ASTMnemonic::STY, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::Y, (*addr + self.x) as u16, memory);
                4
            }
            (ASTMnemonic::STY, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.store_register(Register::Y, *addr, memory);
                4
            }
            // TAX
            (ASTMnemonic::TAX, _, ASTOperand::Implied) => {
                self.load_register(Register::X, self.a);
                2
            }
            // TAY
            (ASTMnemonic::TAY, _, ASTOperand::Implied) => {
                self.load_register(Register::Y, self.a);
                2
            }
            // TSX
            (ASTMnemonic::TSX, _, ASTOperand::Implied) => {
                self.load_register(Register::X, self.sp);
                2
            }
            // TXA
            (ASTMnemonic::TXA, _, ASTOperand::Implied) => {
                self.load_register(Register::A, self.x);
                2
            }
            // TXS
            (ASTMnemonic::TXS, _, ASTOperand::Implied) => {
                self.sp = self.x;
                2
            }
            // TYA
            (ASTMnemonic::TYA, _, ASTOperand::Implied) => {
                self.load_register(Register::A, self.y);
                2
            }
            _ => todo!("Invalid instruction: '{:#?}'", &ins),
        }
    }

    #[tracing::instrument]
    fn indexed_indirect(&self, register: Register, addr: u16) -> (bool, u16) {
        let indexed_addr = addr.wrapping_add(match register {
            Register::X => self.x,
            Register::Y => self.y,
            _ => panic!("Invalid register: '{:#?}'", register),
        } as u16);
        let page_boundary_crossed = (addr & 0xFF00) != (indexed_addr & 0xFF00);

        (page_boundary_crossed, indexed_addr)
    }

    #[tracing::instrument]
    fn indexed_indirect_x(&self, memory: &mut Memory, zp_addr: u8) -> u16 {
        memory.read_word((zp_addr + self.x) as u16) & 0xff
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
            let old_pc = self.pc;
            self.pc = self.pc.wrapping_add_signed(offset as i16);

            let boundery_crossed = (self.pc & 0xFF00) != (old_pc & 0xFF00);
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
        self.status.carry = value & 0x80 != 0;
        let result = value << 1;
        self.set_zero_and_negative_flags(result);
        result
    }

    #[tracing::instrument]
    fn shift_right(&mut self, value: u8) -> u8 {
        self.status.carry = value & 0x01 != 0;
        let result = value >> 1;
        self.set_zero_and_negative_flags(result);
        result
    }

    #[tracing::instrument]
    fn rotate_left(&mut self, value: u8) -> u8 {
        let carry = self.status.carry as u8;
        self.status.carry = value & 0x80 != 0;
        let result = value << 1 | carry;
        self.set_zero_and_negative_flags(result);
        result
    }

    #[tracing::instrument]
    fn rotate_right(&mut self, value: u8) -> u8 {
        let carry = self.status.carry as u8;
        self.status.carry = value & 0x01 != 0;
        let result = value >> 1 | carry << 7;
        self.set_zero_and_negative_flags(result);
        result
    }

    #[tracing::instrument]
    fn add_with_carry(&mut self, value: u8) {
        // TODO: Handle BCD mode

        let result: u16 = value as u16 + self.a as u16 + self.status.carry as u16;
        self.status.zero = result & 0xff == 0;
        self.status.negative = result & 0x80 != 0;
        self.status.overflow = (self.a ^ result as u8) & (value ^ result as u8) & 0x80 != 0;
        self.status.carry = result > 0xff;
        self.a = result as u8;
    }

    #[tracing::instrument]
    fn subtract_with_carry(&mut self, value: u8) {
        // TODO: Handle BCD mode

        let borrow = if self.status.carry { 0 } else { 1 };
        let result: u16 = (self.a as u16)
            .wrapping_sub(value as u16)
            .wrapping_sub(borrow as u16);

        self.status.negative = result & 0x80 != 0;
        self.status.zero = result & 0xff == 0;
        self.status.overflow =
            ((self.a ^ value) & 0x80) != 0 && ((self.a ^ result as u8) & 0x80) != 0;
        self.status.carry = result < 0x100;
        self.a = result as u8;
    }

    #[tracing::instrument]
    fn bit_test(&mut self, value: u8) {
        self.set_zero_and_negative_flags(value);
        self.status.overflow = value & 0x40 != 0;
    }

    #[tracing::instrument]
    fn compare(&mut self, register: Register, value: u8) {
        let register_value = match register {
            Register::A => self.a,
            Register::X => self.x,
            Register::Y => self.y,
        };
        self.set_zero_and_negative_flags(register_value.wrapping_sub(value));
        self.status.carry = register_value >= value;
    }

    #[tracing::instrument]
    fn set_zero_and_negative_flags(&mut self, value: u8) {
        self.status.zero = value == 0;
        self.status.negative = value & 0x80 != 0;
    }

    #[tracing::instrument]
    fn load_register(&mut self, register: Register, value: u8) {
        match register {
            Register::A => self.a = value,
            Register::X => self.x = value,
            Register::Y => self.y = value,
        }
        self.set_zero_and_negative_flags(value);
    }

    #[tracing::instrument]
    fn store_register(&mut self, register: Register, addr: u16, memory: &mut Memory) {
        let value = match register {
            Register::A => self.a,
            Register::X => self.x,
            Register::Y => self.y,
        };
        memory.write_byte(addr, value);
    }

    #[tracing::instrument]
    fn push_to_stack(&mut self, memory: &mut Memory, value: u8) {
        memory.write_byte(STACK_BASE + self.sp as u16, value);
        self.sp = self.sp.wrapping_sub(1);
    }

    #[tracing::instrument]
    fn pop_from_stack(&mut self, memory: &mut Memory) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        memory.read_byte(STACK_BASE + self.sp as u16)
    }

    #[tracing::instrument]
    fn set_program_counter(&mut self, addr: u16) {
        self.pc = addr;
    }

    #[tracing::instrument]
    fn handle_interrupt(&mut self, memory: &mut Memory, vector: u16) -> usize {
        let return_addr = self.pc;
        self.push_to_stack(memory, (return_addr >> 8) as u8);
        self.push_to_stack(memory, return_addr as u8);
        self.push_to_stack(memory, self.status.into());
        self.status.interrupt_disable = true;
        self.pc = memory.read_word(vector);

        7
    }

    #[tracing::instrument]
    fn handle_reset(&mut self, memory: &mut Memory) -> usize {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xFD;
        self.status = Status {
            interrupt_disable: true,
            ..Default::default()
        };
        self.pc = memory.read_word(RESET_VECTOR);

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
            if !self.status.interrupt_disable {
                self.cycles_left_for_instruction = self.handle_interrupt(memory, INTERRUPT_VECTOR);
            }
        }
    }

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
        self.pc += instruction.size() as u16;
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
                    if ins.ins.mnemonic == ASTMnemonic::BRK {
                        break;
                    }
                }
                self.clock(memory);
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{assembler::compile_code, emulator::memory::Memory};

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
            let bytes = compile_code(self.code, PROGRAM_START).expect("Failed to compile code");
            memory.load(PROGRAM_START, &bytes);
            if let Some(init_memory_fn) = self.init_memory_fn {
                init_memory_fn(&mut memory);
            }
            cpu.set_program_counter(PROGRAM_START);

            // Act
            cpu.run(&mut memory, RunOption::UntilCycles(self.expected_cycles));

            // Assert
            assert_eq!(cpu.a, self.expected_cpu.a);
            assert_eq!(cpu.x, self.expected_cpu.x);
            assert_eq!(cpu.y, self.expected_cpu.y);
            assert_eq!(cpu.pc, self.expected_cpu.pc);
            assert_eq!(cpu.sp, self.expected_cpu.sp);
            assert_eq!(cpu.status, self.expected_cpu.status);

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
                    a: 0x20,
                    pc: PROGRAM_START + 2 + 2,
                    status: Status {
                        carry: false,
                        overflow: false,
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Overflow
                code: "LDA #$7f\nADC #$01",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff\nADC #$ff",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff\nADC #$ff\nADC #$ff",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff\nADC #$ff\nADC #$ff\nADC #$ff",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2 + 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "ADC #$80\nADC #$80",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // SBC
            TestCase {
                code: "SEC\nSBC #$01",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$10\nSEC\nSBC #$10",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$10\nSEC\nSBC #$11",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$10\nSEC\nSBC #$ff",
                expected_cpu: Cpu {
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
                    status: Status {
                        carry: false,
                        ..Default::default()
                    },
                    pc: PROGRAM_START + 1 + 1,
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // SEC (set carry flag)
            TestCase {
                code: "SEC",
                expected_cpu: Cpu {
                    status: Status {
                        carry: true,
                        ..Default::default()
                    },
                    pc: PROGRAM_START + 1,
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
                    a: 0x00,
                    status: Status {
                        carry: false,
                        ..Default::default()
                    },
                    pc: PROGRAM_START + 2 + 0x10,
                    ..Default::default()
                },
                expected_cycles: 3, // LDA is not executed
                ..Default::default()
            },
            TestCase {
                // Branch taken, backwards on other page
                code: "BCC $80",
                expected_cpu: Cpu {
                    status: Status {
                        carry: false,
                        ..Default::default()
                    },
                    pc: PROGRAM_START - 126,
                    ..Default::default()
                },
                expected_cycles: 4,
                ..Default::default()
            },
            TestCase {
                // Branch not taken
                code: "SEC\nBCC $01\nLDA #$01",
                expected_cpu: Cpu {
                    a: 0x01,
                    status: Status {
                        carry: true,
                        ..Default::default()
                    },
                    pc: PROGRAM_START + 1 + 2 + 2,
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
                    pc: 0xff00,
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
                    pc: 0x1234,
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
                    pc: 0xff00,
                    sp: 0xfd,
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
                    a: 0x01,
                    pc: PROGRAM_START + 3 + 2, // JSR + LDA
                    sp: 0xff,
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Shift with carry
                code: "LDA #$80\nASL",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // LSR
            TestCase {
                // Simple shift
                code: "LDA #$10\nLSR",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Shift with carry
                code: "LDA #$01\nLSR",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // ROL
            TestCase {
                // Simple rotate
                code: "LDA #$80\nROL\nROL",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2 + 2,
                ..Default::default()
            },
            // ROR
            TestCase {
                // Simple rotate
                code: "LDA #$01\nROR\nROR",
                expected_cpu: Cpu {
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
                    status: Status {
                        zero: true,
                        overflow: false,
                        negative: false,
                        ..Default::default()
                    },
                    pc: PROGRAM_START + 2,
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
                expected_cycles: 2 + 3,
                ..Default::default()
            },
            TestCase {
                // Test BIT with overflow
                code: "LDA #$40\nBIT $00",
                init_memory_fn: Some(|memory| memory.write_byte(0x00, 0x40)),
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 3,
                ..Default::default()
            },
            TestCase {
                // Test BIT with overflow and negative
                code: "LDA #%11000000\nBIT $00",
                init_memory_fn: Some(|memory| memory.write_byte(0x00, 0b11000000)),
                expected_cpu: Cpu {
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
                    a: 0x10,
                    pc: PROGRAM_START + 2 + 2,
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #%10101010\nAND #$0f",
                expected_cpu: Cpu {
                    a: 0b1010,
                    pc: PROGRAM_START + 2 + 2,
                    status: Status {
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
                    a: 0x00,
                    pc: PROGRAM_START + 2,
                    status: Status {
                        zero: true,
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
                    a: 0x00,
                    pc: PROGRAM_START + 2 + 2,
                    status: Status {
                        zero: true,
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
                    a: 0x00,
                    pc: PROGRAM_START + 2 + 2,
                    status: Status {
                        zero: true,
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
                    a: 0xff,
                    pc: PROGRAM_START + 2 + 2,
                    status: Status {
                        negative: true,
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
                    a: 0xf0,
                    pc: PROGRAM_START + 2 + 2,
                    status: Status {
                        negative: true,
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
                    a: 0x10,
                    pc: PROGRAM_START + 2 + 2,
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$f0\nORA #$0f",
                expected_cpu: Cpu {
                    a: 0xff,
                    pc: PROGRAM_START + 2 + 2,
                    status: Status {
                        negative: true,
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
                    a: 0b01011111,
                    pc: PROGRAM_START + 2 + 2,
                    status: Status {
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
                    a: 0xff,
                    pc: PROGRAM_START + 2,
                    status: Status {
                        negative: true,
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Register > Operand
                code: "LDA #$80\nCMP #$40",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Register < Operand
                code: "LDA #$20\nCMP #$40",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                // Register < Operand
                code: "LDA #$f0\nCMP #$30",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$30\nCMP #$f0",
                expected_cpu: Cpu {
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
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff\nCMP #$00",
                expected_cpu: Cpu {
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
                    pc: PROGRAM_START + 2,
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
                    pc: PROGRAM_START + 2,
                    status: Status {
                        zero: true,
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
                    pc: PROGRAM_START + 2,
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
                    x: 0xff,
                    pc: PROGRAM_START + 1,
                    status: Status {
                        negative: true,
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
                    y: 0xff,
                    pc: PROGRAM_START + 1,
                    status: Status {
                        negative: true,
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
                    pc: PROGRAM_START + 2,
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
                    pc: PROGRAM_START + 2,
                    status: Status {
                        zero: true,
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
                    pc: PROGRAM_START + 2,
                    status: Status {
                        negative: true,
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
                    x: 0x01,
                    pc: PROGRAM_START + 1,
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            // INY
            TestCase {
                code: "INY",
                expected_cpu: Cpu {
                    y: 0x01,
                    pc: PROGRAM_START + 1,
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
                    a: 0x10,
                    pc: PROGRAM_START + 2,
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            TestCase {
                code: "LDA #$ff",
                expected_cpu: Cpu {
                    a: 0xff,
                    pc: PROGRAM_START + 2,
                    status: Status {
                        negative: true,
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
                    a: 0x00,
                    pc: PROGRAM_START + 2,
                    status: Status {
                        zero: true,
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
                    a: 0x10,
                    pc: PROGRAM_START + 2,
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
                    a: 0x10,
                    x: 0x01,
                    pc: PROGRAM_START + 2 + 2,
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
                    a: 0x10,
                    pc: PROGRAM_START + 3,
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
                    a: 0x10,
                    x: 0x01,
                    pc: PROGRAM_START + 2 + 3,
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
                    a: 0x10,
                    x: 0x01,
                    pc: PROGRAM_START + 2 + 3,
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
                    a: 0x10,
                    y: 0x01,
                    pc: PROGRAM_START + 2 + 3,
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
                    a: 0x10,
                    y: 0x01,
                    pc: PROGRAM_START + 2 + 3,
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
                    a: 0x10,
                    x: 0x01,
                    pc: PROGRAM_START + 2 + 2,
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
                    a: 0x10,
                    y: 0x01,
                    pc: PROGRAM_START + 2 + 2,
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
                    a: 0x10,
                    y: 0x01,
                    pc: PROGRAM_START + 2 + 2,
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
                    pc: PROGRAM_START + 1,
                    ..Default::default()
                },
                expected_cycles: 2,
                ..Default::default()
            },
            TestCase {
                code: "NOP\nNOP",
                expected_cpu: Cpu {
                    pc: PROGRAM_START + 2,
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
                    a: 0x34,
                    pc: PROGRAM_START + 2 + 2,
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
                    a: 0x34,
                    pc: PROGRAM_START + 2 + 3,
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
                    a: 0x34,
                    x: 0x34,
                    pc: PROGRAM_START + 2 + 1,
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // TAY
            TestCase {
                code: "LDA #$34\nTAY",
                expected_cpu: Cpu {
                    a: 0x34,
                    y: 0x34,
                    pc: PROGRAM_START + 2 + 1,
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // TXA
            TestCase {
                code: "LDX #$34\nTXA",
                expected_cpu: Cpu {
                    a: 0x34,
                    x: 0x34,
                    pc: PROGRAM_START + 2 + 1,
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // TYA
            TestCase {
                code: "LDY #$34\nTYA",
                expected_cpu: Cpu {
                    a: 0x34,
                    y: 0x34,
                    pc: PROGRAM_START + 2 + 1,
                    ..Default::default()
                },
                expected_cycles: 2 + 2,
                ..Default::default()
            },
            // TSX
            TestCase {
                code: "TSX",
                expected_cpu: Cpu {
                    x: 0xff,
                    sp: 0xff,
                    pc: PROGRAM_START + 1,
                    status: Status {
                        negative: true,
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
                    x: 0xfc,
                    sp: 0xfc,
                    pc: PROGRAM_START + 2 + 1,
                    status: Status {
                        negative: true,
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
                    a: 0x34,
                    pc: PROGRAM_START + 2 + 1,
                    sp: 0xff - 1,
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
                    a: 0x34,
                    pc: PROGRAM_START + 2 + 1 + 2 + 1,
                    sp: 0xff,
                    ..Default::default()
                },
                expected_cycles: 2 + 3 + 2 + 4,
                ..Default::default()
            },
            // PHP
            TestCase {
                code: "LDA #$80\nPHP",
                expected_cpu: Cpu {
                    a: 0x80,
                    pc: PROGRAM_START + 2 + 1,
                    sp: 0xff - 1,
                    status: Status {
                        negative: true,
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
                    a: 0x00,
                    pc: PROGRAM_START + 2 + 1 + 2 + 1,
                    sp: 0xff,
                    status: Status {
                        negative: true,
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
                    pc: 0x1200,
                    sp: 0xff - 3,
                    status: Status {
                        break_command: true,
                        interrupt_disable: true,
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
                    a: 0x01,
                    pc: PROGRAM_START + 1 + 2,
                    sp: 0xff,
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
