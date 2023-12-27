use crate::{
    assembler::compiler::opcode::OPCODE_MAPPING,
    ast::{ASTAddressingMode, ASTInstructionNode, ASTMnemonic, ASTOperand},
    emulator::memory::Bus,
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Status {
    /// (C) Carry flag, set if the last operation caused an overflow from bit 7 or an
    /// underflow from bit 0.
    carry: bool,
    /// (Z) Zero flag, set if the result of the last operation was zero.
    zero: bool,
    /// (I) Interrupt disable flag, set if the CPU is not to respond to interrupts.
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

#[derive(Debug)]
enum Register {
    A,
    X,
    Y,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Cpu {
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
}

impl Cpu {
    pub fn new() -> Self {
        Self::default()
    }

    /// Fetches and decodes the next instruction from memory.
    fn fetch_and_decode(&mut self, memory: &mut dyn Bus) -> ASTInstructionNode {
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

    fn execute_instruction(
        &mut self,
        ins: ASTInstructionNode,
        memory: &mut dyn Bus,
        cycles: &mut usize,
    ) {
        match (&ins.ins.mnemonic, &ins.ins.addr_mode, &ins.operand) {
            (ASTMnemonic::BRK, _, ASTOperand::Implied) => {
                self.pc += 1;
                panic!("BRK");
            }
            // DEC
            (ASTMnemonic::DEC, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr).wrapping_sub(1);
                memory.write(addr, value);
                self.set_zero_and_negative_flags(value);
                *cycles -= 5;
            }
            (ASTMnemonic::DEC, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                let addr = (*addr + self.x) as u16;
                let value = memory.read_byte(addr).wrapping_sub(1);
                memory.write(addr, value);
                self.set_zero_and_negative_flags(value);
                *cycles -= 6;
            }
            (ASTMnemonic::DEC, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                let value = memory.read_byte(*addr).wrapping_sub(1);
                memory.write(*addr, value);
                self.set_zero_and_negative_flags(value);
                *cycles -= 6;
            }
            (ASTMnemonic::DEC, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.x as u16);
                let value = memory.read_byte(addr).wrapping_sub(1);
                memory.write(addr, value);
                self.set_zero_and_negative_flags(value);
                *cycles -= 7;
            }
            // DEX
            (ASTMnemonic::DEX, _, ASTOperand::Implied) => {
                self.x = self.x.wrapping_sub(1);
                self.set_zero_and_negative_flags(self.x);
                *cycles -= 2;
            }
            // DEY
            (ASTMnemonic::DEY, _, ASTOperand::Implied) => {
                self.y = self.y.wrapping_sub(1);
                self.set_zero_and_negative_flags(self.y);
                *cycles -= 2;
            }
            // INC
            (ASTMnemonic::INC, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                let addr = *addr as u16;
                let value = memory.read_byte(addr).wrapping_add(1);
                memory.write(addr, value);
                self.set_zero_and_negative_flags(value);
                *cycles -= 5;
            }
            (ASTMnemonic::INC, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                let addr = (*addr + self.x) as u16;
                let value = memory.read_byte(addr).wrapping_add(1);
                memory.write(addr, value);
                self.set_zero_and_negative_flags(value);
                *cycles -= 6;
            }
            (ASTMnemonic::INC, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                let value = memory.read_byte(*addr).wrapping_add(1);
                memory.write(*addr, value);
                self.set_zero_and_negative_flags(value);
                *cycles -= 6;
            }
            (ASTMnemonic::INC, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let addr = addr.wrapping_add(self.x as u16);
                let value = memory.read_byte(addr).wrapping_add(1);
                memory.write(addr, value);
                self.set_zero_and_negative_flags(value);
                *cycles -= 7;
            }
            // INX
            (ASTMnemonic::INX, _, ASTOperand::Implied) => {
                self.x = self.x.wrapping_add(1);
                self.set_zero_and_negative_flags(self.x);
                *cycles -= 2;
            }
            // INY
            (ASTMnemonic::INY, _, ASTOperand::Implied) => {
                self.y = self.y.wrapping_add(1);
                self.set_zero_and_negative_flags(self.y);
                *cycles -= 2;
            }
            // LDA
            (ASTMnemonic::LDA, _, ASTOperand::Immediate(value)) => {
                self.load_register(Register::A, *value);
                *cycles -= 2;
            }
            (ASTMnemonic::LDA, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::A, memory.read_byte(*addr as u16));
                *cycles -= 3;
            }
            (ASTMnemonic::LDA, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::A, memory.read_byte((*addr + self.x) as u16));
                *cycles -= 4;
            }
            (ASTMnemonic::LDA, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.load_register(Register::A, memory.read_byte(*addr));
                *cycles -= 4;
            }
            (ASTMnemonic::LDA, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.load_register(Register::A, memory.read_byte(indexed_addr));
                *cycles -= if page_boundary_crossed { 5 } else { 4 };
            }
            (ASTMnemonic::LDA, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.load_register(Register::A, memory.read_byte(indexed_addr));
                *cycles -= if page_boundary_crossed { 5 } else { 4 };
            }
            (ASTMnemonic::LDA, ASTAddressingMode::IndirectIndexedX, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.load_register(Register::A, memory.read_byte(indirect_addr));
                *cycles -= 6;
            }
            (ASTMnemonic::LDA, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let (page_boundary_crossed, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.load_register(Register::A, memory.read_byte(indexed_addr & 0xff));
                *cycles -= if page_boundary_crossed { 6 } else { 5 };
            }
            // LDX
            (ASTMnemonic::LDX, _, ASTOperand::Immediate(value)) => {
                self.load_register(Register::X, *value);
                *cycles -= 2;
            }
            (ASTMnemonic::LDX, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::X, memory.read_byte(*addr as u16));
                *cycles -= 3;
            }
            (ASTMnemonic::LDX, ASTAddressingMode::ZeroPageY, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::X, memory.read_byte((*addr + self.y) as u16));
                *cycles -= 4;
            }
            (ASTMnemonic::LDX, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.load_register(Register::X, memory.read_byte(*addr));
                *cycles -= 4;
            }
            (ASTMnemonic::LDX, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, *addr);
                self.load_register(Register::X, memory.read_byte(indexed_addr));
                *cycles -= if page_boundary_crossed { 5 } else { 4 };
            }
            // LDY
            (ASTMnemonic::LDY, _, ASTOperand::Immediate(value)) => {
                self.load_register(Register::Y, *value);
                *cycles -= 2;
            }
            (ASTMnemonic::LDY, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::Y, memory.read_byte(*addr as u16));
                *cycles -= 3;
            }
            (ASTMnemonic::LDY, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.load_register(Register::Y, memory.read_byte((*addr + self.x) as u16));
                *cycles -= 4;
            }
            (ASTMnemonic::LDY, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.load_register(Register::Y, memory.read_byte(*addr));
                *cycles -= 4;
            }
            (ASTMnemonic::LDY, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::X, *addr);
                self.load_register(Register::Y, memory.read_byte(indexed_addr));
                *cycles -= if page_boundary_crossed { 5 } else { 4 };
            }
            // NOP
            (ASTMnemonic::NOP, _, ASTOperand::Implied) => {
                *cycles -= 2;
            }
            // STA
            (ASTMnemonic::STA, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::A, *addr as u16, memory);
                *cycles -= 3;
            }
            (ASTMnemonic::STA, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::A, (*addr + self.x) as u16, memory);
                *cycles -= 4;
            }
            (ASTMnemonic::STA, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.store_register(Register::A, *addr, memory);
                *cycles -= 4;
            }
            (ASTMnemonic::STA, ASTAddressingMode::AbsoluteX, ASTOperand::Absolute(addr)) => {
                let (_, indexed_addr) = self.indexed_indirect(Register::X, *addr);
                self.store_register(Register::A, indexed_addr, memory);
                *cycles -= 5;
            }
            (ASTMnemonic::STA, ASTAddressingMode::AbsoluteY, ASTOperand::Absolute(addr)) => {
                let (_, indexed_addr) = self.indexed_indirect(Register::Y, *addr);
                self.store_register(Register::A, indexed_addr, memory);
                *cycles -= 5;
            }
            (ASTMnemonic::STA, ASTAddressingMode::IndirectIndexedX, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = self.indexed_indirect_x(memory, *addr);
                self.store_register(Register::A, indirect_addr, memory);
                *cycles -= 6;
            }
            (ASTMnemonic::STA, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let (_, indexed_addr) = self.indexed_indirect_y(memory, *addr);
                self.store_register(Register::A, indexed_addr, memory);
                *cycles -= 6;
            }
            // STX
            (ASTMnemonic::STX, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::X, *addr as u16, memory);
                *cycles -= 3;
            }
            (ASTMnemonic::STX, ASTAddressingMode::ZeroPageY, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::X, (*addr + self.y) as u16, memory);
                *cycles -= 4;
            }
            (ASTMnemonic::STX, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.store_register(Register::X, *addr, memory);
                *cycles -= 4;
            }
            // STY
            (ASTMnemonic::STY, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::Y, *addr as u16, memory);
                *cycles -= 3;
            }
            (ASTMnemonic::STY, ASTAddressingMode::ZeroPageX, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::Y, (*addr + self.x) as u16, memory);
                *cycles -= 4;
            }
            (ASTMnemonic::STY, ASTAddressingMode::Absolute, ASTOperand::Absolute(addr)) => {
                self.store_register(Register::Y, *addr, memory);
                *cycles -= 4;
            }
            // TAX
            (ASTMnemonic::TAX, _, ASTOperand::Implied) => {
                self.load_register(Register::X, self.a);
                *cycles -= 2;
            }
            // TAY
            (ASTMnemonic::TAY, _, ASTOperand::Implied) => {
                self.load_register(Register::Y, self.a);
                *cycles -= 2;
            }
            // TSX
            // TXA
            (ASTMnemonic::TXA, _, ASTOperand::Implied) => {
                self.load_register(Register::A, self.x);
                *cycles -= 2;
            }
            // TXS
            // TYA
            (ASTMnemonic::TYA, _, ASTOperand::Implied) => {
                self.load_register(Register::A, self.y);
                *cycles -= 2;
            }
            _ => panic!("Invalid instruction: '{:#?}'", &ins),
        }
    }

    fn indexed_indirect(&self, register: Register, addr: u16) -> (bool, u16) {
        let indexed_addr = addr.wrapping_add(match register {
            Register::X => self.x,
            Register::Y => self.y,
            _ => panic!("Invalid register: '{:#?}'", register),
        } as u16);
        let page_boundary_crossed = (addr & 0xFF00) != (indexed_addr & 0xFF00);

        (page_boundary_crossed, indexed_addr)
    }

    fn indexed_indirect_x(&self, memory: &mut dyn Bus, zp_addr: u8) -> u16 {
        memory.read_word((zp_addr + self.x) as u16) & 0xff
    }

    fn indexed_indirect_y(&self, memory: &mut dyn Bus, zp_addr: u8) -> (bool, u16) {
        let indirect_addr = memory.read_word(zp_addr as u16);
        self.indexed_indirect(Register::Y, indirect_addr)
    }

    fn set_zero_and_negative_flags(&mut self, value: u8) {
        self.status.zero = value == 0;
        self.status.negative = value & 0x80 != 0;
    }

    fn load_register(&mut self, register: Register, value: u8) {
        match register {
            Register::A => self.a = value,
            Register::X => self.x = value,
            Register::Y => self.y = value,
        }
        self.set_zero_and_negative_flags(value);
    }

    fn store_register(&mut self, register: Register, addr: u16, memory: &mut dyn Bus) {
        let value = match register {
            Register::A => self.a,
            Register::X => self.x,
            Register::Y => self.y,
        };
        memory.write(addr, value);
    }

    fn set_program_counter(&mut self, addr: u16) {
        self.pc = addr;
    }

    pub fn run(&mut self, memory: &mut dyn Bus, mut cycles: usize) {
        while cycles > 0 {
            let instruction = self.fetch_and_decode(memory);
            self.pc += instruction.size() as u16;
            self.execute_instruction(instruction, memory, &mut cycles);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{assembler::compile_code, emulator::memory::Memory};

    const PROGRAM_START: u16 = 0x0600;

    struct TestCase {
        code: &'static str,
        expected_cpu: Cpu,
        expected_cycles: usize,
        init_memory_fn: Option<fn(&mut Memory)>,
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
            memory.load(
                PROGRAM_START,
                &compile_code(self.code).expect("Failed to compile code"),
            );
            if let Some(init_memory_fn) = self.init_memory_fn {
                init_memory_fn(&mut memory);
            }
            cpu.set_program_counter(PROGRAM_START);

            // Act
            cpu.run(&mut memory, self.expected_cycles);

            // Assert
            assert_eq!(cpu, self.expected_cpu);
            if let Some(expected_memory_fn) = self.expected_memory_fn {
                expected_memory_fn(&memory);
            }
        }
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
                    memory.write(0x10, 0x10);
                }),
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x10), 0x0f);
                }),
                ..Default::default()
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
                    memory.write(0x10, 0x01);
                }),
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x10), 0x00);
                }),
                ..Default::default()
            },
            TestCase {
                code: "DEC $10",
                expected_cpu: Cpu {
                    pc: PROGRAM_START + 2,
                    ..Default::default()
                },
                expected_cycles: 5,
                init_memory_fn: Some(|memory| {
                    memory.write(0x10, 0x80);
                }),
                expected_memory_fn: Some(|memory| {
                    assert_eq!(memory.read_byte(0x10), 0x7f);
                }),
                ..Default::default()
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
                    memory.write(0x10, 0x01);
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
                    memory.write(0x10, 0xff);
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
                    memory.write(0x10, 0x7f);
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
                    memory.write(0x10, 0x10);
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
                    memory.write(0x11, 0x10);
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
                    memory.write(0x1234, 0x10);
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
                    memory.write(0x1235, 0x10);
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
                    memory.write(0x1300, 0x10);
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
                    memory.write(0x1235, 0x10);
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
                    memory.write(0x1300, 0x10);
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
                    memory.write(0x11, 0x34);
                    memory.write(0x34, 0x10);
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
                    memory.write(0x10, 0x34);
                    memory.write(0x35, 0x10);
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
                    memory.write(0x0034, 0xff);
                    memory.write(0x0000, 0x10);
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
        ]
        .into_iter()
        .for_each(|tc| tc.run_test());
    }
}
