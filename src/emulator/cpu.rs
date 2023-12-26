use crate::{
    assembler::compiler::opcode::OPCODE_MAPPING,
    ast::{ASTAddressingMode, ASTInstructionNode, ASTMnemonic, ASTOperand},
    emulator::memory::Bus,
};

const PROGRAM_START_ADDR: u16 = 0x0000;

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
    /// Returns the instruction and increments the program counter.
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
                let indirect_addr = memory.read_word((*addr + self.x) as u16);
                self.load_register(Register::A, memory.read_byte(indirect_addr));
                *cycles -= 6;
            }
            (ASTMnemonic::LDA, ASTAddressingMode::IndirectIndexedY, ASTOperand::ZeroPage(addr)) => {
                let indirect_addr = memory.read_word(*addr as u16);
                let (page_boundary_crossed, indexed_addr) =
                    self.indexed_indirect(Register::Y, indirect_addr);
                self.load_register(Register::A, memory.read_byte(indexed_addr));
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
            // STA
            (ASTMnemonic::STA, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::A, *addr as u16, memory);
                *cycles -= 3;
            }
            // STX
            (ASTMnemonic::STX, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::X, *addr as u16, memory);
                *cycles -= 3;
            }
            // STY
            (ASTMnemonic::STY, ASTAddressingMode::ZeroPage, ASTOperand::ZeroPage(addr)) => {
                self.store_register(Register::Y, *addr as u16, memory);
                *cycles -= 3;
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
    use crate::emulator::memory::Memory;

    fn init(code: &str) -> (Cpu, Memory) {
        let cpu = Cpu::new();
        let mut memory = Memory::new();
        let program = crate::assembler::compile_code(code).expect("Failed to compile code");
        memory.load(PROGRAM_START_ADDR, &program);
        memory.dump(PROGRAM_START_ADDR, PROGRAM_START_ADDR + 0x10);
        (cpu, memory)
    }

    struct TestCase {
        code: &'static str,
        expected_cpu: Cpu,
        expected_cycles: usize,
    }

    #[test]
    fn test_lda_imm() {
        let tests = vec![
            TestCase {
                code: "LDA #$10",
                expected_cpu: Cpu {
                    a: 0x10,
                    pc: 2,
                    ..Default::default()
                },
                expected_cycles: 2,
            },
            TestCase {
                code: "LDA #$ff",
                expected_cpu: Cpu {
                    a: 0xff,
                    pc: 2,
                    status: Status {
                        negative: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
            },
            TestCase {
                code: "LDA #$00",
                expected_cpu: Cpu {
                    a: 0x00,
                    pc: 2,
                    status: Status {
                        zero: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                expected_cycles: 2,
            },
        ];

        for tc in tests {
            let (mut cpu, mut memory) = init(tc.code);
            cpu.run(&mut memory, tc.expected_cycles);
            assert_eq!(cpu, tc.expected_cpu);
        }
    }
}
