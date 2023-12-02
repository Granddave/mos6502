// or mnemonic
#[derive(Debug)]
pub enum Instruction {
    // ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC, CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP, JSR,
    LDA,
    // LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL, ROR, RTI, RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,
}

impl Instruction {}

// fn generate_code(&self) -> Vec<u8> {
//     let opcode: u8 = encode_instruction(self.instruction, self.addressing_mode);
//     match self.operand {
//         Some(operand) => {
//             let mut code = vec![opcode];
//             code.extend(operand.encode());
//             code
//         }
//         None => vec![opcode],
//     }
// }

#[derive(Debug)]
pub enum AddressingMode {
    Absolute,
    // AbsoluteX, AbsoluteY, Accumulator,
    Immediate,
    // Implied, IndexedIndirect, Indirect, IndirectIndexed, Relative, ZeroPage, ZeroPageX, ZeroPageY,
}

#[derive(Debug)]
pub enum Operand {
    Accumulator,
    Implied,
    Immediate(u8),
    Absolute(u16),
    // ZeroPage(u8),
    // Relative(i8),
}

impl Operand {
    pub fn encode(&self) -> Vec<u8> {
        match self {
            Operand::Accumulator => vec![],
            Operand::Implied => vec![],
            Operand::Immediate(value) => vec![*value],
            Operand::Absolute(address) => vec![*address as u8, (*address >> 8) as u8],
        }
    }
}

// #[derive(Debug)]
// pub struct InstructionVariant(Instruction, AddressingMode);

// num_of_operands: usize, // In bytes
// num_of_cycles: usize,
// extra_if_page_crossed: bool,
// extra_if_branch_taken: bool,

type Opcode = u8;

// // Macro to match an opcode to an instruction variant
// macro_rules! match_opcode_to_instruction_variant {
//     ($value:expr, $($pattern:pat => $ins:expr, $mode:expr),*,) => {
//         {
//             match $value {
//                 $( $pattern => Some(InstructionVariant($ins, $mode)), )*
//                 _ => None,
//             }
//         }
//     };
// }
//
// impl InstructionVariant {
//     pub fn try_from(opcode: Opcode) -> Option<InstructionVariant> {
//         match_opcode_to_instruction_variant! {
//             opcode,
//             0x8D => Instruction::LDA, AddressingMode::Immediate,
//             0xAD => Instruction::LDA, AddressingMode::Absolute,
//         }
//     }
// }

// Use?
//type InstructionVariant = (Instruction, AddressingMode);
pub fn try_decode(opcode: Opcode) -> Option<(Instruction, AddressingMode)> {
    match opcode {
        0x8D => Some((Instruction::LDA, AddressingMode::Immediate)),
        0xAD => Some((Instruction::LDA, AddressingMode::Absolute)),
        _ => None,
    }
}

pub fn encode_instruction(instruction: Instruction, addressing_mode: AddressingMode) -> Opcode {
    match (instruction, addressing_mode) {
        (Instruction::LDA, AddressingMode::Immediate) => 0x8D,
        (Instruction::LDA, AddressingMode::Absolute) => 0xAD,
    }
}
