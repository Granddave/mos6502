use std::fmt;

pub mod constant;
pub mod directive;
pub mod instruction;

pub use constant::{Constant, ConstantValue};
pub use directive::Directive;
pub use instruction::{AddressingMode, Instruction, Mnemonic, Operand};

/// A single node in the AST, i.e. a single line in the source code.
#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    /// A CPU instruction
    Instruction(Instruction),
    /// A label to mark a location in the code
    ///
    /// Labels are resolved to absolute addresses during compilation.
    /// E.g. `init:`
    Label(String),
    /// A constant definition
    ///
    /// E.g. `define max_items $FF`
    Constant(Constant),
    Directive(Directive),
}

impl Node {
    pub fn new_instruction(
        mnemonic: Mnemonic,
        addr_mode: AddressingMode,
        operand: Operand,
    ) -> Node {
        Node::Instruction(Instruction::new(mnemonic, addr_mode, operand))
    }

    pub fn get_instruction(&mut self) -> Option<&mut Instruction> {
        match self {
            Node::Instruction(instruction) => Some(instruction),
            _ => None,
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Instruction(instruction) => write!(f, "{}", instruction),
            Node::Label(label) => write!(f, "{}:", label),
            Node::Constant(constant) => write!(f, "  {}", constant),
            Node::Directive(directive) => write!(f, "{}", directive),
        }
    }
}

/// An AST (Abstract Syntax Tree) is a collection of AST nodes.
///
/// The AST is the result of parsing the source code.
pub type AST = Vec<Node>;
