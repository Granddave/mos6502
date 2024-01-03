use crate::{
    assembler::compiler::Compiler,
    ast::{ASTInstructionNode, ASTNode},
};

#[derive(Debug)]
pub struct Listing {
    ast: Vec<ASTNode>,
    current_address: usize,
    str: String,
}

impl Listing {
    #[tracing::instrument]
    pub fn new(ast: Vec<ASTNode>, start_address: usize) -> Self {
        Self {
            ast,
            current_address: start_address,
            str: String::new(),
        }
    }

    #[tracing::instrument]
    pub fn default(ast: Vec<ASTNode>) -> Self {
        Self::new(ast, 0x8000)
    }

    /// Generate listing line from an AST Node and its memory address
    ///
    /// E.g. `$8000  20 06 80  JSR $8006`
    #[tracing::instrument]
    pub fn generate_line(addr: usize, ins: &ASTInstructionNode) -> String {
        let bytes_str = Compiler::instruction_to_bytes(ins)
            .expect("Failed to convert instruction to bytes") // TODO: Return result
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect::<Vec<String>>()
            .join(" ");

        format!("0x{:04x}  {:08}  {}\n", addr, bytes_str, ins)
    }

    #[tracing::instrument]
    pub fn generate(&mut self) -> String {
        self.str.push_str(" Addr  Hexdump   Instructions\n");
        self.str.push_str("-----------------------------\n");
        //                 $8000  20 06 80  JSR $8006

        for node in &self.ast {
            // TODO: Add support for other AST nodes
            if let ASTNode::Instruction(ins_node) = node {
                self.str
                    .push_str(Listing::generate_line(self.current_address, ins_node).as_str());
                self.current_address += ins_node.size();
            }
        }

        self.str.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::ASTOperand;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_listing() {
        use crate::ast::{ASTAddressingMode, ASTMnemonic, ASTNode};
        use crate::disassembler::listing::Listing;
        let ast = vec![
            ASTNode::new_instruction(
                ASTMnemonic::JSR,
                ASTAddressingMode::Absolute,
                ASTOperand::Absolute(0x8006),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::LDA,
                ASTAddressingMode::Immediate,
                ASTOperand::Immediate(0x01),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::LDA,
                ASTAddressingMode::Absolute,
                ASTOperand::Absolute(0x0200),
            ),
            ASTNode::new_instruction(
                ASTMnemonic::LDA,
                ASTAddressingMode::AbsoluteX,
                ASTOperand::Absolute(0x0200),
            ),
        ];
        let mut listing = Listing::default(ast);
        let expected = " Addr  Hexdump   Instructions
-----------------------------
0x8000  20 06 80  JSR $8006
0x8003  a9 01     LDA #$01
0x8005  ad 00 02  LDA $0200
0x8008  bd 00 02  LDA $0200,X
";

        assert_eq!(listing.generate(), expected);
    }
}
