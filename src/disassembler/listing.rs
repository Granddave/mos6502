use crate::ast::{ASTInstructionNode, ASTNode};

#[derive(Debug)]
pub struct Listing {
    ast: Vec<ASTNode>,
    start_address: usize,
    current_address: usize,
    str: String,
}

impl Listing {
    #[tracing::instrument]
    pub fn new(ast: Vec<ASTNode>, start_address: usize) -> Self {
        Self {
            ast,
            start_address,
            current_address: start_address,
            str: String::new(),
        }
    }

    #[tracing::instrument]
    pub fn default(ast: Vec<ASTNode>) -> Self {
        Self::new(ast, 0x0600)
    }

    #[tracing::instrument]
    fn generate_line(&self, node: &ASTInstructionNode) -> String {
        format!("{:04x}: {}\n", self.current_address, node)
    }

    #[tracing::instrument]
    pub fn generate(&mut self) -> String {
        self.str.push_str("Addr  Ins\n");
        self.str.push_str("---------------\n");
        //                 0600: JSR $0606

        for node in &self.ast {
            // TODO: Add support for other AST nodes
            if let ASTNode::Instruction(ins_node) = node {
                self.str.push_str(self.generate_line(ins_node).as_str());
                self.current_address += ins_node.size();
            }
        }

        self.str.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::ASTOperand;

    #[test]
    fn test_listing() {
        use crate::ast::{ASTAddressingMode, ASTMnemonic, ASTNode};
        use crate::disassembler::listing::Listing;
        let ast = vec![
            ASTNode::new_instruction(
                ASTMnemonic::JSR,
                ASTAddressingMode::Absolute,
                ASTOperand::Absolute(0x0606),
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
        let expected = "Addr  Ins
---------------
0600: JSR $0606
0603: LDA #$01
0605: LDA $0200
0608: LDA $0200,X
";

        assert_eq!(listing.generate(), expected);
    }
}
