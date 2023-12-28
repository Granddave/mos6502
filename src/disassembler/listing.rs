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
        Self::new(ast, 0x0600)
    }

    #[tracing::instrument]
    fn generate_line(&self, ins: &ASTInstructionNode) -> String {
        let bytes = Compiler::instruction_to_bytes(ins);
        let bytes_str = bytes
            .iter()
            .map(|b| format!("{:02X}", b))
            .collect::<Vec<String>>()
            .join(" ");
        format!("${:04X}  {:08}  {}\n", self.current_address, bytes_str, ins)
    }

    #[tracing::instrument]
    pub fn generate(&mut self) -> String {
        self.str.push_str(" Addr  Hexdump   Instructions\n");
        self.str.push_str("-----------------------------\n");
        //                 $0600  20 06 06  JSR $0606

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

    use pretty_assertions::assert_eq;

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
        let expected = " Addr  Hexdump   Instructions
-----------------------------
$0600  20 06 06  JSR $0606
$0603  A9 01     LDA #$01
$0605  AD 00 02  LDA $0200
$0608  BD 00 02  LDA $0200,X
";

        assert_eq!(listing.generate(), expected);
    }
}
