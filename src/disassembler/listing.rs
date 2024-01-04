use crate::{
    assembler::compiler::Compiler,
    ast::{ASTInstructionNode, ASTNode, AST},
};

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
pub fn generate(program_addr: usize, ast: AST) -> String {
    let mut string = String::new();
    string.push_str(" Addr  Hexdump   Instructions\n");
    string.push_str("-----------------------------\n");
    //                 $8000  20 06 80  JSR $8006

    let mut current_address = program_addr;
    for node in &ast {
        // TODO: Add support for other AST nodes
        if let ASTNode::Instruction(ins_node) = node {
            string.push_str(generate_line(current_address, ins_node).as_str());
            current_address += ins_node.size();
        }
    }

    string.clone()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ASTOperand;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_listing() {
        use crate::ast::{ASTAddressingMode, ASTMnemonic, ASTNode};
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
        let listing = generate(0x8000, ast);
        let expected = " Addr  Hexdump   Instructions
-----------------------------
0x8000  20 06 80  JSR $8006
0x8003  a9 01     LDA #$01
0x8005  ad 00 02  LDA $0200
0x8008  bd 00 02  LDA $0200,X
";

        assert_eq!(listing, expected);
    }
}
