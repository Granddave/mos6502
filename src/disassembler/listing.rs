use crate::{
    assembler::compiler::Compiler,
    ast::{Instruction, Node, AST},
};

/// Generate listing line from an AST Node and its memory address
///
/// E.g. `$8000  20 06 80  JSR $8006`
#[tracing::instrument]
pub fn generate_line(addr: usize, ins: &Instruction) -> String {
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
    //               $8000  20 06 80  JSR $8006

    let mut current_address = program_addr;
    let mut last_node: Option<&Node> = None;
    let mut first_double = true;
    for node in &ast {
        let same_as_last = if let Some(last_node) = last_node {
            last_node == node
        } else {
            false
        };

        // TODO: Add support for other AST nodes
        if let Node::Instruction(ins) = &node {
            if same_as_last && first_double {
                string.push_str("*\n");
                first_double = false;
            } else if !same_as_last {
                string.push_str(generate_line(current_address, &ins).as_str());
            }
            current_address += ins.size();
        }

        last_node = Some(&node);
    }

    string.clone()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Operand;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_listing() {
        use crate::ast::{AddressingMode, Mnemonic, Node};
        let ast = vec![
            Node::new_instruction(
                Mnemonic::JSR,
                AddressingMode::Absolute,
                Operand::Absolute(0x8006),
            ),
            Node::new_instruction(
                Mnemonic::LDA,
                AddressingMode::Immediate,
                Operand::Immediate(0x01),
            ),
            Node::new_instruction(
                Mnemonic::LDA,
                AddressingMode::Absolute,
                Operand::Absolute(0x0200),
            ),
            Node::new_instruction(
                Mnemonic::LDA,
                AddressingMode::AbsoluteX,
                Operand::Absolute(0x0200),
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
