use crate::{
    assembler::codegen::instruction_to_bytes,
    ast::{Directive, Node, AST},
};

/// Generate listing line from an AST Node and its memory address
///
/// E.g. `$8000  20 06 80  JSR $8006`
#[tracing::instrument]
pub fn generate_line(addr: usize, node: &Node) -> Option<String> {
    match node {
        Node::Instruction(ins) => {
            let bytes_str = instruction_to_bytes(ins)
                .expect("Failed to convert instruction to bytes") // TODO: Return result
                .iter()
                .map(|b| format!("{:02x}", b))
                .collect::<Vec<String>>()
                .join(" ");
            Some(format!("0x{:04x}  {:08}  {}\n", addr, bytes_str, node))
        }
        Node::Directive(directive) => match directive {
            Directive::Byte(byte) => Some(format!("0x{:04x}  {:02x}        .byte\n", addr, byte)),
            Directive::Word(word) => Some(format!(
                "0x{:04x}  {:02x} {:02x}     .word\n",
                addr,
                (word & 0xFF) as u8,
                (word >> 8) as u8,
            )),
            _ => None,
        },
        _ => None,
    }
}

#[tracing::instrument]
pub fn generate(program_addr: usize, ast: AST) -> String {
    let mut string = String::new();
    string.push_str("  Addr  Hexdump   Instructions\n");
    string.push_str("------------------------------\n");
    //               0x8000  20 06 80  JSR $8006

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
        if same_as_last && first_double {
            string.push_str("*\n");
            first_double = false;
        } else if !same_as_last {
            if let Some(node_str) = generate_line(current_address, node) {
                string.push_str(node_str.as_str());
            }
        }
        current_address += node.size();
        last_node = Some(node);
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
            Node::Directive(Directive::Byte(0x20)),
            Node::Directive(Directive::Word(0x8006)),
        ];
        let listing = generate(0x8000, ast);
        let expected = "  Addr  Hexdump   Instructions
------------------------------
0x8000  20 06 80  JSR $8006
0x8003  a9 01     LDA #$01
0x8005  ad 00 02  LDA $0200
0x8008  bd 00 02  LDA $0200,X
0x800b  20        .byte
0x800c  06 80     .word
";

        assert_eq!(listing, expected);
    }
}
