/// Hexdump a program based on <https://skilldrick.github.io/easy6502>
pub fn hexdump_2(program: &[u8], program_offset: u16, addr_width: usize, stride: usize) -> String {
    fn addr(str: &mut String, offset: usize, addr_width: usize) {
        if offset != 0 {
            str.push('\n');
        }
        str.push_str(format!("{:0width$x}:", offset, width = addr_width).as_str());
    }
    let mut str = String::new();

    let mut line = 0;
    for (ix, byte) in program.iter().enumerate() {
        // TODO: Detect if the last line is the same as the previous line and use `*` instead
        if ix % stride == 0 {
            let offset = program_offset as usize + (line * stride);
            addr(&mut str, offset, addr_width);
            line += 1;
        }
        str.push(' ');
        str.push_str(format!("{:02x}", byte).as_str());
    }

    str
}

/// Hexdump a program based on Linux `hexdump`
pub fn hexdump_1(program: &[u8], addr_width: usize, stride: usize) -> String {
    fn addr(str: &mut String, offset: usize, addr_width: usize) {
        if offset != 0 {
            str.push('\n');
        }
        str.push_str(format!("{:0width$X}", offset, width = addr_width).as_str());
    }

    let mut str = String::new();

    let mut line = 0;
    let mut ix = 0;
    for bytes in program.chunks(2) {
        if ix % stride == 0 {
            addr(&mut str, line * stride, addr_width);
            line += 1;
        }
        if ix % 2 == 0 {
            str.push(' ');
        }

        str.push_str(
            format!(
                "{:04x}",
                u16::from_le_bytes([bytes[0], if bytes.len() == 1 { 0 } else { bytes[1] }])
            )
            .as_str(),
        );
        ix += 2;
    }
    addr(&mut str, ix, addr_width);

    str
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_hexdump_1() {
        let program = vec![
            0xA2, 0x00, 0xA0, 0x00, 0x8A, 0x99, 0x00, 0x02, 0x48, 0xE8, 0xC8, 0xC0, 0x10, 0xD0,
            0xF5, 0x68, 0x99, 0x00, 0x02, 0xC8, 0xC0, 0x20, 0xD0, 0xF7,
        ];
        let str = hexdump_1(&program, 7, 16);

        assert_eq!(
            str,
            "0000000 00a2 00a0 998a 0200 e848 c0c8 d010 68f5
0000010 0099 c802 20c0 f7d0
0000018"
        );
    }
}
