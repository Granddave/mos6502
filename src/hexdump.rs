fn addr(str: &mut String, offset: usize, addr_width: usize) {
    if offset != 0 {
        str.push_str("\n");
    }
    str.push_str(format!("{:0width$X}", offset, width = addr_width).as_str());
}

pub fn hexdump(program: &[u8], addr_width: usize, stride: usize) -> String {
    let mut str = String::new();

    let mut line = 0;
    let mut ix = 0;
    for bytes in program.chunks(2) {
        if ix % stride == 0 {
            addr(&mut str, line * stride, addr_width);
            line += 1;
        }
        if ix % 2 == 0 {
            str.push_str(" ");
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
    #[test]
    fn test_hexdump() {
        let program = vec![
            0xA2, 0x00, 0xA0, 0x00, 0x8A, 0x99, 0x00, 0x02, 0x48, 0xE8, 0xC8, 0xC0, 0x10, 0xD0,
            0xF5, 0x68, 0x99, 0x00, 0x02, 0xC8, 0xC0, 0x20, 0xD0, 0xF7,
        ];
        let str = hexdump(&program, 7, 16);

        assert_eq!(
            str,
            "0000000 00a2 00a0 998a 0200 e848 c0c8 d010 68f5
0000010 0099 c802 20c0 f7d0
0000018"
        );
    }
}
