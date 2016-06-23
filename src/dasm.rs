use opcodes::*;

pub fn disassemble(code: &Vec<u8>, load_addr: u16, start_addr: u16) -> Result<String, String> {
    let mut ip = start_addr;
    let mut mem = vec![0; 64 * 1024];

    load(&code, &mut mem, load_addr);

    while (ip as usize) < start_addr as usize + code.len() {
        match disassemble_step(&mem, &mut ip) {
            Ok(s) => println!("{}", &s),
            Err(e) => return Err(e),
        }
    }

    Ok("done".to_string())
}

fn load(from: &Vec<u8>, to: &mut Vec<u8>, load_addr: u16) {
    for ix in 0..from.len() {
        to[load_addr as usize + ix] = from[ix];
    }
}

pub fn disassemble_step(buf: &Vec<u8>, mut ip: &mut u16) -> Result<String, String> {
    let mut result = String::new();

    if (*ip as usize) < buf.len() {
        let s = try!(parse_opcode(&buf, &mut ip));
        result.push_str(&s);
    }

    Ok(result)
}

pub fn parse_opcode(buf: &Vec<u8>, ip: &mut u16) -> Result<String, String> {
    let opcode = match get_opcode(buf[*ip as usize]) {
        Some(op) => op,
        None => {
            return Err(format!("Invalid opcode {:02X} (IP = {:04X})!",
                               buf[*ip as usize],
                               *ip as usize))
        }
    };

    let addr = *ip;
    *ip += 1;

    let s = match opcode.mode {
        Mode::Implied | Mode::Accumulator => "".to_string(),
        Mode::Immediate => {
            *ip += 1;
            format!(" #${:02X}", buf[*ip as usize - 1])
        }
        Mode::Absolute => {
            *ip += 2;
            format!(" ${:02X}{:02X}",
                    buf[*ip as usize - 1],
                    buf[*ip as usize - 2])
        }
        Mode::AbsoluteX => {
            *ip += 2;
            format!(" ${:02X}{:02X}, X",
                    buf[*ip as usize - 1],
                    buf[*ip as usize - 2])
        }
        Mode::AbsoluteY => {
            *ip += 2;
            format!(" ${:02X}{:02X}, Y",
                    buf[*ip as usize - 1],
                    buf[*ip as usize - 2])
        }
        Mode::Indirect => {
            *ip += 2;
            format!(" (${:02X}{:02X})",
                    buf[*ip as usize - 1],
                    buf[*ip as usize - 2])
        }
        Mode::IndirectX => {
            *ip += 1;
            format!(" (${:02X}, X)", buf[*ip as usize - 1])
        }
        Mode::IndirectY => {
            *ip += 1;
            format!(" (${:02X}), Y", buf[*ip as usize - 1])
        }
        Mode::ZeroPage => {
            *ip += 1;
            format!(" ${:02X}", buf[*ip as usize - 1])
        }
        Mode::ZeroPageX => {
            *ip += 1;
            format!(" ${:02X}, X", buf[*ip as usize - 1])
        }
        Mode::ZeroPageY => {
            *ip += 1;
            format!(" ${:02X}, Y", buf[*ip as usize - 1])
        }
        Mode::Relative => {
            *ip += 1;
            format!(" ${:02X}", buf[*ip as usize - 1])
        }
    };

    let result = format!("${:04X} {}{}", addr, opcode.name, s);
    Ok(result)
}

#[cfg(test)]
mod tests {
    use dasm::*;

    #[test]
    fn can_dissasemble_single_instruction() {
        let mut ip: u16 = 0;
        let b: Vec<u8> = vec![0xA9, 0x01];
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0000 LDA #$01");
    }

    #[test]
    fn can_dissasemble_all_addressing_modes() {
        let mut ip: u16 = 0;
        let b: Vec<u8> = vec![0xA9, 0x10, 0xA5, 0x10, 0xB5, 0x10, 0xAD, 0x00, 0x10, 0xBD, 0x00,
                              0x10, 0xB9, 0x00, 0x10, 0xA1, 0x10, 0xB1, 0x10, 0xB6, 0x10, 0x4A,
                              0xD0, 0x10, 0xCA, 0x6C, 0x00, 0x10];

        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0000 LDA #$10");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0002 LDA $10");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0004 LDA $10, X");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0006 LDA $1000");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0009 LDA $1000, X");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$000C LDA $1000, Y");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$000F LDA ($10, X)");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0011 LDA ($10), Y");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0013 LDX $10, Y");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0015 LSR");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0016 BNE $10");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0018 DEX");
        assert_eq!(disassemble_step(&b, &mut ip).unwrap(), "$0019 JMP ($1000)");
    }
}
