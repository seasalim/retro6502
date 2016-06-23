use opcodes::*;
use std::fmt;
use utils::*;

pub struct CPU {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    st: u8,
    sp: u8,
    mem: Vec<u8>,
    brk: bool,
    brkpt: u16,
    io_hook: bool,
    io_r_addr: u16,
    io_r_fn: fn() -> u8,
    io_w_addr: u16,
    io_w_fn: fn(u8),
}

pub enum Register {
    A,
    X,
    Y,
    SP,
}

pub enum Flags {
    Carry = 0b00000001,
    Zero = 0b00000010,
    Interrupt = 0b00000100,
    Decimal = 0b00001000,
    Break = 0b00010000,
    Overflow = 0b01000000,
    Sign = 0b10000000,
}

pub enum Operand {
    Accumulator,
    Value(u8),
    Address(u16),
}

fn nop_r() -> u8 {
    0
}
fn nop_w(_: u8) {}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            pc: 0,
            a: 0,
            x: 0,
            y: 0,
            st: 0x20,
            sp: 0xFF,
            mem: vec![0; 64 * 1024],
            brk: false,
            brkpt: 0xFFFF,
            io_hook: false,
            io_r_addr: 0,
            io_r_fn: nop_r,
            io_w_addr: 0,
            io_w_fn: nop_w,
        }
    }

    pub fn load(&mut self, buf: &Vec<u8>, addr: u16) {
        for ix in 0..buf.len() {
            self.mem[addr as usize + ix] = buf[ix];
        }
    }

    pub fn memdump(&self, from: usize, bytes: usize) {
        hexdump(&self.mem, from, bytes)
    }

    pub fn get_pc(&mut self) -> u16 {
        self.pc
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.pc = pc;
    }

    pub fn get_flag(&self, f: Flags) -> bool {
        (self.st & f as u8) > 0
    }

    pub fn set_flag(&mut self, f: Flags, v: bool) {
        match v {
            true => self.st |= f as u8,
            false => self.st &= !(f as u8),
        }
    }

    pub fn set_breakpoint(&mut self, addr: u16) {
        self.brkpt = addr;
    }

    pub fn hook_io(&mut self, r_addr: u16, read_fn: fn() -> u8, w_addr: u16, write_fn: fn(u8)) {
        self.io_hook = true;
        self.io_r_addr = r_addr;
        self.io_r_fn = read_fn;
        self.io_w_addr = w_addr;
        self.io_w_fn = write_fn
    }

    pub fn get_mem(&self) -> &Vec<u8> {
        &self.mem
    }

    pub fn get_register(&self, r: Register) -> u8 {
        match r {
            Register::A => self.a,
            Register::X => self.x,
            Register::Y => self.y,
            Register::SP => self.sp,
        }
    }

    pub fn single_step(&mut self) -> Result<bool, String> {
        let opcode = match get_opcode(self.mem[self.pc as usize]) {
            Some(op) => op,
            None => {
                return Err(format!("Invalid opcode {:02X} (IP = {:04X})!",
                                   self.mem[self.pc as usize],
                                   self.pc))
            }
        };

        let opr: Operand = self.fetch_operand(opcode);
        let val: u8 = self.operand_source(&opr);
        let addr: u16 = self.operand_target(&opr);

        // println!("{:04X} {}", self.pc, opcode.name);

        self.pc += opcode.bytes as u16;

        match opcode.name {
            "ADC" => self.do_adc(val),
            "AND" => self.do_and(val),
            "ASL" => self.do_asl(opr),
            "BCC" => self.do_bcc(val),
            "BCS" => self.do_bcs(val),
            "BEQ" => self.do_beq(val),
            "BIT" => self.do_bit(val),
            "BRK" => self.do_brk(),
            "BMI" => self.do_bmi(val),
            "BNE" => self.do_bne(val),
            "BPL" => self.do_bpl(val),
            "BVC" => self.do_bvc(val),
            "BVS" => self.do_bvs(val),
            "CLC" => self.do_clc(),
            "CLD" => self.do_cld(),
            "CLI" => self.do_cli(),
            "CLV" => self.do_clv(),
            "CMP" => self.do_cmp(val),
            "CPX" => self.do_cpx(val),
            "CPY" => self.do_cpy(val),
            "DEC" => self.do_dec(addr),
            "DEX" => self.do_dex(),
            "DEY" => self.do_dey(),
            "EOR" => self.do_eor(val),
            "INC" => self.do_inc(addr),
            "INX" => self.do_inx(),
            "INY" => self.do_iny(),
            "JMP" => self.do_jmp(addr),
            "JSR" => self.do_jsr(addr),
            "LDA" => self.do_lda(val),
            "LDX" => self.do_ldx(val),
            "LDY" => self.do_ldy(val),
            "LSR" => self.do_lsr(opr),
            "NOP" => self.do_nop(),
            "ORA" => self.do_ora(val),
            "PHA" => self.do_pha(),
            "PHP" => self.do_php(),
            "PLA" => self.do_pla(),
            "PLP" => self.do_plp(),
            "ROL" => self.do_rol(opr),
            "ROR" => self.do_ror(opr),
            "RTI" => self.do_rti(),
            "RTS" => self.do_rts(),
            "SBC" => self.do_sbc(val),
            "SEC" => self.do_sec(),
            "SED" => self.do_sed(),
            "SEI" => self.do_sei(),
            "STA" => self.do_sta(addr),
            "STX" => self.do_stx(addr),
            "STY" => self.do_sty(addr),
            "TAX" => self.do_tax(),
            "TAY" => self.do_tay(),
            "TSX" => self.do_tsx(),
            "TXA" => self.do_txa(),
            "TYA" => self.do_tya(),
            "TXS" => self.do_txs(),
            _ => return Err(format!("Unhandled opcode {:02X} {}", opcode.code, opcode.name)),
        };

        let dobreak = self.brk;
        self.brk = false;

        Ok(dobreak || (self.brkpt == self.pc))
    }

    fn fetch_operand(&self, opcode: &Opcode) -> Operand {
        match opcode.mode {
            Mode::Immediate => self.opr_immediate(),
            Mode::Absolute => self.opr_absolute(),
            Mode::AbsoluteX => self.opr_absolute_x(),
            Mode::AbsoluteY => self.opr_absolute_y(),
            Mode::ZeroPage => self.opr_zeropage(),
            Mode::ZeroPageX => self.opr_zeropage_x(),
            Mode::ZeroPageY => self.opr_zeropage_y(),
            Mode::Indirect => self.opr_indirect(),
            Mode::IndirectX => self.opr_indirect_x(),
            Mode::IndirectY => self.opr_indirect_y(),
            Mode::Relative => self.opr_relative(),
            Mode::Accumulator => self.opr_accumulator(),
            Mode::Implied => Operand::Value(0),
        }
    }

    fn opr_accumulator(&self) -> Operand {
        Operand::Accumulator
    }

    fn opr_immediate(&self) -> Operand {
        Operand::Value(self.read_byte(self.pc + 1))
    }

    fn opr_absolute(&self) -> Operand {
        Operand::Address(self.read_word(self.pc + 1))
    }

    fn opr_absolute_x(&self) -> Operand {
        Operand::Address(self.x as u16 + self.read_word(self.pc + 1))
    }

    fn opr_absolute_y(&self) -> Operand {
        Operand::Address(self.y as u16 + self.read_word(self.pc + 1))
    }

    fn opr_zeropage(&self) -> Operand {
        Operand::Address((self.read_byte(self.pc + 1) as u16) & 0xFF)
    }

    fn opr_zeropage_x(&self) -> Operand {
        Operand::Address((self.x as u16 + self.read_byte(self.pc + 1) as u16) & 0xFF)
    }

    fn opr_zeropage_y(&self) -> Operand {
        Operand::Address((self.y as u16 + self.read_byte(self.pc + 1) as u16) & 0xFF)
    }

    fn opr_indirect(&self) -> Operand {
        Operand::Address(self.read_word(self.read_word(self.pc + 1) as u16))
    }

    fn opr_indirect_x(&self) -> Operand {
        Operand::Address(self.read_word((self.x as u16 + self.read_byte(self.pc + 1) as u16) & 0xFF))
    }

    fn opr_indirect_y(&self) -> Operand {
        Operand::Address(self.y as u16 + self.read_word(self.read_byte(self.pc + 1) as u16))
    }

    fn opr_relative(&self) -> Operand {
        Operand::Value(self.read_byte(self.pc + 1))
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        if addr == self.io_r_addr {
            (self.io_r_fn)()
        } else {
            self.mem[addr as usize]
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        if addr == self.io_w_addr {
            (self.io_w_fn)(val);
        }
        self.mem[addr as usize] = val
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        let lobyte = self.mem[addr as usize] as u16;
        let hibyte = self.mem[addr as usize + 1] as u16;
        (hibyte << 8) | lobyte
    }

    fn push_byte(&mut self, val: u8) {
        self.mem[0x100 + (self.sp as usize)] = val;
        self.sp = (((self.sp as i16) - 1) & 0xFF) as u8;
    }

    fn pop_byte(&mut self) -> u8 {
        self.sp = (((self.sp as u16) + 1) & 0xFF) as u8;
        self.mem[0x100 + (self.sp as usize)]
    }

    fn push_word(&mut self, addr: u16) {
        let lobyte = addr & 0xFF;
        let hibyte = (addr & 0xFF00) >> 8;
        self.push_byte(hibyte as u8);
        self.push_byte(lobyte as u8)
    }

    fn pop_word(&mut self) -> u16 {
        let lobyte = self.pop_byte();
        let hibyte = self.pop_byte();
        ((hibyte as u16) << 8) | (lobyte as u16)
    }

    fn operand_source(&self, operand: &Operand) -> u8 {
        match *operand {
            Operand::Accumulator => self.a,
            Operand::Value(v) => v,
            Operand::Address(a) => self.read_byte(a),
        }
    }

    fn operand_target(&self, operand: &Operand) -> u16 {
        match *operand {
            Operand::Accumulator => self.a as u16,
            Operand::Value(v) => v as u16,
            Operand::Address(a) => a,
        }
    }

    fn set_zero_sign_for(&mut self, val: u8) {
        self.set_flag(Flags::Zero, val == 0);
        self.set_flag(Flags::Sign, val > 127)
    }

    fn jmp_relative(&mut self, offset: u8) {
        if offset > 127 {
            self.pc = self.pc - (256 - offset as u16);
        } else {
            self.pc = self.pc + offset as u16;
        }
    }

    fn do_adc(&mut self, val: u8) {
        let carry: u8 = if self.get_flag(Flags::Carry) {
            1
        } else {
            0
        };

        let mut result = self.a as u16 + val as u16 + carry as u16;
        let &acc = &self.a;

        if self.get_flag(Flags::Decimal) {
            self.set_flag(Flags::Zero, (result as u8) == 0);
            if ((acc & 0xF) + (val & 0xF) + carry) > 9 {
                result += 6;
            };
            self.set_flag(Flags::Sign, (result as u8) > 127);
            self.set_flag(Flags::Overflow,
                          (((acc ^ val) & 0x80) == 0) && (((acc ^ (result as u8)) & 0x80) > 0));

            if (result as u16) > 0x99 {
                result += 96;
            };
            self.set_flag(Flags::Carry, (result as u16) > 0x99)
        } else {
            self.set_zero_sign_for(result as u8);
            self.set_flag(Flags::Carry, result > 0xFF);

            self.set_flag(Flags::Overflow,
                          (((acc ^ val) & 0x80) == 0) && (((acc ^ (result as u8)) & 0x80) > 0))
        }

        self.a = result as u8
    }

    fn do_and(&mut self, val: u8) {
        let result = self.a & val;
        self.set_zero_sign_for(result);
        self.a = result;
    }

    fn do_asl(&mut self, opr: Operand) {
        let mut val = self.operand_source(&opr);
        self.set_flag(Flags::Carry, (val & 0x80) > 0);
        val = val << 1;
        self.set_zero_sign_for(val);

        match opr {
            Operand::Accumulator => self.a = val,
            _ => {
                let addr = self.operand_target(&opr);
                self.write_byte(addr, val)
            }
        }
    }

    fn do_bcc(&mut self, offset: u8) {
        if !self.get_flag(Flags::Carry) {
            self.jmp_relative(offset)
        }
    }

    fn do_bcs(&mut self, offset: u8) {
        if self.get_flag(Flags::Carry) {
            self.jmp_relative(offset)
        }
    }

    fn do_beq(&mut self, offset: u8) {
        if self.get_flag(Flags::Zero) {
            self.jmp_relative(offset)
        }
    }

    fn do_bit(&mut self, val: u8) {
        self.set_flag(Flags::Sign, val > 127);
        self.set_flag(Flags::Overflow, (val & 0x40) > 0);

        let result = self.a & val;
        self.set_flag(Flags::Zero, result == 0);
    }

    fn do_bmi(&mut self, offset: u8) {
        if self.get_flag(Flags::Sign) {
            self.jmp_relative(offset)
        }
    }

    fn do_bne(&mut self, offset: u8) {
        if !self.get_flag(Flags::Zero) {
            self.jmp_relative(offset)
        }
    }

    fn do_bpl(&mut self, offset: u8) {
        if !self.get_flag(Flags::Sign) {
            self.jmp_relative(offset)
        }
    }

    fn do_bvc(&mut self, offset: u8) {
        if !self.get_flag(Flags::Overflow) {
            self.jmp_relative(offset)
        }
    }

    fn do_bvs(&mut self, offset: u8) {
        if self.get_flag(Flags::Overflow) {
            self.jmp_relative(offset)
        }
    }

    fn do_brk(&mut self) {
        // self.brk = true;
        self.pc = self.pc + 1;
        let retaddr = self.pc;
        self.push_word(retaddr);
        self.set_flag(Flags::Break, true);
        let status = self.st;
        self.push_byte(status);
        self.set_flag(Flags::Interrupt, true);
        self.pc = self.read_word(0xFFFE)
    }

    fn do_clc(&mut self) {
        self.set_flag(Flags::Carry, false)
    }

    fn do_cld(&mut self) {
        self.set_flag(Flags::Decimal, false)
    }

    fn do_cli(&mut self) {
        self.set_flag(Flags::Interrupt, false)
    }

    fn do_clv(&mut self) {
        self.set_flag(Flags::Overflow, false)
    }

    fn do_cmp(&mut self, val: u8) {
        let result: i16 = self.a as i16 - val as i16;
        self.set_flag(Flags::Carry, (result as u16) < 0x100);
        self.set_zero_sign_for(result as u8)
    }

    fn do_cpx(&mut self, val: u8) {
        let result: i16 = self.x as i16 - val as i16;
        self.set_flag(Flags::Carry, (result as u16) < 0x100);
        self.set_zero_sign_for(result as u8);
    }

    fn do_cpy(&mut self, val: u8) {
        let result: i16 = self.y as i16 - val as i16;
        self.set_flag(Flags::Carry, (result as u16) < 0x100);
        self.set_zero_sign_for(result as u8);
    }

    fn do_dec(&mut self, addr: u16) {
        let val = (self.read_byte(addr) as i16 - 1) as u8;
        self.set_zero_sign_for(val);
        self.write_byte(addr, val)
    }

    fn do_dex(&mut self) {
        let result: i16 = self.x as i16 - 1;
        self.x = result as u8;
        self.set_zero_sign_for(result as u8)
    }

    fn do_dey(&mut self) {
        let result: i16 = self.y as i16 - 1;
        self.y = result as u8;
        self.set_zero_sign_for(result as u8)
    }

    fn do_eor(&mut self, val: u8) {
        let result = self.a ^ val;
        self.set_zero_sign_for(result);
        self.a = result;
    }

    fn do_inc(&mut self, addr: u16) {
        let val = (self.read_byte(addr) as u16 + 1) as u8;
        self.set_zero_sign_for(val);
        self.write_byte(addr, val)
    }

    fn do_inx(&mut self) {
        let val = ((self.x as u16) + 1) & 0xFF;
        self.set_zero_sign_for(val as u8);
        self.x = val as u8;
    }

    fn do_iny(&mut self) {
        let val = ((self.y as u16) + 1) & 0xFF;
        self.set_zero_sign_for(val as u8);
        self.y = val as u8
    }

    fn do_jmp(&mut self, addr: u16) {
        self.pc = addr;
    }

    fn do_jsr(&mut self, addr: u16) {
        let val = self.pc - 1;
        self.push_word(val);
        self.pc = addr
    }

    fn do_lda(&mut self, val: u8) {
        self.a = val;
        self.set_zero_sign_for(val)
    }

    fn do_ldx(&mut self, val: u8) {
        self.x = val;
        self.set_zero_sign_for(val)
    }

    fn do_ldy(&mut self, val: u8) {
        self.y = val;
        self.set_zero_sign_for(val)
    }

    fn do_lsr(&mut self, opr: Operand) {
        let mut val = self.operand_source(&opr);
        self.set_flag(Flags::Carry, (val & 0x01) > 0);
        val = val >> 1;
        self.set_zero_sign_for(val);

        match opr {
            Operand::Accumulator => self.a = val,
            _ => {
                let addr = self.operand_target(&opr);
                self.write_byte(addr, val)
            }
        }
    }

    fn do_ora(&mut self, val: u8) {
        let result = self.a | val;
        self.set_zero_sign_for(result);
        self.a = result;
    }

    fn do_rol(&mut self, opr: Operand) {
        let mut val = self.operand_source(&opr) as u16;
        val = val << 1;
        if self.get_flag(Flags::Carry) {
            val = val | 0x01;
        }
        self.set_flag(Flags::Carry, val > 0xFF);
        self.set_zero_sign_for((val & 0xFF) as u8);

        match opr {
            Operand::Accumulator => self.a = val as u8,
            _ => {
                let addr = self.operand_target(&opr);
                self.write_byte(addr, val as u8)
            }
        }
    }

    fn do_ror(&mut self, opr: Operand) {
        let mut val = self.operand_source(&opr) as u16;
        if self.get_flag(Flags::Carry) {
            val = val | 0x100;
        }
        self.set_flag(Flags::Carry, (val & 0x01) > 0);
        val = val >> 1;
        self.set_zero_sign_for((val & 0xFF) as u8);

        match opr {
            Operand::Accumulator => self.a = val as u8,
            _ => {
                let addr = self.operand_target(&opr);
                self.write_byte(addr, val as u8)
            }
        }
    }

    fn do_nop(&mut self) {}

    fn do_pha(&mut self) {
        let val = self.a;
        self.push_byte(val)
    }

    fn do_pla(&mut self) {
        let val = self.pop_byte();
        self.a = val;
        self.set_zero_sign_for(val)
    }

    fn do_plp(&mut self) {
        self.st = self.pop_byte() | 0x20;
    }

    fn do_php(&mut self) {
        let val = self.st | 0x30;
        self.push_byte(val)
    }

    fn do_rti(&mut self) {
        self.st = self.pop_byte() | 0x20;
        self.pc = self.pop_word();
    }

    fn do_rts(&mut self) {
        let addr = self.pop_word() + 1;
        self.pc = addr
    }

    fn do_sbc(&mut self, val: u8) {
        let carry: u8 = if self.get_flag(Flags::Carry) {
            0
        } else {
            1
        };

        let mut result = self.a as i16 - val as i16 - carry as i16;
        self.set_zero_sign_for(result as u8);

        let &acc = &self.a;
        self.set_flag(Flags::Overflow,
                      (((acc ^ (result as u8)) & 0x80) > 0) && (((acc ^ val) & 0x80) > 0));

        if self.get_flag(Flags::Decimal) {
            if (((acc as i16) & 0xF) - carry as i16) < ((val as i16) & 0xF) {
                result -= 6;
            }
            if (result as u16) > 0x99 {
                result -= 0x60;
            }
        }

        self.set_flag(Flags::Carry, (result as u16) < 0x100);
        self.a = result as u8
    }

    fn do_sec(&mut self) {
        self.set_flag(Flags::Carry, true)
    }

    fn do_sed(&mut self) {
        self.set_flag(Flags::Decimal, true)
    }

    fn do_sei(&mut self) {
        self.set_flag(Flags::Interrupt, true)
    }

    fn do_sta(&mut self, addr: u16) {
        let val = self.a;
        self.write_byte(addr, val)
    }

    fn do_stx(&mut self, addr: u16) {
        let val = self.x;
        self.write_byte(addr, val)
    }

    fn do_sty(&mut self, addr: u16) {
        let val = self.y;
        self.write_byte(addr, val)
    }

    fn do_tax(&mut self) {
        let val = self.a;
        self.x = self.a;
        self.set_zero_sign_for(val)
    }

    fn do_tay(&mut self) {
        let val = self.a;
        self.y = self.a;
        self.set_zero_sign_for(val)
    }

    fn do_tsx(&mut self) {
        let val = self.sp;
        self.x = self.sp;
        self.set_zero_sign_for(val)
    }

    fn do_txa(&mut self) {
        let val = self.x;
        self.a = self.x;
        self.set_zero_sign_for(val)
    }

    fn do_tya(&mut self) {
        let val = self.y;
        self.a = self.y;
        self.set_zero_sign_for(val)
    }

    fn do_txs(&mut self) {
        self.sp = self.x
    }

    fn pp_flags(&self, flags: &u8) -> String {
        let mut v = vec!['S', 'V', '-', 'B', 'D', 'I', 'Z', 'C'];
        for ix in 0..8 {
            if (flags & (1 << ix)) == 0 {
                v[7 - ix] = '.';
            }
        }
        v.into_iter().collect()
    }
}

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "PC:${:04X} A:${:02X} X:${:02X} Y:${:02X} SP:${:02X} ST:{:08b} [{}]",
               self.pc,
               self.a,
               self.x,
               self.y,
               self.sp,
               self.st,
               self.pp_flags(&self.st))
    }
}

#[cfg(test)]
mod tests {
    use cpu::*;

    #[test]
    fn can_load_program_to_location() {
        let code: Vec<u8> = vec![0xa9, 0x01];
        let mut cpu = CPU::new();
        cpu.load(&code, 0x100);
        assert_eq!(cpu.mem[0x100], 0xa9);
        assert_eq!(cpu.mem[0x101], 0x01);
    }

    #[test]
    #[should_panic]
    fn fails_load_program_to_bad_location() {
        let code: Vec<u8> = vec![0xa9, 0x01];
        let mut cpu = CPU::new();
        cpu.load(&code, 0xFFFF);
    }

    #[test]
    fn sbc_basic_test() {
        let code: Vec<u8> = vec![0xA9, 0x08, 0xE9, 0x02];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        match cpu.single_step() {  // A9 08 LDA #$08
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0x08),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // E9 02 SBC #$02
            Ok(_) => {
                assert_eq!(cpu.get_register(Register::A), 0x05);
                assert_eq!(cpu.get_flag(Flags::Zero), false);
                assert_eq!(cpu.get_flag(Flags::Carry), true);
                assert_eq!(cpu.get_flag(Flags::Overflow), false);
            }
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn sbc_decimal_mode_basic_test() {
        let code: Vec<u8> = vec![0xF8, 0x38, 0xA9, 0x00, 0xE9, 0x01, 0xA9, 0x00, 0xE9, 0x01];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        match cpu.single_step() {  // F8 SED
            Ok(_) => assert_eq!(cpu.get_flag(Flags::Decimal), true),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // 38 SEC
            Ok(_) => assert_eq!(cpu.get_flag(Flags::Carry), true),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A9 08 LDA #$00
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0x00),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // E9 02 SBC #$01
            Ok(_) => {
                assert_eq!(cpu.get_register(Register::A), 0x99);
                assert_eq!(cpu.get_flag(Flags::Zero), false);
                assert_eq!(cpu.get_flag(Flags::Carry), false);
                assert_eq!(cpu.get_flag(Flags::Overflow), false);
            }
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A9 08 LDA #$00
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0x00),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // E9 02 SBC #$01
            Ok(_) => {
                assert_eq!(cpu.get_register(Register::A), 0x98);
                assert_eq!(cpu.get_flag(Flags::Zero), false);
                assert_eq!(cpu.get_flag(Flags::Carry), false);
                assert_eq!(cpu.get_flag(Flags::Overflow), false);
            }
            Err(_) => assert!(false),
        }
    }


    #[test]
    fn bne_works_correctly() {
        let code: Vec<u8> = vec![0xA2, 0x02, 0xCA, 0xD0, 0xFD, 0x00];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        match cpu.single_step() {  // A2 02 LDX #$02
            Ok(_) => assert_eq!(cpu.get_register(Register::X), 0x02),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // CA DEX
            Ok(_) => assert_eq!(cpu.get_flag(Flags::Zero), false),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // D0 FC BNE $0002
            Ok(_) => {
                assert_eq!(cpu.pc, 0x0002);
                assert_eq!(cpu.get_flag(Flags::Zero), false);
            }
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // CA DEX
            Ok(_) => {
                assert_eq!(cpu.get_register(Register::X), 0x00);
                assert_eq!(cpu.get_flag(Flags::Zero), true);
            }
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // D0 FC BNE $0002
            Ok(_) => {
                assert_eq!(cpu.pc, 0x0005);
                assert_eq!(cpu.get_flag(Flags::Zero), true);
            }
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn dex_works_correctly() {
        let code: Vec<u8> = vec![0xA2, 0x81, 0xCA, 0xA2, 0x01, 0xCA, 0xCA];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        match cpu.single_step() {  // LDX #$81
            Ok(_) => assert_eq!(cpu.get_register(Register::X), 0x81),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // DEX
            Ok(_) => {
                assert_eq!(cpu.get_register(Register::X), 0x80);
                assert_eq!(cpu.get_flag(Flags::Sign), true);
                assert_eq!(cpu.get_flag(Flags::Zero), false);
            }
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // LDX #$01
            Ok(_) => assert_eq!(cpu.get_register(Register::X), 0x01),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // DEX
            Ok(_) => {
                assert_eq!(cpu.get_register(Register::X), 0x00);
                assert_eq!(cpu.get_flag(Flags::Sign), false);
                assert_eq!(cpu.get_flag(Flags::Zero), true);
            }
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // DEX
            Ok(_) => {
                assert_eq!(cpu.get_register(Register::X), 0xFF);
                assert_eq!(cpu.get_flag(Flags::Sign), true);
                assert_eq!(cpu.get_flag(Flags::Zero), false);
            }
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn jmp_works_correctly() {
        // LDX #$01
        let code: Vec<u8> = vec![0x4C, 0xA7, 0x04];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        match cpu.single_step() {  // 4C A7 04 JMP $04A7
            Ok(_) => assert_eq!(cpu.get_pc(), 0x04A7),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn jsr_rts_works_correctly() {
        // LDX #$01
        let code: Vec<u8> = vec![0x20, 0xD2, 0xFF];
        let code2: Vec<u8> = vec![0x60];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        cpu.load(&code2, 0xFFD2);
        match cpu.single_step() {  // 20 00 10 JSR ($1000)
            Ok(_) => {
                assert_eq!(cpu.get_pc(), 0xFFD2);
                assert_eq!(cpu.get_register(Register::SP), 0xFD);
                assert_eq!(cpu.read_byte(0x01FF), 0x00);
                assert_eq!(cpu.read_byte(0x01FE), 0x02)
            }
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // 60 RTS
            Ok(_) => {
                assert_eq!(cpu.get_pc(), 0x0003);
                assert_eq!(cpu.get_register(Register::SP), 0xFF);
            }
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn lda_addressing_modes_work_correctly() {
        let code: Vec<u8> = vec![0xa9, 0x01, 0xAD, 0x00, 0x10, 0xA5, 0xF0, 0xA2, 0x02, 0xB5, 0xF0,
                                 0xBD, 0x00, 0x10, 0xA0, 0x02, 0xB9, 0x00, 0x10, 0xA2, 0x04, 0xA1,
                                 0xF0, 0xB1, 0xF4];
        let data: Vec<u8> = vec![0xF0, 0xF1, 0xF2, 0xF3, 0x00, 0x10];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        cpu.load(&data, 0xF0);
        cpu.load(&data, 0x1000);
        match cpu.single_step() {  // A9 01 LDA #$01
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0x01),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // AD 00 10 LDA $1000
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0xF0),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A5 F0 LDA $F0
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0xF0),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A2 02 LDX #$02
            Ok(_) => assert_eq!(cpu.get_register(Register::X), 0x02),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // B5 F0 LDA $F0, X
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0xF2),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // BD 00 10 LDA $1000, X
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0xF2),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A0 02 LDY #$02
            Ok(_) => assert_eq!(cpu.get_register(Register::Y), 0x02),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // B9 00 10 LDA $1000, Y
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0xF2),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A2 04 LDX #$04
            Ok(_) => assert_eq!(cpu.get_register(Register::X), 0x04),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A1 F0 LDA ($F0, X)
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0xF0),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // B1 F4 LDA ($F4), Y
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0xF2),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn lda_sets_flags_correctly() {
        let code: Vec<u8> = vec![0xa9, 0x81, 0xa9, 0x00];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        match cpu.single_step() {
            Ok(_) => {
                assert_eq!(cpu.get_flag(Flags::Sign), true);
                assert_eq!(cpu.get_flag(Flags::Zero), false)
            }
            Err(_) => assert!(false),
        }
        match cpu.single_step() {
            Ok(_) => {
                assert_eq!(cpu.get_flag(Flags::Sign), false);
                assert_eq!(cpu.get_flag(Flags::Zero), true)
            }
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn ldx_addressing_modes_work_correctly() {
        // LDX #$01
        let code: Vec<u8> = vec![0xa2, 0x01];
        let data: Vec<u8> = vec![0xF0, 0xF1, 0xF2, 0xF3];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        cpu.load(&data, 0xF0);
        cpu.load(&data, 0x1000);
        match cpu.single_step() {
            Ok(_) => assert_eq!(cpu.get_register(Register::X), 0x01),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn ldy_addressing_modes_work_correctly() {
        // LDY #$01
        let code: Vec<u8> = vec![0xa0, 0x01];
        let data: Vec<u8> = vec![0xF0, 0xF1, 0xF2, 0xF3];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        cpu.load(&data, 0xF0);
        cpu.load(&data, 0x1000);
        match cpu.single_step() {  // LDY #$01
            Ok(_) => assert_eq!(cpu.get_register(Register::Y), 0x01),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn sta_addressing_modes_work_correctly() {
        let code: Vec<u8> = vec![0xA9, 0xD0, 0x8D, 0x00, 0x10, 0x85, 0xF0, 0xA2, 0x02, 0x95, 0xF0,
                                 0x9D, 0x00, 0x10, 0xA0, 0x04, 0x99, 0x00, 0x10, 0xA2, 0x04, 0xA9,
                                 0xD1, 0x81, 0xF0, 0x91, 0xF0];
        let data: Vec<u8> = vec![0xF0, 0xF1, 0xF2, 0xF3, 0x00, 0x10];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        cpu.load(&data, 0xF0);
        cpu.load(&data, 0x1000);
        match cpu.single_step() {  // A9 D0 LDA #$D0
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0xD0),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // 8D 00 10 STA $1000
            Ok(_) => assert_eq!(cpu.read_byte(0x1000), 0xD0),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // 85 F0 STA $F0
            Ok(_) => assert_eq!(cpu.read_byte(0xF0), 0xD0),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A2 02 LDX #$02
            Ok(_) => assert_eq!(cpu.get_register(Register::X), 0x02),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // 95 F0 STA $F0, X
            Ok(_) => assert_eq!(cpu.read_byte(0xF2), 0xD0),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // 9D 00 10 STA $1000, X
            Ok(_) => assert_eq!(cpu.read_byte(0x1002), 0xD0),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A0 02 LDY #$04
            Ok(_) => assert_eq!(cpu.get_register(Register::Y), 0x04),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // 99 00 10 STA $1000, Y
            Ok(_) => assert_eq!(cpu.read_byte(0x1004), 0xD0),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A2 04 LDX #$04
            Ok(_) => assert_eq!(cpu.get_register(Register::X), 0x04),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // A9 D1 LDA #$D1
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0xD1),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // 81 F0 STA ($F0, X)
            Ok(_) => assert_eq!(cpu.read_byte(0x1000), 0xD1),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // 91 F0 STA ($F0), Y
            Ok(_) => assert_eq!(cpu.read_byte(0xF1D4), 0xD1),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn txa_works_correctly() {
        // LDX #$01
        let code: Vec<u8> = vec![0xA2, 0x81, 0x8A];
        let mut cpu = CPU::new();
        cpu.load(&code, 0);
        match cpu.single_step() {  // A2 81 LDX #$81
            Ok(_) => assert_eq!(cpu.get_register(Register::X), 0x81),
            Err(_) => assert!(false),
        }
        match cpu.single_step() {  // 8A TXA
            Ok(_) => assert_eq!(cpu.get_register(Register::A), 0x81),
            Err(_) => assert!(false),
        }
    }
}
