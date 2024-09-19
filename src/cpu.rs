/// Represents the registers on the cpu.
#[derive(Clone, Copy, Debug, Default)]
struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: FlagsRegister,
    h: u8,
    l: u8,
}

impl Registers {
    /// Returns the combined value of the a and f registers.
    fn get_af(&self) -> u16 {
        let f: u8 = self.f.into();

        (self.a as u16) << 8 | f as u16
    }
    /// Sets the values of the a and f registers by treating them as one 16 byte value.
    fn set_af(&mut self, value: u16) {
        self.a = ((value & 0xFF00) >> 8) as u8;
        self.f = ((value & 0xFF) as u8).into();
    }
    /// Returns the combined value of the b and c registers.
    fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }
    /// Sets the values of the b and c registers by treating them as one 16 byte value.
    fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0xFF) as u8;
    }
    /// Returns the combined value of the d and e registers.
    fn get_de(&self) -> u16 {
        (self.d as u16) << 8 | self.e as u16
    }
    /// Sets the values of the d and e registers by treating them as one 16 byte value.
    fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0xFF) as u8;
    }
    /// Returns the combined value of the h and l registers.
    fn get_hl(&self) -> u16 {
        (self.h as u16) << 8 | self.l as u16
    }
    /// Sets the values of the h and l registers by treating them as one 16 byte value.
    fn set_hl(&mut self, value: u16) {
        self.h = ((value & 0xFF00) >> 8) as u8;
        self.l = (value & 0xFF) as u8;
    }
}

/// Eases the special handling required for the `f` register.
#[derive(Clone, Copy, Debug, Default)]
struct FlagsRegister {
    zero: bool,
    subtract: bool,
    half_carry: bool,
    carry: bool,
}

/// Byte position of the carry flag in the [u8] representation of a [FlagsRegister].
const CARRY_FLAG_BYTE_POSITION: u8 = 4;

/// Byte position of the half carry flag in the [u8] representation of a [FlagsRegister].
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;

/// Byte position of the subtract flag in the [u8] representation of a [FlagsRegister].
const SUBTRACT_FLAG_BYTE_POSITION: u8 = 6;

/// Byte position of the zero flag in the [u8] representation of a [FlagsRegister].
const ZERO_FLAG_BYTE_POSITION: u8 = 7;

impl From<FlagsRegister> for u8 {
    /// Transforms a [FlagsRegister] into a [u8].
    fn from(flag: FlagsRegister) -> u8 {
        (if flag.zero { 1 } else { 0 }) << ZERO_FLAG_BYTE_POSITION
            | (if flag.subtract { 1 } else { 0 }) << SUBTRACT_FLAG_BYTE_POSITION
            | (if flag.half_carry { 1 } else { 0 }) << HALF_CARRY_FLAG_BYTE_POSITION
            | (if flag.carry { 1 } else { 0 }) << CARRY_FLAG_BYTE_POSITION
    }
}

impl From<u8> for FlagsRegister {
    /// Transforms a [u8] into a [FlagsRegister].
    fn from(byte: u8) -> Self {
        let zero = ((byte >> ZERO_FLAG_BYTE_POSITION) & 0b1) != 0;
        let subtract = ((byte >> SUBTRACT_FLAG_BYTE_POSITION) & 0b1) != 0;
        let half_carry = ((byte >> HALF_CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;
        let carry = ((byte >> CARRY_FLAG_BYTE_POSITION) & 0b1) != 0;

        FlagsRegister {
            zero,
            subtract,
            half_carry,
            carry,
        }
    }
}

/// Enumerates the target registers for arithmetic operations executed by the [Cpu].
#[derive(Debug)]
pub enum ArithmeticTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

/// Enumerates the target registers for increment and decrement operations executed by the [Cpu].
#[derive(Debug)]
pub enum IncDecTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    AF,
    BC,
    DE,
    HL,
}

/// Enumeration of the instructions the [Cpu] is capable of executing.
#[derive(Debug)]
pub enum Instruction {
    /// Add with carry - just like ADD except that the value of the carry flag is also added to the number
    ADC(ArithmeticTarget),
    /// Adds specific register's contents to the A register's contents.
    ADD(ArithmeticTarget),
    /// Add to HL - just like ADD except that the target is added to the HL register
    ADDHL(ArithmeticTarget),
    /// Logical AND - do a bitwise and on the value in a specific register and the value in the A register
    AND(ArithmeticTarget),
    /// Bit test - test to see if a specific bit of a specific register is set
    BIT(ArithmeticTarget, u8),
    /// Complement carry flag - toggle the value of the carry flag
    CCF,
    /// Compare - just like SUB except the result of the subtraction is not stored back into A
    CP(ArithmeticTarget),
    /// Complement - toggle every bit of the A register
    CPL,
    /// Decrement - decrement the value in a specific register by 1
    DEC(IncDecTarget),
    /// Increment - increment the value in a specific register by 1
    INC(IncDecTarget),
    /// Logical OR - do a bitwise or on the value in a specific register and the value in the A register
    OR(ArithmeticTarget),
    /// Bit reset - set a specific bit of a specific register to 0
    RESET(ArithmeticTarget, u8),
    /// Rotate left - bit rotate a specific register left by 1 through the carry flag
    RL(ArithmeticTarget),
    /// Rotate left A register - bit rotate A register left through the carry flag
    RLA,
    /// Rorate left - bit rotate a specific register left by 1 not through the carry flag
    RLC(ArithmeticTarget),
    /// Rotate right - bit rotate a specific register right by 1 through the carry flag
    RR(ArithmeticTarget),
    /// Rotate right A register - bit rotate A register right through the carry flag
    RRA,
    /// Rorate right - bit rotate a specific register right by 1 not through the carry flag
    RRC(ArithmeticTarget),
    /// Rotate right A register - bit rotate A register right not through the carry flag
    RRCA,
    /// Rotate left A register - bit rotate A register left not through the carry flag
    RRLA,
    /// Subtract with carry - just like ADD except that the value of the carry flag is also subtracted from the number
    SBC(ArithmeticTarget),
    /// Set carry flag - set the carry flag to true
    SCF,
    /// Bit set - set a specific bit of a specific register to 1
    SET(ArithmeticTarget, u8),
    /// Shift left arithmetic - arithmetic shift a specific register left by 1
    SLA(ArithmeticTarget),
    /// Shift right arithmetic - arithmetic shift a specific register right by 1
    SRA(ArithmeticTarget),
    /// Shift right logical - bit shift a specific register right by 1
    SRL(ArithmeticTarget),
    /// Subtracts the value stored in a specific register with the value in the A register
    SUB(ArithmeticTarget),
    /// Swap nibbles - switch upper and lower nibble of a specific register
    SWAP(ArithmeticTarget),
    /// Logical XOR - do a bitwise xor on the value in a specific register and the value in the A register
    XOR(ArithmeticTarget),
}

impl Instruction {
    fn from_byte(byte: u8) -> Option<Instruction> {
        match byte {
            0x02 => Some(Instruction::INC(IncDecTarget::BC)),
            0x13 => Some(Instruction::INC(IncDecTarget::DE)),
            // TODO: add mapping for rest of instructions
            _ => None,
        }
    }
}

#[derive(Debug)]
struct MemoryBus {
    data: [u8; 0xFFFF],
}

impl MemoryBus {
    fn read_byte(&self, address: u16) -> u8 {
        self.data[address as usize]
    }
    fn write_byte(&mut self, address: u16, byte: u8) {
        self.data[address as usize] = byte;
    }
}

impl Default for MemoryBus {
    fn default() -> Self {
        Self { data: [0; 0xFFFF] }
    }
}

#[derive(Debug, Default)]
pub struct Cpu {
    registers: Registers,
    prog_counter: u16,
    _stack_ptr: u16,
    mem_bus: MemoryBus,
}

impl Cpu {
    /// Reads and executes the next instruction based on the current program counter.
    pub fn step(&mut self) {
        let instruction_byte = self.mem_bus.read_byte(self.prog_counter);

        let next_pc = if let Some(instruction) = Instruction::from_byte(instruction_byte) {
            self.execute(instruction)
        } else {
            panic!("unkown instruction encountered: 0x{:x}", instruction_byte);
        };

        self.prog_counter = next_pc;
    }
    /// Executes the specified [Instruction].
    pub fn execute(&mut self, instruction: Instruction) -> u16 {
        match instruction {
            Instruction::ADC(target) => self.add_carry_a(self.target_value(target)),
            Instruction::ADD(target) => self.add_a(self.target_value(target)),
            Instruction::ADDHL(target) => self.addhl(self.target_value(target)),
            Instruction::AND(target) => self.and_a(self.target_value(target)),
            Instruction::BIT(target, bit) => self.bit(self.target_value(target), bit),
            Instruction::CCF => self.ccf(),
            Instruction::CP(target) => self.cp(self.target_value(target)),
            Instruction::CPL => self.cpl(),
            Instruction::DEC(target) => self.dec(target),
            Instruction::INC(target) => self.inc(target),
            Instruction::OR(target) => self.or(self.target_value(target)),
            Instruction::RESET(target, bit) => self.reset(target, bit),
            Instruction::RL(target) => self.rl(target),
            Instruction::RLA => self.rl(ArithmeticTarget::A),
            Instruction::RLC(target) => self.rlc(target),
            Instruction::RR(target) => self.rr(target),
            Instruction::RRA => self.rr(ArithmeticTarget::A),
            Instruction::RRC(target) => self.rrc(target),
            Instruction::RRCA => self.rrc(ArithmeticTarget::A),
            Instruction::RRLA => self.rrla(),
            Instruction::SBC(target) => self.sbc(self.target_value(target)),
            Instruction::SCF => self.scf(),
            Instruction::SET(target, bit) => self.set(target, bit),
            Instruction::SLA(target) => self.sla(target),
            Instruction::SRA(target) => self.sra(target),
            Instruction::SRL(target) => self.srl(target),
            Instruction::SUB(target) => self.sub(self.target_value(target)),
            Instruction::SWAP(target) => self.swap(target),
            Instruction::XOR(target) => self.xor(self.target_value(target)),
        }

        self.prog_counter.wrapping_add(1)
    }
    fn target_value(&self, target: ArithmeticTarget) -> u8 {
        match target {
            ArithmeticTarget::A => self.registers.a,
            ArithmeticTarget::B => self.registers.b,
            ArithmeticTarget::C => self.registers.c,
            ArithmeticTarget::D => self.registers.d,
            ArithmeticTarget::E => self.registers.e,
            ArithmeticTarget::H => self.registers.h,
            ArithmeticTarget::L => self.registers.l,
        }
    }
    fn add(&mut self, value: u8) -> u8 {
        let (new_value, overflowed) = self.registers.a.overflowing_add(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = overflowed;
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        new_value
    }
    fn add_a(&mut self, value: u8) {
        self.registers.a = self.add(value);
    }
    fn add_carry(&mut self, value: u8) -> u8 {
        let carry = if self.registers.f.carry { 1 } else { 0 };

        let (new_value, overflowed) = self.registers.a.overflowing_add(value + carry);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = overflowed;
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        new_value
    }
    fn add_carry_a(&mut self, value: u8) {
        self.registers.a = self.add_carry(value);
    }
    fn addhl(&mut self, value: u8) {
        let (new_value, overflowed) = self.registers.get_hl().overflowing_add(value as u16);

        self.registers.set_hl(new_value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = overflowed;
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;
    }
    fn and_a(&mut self, value: u8) {
        let new_value = self.registers.a & value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = true;
    }
    fn bit(&mut self, value: u8, bit: u8) {
        let zero = (value & (1 << bit)) != 0;

        self.registers.f.zero = zero;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = true;
    }
    fn ccf(&mut self) {
        self.registers.f.subtract = false;
        self.registers.f.carry = !self.registers.f.carry;
        self.registers.f.half_carry = false;
    }
    fn cp(&mut self, value: u8) {
        let (new_value, overflowed) = self.registers.a.overflowing_sub(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = overflowed;
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;
    }
    fn cpl(&mut self) {
        self.registers.a = !self.registers.a;

        self.registers.f.subtract = true;
        self.registers.f.half_carry = true;
    }
    fn dec(&mut self, target: IncDecTarget) {
        let value = match target {
            IncDecTarget::A => {
                self.registers.a -= 1;
                self.registers.a
            }
            IncDecTarget::B => {
                self.registers.b -= 1;
                self.registers.b
            }
            IncDecTarget::C => {
                self.registers.c -= 1;
                self.registers.c
            }
            IncDecTarget::D => {
                self.registers.d -= 1;
                self.registers.d
            }
            IncDecTarget::E => {
                self.registers.e -= 1;
                self.registers.e
            }
            IncDecTarget::H => {
                self.registers.h -= 1;
                self.registers.h
            }
            IncDecTarget::L => {
                self.registers.l -= 1;
                self.registers.l
            }
            _ => todo!(),
        };

        self.registers.f.zero = value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry = (value & 0x0F) == 0x0F;
    }
    fn inc(&mut self, target: IncDecTarget) {
        let value = match target {
            IncDecTarget::A => {
                self.registers.a += 1;
                self.registers.a
            }
            IncDecTarget::B => {
                self.registers.b += 1;
                self.registers.b
            }
            IncDecTarget::C => {
                self.registers.c += 1;
                self.registers.c
            }
            IncDecTarget::D => {
                self.registers.d += 1;
                self.registers.d
            }
            IncDecTarget::E => {
                self.registers.e += 1;
                self.registers.e
            }
            IncDecTarget::H => {
                self.registers.h += 1;
                self.registers.h
            }
            IncDecTarget::L => {
                self.registers.l += 1;
                self.registers.l
            }
            _ => todo!(),
        };

        self.registers.f.zero = value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = (value & 0x0F) == 0x00;
    }
    fn or(&mut self, value: u8) {
        self.registers.a |= value;

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = false;
    }
    fn reset(&mut self, target: ArithmeticTarget, bit: u8) {
        match target {
            ArithmeticTarget::A => self.registers.a &= !(1 << bit),
            ArithmeticTarget::B => self.registers.b &= !(1 << bit),
            ArithmeticTarget::C => self.registers.c &= !(1 << bit),
            ArithmeticTarget::D => self.registers.d &= !(1 << bit),
            ArithmeticTarget::E => self.registers.e &= !(1 << bit),
            ArithmeticTarget::H => self.registers.h &= !(1 << bit),
            ArithmeticTarget::L => self.registers.l &= !(1 << bit),
        };
    }
    fn rl(&mut self, target: ArithmeticTarget) {
        let value = match target {
            ArithmeticTarget::A => &mut self.registers.a,
            ArithmeticTarget::B => &mut self.registers.b,
            ArithmeticTarget::C => &mut self.registers.c,
            ArithmeticTarget::D => &mut self.registers.d,
            ArithmeticTarget::E => &mut self.registers.e,
            ArithmeticTarget::H => &mut self.registers.h,
            ArithmeticTarget::L => &mut self.registers.l,
        };

        let carry_value = if self.registers.f.carry { 1 } else { 0 };
        let will_carry = (*value & (1 << 7)) != 0;

        let mut new_value = *value << 1;
        new_value |= carry_value;

        *value = new_value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = will_carry;
        self.registers.f.half_carry = false;
    }
    fn rlc(&mut self, target: ArithmeticTarget) {
        let value = match target {
            ArithmeticTarget::A => &mut self.registers.a,
            ArithmeticTarget::B => &mut self.registers.b,
            ArithmeticTarget::C => &mut self.registers.c,
            ArithmeticTarget::D => &mut self.registers.d,
            ArithmeticTarget::E => &mut self.registers.e,
            ArithmeticTarget::H => &mut self.registers.h,
            ArithmeticTarget::L => &mut self.registers.l,
        };

        let will_carry = (*value & (1 << 7)) != 0;
        let truncated_bit = *value & (1 << 7);
        let new_value = (*value << 1) | truncated_bit;

        *value = new_value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = will_carry;
        self.registers.f.half_carry = false;
    }
    fn rr(&mut self, target: ArithmeticTarget) {
        let value = match target {
            ArithmeticTarget::A => &mut self.registers.a,
            ArithmeticTarget::B => &mut self.registers.b,
            ArithmeticTarget::C => &mut self.registers.c,
            ArithmeticTarget::D => &mut self.registers.d,
            ArithmeticTarget::E => &mut self.registers.e,
            ArithmeticTarget::H => &mut self.registers.h,
            ArithmeticTarget::L => &mut self.registers.l,
        };

        let carry_value = if self.registers.f.carry { 1 } else { 0 };
        let will_carry = (*value & (1 << 0)) != 0;

        let mut new_value = *value >> 1;
        new_value |= carry_value << 7;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = will_carry;
        self.registers.f.half_carry = false;
    }
    fn rrc(&mut self, target: ArithmeticTarget) {
        let value = match target {
            ArithmeticTarget::A => &mut self.registers.a,
            ArithmeticTarget::B => &mut self.registers.b,
            ArithmeticTarget::C => &mut self.registers.c,
            ArithmeticTarget::D => &mut self.registers.d,
            ArithmeticTarget::E => &mut self.registers.e,
            ArithmeticTarget::H => &mut self.registers.h,
            ArithmeticTarget::L => &mut self.registers.l,
        };

        let will_carry = (*value & (1 << 0)) != 0;
        let truncated_bit = *value & (1 << 0);

        let new_value = (*value >> 1) | (truncated_bit << 7);

        *value = new_value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = will_carry;
        self.registers.f.half_carry = false;
    }
    fn rrla(&mut self) {
        let value = self.registers.a;

        let will_carry = (value & (1 << 7)) != 0;
        let truncated_bit = value & (1 << 7);

        let new_value = (value >> 1) | (truncated_bit << 7);

        self.registers.a = new_value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = will_carry;
        self.registers.f.half_carry = false;
    }
    fn sbc(&mut self, value: u8) {
        //u8 carry = f.flag_carry_value();
        //u8 reg = a.value();
        //
        //int result_full = reg - value - carry;
        //u8 result = static_cast<u8>(result_full);
        //
        //set_flag_zero(result == 0);
        //set_flag_subtract(true);
        //set_flag_carry(result_full < 0);
        //set_flag_half_carry(((reg & 0xf) - (value & 0xf) - carry) < 0);
        //
        //a.set(result);
    }
    fn scf(&mut self) {
        self.registers.f.carry = true;
    }
    fn set(&mut self, target: ArithmeticTarget, bit: u8) {
        let value = match target {
            ArithmeticTarget::A => &mut self.registers.a,
            ArithmeticTarget::B => &mut self.registers.b,
            ArithmeticTarget::C => &mut self.registers.c,
            ArithmeticTarget::D => &mut self.registers.d,
            ArithmeticTarget::E => &mut self.registers.e,
            ArithmeticTarget::H => &mut self.registers.h,
            ArithmeticTarget::L => &mut self.registers.l,
        };

        *value |= 1 << bit;
    }
    fn sla(&mut self, target: ArithmeticTarget) {
        let value = match target {
            ArithmeticTarget::A => &mut self.registers.a,
            ArithmeticTarget::B => &mut self.registers.b,
            ArithmeticTarget::C => &mut self.registers.c,
            ArithmeticTarget::D => &mut self.registers.d,
            ArithmeticTarget::E => &mut self.registers.e,
            ArithmeticTarget::H => &mut self.registers.h,
            ArithmeticTarget::L => &mut self.registers.l,
        };

        let will_carry = (*value & (1 << 7)) != 0;
        let new_value = *value << 1;

        *value = new_value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = will_carry;
        self.registers.f.half_carry = false;
    }
    fn sra(&mut self, target: ArithmeticTarget) {
        //u8 carry_bit = check_bit(value, 0);
        //u8 top_bit = check_bit(value, 7);
        //
        //u8 result = static_cast<u8>(value >> 1);
        //result = bitwise::set_bit_to(result, 7, top_bit);
        //
        //set_flag_zero(result == 0);
        //set_flag_carry(carry_bit);
        //set_flag_half_carry(false);
        //set_flag_subtract(false);

        let _value = match target {
            ArithmeticTarget::A => &mut self.registers.a,
            ArithmeticTarget::B => &mut self.registers.b,
            ArithmeticTarget::C => &mut self.registers.c,
            ArithmeticTarget::D => &mut self.registers.d,
            ArithmeticTarget::E => &mut self.registers.e,
            ArithmeticTarget::H => &mut self.registers.h,
            ArithmeticTarget::L => &mut self.registers.l,
        };
    }
    fn srl(&mut self, target: ArithmeticTarget) {
        let value = match target {
            ArithmeticTarget::A => &mut self.registers.a,
            ArithmeticTarget::B => &mut self.registers.b,
            ArithmeticTarget::C => &mut self.registers.c,
            ArithmeticTarget::D => &mut self.registers.d,
            ArithmeticTarget::E => &mut self.registers.e,
            ArithmeticTarget::H => &mut self.registers.h,
            ArithmeticTarget::L => &mut self.registers.l,
        };

        let will_carry = (*value & (1 << 0)) != 0;
        let new_value = *value >> 1;

        *value = new_value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = will_carry;
    }
    fn sub(&mut self, value: u8) {
        let curr_value = self.registers.a;
        let new_value = curr_value.wrapping_sub(value);

        self.registers.a = new_value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry = (curr_value as i16 & 0xF) - (value as i16 & 0xF) < 0;
        self.registers.f.carry = curr_value < value;
    }
    fn swap(&mut self, _target: ArithmeticTarget) {
        //using bitwise::compose_nibbles;
        //
        //u8 lower_nibble = value & 0x0F;
        //u8 upper_nibble = (value & 0xF0) >> 4;
        //
        //u8 result = compose_nibbles(lower_nibble, upper_nibble);
        //
        //set_flag_zero(result == 0);
        //set_flag_subtract(false);
        //set_flag_half_carry(false);
        //set_flag_carry(false);
    }
    fn xor(&mut self, value: u8) {
        let new_value = self.registers.a ^ value;

        self.registers.a = new_value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
    }
}
