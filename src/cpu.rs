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
    fn get_af(&self) -> u16 {
        let f: u8 = self.f.into();

        (self.a as u16) << 8 | f as u16
    }
    fn set_af(&mut self, value: u16) {
        self.a = ((value & 0xFF00) >> 8) as u8;
        self.f = ((value & 0xFF) as u8).into();
    }
    fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }
    fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0xFF) as u8;
    }
    fn get_de(&self) -> u16 {
        (self.d as u16) << 8 | self.e as u16
    }
    fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0xFF) as u8;
    }
    fn get_hl(&self) -> u16 {
        (self.h as u16) << 8 | self.l as u16
    }
    fn set_hl(&mut self, value: u16) {
        self.h = ((value & 0xFF00) >> 8) as u8;
        self.l = (value & 0xFF) as u8;
    }
}

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

/// Enumeration of the instructions the [Cpu] is capable of executing.
#[derive(Debug)]
pub enum Instruction {
    /// Add with carry - just like ADD except that the value of the carry flag is also added to the number
    ADC(ArithmeticTarget),
    /// Adds specific register's contents to the A register's contents.
    ADD(ArithmeticTarget),
    /// Add to HL - just like ADD except that the target is added to the HL register
    ADDHL,
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
    /// Rotate right) - bit rotate a specific register right by 1 through the carry flag
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

#[derive(Debug)]
pub enum IncDecTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    /*
    AF,
    BC,
    DE,
    HL,
    */
}

#[derive(Debug)]
struct MemoryBus {
    data: [u8; 0xFFFF],
}

impl MemoryBus {
    fn read_byte(&self, address: u16) -> u8 {
        self.data[address as usize]
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
    counter: u16,
    mem_bus: MemoryBus,
}

impl Cpu {
    /// Reads and executes the next instruction based on the current program counter.
    pub fn step(&mut self) {
        todo!()
    }
    /// Executes the specified [Instruction].
    pub fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::ADC(target) => match target {
                ArithmeticTarget::A => self.add_carry_a(self.registers.a),
                ArithmeticTarget::B => self.add_carry_a(self.registers.b),
                ArithmeticTarget::C => self.add_carry_a(self.registers.c),
                ArithmeticTarget::D => self.add_carry_a(self.registers.d),
                ArithmeticTarget::E => self.add_carry_a(self.registers.e),
                ArithmeticTarget::H => self.add_carry_a(self.registers.h),
                ArithmeticTarget::L => self.add_carry_a(self.registers.l),
            },
            Instruction::ADD(target) => match target {
                ArithmeticTarget::A => self.add_a(self.registers.a),
                ArithmeticTarget::B => self.add_a(self.registers.b),
                ArithmeticTarget::C => self.add_a(self.registers.c),
                ArithmeticTarget::D => self.add_a(self.registers.d),
                ArithmeticTarget::E => self.add_a(self.registers.e),
                ArithmeticTarget::H => self.add_a(self.registers.h),
                ArithmeticTarget::L => self.add_a(self.registers.l),
            },
            Instruction::ADDHL => {
                let new_value = self.add(self.registers.c);
                self.registers.set_hl(new_value as u16);
            }
            Instruction::AND(target) => match target {
                ArithmeticTarget::A => self.and_a(self.registers.a),
                ArithmeticTarget::B => self.and_a(self.registers.b),
                ArithmeticTarget::C => self.and_a(self.registers.c),
                ArithmeticTarget::D => self.and_a(self.registers.d),
                ArithmeticTarget::E => self.and_a(self.registers.e),
                ArithmeticTarget::H => self.and_a(self.registers.h),
                ArithmeticTarget::L => self.and_a(self.registers.l),
            },
            Instruction::BIT(target, bit) => match target {
                ArithmeticTarget::A => self.bit(self.registers.a, bit),
                ArithmeticTarget::B => self.bit(self.registers.b, bit),
                ArithmeticTarget::C => self.bit(self.registers.c, bit),
                ArithmeticTarget::D => self.bit(self.registers.d, bit),
                ArithmeticTarget::E => self.bit(self.registers.e, bit),
                ArithmeticTarget::H => self.bit(self.registers.h, bit),
                ArithmeticTarget::L => self.bit(self.registers.l, bit),
            },
            Instruction::CCF => self.ccf(),
            Instruction::CP(target) => match target {
                ArithmeticTarget::A => self.cp(self.registers.a),
                ArithmeticTarget::B => self.cp(self.registers.b),
                ArithmeticTarget::C => self.cp(self.registers.c),
                ArithmeticTarget::D => self.cp(self.registers.d),
                ArithmeticTarget::E => self.cp(self.registers.e),
                ArithmeticTarget::H => self.cp(self.registers.h),
                ArithmeticTarget::L => self.cp(self.registers.l),
            },
            Instruction::CPL => self.cpl(),
            Instruction::DEC(target) => self.dec(target),
            Instruction::INC(target) => self.inc(target),
            Instruction::OR(target) => match target {
                ArithmeticTarget::A => self.or(self.registers.a),
                ArithmeticTarget::B => self.or(self.registers.b),
                ArithmeticTarget::C => self.or(self.registers.c),
                ArithmeticTarget::D => self.or(self.registers.d),
                ArithmeticTarget::E => self.or(self.registers.e),
                ArithmeticTarget::H => self.or(self.registers.h),
                ArithmeticTarget::L => self.or(self.registers.l),
            },
            Instruction::RESET(target, bit) => self.reset(target, bit),
            _ => todo!(),
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
        };

        self.registers.f.zero = value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = (value & 0x0F) == 0x00;
    }
    fn or(&mut self, value: u8) {
        self.registers.a = self.registers.a | value;

        self.registers.f.zero = self.registers.a == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = false;
        self.registers.f.half_carry = false;
    }
    fn reset(&mut self, target: ArithmeticTarget, bit: u8) {
        match target {
            ArithmeticTarget::A => self.registers.a = self.registers.a & !(1 << bit),
            ArithmeticTarget::B => self.registers.b = self.registers.b & !(1 << bit),
            ArithmeticTarget::C => self.registers.c = self.registers.c & !(1 << bit),
            ArithmeticTarget::D => self.registers.d = self.registers.d & !(1 << bit),
            ArithmeticTarget::E => self.registers.e = self.registers.e & !(1 << bit),
            ArithmeticTarget::H => self.registers.h = self.registers.h & !(1 << bit),
            ArithmeticTarget::L => self.registers.l = self.registers.l & !(1 << bit),
        };
    }
}
