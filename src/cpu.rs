use crate::mem::Mapper;

use bounded_vec_deque::BoundedVecDeque;
use std::fmt::Display;

/// Represents the registers on the cpu. Allows for easy manipulation of combined 16-bit registers as well.
#[derive(Clone, Debug, Default)]
pub struct Registers {
    /// A register.
    pub a: u8,
    /// B register.
    pub b: u8,
    /// C register.
    pub c: u8,
    /// D register.
    pub d: u8,
    /// E register.
    pub e: u8,
    /// Special flags register.
    pub f: Flags,
    /// H register.
    pub h: u8,
    /// L register.
    pub l: u8,
    /// Program counter.
    pub pc: u16,
    /// Srack pointer.
    pub sp: u16,
}

impl Registers {
    /// Returns the combined value of the `A` and `F` registers.
    pub fn af(&self) -> u16 {
        let f: u8 = self.f.into();

        ((self.a as u16) << 8) | f as u16
    }
    /// Sets the values of the `A` and `F` registers by treating them as one 16 bit value.
    pub fn set_af(&mut self, value: u16) {
        self.a = ((value & 0xFF00) >> 8) as u8;
        self.f = ((value & 0xFF) as u8).into();
    }
    /// Returns the combined value of the `B` and `C` registers.
    pub fn bc(&self) -> u16 {
        ((self.b as u16) << 8) | self.c as u16
    }
    /// Sets the values of the `B` and `C` registers by treating them as one 16 bit value.
    pub fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0xFF) as u8;
    }
    /// Returns the combined value of the `D` and `E` registers.
    pub fn de(&self) -> u16 {
        ((self.d as u16) << 8) | self.e as u16
    }
    /// Sets the values of the `D` and `E` registers by treating them as one 16 bit value.
    pub fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0xFF) as u8;
    }
    /// Returns the combined value of the `H` and `L` registers.
    pub fn hl(&self) -> u16 {
        ((self.h as u16) << 8) | self.l as u16
    }
    /// Sets the values of the `H` and `L` registers by treating them as one 16 bit value.
    pub fn set_hl(&mut self, value: u16) {
        self.h = ((value & 0xFF00) >> 8) as u8;
        self.l = (value & 0xFF) as u8;
    }
}

/// Bit position of the carry flag in the [`u8`] representation of [`Flags`].
const FLAGS_CARRY_BIT_POSITION: u8 = 4;

/// Bit position of the half carry flag in the [`u8`] representation of [`Flags`].
const FLAGS_HALF_CARRY_BIT_POSITION: u8 = 5;

/// Bit position of the subtract flag in the [`u8`] representation of [`Flags`].
const FLAGS_SUBTRACT_BIT_POSITION: u8 = 6;

/// Bit position of the zero flag in the [`u8`] representation of [`Flags`].
const FLAGS_ZERO_BIT_POSITION: u8 = 7;

/// Eases the special handling required for the `F` register which uses the top 4 bits for the
/// following flags.
///
/// 76543210 <- Bit position
/// --------
/// 00000000
/// ||||
/// |||-- Carry
/// ||--- Half Carry
/// |---- Subtract
/// ----- Zero
#[derive(Clone, Copy, Debug, Default)]
pub struct Flags(u8);

impl Flags {
    /// Retrieves the current status of the carry flag.
    pub fn c(&self) -> bool {
        (self.0 >> FLAGS_CARRY_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the carry flag.
    pub fn set_c(&mut self, on: bool) {
        self.set_flag(FLAGS_CARRY_BIT_POSITION, on);
    }
    /// Retrieves the current status of the half carry flag.
    pub fn h(&self) -> bool {
        (self.0 >> FLAGS_HALF_CARRY_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the half carry flag.
    pub fn set_h(&mut self, on: bool) {
        self.set_flag(FLAGS_HALF_CARRY_BIT_POSITION, on);
    }
    /// Retrieves the current status of the subtract flag.
    pub fn n(&self) -> bool {
        (self.0 >> FLAGS_SUBTRACT_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the subtract flag.
    pub fn set_n(&mut self, on: bool) {
        self.set_flag(FLAGS_SUBTRACT_BIT_POSITION, on);
    }
    /// Retrieves the current status of the zero flag.
    pub fn z(&self) -> bool {
        (self.0 >> FLAGS_ZERO_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the zero flag.
    pub fn set_z(&mut self, on: bool) {
        self.set_flag(FLAGS_ZERO_BIT_POSITION, on);
    }
    /// Sets the status of the flag at the given position.
    pub fn set_flag(&mut self, pos: u8, on: bool) {
        let flag = 1 << pos;
        let is_set = (self.0 & flag) != 0;

        if is_set != on {
            self.0 ^= flag
        }
    }
}

impl From<u8> for Flags {
    /// Converts the given [`u8`] into a [`Flags`].
    fn from(value: u8) -> Self {
        Self(value & 0xF0)
    }
}

impl From<Flags> for u8 {
    /// Converts the given [`Flags`] into a [`u8`].
    fn from(value: Flags) -> Self {
        value.0
    }
}

impl Display for Flags {
    /// Writes a string representation of the [`Flags`] value to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// Enumeration of the flag registers available for the conditional jump relative cpu instruction.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CondJumpTarget {
    /// Zero flag not on.
    NZ,
    /// Zero flag on.
    Z,
    /// Carry flag not on.
    NC,
    /// Carry flag on.
    C,
}

impl Display for CondJumpTarget {
    /// Writes a string representation of the [`CondJumpTarget`] value to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

/// Enumeration of the target registers available to be manipulated by the cpu instructions.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Target {
    /// A register.
    A,
    /// B register.
    B,
    /// C register.
    C,
    /// D register.
    D,
    /// E register.
    E,
    /// H register.
    H,
    /// L register.
    L,
    /// Combined BC 16-bit register.
    BC,
    /// Combined DE 16-bit register.
    DE,
    /// Combined HL 16-bit register.
    HL,
    /// Stack pointer.
    SP,
}

impl Display for Target {
    /// Writes a string representation of the [`Target`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

/// Enumeration of the valid target 8 bit registers for the cpu instructions.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Target8Bit {
    /// A register.
    A,
    /// B register.
    B,
    /// C register.
    C,
    /// D register.
    D,
    /// E register.
    E,
    /// H register.
    H,
    /// L register.
    L,
}

impl Display for Target8Bit {
    /// Writes a string representation of the [`Target8Bit`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

/// Enumeration of the valid target 16-bit registers for the load instructions.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Target16Bit {
    /// Combined BC 16-bit register.
    BC,
    /// Combined DE 16-bit register.
    DE,
    /// Combined HL 16-bit register.
    HL,
    /// Stack pointer.
    SP,
}

impl Display for Target16Bit {
    /// Writes a string representation of the [`Target16Bit`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

/// Enumeration of the valid target registers for the `PUSH` and `POP` instructions.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PushPopTarget {
    /// Combined BC register.
    BC,
    /// Combined DE register.
    DE,
    /// Combined HL register.
    HL,
    /// Combined AF register.
    AF,
}

impl Display for PushPopTarget {
    /// Writes a string representation of the [`PushPopTarget`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

/// Enumeration of the operations the [`Cpu`] is capable of executing.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Operation {
    /// Adds the value in the [`Target8Bit`] register and the value of the carry flag to the value
    /// in the `A` register and stores the result back to the `A` register.
    ADCA { target: Target8Bit },
    /// Adds the value at the memory address pointed to by the value in the `HL` register and the
    /// carry flag to the value in the `A` register and stores the result back to the `A` register.
    ADCAMEM,
    /// Adds the `u8` value and the value of the carry flag to the value in the `A` register and
    /// stores the result back to the `A` register.
    ADCAU8 { value: u8 },
    /// Adds the value in the [`Target8Bit`] register to the value in the `A` register and stores
    /// the result back to the `A` register.
    ADDA { target: Target8Bit },
    /// Adds the value at the memory address pointed to by the value in the `HL` register to the
    /// value in the `A` register and stores the result back to the `A` register.
    ADDAMEM,
    /// Adds the `u8` to the value in the `A` register and stores the result back into the `A`
    /// register.
    ADDAU8 { value: u8 },
    /// Adds the value in the [`Target16Bit`] register to the value in the `HL` register and stores
    /// it back to the `HL` register.
    ADDHL { target: Target16Bit },
    /// Adds the value to the value in the `SP` register and stores it back to the `SP` register.
    ADDSP { value: i8 },
    /// Takes the logical AND for each bit of the contents of the [`Target8Bit`] register and the
    /// contents of the `A` register and stores the result back to the `A` register.
    ANDA { target: Target8Bit },
    /// Takes the logical AND for each bit of the contents of the value in memory pointed at by the
    /// [`HL`] register and the contents of the `A` register and stores the result back to the `A`
    /// register.
    ANDAMEM,
    /// Takes the logical AND for each bit of the value and the contents of the `A` register and
    /// stores the result back to the `A` register.
    ANDAU8 { value: u8 },
    /// Copies the complement of the contents of `bit` in the [`Target8Bit`] register to the zero
    /// flag.
    BIT { bit: u8, target: Target8Bit },
    /// Copies the complement of the contents of `bit` in the `HL` register to the zero flag.
    BITHL { bit: u8 },
    /// Pushes the current program counter on to the stack and then sets it to the value.
    CALL { value: u16 },
    /// Conditionally pushes the current program counter on to the stack and then sets it to the
    /// value based on the state of the [`CondJumpTarget`].
    CALLC {
        target: CondJumpTarget,
        jump: bool,
        value: u16,
    },
    /// Toggles the value of the carry flag.
    CCF,
    /// Compares the contents of the [`Target8Bit`] register and the contents of `A` register by
    /// subtracting the two values and set the Z flag if they are equal. The execution of this
    /// instruction does not affect the contents of the `A` register.
    CPA { target: Target8Bit },
    /// Compares the contents of the value in memory pointed to by the [`HL`] register and the
    /// contents of `A` register by subtracting the two values and set the Z flag if they are equal.
    /// The execution of this instruction does not affect the contents of the `A` register.
    CPAMEM,
    /// Compares the contents of the value and the contents of `A` register by subtracting the two
    /// values and set the Z flag if they are equal. The execution of this instruction does not
    /// affect the contents of the `A` register.
    CPAU8 { value: u8 },
    /// Take the one's complement (i.e., flips all the bits) of the value in the `A` register.
    CPL,
    /// Adjust the `A` register to a binary-coded decimal (BCD) number after BCD addition and
    /// subtraction operations.
    DAA,
    /// Decrements the value in the [`Target`] register by one.
    DEC { target: Target },
    /// Decrements the value at the memory address specified by value of the `HL` register by 1.
    DECMEM,
    /// Resets the interrupt master enable (IME) flag and prohibits maskable interrupts.
    DI,
    /// Sets the interrupt master enable (IME) flag and enables maskable interrupts.
    EI,
    /// Halts the system clock and halt mode is entered.
    HALT,
    /// Increments the value in the [`Target`] register by one.
    INC { target: Target },
    /// Increments the value at the memory address specified by value of the `HL` register by 1.
    INCMEM,
    /// Jumps to a memory address by setting the program counter to the value.
    JP { value: u16 },
    /// Conditionally jumps to a memory address by setting the program counter to the value based
    /// on the state of the [`CondJumpTarget`].
    JPC {
        target: CondJumpTarget,
        jump: bool,
        value: u16,
    },
    /// Jumps to a memory address by setting the program counter to the value.
    JPHL,
    /// Jumps to a relative memory address by advancing the program counter by the offset.
    JR { offset: i8 },
    /// Conditionally jumps to a relative memory address by advancing the program counter by the
    /// offset based on the state of the [`CondJumpTarget`].
    JRC {
        target: CondJumpTarget,
        jump: bool,
        offset: i8,
    },
    /// Loads the value from the `A` register and stores it in the memory address corresponding to
    /// the value in the [`Target16Bit`] register.
    LDA { target: Target16Bit },
    /// Loads the value from the `A` register and stores it in the memory address corresponding to
    /// the value in the `HL` register and then decrements the `HL` register.
    LDADEC,
    /// Loads the value from the `A` register and stores it in the memory address corresponding to
    /// the value in the `HL` register and then increments the `HL` register.
    LDAINC,
    /// Loads the value from memory at the address `A` register and stores it in the memory address
    /// corresponding to the value of the [`Target8Bit`] register and stores it in the `A` register.
    LDAMEM { target: Target16Bit },
    /// Loads the the byte from memory at the address corresponding to the value of the
    /// `HL` register and stores it in the `A` register, then decrements the value in the `HL`
    /// register.
    LDAMEMDEC,
    /// Loads the the byte from memory at the address corresponding to the value of the
    /// `HL` register and stores it in the `A` register, then increments the value in the `HL`
    /// register.
    LDAMEMINC,
    /// Loads the value in memory at the address 0xFF00 + value into the `A` register.
    LDAOFFSET { value: u8 },
    /// Loads the value in memory at the address 0xFF00 + the value of the carry flag into the `A`
    /// register.
    LDAOFFSETC,
    /// Loads the value of the `A` register and stores it in memory at the address.
    LDAU16 { address: u16 },
    /// Loads the value in the [`Target16Bit`] register and stores it at the address in memory.
    LDA16 { address: u16, target: Target16Bit },
    /// Loads the sum of the `SP` register and the value and stores it into the `HL` register.
    LDHLSP { value: i8 },
    /// Loads the value in memory at the address and stores it in the `A` register.
    LDMEMA { address: u16 },
    /// Loads the value in the `target` 8-bit register and stores it in the memory address pointed
    /// to by the value in the `store` 16-bit register.
    LDMEMREG {
        store: Target16Bit,
        target: Target8Bit,
    },
    /// Loads the value of the `A` register and stores it in memory at the address 0xFF00 + value.
    LDOFFSETA { value: u8 },
    /// Loads the value of the `A` register and stores it in memory at the address 0xFF00 + the
    /// value of the carry flag.
    LDOFFSETCA,
    /// Loads the value in the `target` 8-bit register and stores it into the `store` 8-bit
    /// register.
    LDREG {
        store: Target8Bit,
        target: Target8Bit,
    },
    /// Loads the value stored in memory at the address in the `target` 16-bit register and stores
    /// it into the `store` 8-bit register.
    LDREGMEM {
        store: Target8Bit,
        target: Target16Bit,
    },
    /// Loads the value of the `HL` register and stores it into the `SP` register.
    LDSPHL,
    /// Loads the `u16` value from memory and stores it in the [`Target16Bit`] register.
    LDU16 { target: Target16Bit, value: u16 },
    /// Loads the [`u8`] value from memory and stores it in the [`Target8Bit`] register.
    LDU8 { target: Target8Bit, value: u8 },
    /// Loads an immediate u8 and then writes it to the memory address pointed to by the value
    /// in the `HL` register.
    LDU8MEM { value: u8 },
    /// No operation.
    NOP,
    /// Takes the logical OR for each bit of the contents of the [`Target8Bit`] register and the
    /// contents of the `A` register and stores the result back to the `A` register.
    ORA { target: Target8Bit },
    /// Takes the logical OR for each bit of the contents of the value in memory pointed at by the
    /// [`HL`] register and the contents of the `A` register and stores the result back to the `A`
    /// register.
    ORAMEM,
    /// Takes the logical OR for each bit of the value and the contents of the `A` register and
    /// stores the result back to the `A` register.
    ORAU8 { value: u8 },
    /// Pops the contents from the memory stack into the [`PushPopTarget`] register.
    POP { target: PushPopTarget },
    /// Prefix op code which causes the subsequent byte to represent a different set of
    /// instructions.
    PREFIX,
    /// Pushes the contents of the [`PushPopTarget`] register onto the memory stack.
    PUSH { target: PushPopTarget },
    /// Resets the `bit` in the [`Target8Bit`] register to 0.
    RES { bit: u8, target: Target8Bit },
    /// Resets the `bit` in the value in memory pointed to by the `HL` register to 0.
    RESHL { bit: u8 },
    /// Pops from the memory stack the program counter value that was pushed to the
    /// stack when the subroutine was called.
    RET,
    /// Conditionally pops from the memory stack the program counter value that was pushed to the
    /// stack when the subroutine was called based on the state of the [`CondJumpTarget`].
    RETC { target: CondJumpTarget, jump: bool },
    /// Pops from the memory stack the program counter value that was pushed to the
    /// stack when the subroutine was called and resumes allowing of interrupts.
    RETI,
    /// Bit rotate the [`Target8Bit`] register left by one, through the carry flag.
    RL { target: Target8Bit },
    /// Bit rotate the [`Target`] register left by one, through the carry flag. Sets the zero flag
    /// to false.
    RLA,
    /// Bit rotate the value in memory at the address pointed to by the `HL`register left by one,
    /// through the carry flag.
    RLHL,
    /// Bit rotate the [`Target8Bit`] register left by one, not through the carry flag.
    RLC { target: Target8Bit },
    /// Bit rotate the `A` register left by one, not through the carry flag. Sets the zero flag to
    /// false.
    RLCA,
    /// Bit rotate the value in memory at the address pointed to by the value in the `HL` register
    /// left by one, not through the carry flag.
    RLCHL,
    /// Bit rotate the [`Target8Bit`] register right by one, through the carry flag.
    RR { target: Target8Bit },
    /// Bit rotate the value in the `A` register right by one, through the carry flag. Sets the zero
    /// flag to false.
    RRA,
    /// Bit rotate the value in memory at the address pointed to by the value in the `HL` register
    /// right by one, through the carry flag.
    RRHL,
    /// Bit rotate the [`Target8Bit`] register right by one, not through the carry flag.
    RRC { target: Target8Bit },
    /// Bit rotate the [`Target8Bit`] register right by one, not through the carry flag. Sets the
    /// zero flag to false.
    RRCA,
    /// Bit rotate the value in memory at the address pointed to by the value in the `HL` register
    /// right by one, not through the carry flag.
    RRCHL,
    /// Pushes the current value of the program counter PC onto the memory stack, and load into PC
    /// the byte defined by the value of page 0 memory addresses.
    RST { value: u8 },
    /// Subtracts the value in the [`Target8Bit`] register and the value of the carry flag from
    /// the value in the `A` register and stores the result back to the `A` register.
    SBCA { target: Target8Bit },
    /// Subtracts the value at the memory address pointed to by the value in the `HL` register and
    /// the carry flag from the value in the `A` register and stores the result back to the `A`
    /// register.
    SBCAMEM,
    /// Subtracts the `u8` value and the value of the carry flag from the value in the `A` register
    /// and stores the result back to the `A` register.
    SBCAU8 { value: u8 },
    /// Sets the carry flag.
    SCF,
    /// Sets the `bit` in the [`Target8Bit`] register to 1.
    SET { bit: u8, target: Target8Bit },
    /// Sets the `bit` in the value in memory pointed to by the `HL` register to 1.
    SETHL { bit: u8 },
    /// Shift the contents of the [`Target8Bit`] register to the left by one.
    SLA { target: Target8Bit },
    /// Shift the value in memory pointed to by the `HL` register to the left by one.
    SLAHL,
    /// Shift the contents of the [`Target8Bit`] register to the right by one.
    SRA { target: Target8Bit },
    /// Shift the value in memory pointed to by the `HL` register to the right by one.
    SRAHL,
    /// Shift the contents of the [`Target8Bit`] register to the right by one. Sets the last bit of
    /// the register value to 0.
    SRL { target: Target8Bit },
    /// Shift the value in memory pointed to by the `HL` register to the right by one. Sets the
    /// last bit of the value in memory to 0.
    SRLHL,
    /// Stops the system clock and stop mode is entered.
    STOP,
    /// Subtracts the value in the [`Target8Bit`] register from the value in the `A` register
    /// and stores the result back to the `A` register.
    SUBA { target: Target8Bit },
    /// Subtracts the value at the memory address pointed to by the value in the `HL` register
    /// from the value in the `A` register and stores the result back to the `A` register.
    SUBAMEM,
    /// Subtracts the `u8` from the value in the `A` register and stores the result back into the
    /// `A` register.
    SUBAU8 { value: u8 },
    /// Swaps the high and low nibbles of the value in the [`Target8Bit`] register.
    SWAP { target: Target8Bit },
    /// Swaps the high and low nibbles of the value in memory pointed to by the `HL` register.
    SWAPHL,
    /// Takes the logical XOR for each bit of the contents of the [`Target8Bit`] register and the
    /// contents of the `A` register and stores the result back to the `A` register.
    XORA { target: Target8Bit },
    /// Takes the logical XOR for each bit of the contents of the value in memory pointed at by the
    /// [`HL`] register and the contents of the `A` register and stores the result back to the `A`
    /// register.
    XORAMEM,
    /// Takes the logical XOR for each bit of the contents of the `A` register and the contents of
    /// the `A` register and stores the result back to the `A` register.
    XORAU8 { value: u8 },
}

impl Display for Operation {
    /// Writes a string representation of the [`Operation`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::ADCA { target } => f.write_fmt(format_args!("ADC A, {}", target)),
            Operation::ADCAMEM => f.write_str("ADC A, [HL]"),
            Operation::ADCAU8 { value } => f.write_fmt(format_args!("ADC A, {:#04x}", value)),
            Operation::ADDA { target } => f.write_fmt(format_args!("ADD A, {}", target)),
            Operation::ADDAMEM => f.write_str("ADD A, [HL]"),
            Operation::ADDAU8 { value } => f.write_fmt(format_args!("ADD A, {:#04x}", value)),
            Operation::ADDHL { target } => f.write_fmt(format_args!("ADD HL, {}", target)),
            Operation::ADDSP { value } => f.write_fmt(format_args!("ADD SP, {:#04x}", value)),
            Operation::ANDA { target } => f.write_fmt(format_args!("AND A, {}", target)),
            Operation::ANDAMEM => f.write_str("AND A, [HL]"),
            Operation::ANDAU8 { value } => f.write_fmt(format_args!("AND A, {:#04x}", value)),
            Operation::BIT { bit, target } => f.write_fmt(format_args!("BIT {}, {}", bit, target)),
            Operation::BITHL { bit } => f.write_fmt(format_args!("BIT {}, [HL]", bit)),
            Operation::CALL { value } => f.write_fmt(format_args!("CALL {:#06x}", value)),
            Operation::CALLC { target, value, .. } => {
                f.write_fmt(format_args!("CALL {}, {:#06x}", target, value))
            }
            Operation::CCF => f.write_str("CCF"),
            Operation::CPA { target } => f.write_fmt(format_args!("CP A, {}", target)),
            Operation::CPAMEM => f.write_str("CP A, [HL]"),
            Operation::CPAU8 { value } => f.write_fmt(format_args!("CP A, {:#04x}", value)),
            Operation::CPL => f.write_str("CPL"),
            Operation::DAA => f.write_str("DAA"),
            Operation::DEC { target } => f.write_fmt(format_args!("DEC {}", target)),
            Operation::DECMEM => f.write_str("DEC [HL]"),
            Operation::DI => f.write_str("DI"),
            Operation::EI => f.write_str("EI"),
            Operation::HALT => f.write_str("HALT"),
            Operation::INC { target } => f.write_fmt(format_args!("INC {}", target)),
            Operation::INCMEM => f.write_str("INC [HL]"),
            Operation::JP { value } => f.write_fmt(format_args!("JP {:#06x}", value)),
            Operation::JPC { target, value, .. } => {
                f.write_fmt(format_args!("JP {}, {:#06x}", target, value))
            }
            Operation::JPHL => f.write_str("JP HL"),
            Operation::JR { offset } => f.write_fmt(format_args!("JR {:#04x}", offset)),
            Operation::JRC { target, offset, .. } => {
                f.write_fmt(format_args!("JR {}, {:#04x}", target, offset))
            }
            Operation::LDA { target } => f.write_fmt(format_args!("LD [{}], A", target)),
            Operation::LDADEC => f.write_fmt(format_args!("LD [HL-], A")),
            Operation::LDAINC => f.write_fmt(format_args!("LD [HL+], A")),
            Operation::LDAMEM { target } => f.write_fmt(format_args!("LD A, [{}]", target)),
            Operation::LDAMEMDEC => f.write_str("LD A, [HL-]"),
            Operation::LDAMEMINC => f.write_str("LD A, [HL+]"),
            Operation::LDAOFFSET { value } => {
                f.write_fmt(format_args!("LD [$FF00 + {:#04x}], A", value))
            }
            Operation::LDAOFFSETC => f.write_str("LD [$FF00 + C], A"),
            Operation::LDAU16 { address } => f.write_fmt(format_args!("LD [{:#06x}], A", address)),
            Operation::LDA16 { address, target } => {
                f.write_fmt(format_args!("LD [{:#06x}], {}", address, target))
            }
            Operation::LDHLSP { value } => f.write_fmt(format_args!("LD HL, SP + {:#04x}", value)),
            Operation::LDMEMA { address } => f.write_fmt(format_args!("LD A, [{:#06x}]", address)),
            Operation::LDMEMREG { store, target } => {
                f.write_fmt(format_args!("LD [{}], {}", store, target))
            }
            Operation::LDOFFSETA { value } => {
                f.write_fmt(format_args!("LD A, [$FF00 + {:#04x}]", value))
            }
            Operation::LDOFFSETCA => f.write_str("LD A, [$FF00 + C]"),
            Operation::LDREG { store, target } => {
                f.write_fmt(format_args!("LD {}, {}", store, target))
            }
            Operation::LDREGMEM { store, target } => {
                f.write_fmt(format_args!("LD {}, [{}]", store, target))
            }
            Operation::LDSPHL => f.write_str("LD SP, HL"),
            Operation::LDU16 { target, value } => {
                f.write_fmt(format_args!("LD {}, {:#06x}", target, value))
            }
            Operation::LDU8 { target, value } => {
                f.write_fmt(format_args!("LD {}, {:#04x}", target, value))
            }
            Operation::LDU8MEM { value } => f.write_fmt(format_args!("LD [HL], {:#04x}", value)),
            Operation::NOP => f.write_str("NOP"),
            Operation::ORA { target } => f.write_fmt(format_args!("OR A, {}", target)),
            Operation::ORAMEM => f.write_str("OR A, [HL]"),
            Operation::ORAU8 { value } => f.write_fmt(format_args!("OR A, {:#04x}", value)),
            Operation::POP { target } => f.write_fmt(format_args!("POP {}", target)),
            Operation::PREFIX => f.write_str("PREFIX"),
            Operation::PUSH { target } => f.write_fmt(format_args!("PUSH {}", target)),
            Operation::RET => f.write_str("RET"),
            Operation::RETC { target, .. } => f.write_fmt(format_args!("RET {}", target)),
            Operation::RETI => f.write_str("RETI"),
            Operation::RES { bit, target } => f.write_fmt(format_args!("RES {}, {}", bit, target)),
            Operation::RESHL { bit } => f.write_fmt(format_args!("RES {}, [HL]", bit)),
            Operation::RL { target } => f.write_fmt(format_args!("RL {}", target)),
            Operation::RLA => f.write_str("RLA"),
            Operation::RLHL => f.write_str("RL [HL]"),
            Operation::RLC { target } => f.write_fmt(format_args!("RLC {}", target)),
            Operation::RLCA => f.write_str("RLCA"),
            Operation::RLCHL => f.write_fmt(format_args!("RLC [HL]")),
            Operation::RR { target } => f.write_fmt(format_args!("RR {}", target)),
            Operation::RRA => f.write_str("RRA"),
            Operation::RRHL => f.write_str("RR [HL]"),
            Operation::RRC { target } => f.write_fmt(format_args!("RRC {}", target)),
            Operation::RRCA => f.write_str("RRCA"),
            Operation::RRCHL => f.write_str("RRC [HL]"),
            Operation::RST { value } => f.write_fmt(format_args!("RST {:#04x}", value)),
            Operation::SBCA { target } => f.write_fmt(format_args!("SBC A, {}", target)),
            Operation::SBCAMEM => f.write_str("SBC A, [HL]"),
            Operation::SBCAU8 { value } => f.write_fmt(format_args!("SBC A, {:#04x}", value)),
            Operation::SCF => f.write_str("SCF"),
            Operation::SET { bit, target } => f.write_fmt(format_args!("SET {}, {}", bit, target)),
            Operation::SETHL { bit } => f.write_fmt(format_args!("SET {}, [HL]", bit)),
            Operation::SLA { target } => f.write_fmt(format_args!("SLA {}", target)),
            Operation::SLAHL => f.write_str("SLA [HL]"),
            Operation::SRA { target } => f.write_fmt(format_args!("SRA {}", target)),
            Operation::SRAHL => f.write_str("SRA [HL]"),
            Operation::SRL { target } => f.write_fmt(format_args!("SRL {}", target)),
            Operation::SRLHL => f.write_str("SRL [HL]"),
            Operation::STOP => f.write_str("STOP"),
            Operation::SUBA { target } => f.write_fmt(format_args!("SUB A, {}", target)),
            Operation::SUBAMEM => f.write_str("SUB A, [HL]"),
            Operation::SUBAU8 { value } => f.write_fmt(format_args!("SUB A, {:#04x}", value)),
            Operation::SWAP { target } => f.write_fmt(format_args!("SWAP {}", target)),
            Operation::SWAPHL => f.write_str("SWAP [HL]"),
            Operation::XORA { target } => f.write_fmt(format_args!("XOR A, {}", target)),
            Operation::XORAMEM => f.write_str("XOR A, [HL]"),
            Operation::XORAU8 { value } => f.write_fmt(format_args!("XOR A, {:#04x}", value)),
        }
    }
}

/// An instruction that is ready to be executed by the [`Cpu`]. It contains not only the
/// [`Operation`] that should be executed by the cpu but also how wide in bytes the instruction
/// is as well as the number of clock ticks it takes to execute.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Instruction {
    /// Number of bytes that make up the instruction.
    pub num_bytes: u16,
    /// Number of system clock ticks it takes to execute the instruction.
    pub clock_ticks: u64,
    /// [`Operation`] which should be executed by the cpu.
    pub operation: Operation,
}

impl Instruction {
    /// Creates a new [`Instruction`] from the given values.
    fn new(num_bytes: u16, clock_ticks: u64, operation: Operation) -> Self {
        Self {
            num_bytes,
            clock_ticks,
            operation,
        }
    }
    /// Creates a new instruction that adds the value in the [`Target8Bit`] register and
    /// the value of the carry flag to the value in the `A` register and stores the result
    /// back to the `A` register.
    fn adc_a(target: Target8Bit) -> Self {
        Self::new(1, 4, Operation::ADCA { target })
    }
    /// Creates a new instruction that adds the value at the memory address pointed to by the
    /// value in the `HL` register and the carry flag to the value in the `A` register and stores
    /// the result back to the `A` register.
    fn adc_a_mem() -> Self {
        Self::new(1, 8, Operation::ADCAMEM)
    }
    /// Creates an instruction that adds the `u8` value and the value of the carry flag to the
    /// value in the `A` register and stores the result back to the `A` register.
    fn adc_a_u8(value: u8) -> Self {
        Self::new(2, 8, Operation::ADCAU8 { value })
    }
    /// Creates a new instruction that adds the value in the [`Target8Bit`] register to the
    /// value in the HL register and stores it back to the HL register.
    fn add_a(target: Target8Bit) -> Self {
        Self::new(1, 4, Operation::ADDA { target })
    }
    /// Creates a new instruction that adds the value at the memory address pointed to by the
    /// value in the `HL` register to the value in the `A` register and stores the result back
    /// to the `A` register.
    fn add_a_mem() -> Self {
        Self::new(1, 8, Operation::ADDAMEM)
    }
    /// Creates a new instruction that adds the `u8` to the value in the `A` register and stores
    /// the result back into the `A` register.
    fn add_a_u8(value: u8) -> Self {
        Self::new(2, 8, Operation::ADDAU8 { value })
    }
    /// Creates a new instruction that adds the value in the [`Target16Bit`] register to the
    /// value in the `HL` register and stores the result back to the `HL` register.
    fn add_hl(target: Target16Bit) -> Self {
        Self::new(1, 8, Operation::ADDHL { target })
    }
    /// Creates a new instruction that adds the value to the value in the `SP` register and
    /// stores it back to the `SP` register.
    fn add_sp(value: i8) -> Self {
        Self::new(2, 16, Operation::ADDSP { value })
    }
    /// Creates a new instruction that takes the logical AND for each bit of the contents of
    /// the [`Target8Bit`] register and the contents of the `A` register and stores the result
    /// back to the `A` register.
    fn and_a(target: Target8Bit) -> Self {
        Self::new(1, 4, Operation::ANDA { target })
    }
    /// Creates a new instruction that takes the logical AND for each bit of the contents of the
    /// value in memory pointed at by the [`HL`] register and the contents of the `A` register
    /// and stores the result back to the `A` register.
    fn and_a_mem() -> Self {
        Self::new(1, 8, Operation::ANDAMEM)
    }
    /// Creates a new instruction that takes the logical AND for each bit of the value and the
    /// contents of the `A` register and stores the result back to the `A` register.
    fn and_a_u8(value: u8) -> Self {
        Self::new(2, 8, Operation::ANDAU8 { value })
    }
    /// Creates a new instruction that copies the complement of the contents of `bit` in the
    /// [`Target8Bit`] register to the zero flag.
    fn bit(bit: u8, target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::BIT { bit, target })
    }
    /// Creates a new instruction that copies the complement of the contents of `bit` in the `HL`
    /// register to the zero flag.
    fn bit_hl(bit: u8) -> Self {
        Self::new(1, 12, Operation::BITHL { bit })
    }
    /// Creates a new instruction that pushes the current program counter on to the stack and
    /// then sets it to the value.
    fn call(value: u16) -> Self {
        Self::new(3, 24, Operation::CALL { value })
    }
    /// Creates a new instruction that conditionally pushes the current program counter on to the
    /// stack and then sets it to the value based on the state of the [`CondJumpTarget`].
    fn callc(target: CondJumpTarget, jump: bool, value: u16) -> Self {
        let t = if jump { 24 } else { 12 };
        Self::new(
            3,
            t,
            Operation::CALLC {
                target,
                jump,
                value,
            },
        )
    }
    /// Creates a new instruction that toggles the value of the carry flag.
    fn ccf() -> Self {
        Self::new(1, 4, Operation::CCF)
    }
    /// Creates a new instruction that compares the contents of the [`Target8Bit`] register and
    /// the contents of `A` register by subtracting the two values and set the Z flag if they are
    /// equal. The execution of this instruction does not affect the contents of the `A` register.
    fn cp_a(target: Target8Bit) -> Self {
        Self::new(1, 4, Operation::CPA { target })
    }
    /// Creates a new instruction that compares the contents of the value in memory pointed to by
    /// the [`HL`] register and the contents of `A` register by subtracting the two values and set
    /// the Z flag if they are equal. The execution of this instruction does not affect the contents
    /// of the `A` register.
    fn cp_a_mem() -> Self {
        Self::new(1, 8, Operation::CPAMEM)
    }
    /// Creates a new instruction that compares the contents of the value and the contents of
    /// `A` register by subtracting the two values and set the Z flag if they are equal. The
    /// execution of this instruction does not affect the contents of the `A` register.
    fn cp_a_u8(value: u8) -> Self {
        Self::new(2, 8, Operation::CPAU8 { value })
    }
    /// Creates a new instruction which takes the one's complement (i.e., flips all the bits) of
    /// the value in the `A` register.
    fn cpl() -> Self {
        Self::new(1, 4, Operation::CPL)
    }
    /// Creates a new instruction that adjusts the `A` register to a binary-coded decimal (BCD)
    /// number after BCD addition and subtraction operations.
    fn daa() -> Self {
        Self::new(1, 4, Operation::DAA)
    }
    /// Creates a new decrement instruction that targets an single 8 bit register.
    fn dec(target: Target) -> Self {
        Self::new(1, 4, Operation::DEC { target })
    }
    /// Creates a new instruction which decrements the value at the memory address specified by
    /// value of the `HL` register by 1.
    fn dec_mem() -> Self {
        Self::new(1, 12, Operation::DECMEM)
    }
    /// Creates a new decrement instruction that targets a combined 16 bit register.
    fn dec_u16(target: Target) -> Self {
        Self::new(1, 8, Operation::DEC { target })
    }
    /// Creates a new instruction that resets the interrupt master enable (IME) flag and
    /// prohibits maskable interrupts.
    fn di() -> Self {
        Self::new(1, 4, Operation::DI)
    }
    /// Creates a new instruction that sets the interrupt master enable (IME) flag and enables
    /// maskable interrupts.
    fn ei() -> Self {
        Self::new(1, 4, Operation::EI)
    }
    /// Creates a new instruction that halts the system clock and causes the emulator to enter
    /// halt mode.
    fn halt() -> Self {
        Self::new(1, 4, Operation::HALT)
    }
    /// Creates a new increment instruction that targets an single 8 bit register.
    fn inc(target: Target) -> Self {
        Self::new(1, 4, Operation::INC { target })
    }
    /// Creates a new instruction which increments the value at the memory address specified by
    /// value of the `HL` register by 1.
    fn inc_mem() -> Self {
        Self::new(1, 12, Operation::INCMEM)
    }
    /// Creates a new increment instruction that targets a combined 16 bit register.
    fn inc_u16(target: Target) -> Self {
        Self::new(1, 8, Operation::INC { target })
    }
    /// Creates a new instruction that jumps to a memory address by setting the program counter
    /// to the value.
    fn jp(value: u16) -> Self {
        Self::new(3, 16, Operation::JP { value })
    }
    /// Creates a new instruction that conditionally jumps to a memory address by setting the
    /// program counter to the value based on the state of the [`CondJumpTarget`].
    fn jpc(target: CondJumpTarget, jump: bool, value: u16) -> Self {
        let t = if jump { 16 } else { 12 };
        Self::new(
            3,
            t,
            Operation::JPC {
                target,
                jump,
                value,
            },
        )
    }
    /// Creates a new instruction that jumps to a memory address by setting the program counter
    /// to the value.
    fn jp_hl() -> Self {
        Self::new(1, 4, Operation::JPHL)
    }
    /// Creates a new jump relative instruction that advances the program counter by the given
    /// offset.
    fn jr(offset: i8) -> Self {
        Self::new(2, 12, Operation::JR { offset })
    }
    /// Creates a new instruction that conditionally jumps to a relative memory address by advancing
    /// the program counter by the offset based on the state of the [`CondJumpTarget`].
    fn jrc(target: CondJumpTarget, jump: bool, offset: i8) -> Self {
        let t = if jump { 12 } else { 8 };
        Self::new(
            2,
            t,
            Operation::JRC {
                target,
                jump,
                offset,
            },
        )
    }
    /// Creates a new instruction which loads the the contents of the `A` register into memory at
    /// the address held in the target register.
    fn ld_a(target: Target16Bit) -> Self {
        Self::new(1, 8, Operation::LDA { target })
    }
    /// Creates a new instruction which loads the the contents of the `A` register into memory at
    /// the address held in the `HL` register and then decrements the value in the `HL` register.
    fn ld_a_dec() -> Self {
        Self::new(1, 8, Operation::LDADEC)
    }
    /// Creates a new instruction which loads the the contents of the `A` register into memory at
    /// the address held in the `HL` register and then increments the value in the `HL` register.
    fn ld_a_inc() -> Self {
        Self::new(1, 8, Operation::LDAINC)
    }
    /// Creates a new instruction which loads the the byte from memory at the address corresponding
    /// to the value of the [`Target16Bit`] register and stores it in the `A` register.
    fn ld_a_mem(target: Target16Bit) -> Self {
        Self::new(1, 8, Operation::LDAMEM { target })
    }
    /// Creates a new instruction which loads the the byte from memory at the address corresponding
    /// to the value of the `HL` register and stores it in the `A` register, then decrements the
    /// value in the `HL` register.
    fn ld_a_mem_dec() -> Self {
        Self::new(1, 8, Operation::LDAMEMDEC)
    }
    /// Creates a new instruction which loads the the byte from memory at the address corresponding
    /// to the value of the `HL` register and stores it in the `A` register, then increments the
    /// value in the `HL` register.
    fn ld_a_mem_inc() -> Self {
        Self::new(1, 8, Operation::LDAMEMINC)
    }
    /// Creates a new instruction that loads the sum of the `SP` register and the value and stores
    /// it into the `HL` register.
    fn ld_hl_sp(value: i8) -> Self {
        Self::new(2, 12, Operation::LDHLSP { value })
    }
    /// Creates a new instruction that loads the value in memory at the address and stores it in
    /// the `A` register.
    fn ld_mem_a(address: u16) -> Self {
        Self::new(3, 16, Operation::LDMEMA { address })
    }
    /// Creates a new instruction that loads the value in memory at the address 0xFF00 + value
    /// into the `A` register.
    fn ld_a_offset(value: u8) -> Self {
        Self::new(2, 12, Operation::LDAOFFSET { value })
    }
    /// Creates a new instruction that loads the value in memory at the address 0xFF00 + the value
    /// of the carry flag into the `A` register.
    fn ld_a_offset_c() -> Self {
        Self::new(1, 8, Operation::LDAOFFSETC)
    }
    /// Creates a new instruction that loads the value of the `A` register and stores it in memory
    /// at the address.
    fn ld_a_u16(address: u16) -> Self {
        Self::new(3, 16, Operation::LDAU16 { address })
    }
    /// Creates a new instruction which loads the value in the target register and stores it at
    /// the given address in memory.
    fn ld_a16(address: u16, target: Target16Bit) -> Self {
        Self::new(3, 20, Operation::LDA16 { address, target })
    }
    /// Creates a new instruction that loads the value in the `target` 8-bit register and stores it
    /// in the memory address pointed to by the value in the `store` 16-bit register.
    fn ld_mem_reg(store: Target16Bit, target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::LDMEMREG { store, target })
    }
    /// Creates a new instruction that loads the value of the `A` register and stores it in memory
    /// at the address 0xFF00 + value.
    fn ld_offset_a(value: u8) -> Self {
        Self::new(2, 12, Operation::LDOFFSETA { value })
    }
    /// Creates a new instruction that loads the value of the `A` register and stores it in memory
    /// at the address 0xFF00 + the value of the carry flag.
    fn ld_offset_c_a() -> Self {
        Self::new(1, 8, Operation::LDOFFSETCA)
    }
    /// Creates a new instruction which loads the value in the `target` register and stores it into
    /// the `store` register.
    fn ld_reg(store: Target8Bit, target: Target8Bit) -> Self {
        Self::new(1, 4, Operation::LDREG { store, target })
    }
    /// Creates a new instruction that loads the value stored in memory at the address in the
    /// `target` 16-bit register and stores it into the `store` 8-bit register.
    fn ld_reg_mem(store: Target8Bit, target: Target16Bit) -> Self {
        Self::new(1, 8, Operation::LDREGMEM { store, target })
    }
    /// Creates a new instruction that loads the value of the `HL` register and stores it into
    /// the `SP` register.
    fn ld_sp_hl() -> Self {
        Self::new(1, 8, Operation::LDSPHL)
    }
    /// Creates a new load immediate u8 instruction with the given target 8 bit register and
    /// value to load.
    fn ld_u8(target: Target8Bit, value: u8) -> Self {
        Self::new(2, 8, Operation::LDU8 { target, value })
    }
    /// Creates a new load immediate u8 instruction which writes the u8 value to the memory address
    /// pointed to by the value in the `HL` register.
    fn ld_u8_mem(value: u8) -> Self {
        Self::new(2, 12, Operation::LDU8MEM { value })
    }
    /// Creates a new load immediate u16 instruction with the given target 16 bit register and
    /// value to load.
    fn ld_u16(target: Target16Bit, value: u16) -> Self {
        Self::new(3, 12, Operation::LDU16 { target, value })
    }
    /// Creates a new no op instruction.
    fn nop() -> Self {
        Self::new(1, 4, Operation::NOP)
    }
    /// Creates a new instruction that takes the logical OR for each bit of the contents of
    /// the [`Target8Bit`] register and the contents of the `A` register and stores the result
    /// back to the `A` register.
    fn or_a(target: Target8Bit) -> Self {
        Self::new(1, 4, Operation::ORA { target })
    }
    /// Creates a new instruction that takes the logical OR for each bit of the contents of the
    /// value in memory pointed at by the [`HL`] register and the contents of the `A` register
    /// and stores the result back to the `A` register.
    fn or_a_mem() -> Self {
        Self::new(1, 8, Operation::ORAMEM)
    }
    /// Creates a new instruction that takes the logical OR for each bit of the value and the
    /// contents of the `A` register and stores the result back to the `A` register.
    fn or_a_u8(value: u8) -> Self {
        Self::new(2, 8, Operation::ORAU8 { value })
    }
    /// Creates a new instruction that pops the contents from the memory stack into the
    /// [`PushPopTarget`] register.
    fn pop(target: PushPopTarget) -> Self {
        Self::new(1, 12, Operation::POP { target })
    }
    /// Creates a new prefix instruction.
    fn prefix() -> Self {
        Self::new(1, 4, Operation::PREFIX)
    }
    /// Creates a new instruction that pushes the contents of the [`PushPopTarget`] register onto
    /// the memory stack.
    fn push(target: PushPopTarget) -> Self {
        Self::new(1, 16, Operation::PUSH { target })
    }
    /// Creates a new instruction that resets the `bit` in the [`Target8Bit`] register to 0.
    fn res(bit: u8, target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::RES { bit, target })
    }
    /// Creates a new instruction that resets the `bit` in the value in memory pointed to by
    /// the `HL` register to 0.
    fn res_hl(bit: u8) -> Self {
        Self::new(1, 16, Operation::RESHL { bit })
    }
    /// Creates a new instruction that pops from the memory stack the program counter value that
    /// was pushed to the stack when the subroutine was called based on the state of the
    /// [`CondJumpTarget`].
    fn ret() -> Self {
        Self::new(1, 16, Operation::RET)
    }
    /// Creates a new instruction that conditionally pops from the memory stack the program
    /// counter value that was pushed to the stack when the subroutine was called based on the
    /// state of the [`CondJumpTarget`].
    fn retc(target: CondJumpTarget, jump: bool) -> Self {
        let t = if jump { 20 } else { 8 };

        Self::new(1, t, Operation::RETC { target, jump })
    }
    /// Creates a new instruction that pops from the memory stack the program counter value that
    /// was pushed to the stack when the subroutine was called and resumes allowing interrupts.
    fn reti() -> Self {
        Self::new(1, 16, Operation::RETI)
    }
    /// Creates a new instruction that bit rotates the value in the [`Target8Bit`] register left
    /// by one through the carry flag.
    fn rl(target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::RL { target })
    }
    /// Creates a new instruction that bit rotates the value in the `A` register left by one
    /// through the carry flag. Sets the zero flag to false.
    fn rl_a() -> Self {
        Self::new(1, 4, Operation::RLA)
    }
    /// Creates a new instruction that bit rotate the value in memory at the address pointed to
    /// by the `HL`register left by one, through the carry flag.
    fn rl_hl() -> Self {
        Self::new(1, 16, Operation::RLHL)
    }
    /// Creates a new instruction that bit rotates the value in the [`Target`] register left by
    /// one, not through the carry flag.
    fn rlc(target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::RLC { target })
    }
    /// Creates a new instruction that bit rotates the value in the `A` register left by one, not
    /// through the carry flag.
    fn rlc_a() -> Self {
        Self::new(1, 4, Operation::RLCA)
    }
    /// Creates a new instruction that bit rotate the value in memory at the address pointed to
    /// by the value in the `HL` register left by one, not through the carry flag.
    fn rlc_hl() -> Self {
        Self::new(1, 16, Operation::RLCHL)
    }
    /// Creates a new instruction that bit rotates the value in the [`Target8Bit`] register right
    /// by one through the carry flag.
    fn rr(target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::RR { target })
    }
    /// Creates a new instruction that bit rotates the value in the `A` register right by one
    /// through the carry flag.
    fn rr_a() -> Self {
        Self::new(1, 4, Operation::RRA)
    }
    /// Creates a new instruction that bit rotate the value in memory at the address pointed to by
    /// the value in the `HL` register right by one, through the carry flag.
    fn rr_hl() -> Self {
        Self::new(1, 16, Operation::RRHL)
    }
    /// Creates a new instruction that bit rotates the value in the [`Target8Bit`] register right
    /// by one, not through the carry flag.
    fn rrc(target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::RRC { target })
    }
    /// Creates a new instruction that bit rotates the value in the `A` register right by one, not
    /// through the carry flag. Sets the zero flag to false.
    fn rrc_a() -> Self {
        Self::new(1, 4, Operation::RRCA)
    }
    /// Creates a new instruction that bit rotate the value in memory at the address pointed to by
    /// the value in the `HL` register right by one, not through the carry flag.
    fn rrc_hl() -> Self {
        Self::new(1, 16, Operation::RRCHL)
    }
    /// Creates a new instruction that pushes the current value of the program counter PC onto the
    /// memory stack, and load into PC the byte defined by the value of page 0 memory addresses.
    fn rst(value: u8) -> Self {
        Self::new(1, 16, Operation::RST { value })
    }
    /// Creates a new instruction that subtracts the value in the [`Target8Bit`] register and
    /// the value of the carry flag from the value in the `A` register and stores the result
    /// back to the `A` register.
    fn sbc_a(target: Target8Bit) -> Self {
        Self::new(1, 4, Operation::SBCA { target })
    }
    /// Creates a new instruction that subtracts the value at the memory address pointed to by the
    /// value in the `HL` register and the carry flag from the value in the `A` register and stores
    /// the result back to the `A` register.
    fn sbc_a_mem() -> Self {
        Self::new(1, 8, Operation::SBCAMEM)
    }
    /// Creates an instruction that subtracts the `u8` value and the value of the carry flag from
    /// the value in the `A` register and stores the result back to the `A` register.
    fn sbc_a_u8(value: u8) -> Self {
        Self::new(2, 8, Operation::SBCAU8 { value })
    }
    /// Creates a new stop instruction that sets the carry flag.
    fn scf() -> Self {
        Self::new(1, 4, Operation::SCF)
    }

    /// Creates a new instruction that sets the `bit` in the [`Target8Bit`] register to 1.
    fn set(bit: u8, target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::SET { bit, target })
    }
    /// Creates a new instruction that sets the `bit` in the value in memory pointed to by
    /// the `HL` register to 1.
    fn set_hl(bit: u8) -> Self {
        Self::new(1, 16, Operation::SETHL { bit })
    }
    /// Creates a new instruction that shift the contents of the [`Target8Bit`] register to the
    /// left by one.
    fn sla(target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::SLA { target })
    }
    /// Creates a new instruction that shift the value in memory pointed to by the `HL` register
    /// to the left by one.
    fn sla_hl() -> Self {
        Self::new(1, 16, Operation::SLAHL)
    }
    /// Creates a new instruction that shift the contents of the [`Target8Bit`] register to the
    /// right by one.
    fn sra(target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::SRA { target })
    }
    /// Creates a new instruction that shift the value in memory pointed to by the `HL` register
    /// to the right by one.
    fn sra_hl() -> Self {
        Self::new(1, 16, Operation::SRAHL)
    }
    /// Creates a new instruction that shift the contents of the [`Target8Bit`] register to the
    /// right by one. Sets the last bit of the register value to 0.
    fn srl(target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::SRL { target })
    }
    /// Creates a new instruction that shift the value in memory pointed to by the `HL` register
    /// to the right by one. Sets the last bit of the value in memory to 0.
    fn srl_hl() -> Self {
        Self::new(1, 16, Operation::SRLHL)
    }
    /// Creates a new stop instruction that is akin to [`Operation::NOP`] for the emulator.
    fn stop() -> Self {
        Self::new(1, 4, Operation::STOP)
    }
    /// Creates a new instruction that subtracts the value in the [`Target8Bit`] register from
    /// the value in the HL register and stores it back to the HL register.
    fn sub_a(target: Target8Bit) -> Self {
        Self::new(1, 4, Operation::SUBA { target })
    }
    /// Creates a new instruction that subtracts the value at the memory address pointed to by the
    /// value in the `HL` register from the value in the `A` register and stores the result back to
    /// the `A` register.
    fn sub_a_mem() -> Self {
        Self::new(1, 8, Operation::SUBAMEM)
    }
    /// Creates a new instruction that subtracts the `u8` to the value in the `A` register and
    /// stores the result back into the `A` register.
    fn sub_a_u8(value: u8) -> Self {
        Self::new(2, 8, Operation::SUBAU8 { value })
    }
    /// Creates a new instruction that swaps the high and low nibbles of the value in the
    /// [`Target8Bit`] register.
    fn swap(target: Target8Bit) -> Self {
        Self::new(1, 8, Operation::SWAP { target })
    }
    /// Creates an instruction that swaps the high and low nibbles of the value in memory pointed
    /// to by the `HL` register.
    fn swap_hl() -> Self {
        Self::new(1, 16, Operation::SWAPHL)
    }
    /// Creates a new instruction that takes the logical XOR for each bit of the contents of
    /// the [`Target8Bit`] register and the contents of the `A` register and stores the result
    /// back to the `A` register.
    fn xor_a(target: Target8Bit) -> Self {
        Self::new(1, 4, Operation::XORA { target })
    }
    /// Creates a new instruction that takes the logical XOR for each bit of the contents of the
    /// value in memory pointed at by the [`HL`] register and the contents of the `A` register
    /// and stores the result back to the `A` register.
    fn xor_a_mem() -> Self {
        Self::new(1, 8, Operation::XORAMEM)
    }

    /// Creates a new instruction that takes the logical XOR for each bit of value the contents of
    /// the `A` register and stores the result back to the `A` register.
    fn xor_a_u8(value: u8) -> Self {
        Self::new(2, 8, Operation::XORAU8 { value })
    }
}

impl Display for Instruction {
    /// Writes a string representation of the [`Instruction`] to the formatter. This implementation
    /// simply defers to the [`Display`] trait implementation for the [`Operation`] owned by the
    /// [`Instruction`].
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} [{}, {}]",
            self.operation, self.num_bytes, self.clock_ticks
        ))
    }
}

/// Enumerates the different instruction sets that can be used when creating an [`Instruction`]
/// from an op code.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InstructionSet {
    /// Default instruction set.
    Standard,
    /// Active when the previous instruction executed was `PREFIX` which has op code 0xCB.
    Prefixed,
}

impl Display for InstructionSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

impl Default for InstructionSet {
    /// Returns the default value for [`InstructionSet`]. The default value is
    /// [`InstructionSet::Standard`].
    fn default() -> Self {
        Self::Standard
    }
}

/// Represents the central processing unit of the Game Boy system. It is responsible for reading,
/// decoding and executing instructions which drive the game.
#[derive(Debug)]
pub struct Cpu {
    /// Registers read and written by the instructions.
    pub registers: Registers,
    /// Indicates the instruction set that should be used when decoding an op code.
    pub instruction_set: InstructionSet,
    /// Tracks the history of [`Instruction`]s that were executed by the cpu.
    pub history: BoundedVecDeque<Instruction>,
    /// Flag indicating whether the cpu is in HALT mode.
    pub halted: bool,
    /// Interrupt master enabled flag indicating whether interrupts are currently enabled.
    pub ime: bool,
}

/// Default value for the maximum number of instructions stored in the instruction execution
/// history of the [`Cpu`].
const DEFAULT_CPU_MAX_HISTORY: usize = 30;

impl Default for Cpu {
    /// Creates a default [`Cpu`] with max history size of 250.
    fn default() -> Self {
        Self::with_max_history(DEFAULT_CPU_MAX_HISTORY)
    }
}

impl Cpu {
    /// Creates a new default [`Cpu`].
    pub fn new() -> Self {
        Self::default()
    }
    /// Creates a new [`Cpu`] with the given max instruction history length.
    fn with_max_history(max: usize) -> Self {
        Self {
            registers: Registers::default(),
            instruction_set: InstructionSet::default(),
            history: BoundedVecDeque::with_capacity(max, max),
            halted: false,
            ime: false,
        }
    }
    /// Reads and executes the next instruction based on the current program counter. Returns the
    /// number of clock ticks it took to execute the instruction.
    pub fn step(&mut self, memory: &mut dyn Mapper) -> u64 {
        let op_code = memory.read_u8(self.registers.pc);

        let instruction = match self.instruction_set {
            InstructionSet::Standard => self.decode(op_code, memory),
            InstructionSet::Prefixed => self.decode_prefixed(op_code),
        };

        if let Some(instruction) = instruction {
            // enabling ime with the EI instruction is delayed by one step so check previous
            // instruction and enable if required
            if let Some(prev_instruction) = self.history.front() {
                if prev_instruction.operation == Operation::EI {
                    self.ime = true;
                }
            }

            self.registers.pc = self.registers.pc.wrapping_add(instruction.num_bytes);

            self.execute(&instruction, memory);

            self.instruction_set = if instruction.operation == Operation::PREFIX {
                InstructionSet::Prefixed
            } else {
                InstructionSet::Standard
            };

            let clock_ticks = instruction.clock_ticks;

            self.history.push_front(instruction);

            clock_ticks
        } else {
            tracing::warn!("skipping unknown instruction: {:#04x}", op_code);
            0
        }
    }
    /// Transforms the given op code into an [`Instruction`] which can be executed by the [`Cpu`].
    fn decode(&self, op_code: u8, memory: &dyn Mapper) -> Option<Instruction> {
        tracing::debug!("decode op code {:#04x}", op_code);

        match op_code {
            // 0x0x
            0x00 => Some(Instruction::nop()),
            0x01 => Some(Instruction::ld_u16(
                Target16Bit::BC,
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0x02 => Some(Instruction::ld_a(Target16Bit::BC)),
            0x03 => Some(Instruction::inc_u16(Target::BC)),
            0x04 => Some(Instruction::inc(Target::B)),
            0x05 => Some(Instruction::dec(Target::B)),
            0x06 => Some(Instruction::ld_u8(
                Target8Bit::B,
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0x07 => Some(Instruction::rlc_a()),
            0x08 => Some(Instruction::ld_a16(
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
                Target16Bit::SP,
            )),
            0x09 => Some(Instruction::add_hl(Target16Bit::BC)),
            0x0A => Some(Instruction::ld_a_mem(Target16Bit::BC)),
            0x0B => Some(Instruction::dec_u16(Target::BC)),
            0x0C => Some(Instruction::inc(Target::C)),
            0x0D => Some(Instruction::dec(Target::C)),
            0x0E => Some(Instruction::ld_u8(
                Target8Bit::C,
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0x0F => Some(Instruction::rrc_a()),

            // 0x1x
            0x10 => Some(Instruction::stop()),
            0x11 => Some(Instruction::ld_u16(
                Target16Bit::DE,
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0x12 => Some(Instruction::ld_a(Target16Bit::DE)),
            0x13 => Some(Instruction::inc_u16(Target::DE)),
            0x14 => Some(Instruction::inc(Target::D)),
            0x15 => Some(Instruction::dec(Target::D)),
            0x16 => Some(Instruction::ld_u8(
                Target8Bit::D,
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0x17 => Some(Instruction::rl_a()),
            0x18 => Some(Instruction::jr(
                memory.read_u8(self.registers.pc.wrapping_add(1)) as i8,
            )),
            0x19 => Some(Instruction::add_hl(Target16Bit::DE)),
            0x1A => Some(Instruction::ld_a_mem(Target16Bit::DE)),
            0x1B => Some(Instruction::dec_u16(Target::DE)),
            0x1C => Some(Instruction::inc(Target::E)),
            0x1D => Some(Instruction::dec(Target::E)),
            0x1E => Some(Instruction::ld_u8(
                Target8Bit::E,
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0x1F => Some(Instruction::rr_a()),

            // 0x2x
            0x20 => Some(Instruction::jrc(
                CondJumpTarget::NZ,
                !self.registers.f.z(),
                memory.read_u8(self.registers.pc.wrapping_add(1)) as i8,
            )),
            0x21 => Some(Instruction::ld_u16(
                Target16Bit::HL,
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0x22 => Some(Instruction::ld_a_inc()),
            0x23 => Some(Instruction::inc_u16(Target::HL)),
            0x24 => Some(Instruction::inc(Target::H)),
            0x25 => Some(Instruction::dec(Target::H)),
            0x26 => Some(Instruction::ld_u8(
                Target8Bit::H,
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0x27 => Some(Instruction::daa()),
            0x28 => Some(Instruction::jrc(
                CondJumpTarget::Z,
                self.registers.f.z(),
                memory.read_u8(self.registers.pc.wrapping_add(1)) as i8,
            )),
            0x29 => Some(Instruction::add_hl(Target16Bit::HL)),
            0x2A => Some(Instruction::ld_a_mem_inc()),
            0x2B => Some(Instruction::dec_u16(Target::HL)),
            0x2C => Some(Instruction::inc(Target::L)),
            0x2D => Some(Instruction::dec(Target::L)),
            0x2E => Some(Instruction::ld_u8(
                Target8Bit::L,
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0x2F => Some(Instruction::cpl()),

            // 0x3x
            0x30 => Some(Instruction::jrc(
                CondJumpTarget::NC,
                !self.registers.f.c(),
                memory.read_u8(self.registers.pc.wrapping_add(1)) as i8,
            )),
            0x31 => Some(Instruction::ld_u16(
                Target16Bit::SP,
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0x32 => Some(Instruction::ld_a_dec()),
            0x33 => Some(Instruction::inc_u16(Target::SP)),
            0x34 => Some(Instruction::inc_mem()),
            0x35 => Some(Instruction::dec_mem()),
            0x36 => Some(Instruction::ld_u8_mem(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0x37 => Some(Instruction::scf()),
            0x38 => Some(Instruction::jrc(
                CondJumpTarget::C,
                self.registers.f.c(),
                memory.read_u8(self.registers.pc.wrapping_add(1)) as i8,
            )),
            0x39 => Some(Instruction::add_hl(Target16Bit::SP)),
            0x3A => Some(Instruction::ld_a_mem_dec()),
            0x3B => Some(Instruction::dec_u16(Target::SP)),
            0x3C => Some(Instruction::inc(Target::A)),
            0x3D => Some(Instruction::dec(Target::A)),
            0x3E => Some(Instruction::ld_u8(
                Target8Bit::A,
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0x3F => Some(Instruction::ccf()),

            // 0x4x
            0x40 => Some(Instruction::ld_reg(Target8Bit::B, Target8Bit::B)),
            0x41 => Some(Instruction::ld_reg(Target8Bit::B, Target8Bit::C)),
            0x42 => Some(Instruction::ld_reg(Target8Bit::B, Target8Bit::D)),
            0x43 => Some(Instruction::ld_reg(Target8Bit::B, Target8Bit::E)),
            0x44 => Some(Instruction::ld_reg(Target8Bit::B, Target8Bit::H)),
            0x45 => Some(Instruction::ld_reg(Target8Bit::B, Target8Bit::L)),
            0x46 => Some(Instruction::ld_reg_mem(Target8Bit::B, Target16Bit::HL)),
            0x47 => Some(Instruction::ld_reg(Target8Bit::B, Target8Bit::A)),
            0x48 => Some(Instruction::ld_reg(Target8Bit::C, Target8Bit::B)),
            0x49 => Some(Instruction::ld_reg(Target8Bit::C, Target8Bit::C)),
            0x4A => Some(Instruction::ld_reg(Target8Bit::C, Target8Bit::D)),
            0x4B => Some(Instruction::ld_reg(Target8Bit::C, Target8Bit::E)),
            0x4C => Some(Instruction::ld_reg(Target8Bit::C, Target8Bit::H)),
            0x4D => Some(Instruction::ld_reg(Target8Bit::C, Target8Bit::L)),
            0x4E => Some(Instruction::ld_reg_mem(Target8Bit::C, Target16Bit::HL)),
            0x4F => Some(Instruction::ld_reg(Target8Bit::C, Target8Bit::A)),

            // 0x5x
            0x50 => Some(Instruction::ld_reg(Target8Bit::D, Target8Bit::B)),
            0x51 => Some(Instruction::ld_reg(Target8Bit::D, Target8Bit::C)),
            0x52 => Some(Instruction::ld_reg(Target8Bit::D, Target8Bit::D)),
            0x53 => Some(Instruction::ld_reg(Target8Bit::D, Target8Bit::E)),
            0x54 => Some(Instruction::ld_reg(Target8Bit::D, Target8Bit::H)),
            0x55 => Some(Instruction::ld_reg(Target8Bit::D, Target8Bit::L)),
            0x56 => Some(Instruction::ld_reg_mem(Target8Bit::D, Target16Bit::HL)),
            0x57 => Some(Instruction::ld_reg(Target8Bit::D, Target8Bit::A)),
            0x58 => Some(Instruction::ld_reg(Target8Bit::E, Target8Bit::B)),
            0x59 => Some(Instruction::ld_reg(Target8Bit::E, Target8Bit::C)),
            0x5A => Some(Instruction::ld_reg(Target8Bit::E, Target8Bit::D)),
            0x5B => Some(Instruction::ld_reg(Target8Bit::E, Target8Bit::E)),
            0x5C => Some(Instruction::ld_reg(Target8Bit::E, Target8Bit::H)),
            0x5D => Some(Instruction::ld_reg(Target8Bit::E, Target8Bit::L)),
            0x5E => Some(Instruction::ld_reg_mem(Target8Bit::E, Target16Bit::HL)),
            0x5F => Some(Instruction::ld_reg(Target8Bit::E, Target8Bit::A)),

            // 0x6x
            0x60 => Some(Instruction::ld_reg(Target8Bit::H, Target8Bit::B)),
            0x61 => Some(Instruction::ld_reg(Target8Bit::H, Target8Bit::C)),
            0x62 => Some(Instruction::ld_reg(Target8Bit::H, Target8Bit::D)),
            0x63 => Some(Instruction::ld_reg(Target8Bit::H, Target8Bit::E)),
            0x64 => Some(Instruction::ld_reg(Target8Bit::H, Target8Bit::H)),
            0x65 => Some(Instruction::ld_reg(Target8Bit::H, Target8Bit::L)),
            0x66 => Some(Instruction::ld_reg_mem(Target8Bit::H, Target16Bit::HL)),
            0x67 => Some(Instruction::ld_reg(Target8Bit::H, Target8Bit::A)),
            0x68 => Some(Instruction::ld_reg(Target8Bit::L, Target8Bit::B)),
            0x69 => Some(Instruction::ld_reg(Target8Bit::L, Target8Bit::C)),
            0x6A => Some(Instruction::ld_reg(Target8Bit::L, Target8Bit::D)),
            0x6B => Some(Instruction::ld_reg(Target8Bit::L, Target8Bit::E)),
            0x6C => Some(Instruction::ld_reg(Target8Bit::L, Target8Bit::H)),
            0x6D => Some(Instruction::ld_reg(Target8Bit::L, Target8Bit::L)),
            0x6E => Some(Instruction::ld_reg_mem(Target8Bit::L, Target16Bit::HL)),
            0x6F => Some(Instruction::ld_reg(Target8Bit::L, Target8Bit::A)),

            // 0x7x
            0x70 => Some(Instruction::ld_mem_reg(Target16Bit::HL, Target8Bit::B)),
            0x71 => Some(Instruction::ld_mem_reg(Target16Bit::HL, Target8Bit::C)),
            0x72 => Some(Instruction::ld_mem_reg(Target16Bit::HL, Target8Bit::D)),
            0x73 => Some(Instruction::ld_mem_reg(Target16Bit::HL, Target8Bit::E)),
            0x74 => Some(Instruction::ld_mem_reg(Target16Bit::HL, Target8Bit::H)),
            0x75 => Some(Instruction::ld_mem_reg(Target16Bit::HL, Target8Bit::L)),
            0x76 => Some(Instruction::halt()),
            0x77 => Some(Instruction::ld_mem_reg(Target16Bit::HL, Target8Bit::A)),
            0x78 => Some(Instruction::ld_reg(Target8Bit::A, Target8Bit::B)),
            0x79 => Some(Instruction::ld_reg(Target8Bit::A, Target8Bit::C)),
            0x7A => Some(Instruction::ld_reg(Target8Bit::A, Target8Bit::D)),
            0x7B => Some(Instruction::ld_reg(Target8Bit::A, Target8Bit::E)),
            0x7C => Some(Instruction::ld_reg(Target8Bit::A, Target8Bit::H)),
            0x7D => Some(Instruction::ld_reg(Target8Bit::A, Target8Bit::L)),
            0x7E => Some(Instruction::ld_reg_mem(Target8Bit::A, Target16Bit::HL)),
            0x7F => Some(Instruction::ld_reg(Target8Bit::A, Target8Bit::A)),

            // 0x8x
            0x80 => Some(Instruction::add_a(Target8Bit::B)),
            0x81 => Some(Instruction::add_a(Target8Bit::C)),
            0x82 => Some(Instruction::add_a(Target8Bit::D)),
            0x83 => Some(Instruction::add_a(Target8Bit::E)),
            0x84 => Some(Instruction::add_a(Target8Bit::H)),
            0x85 => Some(Instruction::add_a(Target8Bit::L)),
            0x86 => Some(Instruction::add_a_mem()),
            0x87 => Some(Instruction::add_a(Target8Bit::A)),
            0x88 => Some(Instruction::adc_a(Target8Bit::B)),
            0x89 => Some(Instruction::adc_a(Target8Bit::C)),
            0x8A => Some(Instruction::adc_a(Target8Bit::D)),
            0x8B => Some(Instruction::adc_a(Target8Bit::E)),
            0x8C => Some(Instruction::adc_a(Target8Bit::H)),
            0x8D => Some(Instruction::adc_a(Target8Bit::L)),
            0x8E => Some(Instruction::adc_a_mem()),
            0x8F => Some(Instruction::adc_a(Target8Bit::A)),

            // 0x9x
            0x90 => Some(Instruction::sub_a(Target8Bit::B)),
            0x91 => Some(Instruction::sub_a(Target8Bit::C)),
            0x92 => Some(Instruction::sub_a(Target8Bit::D)),
            0x93 => Some(Instruction::sub_a(Target8Bit::E)),
            0x94 => Some(Instruction::sub_a(Target8Bit::H)),
            0x95 => Some(Instruction::sub_a(Target8Bit::L)),
            0x96 => Some(Instruction::sub_a_mem()),
            0x97 => Some(Instruction::sub_a(Target8Bit::A)),
            0x98 => Some(Instruction::sbc_a(Target8Bit::B)),
            0x99 => Some(Instruction::sbc_a(Target8Bit::C)),
            0x9A => Some(Instruction::sbc_a(Target8Bit::D)),
            0x9B => Some(Instruction::sbc_a(Target8Bit::E)),
            0x9C => Some(Instruction::sbc_a(Target8Bit::H)),
            0x9D => Some(Instruction::sbc_a(Target8Bit::L)),
            0x9E => Some(Instruction::sbc_a_mem()),
            0x9F => Some(Instruction::sbc_a(Target8Bit::A)),

            // 0xAx
            0xA0 => Some(Instruction::and_a(Target8Bit::B)),
            0xA1 => Some(Instruction::and_a(Target8Bit::C)),
            0xA2 => Some(Instruction::and_a(Target8Bit::D)),
            0xA3 => Some(Instruction::and_a(Target8Bit::E)),
            0xA4 => Some(Instruction::and_a(Target8Bit::H)),
            0xA5 => Some(Instruction::and_a(Target8Bit::L)),
            0xA6 => Some(Instruction::and_a_mem()),
            0xA7 => Some(Instruction::and_a(Target8Bit::A)),
            0xA8 => Some(Instruction::xor_a(Target8Bit::B)),
            0xA9 => Some(Instruction::xor_a(Target8Bit::C)),
            0xAA => Some(Instruction::xor_a(Target8Bit::D)),
            0xAB => Some(Instruction::xor_a(Target8Bit::E)),
            0xAC => Some(Instruction::xor_a(Target8Bit::H)),
            0xAD => Some(Instruction::xor_a(Target8Bit::L)),
            0xAE => Some(Instruction::xor_a_mem()),
            0xAF => Some(Instruction::xor_a(Target8Bit::A)),

            // 0xBx
            0xB0 => Some(Instruction::or_a(Target8Bit::B)),
            0xB1 => Some(Instruction::or_a(Target8Bit::C)),
            0xB2 => Some(Instruction::or_a(Target8Bit::D)),
            0xB3 => Some(Instruction::or_a(Target8Bit::E)),
            0xB4 => Some(Instruction::or_a(Target8Bit::H)),
            0xB5 => Some(Instruction::or_a(Target8Bit::L)),
            0xB6 => Some(Instruction::or_a_mem()),
            0xB7 => Some(Instruction::or_a(Target8Bit::A)),
            0xB8 => Some(Instruction::cp_a(Target8Bit::B)),
            0xB9 => Some(Instruction::cp_a(Target8Bit::C)),
            0xBA => Some(Instruction::cp_a(Target8Bit::D)),
            0xBB => Some(Instruction::cp_a(Target8Bit::E)),
            0xBC => Some(Instruction::cp_a(Target8Bit::H)),
            0xBD => Some(Instruction::cp_a(Target8Bit::L)),
            0xBE => Some(Instruction::cp_a_mem()),
            0xBF => Some(Instruction::cp_a(Target8Bit::A)),

            // 0xCx
            0xC0 => Some(Instruction::retc(CondJumpTarget::NZ, !self.registers.f.z())),
            0xC1 => Some(Instruction::pop(PushPopTarget::BC)),
            0xC2 => Some(Instruction::jpc(
                CondJumpTarget::NZ,
                !self.registers.f.z(),
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xC3 => Some(Instruction::jp(
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xC4 => Some(Instruction::callc(
                CondJumpTarget::NZ,
                !self.registers.f.z(),
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xC5 => Some(Instruction::push(PushPopTarget::BC)),
            0xC6 => Some(Instruction::add_a_u8(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0xC7 => Some(Instruction::rst(0x00)),
            0xC8 => Some(Instruction::retc(CondJumpTarget::Z, self.registers.f.z())),
            0xC9 => Some(Instruction::ret()),
            0xCA => Some(Instruction::jpc(
                CondJumpTarget::Z,
                self.registers.f.z(),
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xCB => Some(Instruction::prefix()),
            0xCC => Some(Instruction::callc(
                CondJumpTarget::Z,
                self.registers.f.z(),
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xCD => Some(Instruction::call(
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xCE => Some(Instruction::adc_a_u8(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0xCF => Some(Instruction::rst(0x08)),

            // 0xDx
            0xD0 => Some(Instruction::retc(CondJumpTarget::NC, !self.registers.f.c())),
            0xD1 => Some(Instruction::pop(PushPopTarget::DE)),
            0xD2 => Some(Instruction::jpc(
                CondJumpTarget::NC,
                !self.registers.f.c(),
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xD4 => Some(Instruction::callc(
                CondJumpTarget::NC,
                !self.registers.f.c(),
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xD5 => Some(Instruction::push(PushPopTarget::DE)),
            0xD6 => Some(Instruction::sub_a_u8(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0xD7 => Some(Instruction::rst(0x10)),
            0xD8 => Some(Instruction::retc(CondJumpTarget::C, self.registers.f.c())),
            0xD9 => Some(Instruction::reti()),
            0xDA => Some(Instruction::jpc(
                CondJumpTarget::C,
                self.registers.f.c(),
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xDC => Some(Instruction::callc(
                CondJumpTarget::C,
                self.registers.f.c(),
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xDE => Some(Instruction::sbc_a_u8(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0xDF => Some(Instruction::rst(0x18)),

            // 0xEx
            0xE0 => Some(Instruction::ld_offset_a(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0xE1 => Some(Instruction::pop(PushPopTarget::HL)),
            0xE2 => Some(Instruction::ld_offset_c_a()),
            0xE5 => Some(Instruction::push(PushPopTarget::HL)),
            0xE6 => Some(Instruction::and_a_u8(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0xE7 => Some(Instruction::rst(0x20)),
            0xE8 => Some(Instruction::add_sp(
                memory.read_u8(self.registers.pc.wrapping_add(1)) as i8,
            )),
            0xE9 => Some(Instruction::jp_hl()),
            0xEA => Some(Instruction::ld_a_u16(
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xEE => Some(Instruction::xor_a_u8(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0xEF => Some(Instruction::rst(0x28)),

            // 0xFx
            0xF0 => Some(Instruction::ld_a_offset(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0xF1 => Some(Instruction::pop(PushPopTarget::AF)),
            0xF2 => Some(Instruction::ld_a_offset_c()),
            0xF3 => Some(Instruction::di()),
            0xF5 => Some(Instruction::push(PushPopTarget::AF)),
            0xF6 => Some(Instruction::or_a_u8(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0xF7 => Some(Instruction::rst(0x30)),
            0xF8 => Some(Instruction::ld_hl_sp(
                memory.read_u8(self.registers.pc.wrapping_add(1)) as i8,
            )),
            0xF9 => Some(Instruction::ld_sp_hl()),
            0xFA => Some(Instruction::ld_mem_a(
                self.read_u16(memory, self.registers.pc.wrapping_add(1)),
            )),
            0xFB => Some(Instruction::ei()),
            0xFE => Some(Instruction::cp_a_u8(
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0xFF => Some(Instruction::rst(0x38)),

            // invalid op code
            _ => None,
        }
    }
    /// Transforms the given prefixed op code into an [`Instruction`] which can be executed by the
    /// [`Cpu`]. An op code is prefixed if the preceding op code byte was 0xCB.
    fn decode_prefixed(&self, op_code: u8) -> Option<Instruction> {
        tracing::debug!("decode prefixed op code {:#04x}", op_code);

        match op_code {
            // 0x0x
            0x00 => Some(Instruction::rlc(Target8Bit::B)),
            0x01 => Some(Instruction::rlc(Target8Bit::C)),
            0x02 => Some(Instruction::rlc(Target8Bit::D)),
            0x03 => Some(Instruction::rlc(Target8Bit::E)),
            0x04 => Some(Instruction::rlc(Target8Bit::H)),
            0x05 => Some(Instruction::rlc(Target8Bit::L)),
            0x06 => Some(Instruction::rlc_hl()),
            0x07 => Some(Instruction::rlc(Target8Bit::A)),
            0x08 => Some(Instruction::rrc(Target8Bit::B)),
            0x09 => Some(Instruction::rrc(Target8Bit::C)),
            0x0A => Some(Instruction::rrc(Target8Bit::D)),
            0x0B => Some(Instruction::rrc(Target8Bit::E)),
            0x0C => Some(Instruction::rrc(Target8Bit::H)),
            0x0D => Some(Instruction::rrc(Target8Bit::L)),
            0x0E => Some(Instruction::rrc_hl()),
            0x0F => Some(Instruction::rrc(Target8Bit::A)),

            // 0x1x
            0x10 => Some(Instruction::rl(Target8Bit::B)),
            0x11 => Some(Instruction::rl(Target8Bit::C)),
            0x12 => Some(Instruction::rl(Target8Bit::D)),
            0x13 => Some(Instruction::rl(Target8Bit::E)),
            0x14 => Some(Instruction::rl(Target8Bit::H)),
            0x15 => Some(Instruction::rl(Target8Bit::L)),
            0x16 => Some(Instruction::rl_hl()),
            0x17 => Some(Instruction::rl(Target8Bit::A)),
            0x18 => Some(Instruction::rr(Target8Bit::B)),
            0x19 => Some(Instruction::rr(Target8Bit::C)),
            0x1A => Some(Instruction::rr(Target8Bit::D)),
            0x1B => Some(Instruction::rr(Target8Bit::E)),
            0x1C => Some(Instruction::rr(Target8Bit::H)),
            0x1D => Some(Instruction::rr(Target8Bit::L)),
            0x1E => Some(Instruction::rr_hl()),
            0x1F => Some(Instruction::rr(Target8Bit::A)),

            // 0x2x
            0x20 => Some(Instruction::sla(Target8Bit::B)),
            0x21 => Some(Instruction::sla(Target8Bit::C)),
            0x22 => Some(Instruction::sla(Target8Bit::D)),
            0x23 => Some(Instruction::sla(Target8Bit::E)),
            0x24 => Some(Instruction::sla(Target8Bit::H)),
            0x25 => Some(Instruction::sla(Target8Bit::L)),
            0x26 => Some(Instruction::sla_hl()),
            0x27 => Some(Instruction::sla(Target8Bit::A)),
            0x28 => Some(Instruction::sra(Target8Bit::B)),
            0x29 => Some(Instruction::sra(Target8Bit::C)),
            0x2A => Some(Instruction::sra(Target8Bit::D)),
            0x2B => Some(Instruction::sra(Target8Bit::E)),
            0x2C => Some(Instruction::sra(Target8Bit::H)),
            0x2D => Some(Instruction::sra(Target8Bit::L)),
            0x2E => Some(Instruction::sra_hl()),
            0x2F => Some(Instruction::sra(Target8Bit::A)),

            // 0x3x
            0x30 => Some(Instruction::swap(Target8Bit::B)),
            0x31 => Some(Instruction::swap(Target8Bit::C)),
            0x32 => Some(Instruction::swap(Target8Bit::D)),
            0x33 => Some(Instruction::swap(Target8Bit::E)),
            0x34 => Some(Instruction::swap(Target8Bit::H)),
            0x35 => Some(Instruction::swap(Target8Bit::L)),
            0x36 => Some(Instruction::swap_hl()),
            0x37 => Some(Instruction::swap(Target8Bit::A)),
            0x38 => Some(Instruction::srl(Target8Bit::B)),
            0x39 => Some(Instruction::srl(Target8Bit::C)),
            0x3A => Some(Instruction::srl(Target8Bit::D)),
            0x3B => Some(Instruction::srl(Target8Bit::E)),
            0x3C => Some(Instruction::srl(Target8Bit::H)),
            0x3D => Some(Instruction::srl(Target8Bit::L)),
            0x3E => Some(Instruction::srl_hl()),
            0x3F => Some(Instruction::srl(Target8Bit::A)),

            // 0x4x
            0x40 => Some(Instruction::bit(0, Target8Bit::B)),
            0x41 => Some(Instruction::bit(0, Target8Bit::C)),
            0x42 => Some(Instruction::bit(0, Target8Bit::D)),
            0x43 => Some(Instruction::bit(0, Target8Bit::E)),
            0x44 => Some(Instruction::bit(0, Target8Bit::H)),
            0x45 => Some(Instruction::bit(0, Target8Bit::L)),
            0x46 => Some(Instruction::bit_hl(0)),
            0x47 => Some(Instruction::bit(0, Target8Bit::A)),
            0x48 => Some(Instruction::bit(1, Target8Bit::B)),
            0x49 => Some(Instruction::bit(1, Target8Bit::C)),
            0x4A => Some(Instruction::bit(1, Target8Bit::D)),
            0x4B => Some(Instruction::bit(1, Target8Bit::E)),
            0x4C => Some(Instruction::bit(1, Target8Bit::H)),
            0x4D => Some(Instruction::bit(1, Target8Bit::L)),
            0x4E => Some(Instruction::bit_hl(1)),
            0x4F => Some(Instruction::bit(1, Target8Bit::A)),

            // 0x5x
            0x50 => Some(Instruction::bit(2, Target8Bit::B)),
            0x51 => Some(Instruction::bit(2, Target8Bit::C)),
            0x52 => Some(Instruction::bit(2, Target8Bit::D)),
            0x53 => Some(Instruction::bit(2, Target8Bit::E)),
            0x54 => Some(Instruction::bit(2, Target8Bit::H)),
            0x55 => Some(Instruction::bit(2, Target8Bit::L)),
            0x56 => Some(Instruction::bit_hl(2)),
            0x57 => Some(Instruction::bit(2, Target8Bit::A)),
            0x58 => Some(Instruction::bit(3, Target8Bit::B)),
            0x59 => Some(Instruction::bit(3, Target8Bit::C)),
            0x5A => Some(Instruction::bit(3, Target8Bit::D)),
            0x5B => Some(Instruction::bit(3, Target8Bit::E)),
            0x5C => Some(Instruction::bit(3, Target8Bit::H)),
            0x5D => Some(Instruction::bit(3, Target8Bit::L)),
            0x5E => Some(Instruction::bit_hl(3)),
            0x5F => Some(Instruction::bit(3, Target8Bit::A)),

            // 0x6x
            0x60 => Some(Instruction::bit(4, Target8Bit::B)),
            0x61 => Some(Instruction::bit(4, Target8Bit::C)),
            0x62 => Some(Instruction::bit(4, Target8Bit::D)),
            0x63 => Some(Instruction::bit(4, Target8Bit::E)),
            0x64 => Some(Instruction::bit(4, Target8Bit::H)),
            0x65 => Some(Instruction::bit(4, Target8Bit::L)),
            0x66 => Some(Instruction::bit_hl(4)),
            0x67 => Some(Instruction::bit(4, Target8Bit::A)),
            0x68 => Some(Instruction::bit(5, Target8Bit::B)),
            0x69 => Some(Instruction::bit(5, Target8Bit::C)),
            0x6A => Some(Instruction::bit(5, Target8Bit::D)),
            0x6B => Some(Instruction::bit(5, Target8Bit::E)),
            0x6C => Some(Instruction::bit(5, Target8Bit::H)),
            0x6D => Some(Instruction::bit(5, Target8Bit::L)),
            0x6E => Some(Instruction::bit_hl(5)),
            0x6F => Some(Instruction::bit(5, Target8Bit::A)),

            // 0x7x
            0x70 => Some(Instruction::bit(6, Target8Bit::B)),
            0x71 => Some(Instruction::bit(6, Target8Bit::C)),
            0x72 => Some(Instruction::bit(6, Target8Bit::D)),
            0x73 => Some(Instruction::bit(6, Target8Bit::E)),
            0x74 => Some(Instruction::bit(6, Target8Bit::H)),
            0x75 => Some(Instruction::bit(6, Target8Bit::L)),
            0x76 => Some(Instruction::bit_hl(6)),
            0x77 => Some(Instruction::bit(6, Target8Bit::A)),
            0x78 => Some(Instruction::bit(7, Target8Bit::B)),
            0x79 => Some(Instruction::bit(7, Target8Bit::C)),
            0x7A => Some(Instruction::bit(7, Target8Bit::D)),
            0x7B => Some(Instruction::bit(7, Target8Bit::E)),
            0x7C => Some(Instruction::bit(7, Target8Bit::H)),
            0x7D => Some(Instruction::bit(7, Target8Bit::L)),
            0x7E => Some(Instruction::bit_hl(7)),
            0x7F => Some(Instruction::bit(7, Target8Bit::A)),

            // 0x8x
            0x80 => Some(Instruction::res(0, Target8Bit::B)),
            0x81 => Some(Instruction::res(0, Target8Bit::C)),
            0x82 => Some(Instruction::res(0, Target8Bit::D)),
            0x83 => Some(Instruction::res(0, Target8Bit::E)),
            0x84 => Some(Instruction::res(0, Target8Bit::H)),
            0x85 => Some(Instruction::res(0, Target8Bit::L)),
            0x86 => Some(Instruction::res_hl(0)),
            0x87 => Some(Instruction::res(0, Target8Bit::A)),
            0x88 => Some(Instruction::res(1, Target8Bit::B)),
            0x89 => Some(Instruction::res(1, Target8Bit::C)),
            0x8A => Some(Instruction::res(1, Target8Bit::D)),
            0x8B => Some(Instruction::res(1, Target8Bit::E)),
            0x8C => Some(Instruction::res(1, Target8Bit::H)),
            0x8D => Some(Instruction::res(1, Target8Bit::L)),
            0x8E => Some(Instruction::res_hl(1)),
            0x8F => Some(Instruction::res(1, Target8Bit::A)),

            // 0x9x
            0x90 => Some(Instruction::res(2, Target8Bit::B)),
            0x91 => Some(Instruction::res(2, Target8Bit::C)),
            0x92 => Some(Instruction::res(2, Target8Bit::D)),
            0x93 => Some(Instruction::res(2, Target8Bit::E)),
            0x94 => Some(Instruction::res(2, Target8Bit::H)),
            0x95 => Some(Instruction::res(2, Target8Bit::L)),
            0x96 => Some(Instruction::res_hl(2)),
            0x97 => Some(Instruction::res(2, Target8Bit::A)),
            0x98 => Some(Instruction::res(3, Target8Bit::B)),
            0x99 => Some(Instruction::res(3, Target8Bit::C)),
            0x9A => Some(Instruction::res(3, Target8Bit::D)),
            0x9B => Some(Instruction::res(3, Target8Bit::E)),
            0x9C => Some(Instruction::res(3, Target8Bit::H)),
            0x9D => Some(Instruction::res(3, Target8Bit::L)),
            0x9E => Some(Instruction::res_hl(3)),
            0x9F => Some(Instruction::res(3, Target8Bit::A)),

            // 0xAx
            0xA0 => Some(Instruction::res(4, Target8Bit::B)),
            0xA1 => Some(Instruction::res(4, Target8Bit::C)),
            0xA2 => Some(Instruction::res(4, Target8Bit::D)),
            0xA3 => Some(Instruction::res(4, Target8Bit::E)),
            0xA4 => Some(Instruction::res(4, Target8Bit::H)),
            0xA5 => Some(Instruction::res(4, Target8Bit::L)),
            0xA6 => Some(Instruction::res_hl(4)),
            0xA7 => Some(Instruction::res(4, Target8Bit::A)),
            0xA8 => Some(Instruction::res(5, Target8Bit::B)),
            0xA9 => Some(Instruction::res(5, Target8Bit::C)),
            0xAA => Some(Instruction::res(5, Target8Bit::D)),
            0xAB => Some(Instruction::res(5, Target8Bit::E)),
            0xAC => Some(Instruction::res(5, Target8Bit::H)),
            0xAD => Some(Instruction::res(5, Target8Bit::L)),
            0xAE => Some(Instruction::res_hl(5)),
            0xAF => Some(Instruction::res(5, Target8Bit::A)),

            // 0xBx
            0xB0 => Some(Instruction::res(6, Target8Bit::B)),
            0xB1 => Some(Instruction::res(6, Target8Bit::C)),
            0xB2 => Some(Instruction::res(6, Target8Bit::D)),
            0xB3 => Some(Instruction::res(6, Target8Bit::E)),
            0xB4 => Some(Instruction::res(6, Target8Bit::H)),
            0xB5 => Some(Instruction::res(6, Target8Bit::L)),
            0xB6 => Some(Instruction::res_hl(6)),
            0xB7 => Some(Instruction::res(6, Target8Bit::A)),
            0xB8 => Some(Instruction::res(7, Target8Bit::B)),
            0xB9 => Some(Instruction::res(7, Target8Bit::C)),
            0xBA => Some(Instruction::res(7, Target8Bit::D)),
            0xBB => Some(Instruction::res(7, Target8Bit::E)),
            0xBC => Some(Instruction::res(7, Target8Bit::H)),
            0xBD => Some(Instruction::res(7, Target8Bit::L)),
            0xBE => Some(Instruction::res_hl(7)),
            0xBF => Some(Instruction::res(7, Target8Bit::A)),

            // 0xCx
            0xC0 => Some(Instruction::set(0, Target8Bit::B)),
            0xC1 => Some(Instruction::set(0, Target8Bit::C)),
            0xC2 => Some(Instruction::set(0, Target8Bit::D)),
            0xC3 => Some(Instruction::set(0, Target8Bit::E)),
            0xC4 => Some(Instruction::set(0, Target8Bit::H)),
            0xC5 => Some(Instruction::set(0, Target8Bit::L)),
            0xC6 => Some(Instruction::set_hl(0)),
            0xC7 => Some(Instruction::set(0, Target8Bit::A)),
            0xC8 => Some(Instruction::set(1, Target8Bit::B)),
            0xC9 => Some(Instruction::set(1, Target8Bit::C)),
            0xCA => Some(Instruction::set(1, Target8Bit::D)),
            0xCB => Some(Instruction::set(1, Target8Bit::E)),
            0xCC => Some(Instruction::set(1, Target8Bit::H)),
            0xCD => Some(Instruction::set(1, Target8Bit::L)),
            0xCE => Some(Instruction::set_hl(1)),
            0xCF => Some(Instruction::set(1, Target8Bit::A)),

            // 0xDx
            0xD0 => Some(Instruction::set(2, Target8Bit::B)),
            0xD1 => Some(Instruction::set(2, Target8Bit::C)),
            0xD2 => Some(Instruction::set(2, Target8Bit::D)),
            0xD3 => Some(Instruction::set(2, Target8Bit::E)),
            0xD4 => Some(Instruction::set(2, Target8Bit::H)),
            0xD5 => Some(Instruction::set(2, Target8Bit::L)),
            0xD6 => Some(Instruction::set_hl(2)),
            0xD7 => Some(Instruction::set(2, Target8Bit::A)),
            0xD8 => Some(Instruction::set(3, Target8Bit::B)),
            0xD9 => Some(Instruction::set(3, Target8Bit::C)),
            0xDA => Some(Instruction::set(3, Target8Bit::D)),
            0xDB => Some(Instruction::set(3, Target8Bit::E)),
            0xDC => Some(Instruction::set(3, Target8Bit::H)),
            0xDD => Some(Instruction::set(3, Target8Bit::L)),
            0xDE => Some(Instruction::set_hl(3)),
            0xDF => Some(Instruction::set(3, Target8Bit::A)),

            // 0xEx
            0xE0 => Some(Instruction::set(4, Target8Bit::B)),
            0xE1 => Some(Instruction::set(4, Target8Bit::C)),
            0xE2 => Some(Instruction::set(4, Target8Bit::D)),
            0xE3 => Some(Instruction::set(4, Target8Bit::E)),
            0xE4 => Some(Instruction::set(4, Target8Bit::H)),
            0xE5 => Some(Instruction::set(4, Target8Bit::L)),
            0xE6 => Some(Instruction::set_hl(4)),
            0xE7 => Some(Instruction::set(4, Target8Bit::A)),
            0xE8 => Some(Instruction::set(5, Target8Bit::B)),
            0xE9 => Some(Instruction::set(5, Target8Bit::C)),
            0xEA => Some(Instruction::set(5, Target8Bit::D)),
            0xEB => Some(Instruction::set(5, Target8Bit::E)),
            0xEC => Some(Instruction::set(5, Target8Bit::H)),
            0xED => Some(Instruction::set(5, Target8Bit::L)),
            0xEE => Some(Instruction::set_hl(5)),
            0xEF => Some(Instruction::set(5, Target8Bit::A)),

            // 0xFx
            0xF0 => Some(Instruction::set(6, Target8Bit::B)),
            0xF1 => Some(Instruction::set(6, Target8Bit::C)),
            0xF2 => Some(Instruction::set(6, Target8Bit::D)),
            0xF3 => Some(Instruction::set(6, Target8Bit::E)),
            0xF4 => Some(Instruction::set(6, Target8Bit::H)),
            0xF5 => Some(Instruction::set(6, Target8Bit::L)),
            0xF6 => Some(Instruction::set_hl(6)),
            0xF7 => Some(Instruction::set(6, Target8Bit::A)),
            0xF8 => Some(Instruction::set(7, Target8Bit::B)),
            0xF9 => Some(Instruction::set(7, Target8Bit::C)),
            0xFA => Some(Instruction::set(7, Target8Bit::D)),
            0xFB => Some(Instruction::set(7, Target8Bit::E)),
            0xFC => Some(Instruction::set(7, Target8Bit::H)),
            0xFD => Some(Instruction::set(7, Target8Bit::L)),
            0xFE => Some(Instruction::set_hl(7)),
            0xFF => Some(Instruction::set(7, Target8Bit::A)),
        }
    }
    /// Executes the given [`Instruction`] returning the new program counter value and whether
    /// the op code for the next instruction is prefixed.
    fn execute(&mut self, instruction: &Instruction, memory: &mut dyn Mapper) {
        tracing::debug!("execute instruction '{}'", instruction);

        match instruction.operation {
            Operation::ADCA { target } => {
                let a = self.registers.a;

                let target_value = match target {
                    Target8Bit::A => self.registers.a,
                    Target8Bit::B => self.registers.b,
                    Target8Bit::C => self.registers.c,
                    Target8Bit::D => self.registers.d,
                    Target8Bit::E => self.registers.e,
                    Target8Bit::H => self.registers.h,
                    Target8Bit::L => self.registers.l,
                };

                let carry_value = self.registers.f.c() as u8;

                let (intermediate, overflowed_intermediate) =
                    target_value.overflowing_add(carry_value);
                let (new_value, overflowed) = a.overflowing_add(intermediate);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers
                    .f
                    .set_h(((a & 0x0F) + (target_value & 0x0F) + carry_value) > 0x0F);
                self.registers
                    .f
                    .set_c(overflowed || overflowed_intermediate);
            }
            Operation::ADCAMEM => {
                let a = self.registers.a;
                let hl = self.registers.hl();

                let mem_value = memory.read_u8(hl);

                let carry_value = self.registers.f.c() as u8;

                let (intermediate, overflowed_intermediate) =
                    mem_value.overflowing_add(carry_value);
                let (new_value, overflowed) = a.overflowing_add(intermediate);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers
                    .f
                    .set_h(((a & 0x0F) + (mem_value & 0x0F) + carry_value) > 0x0F);
                self.registers
                    .f
                    .set_c(overflowed || overflowed_intermediate);
            }
            Operation::ADCAU8 { value } => {
                let a = self.registers.a;

                let carry_value = self.registers.f.c() as u8;

                let (intermediate, overflowed_intermediate) = value.overflowing_add(carry_value);
                let (new_value, overflowed) = a.overflowing_add(intermediate);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers
                    .f
                    .set_h(((a & 0x0F) + (value & 0x0F) + carry_value) > 0x0F);
                self.registers
                    .f
                    .set_c(overflowed || overflowed_intermediate);
            }
            Operation::ADDA { target } => {
                let a = self.registers.a;

                let target_value = match target {
                    Target8Bit::A => self.registers.a,
                    Target8Bit::B => self.registers.b,
                    Target8Bit::C => self.registers.c,
                    Target8Bit::D => self.registers.d,
                    Target8Bit::E => self.registers.e,
                    Target8Bit::H => self.registers.h,
                    Target8Bit::L => self.registers.l,
                };

                let (new_value, overflowed) = a.overflowing_add(target_value);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers
                    .f
                    .set_h(will_half_carry_add_u8(a, target_value));
                self.registers.f.set_c(overflowed);
            }
            Operation::ADDAMEM => {
                let a = self.registers.a;
                let hl = self.registers.hl();

                let mem_value = memory.read_u8(hl);

                let (new_value, overflowed) = a.overflowing_add(mem_value);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(will_half_carry_add_u8(a, mem_value));
                self.registers.f.set_c(overflowed);
            }
            Operation::ADDAU8 { value } => {
                let a = self.registers.a;

                let (new_value, overflowed) = a.overflowing_add(value);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(will_half_carry_add_u8(a, value));
                self.registers.f.set_c(overflowed);
            }
            Operation::ADDHL { target } => {
                let hl = self.registers.hl();

                let target_value = match target {
                    Target16Bit::BC => self.registers.bc(),
                    Target16Bit::DE => self.registers.de(),
                    Target16Bit::HL => self.registers.hl(),
                    Target16Bit::SP => self.registers.sp,
                };

                let (new_value, overflowed) = hl.overflowing_add(target_value);
                let will_half_carry = will_half_carry_add_u16(hl, target_value);

                self.registers.set_hl(new_value);

                self.registers.f.set_n(false);
                self.registers.f.set_h(will_half_carry);
                self.registers.f.set_c(overflowed);
            }
            Operation::ADDSP { value } => {
                let sp = self.registers.sp;
                let value = value as i16;

                let new_value = sp.wrapping_add_signed(value);

                self.registers.sp = new_value;

                self.registers.f.set_z(false);
                self.registers.f.set_n(false);
                self.registers
                    .f
                    .set_h((sp & 0x000F) + (value as u16 & 0x000F) > 0x000F);
                self.registers
                    .f
                    .set_c((sp & 0x00FF) + (value as u16 & 0x00FF) > 0x00FF);
            }
            Operation::ANDA { target } => {
                let target_value = match target {
                    Target8Bit::A => self.registers.a,
                    Target8Bit::B => self.registers.b,
                    Target8Bit::C => self.registers.c,
                    Target8Bit::D => self.registers.d,
                    Target8Bit::E => self.registers.e,
                    Target8Bit::H => self.registers.h,
                    Target8Bit::L => self.registers.l,
                };

                self.registers.a &= target_value;

                self.registers.f.set_z(self.registers.a == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(true);
                self.registers.f.set_c(false);
            }
            Operation::ANDAMEM => {
                let hl = self.registers.hl();

                let mem_value = memory.read_u8(hl);

                self.registers.a &= mem_value;

                self.registers.f.set_z(self.registers.a == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(true);
                self.registers.f.set_c(false);
            }
            Operation::ANDAU8 { value } => {
                self.registers.a &= value;

                self.registers.f.set_z(self.registers.a == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(true);
                self.registers.f.set_c(false);
            }
            Operation::BIT { bit, target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                let complement = *value & (1 << bit) == 0;

                self.registers.f.set_z(complement);
                self.registers.f.set_n(false);
                self.registers.f.set_h(true);
            }
            Operation::BITHL { bit } => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let complement = value & (1 << bit) == 0;

                self.registers.f.set_z(complement);
                self.registers.f.set_n(false);
                self.registers.f.set_h(true);
            }
            Operation::CALL { value } => {
                self.registers.sp = self.registers.sp.wrapping_sub(1);
                let high = (self.registers.pc >> 8) as u8;
                memory.write_u8(self.registers.sp, high);

                self.registers.sp = self.registers.sp.wrapping_sub(1);
                let low = self.registers.pc as u8;
                memory.write_u8(self.registers.sp, low);

                self.registers.pc = value;
            }
            Operation::CALLC { jump, value, .. } => {
                if jump {
                    self.registers.sp = self.registers.sp.wrapping_sub(1);
                    let high = (self.registers.pc >> 8) as u8;
                    memory.write_u8(self.registers.sp, high);

                    self.registers.sp = self.registers.sp.wrapping_sub(1);
                    let low = self.registers.pc as u8;
                    memory.write_u8(self.registers.sp, low);

                    self.registers.pc = value;
                }
            }
            Operation::CCF => {
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(!self.registers.f.c());
            }
            Operation::CPA { target } => {
                let a = self.registers.a;

                let target_value = match target {
                    Target8Bit::A => self.registers.a,
                    Target8Bit::B => self.registers.b,
                    Target8Bit::C => self.registers.c,
                    Target8Bit::D => self.registers.d,
                    Target8Bit::E => self.registers.e,
                    Target8Bit::H => self.registers.h,
                    Target8Bit::L => self.registers.l,
                };

                let (new_value, overflowed) = a.overflowing_sub(target_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(true);
                self.registers
                    .f
                    .set_h(will_half_carry_sub_u8(a, target_value));
                self.registers.f.set_c(overflowed);
            }
            Operation::CPAMEM => {
                let a = self.registers.a;
                let hl = self.registers.hl();

                let mem_value = memory.read_u8(hl);

                let (new_value, overflowed) = a.overflowing_sub(mem_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(true);
                self.registers.f.set_h(will_half_carry_sub_u8(a, mem_value));
                self.registers.f.set_c(overflowed);
            }
            Operation::CPAU8 { value } => {
                let a = self.registers.a;

                let (new_value, overflowed) = a.overflowing_sub(value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(true);
                self.registers.f.set_h(will_half_carry_sub_u8(a, value));
                self.registers.f.set_c(overflowed);
            }
            Operation::CPL => {
                self.registers.a = !self.registers.a;
                self.registers.f.set_n(true);
                self.registers.f.set_h(true);
            }
            Operation::DAA => {
                let mut value = self.registers.a;

                let mut correction: u16 = if self.registers.f.c() { 0x60 } else { 0x00 };

                if self.registers.f.h() || (!self.registers.f.n() && ((value & 0x0F) > 9)) {
                    correction |= 0x06;
                }

                if self.registers.f.c() || (!self.registers.f.n() && (value > 0x99)) {
                    correction |= 0x60;
                }

                if self.registers.f.n() {
                    value = ((value as u16).wrapping_sub(correction)) as u8;
                } else {
                    value = ((value as u16).wrapping_add(correction)) as u8;
                }

                self.registers.a = value;

                if ((correction << 2) & 0x100) != 0 {
                    self.registers.f.set_c(true);
                }

                self.registers.f.set_z(self.registers.a == 0);
                self.registers.f.set_h(false);
            }
            // TODO: cleanup
            Operation::DEC { target } => match target {
                Target::A => {
                    let old_value = self.registers.a;
                    self.registers.a = self.registers.a.wrapping_sub(1);

                    self.registers.f.set_z(self.registers.a == 0);
                    self.registers.f.set_n(true);
                    self.registers.f.set_h(will_half_carry_sub_u8(old_value, 1));
                }
                Target::B => {
                    let old_value = self.registers.b;
                    self.registers.b = self.registers.b.wrapping_sub(1);

                    self.registers.f.set_z(self.registers.b == 0);
                    self.registers.f.set_n(true);
                    self.registers.f.set_h(will_half_carry_sub_u8(old_value, 1));
                }
                Target::C => {
                    let old_value = self.registers.c;
                    self.registers.c = self.registers.c.wrapping_sub(1);

                    self.registers.f.set_z(self.registers.c == 0);
                    self.registers.f.set_n(true);
                    self.registers.f.set_h(will_half_carry_sub_u8(old_value, 1));
                }
                Target::D => {
                    let old_value = self.registers.d;
                    self.registers.d = self.registers.d.wrapping_sub(1);

                    self.registers.f.set_z(self.registers.d == 0);
                    self.registers.f.set_n(true);
                    self.registers.f.set_h(will_half_carry_sub_u8(old_value, 1));
                }
                Target::E => {
                    let old_value = self.registers.e;
                    self.registers.e = self.registers.e.wrapping_sub(1);

                    self.registers.f.set_z(self.registers.e == 0);
                    self.registers.f.set_n(true);
                    self.registers.f.set_h(will_half_carry_sub_u8(old_value, 1));
                }
                Target::H => {
                    let old_value = self.registers.h;
                    self.registers.h = self.registers.h.wrapping_sub(1);

                    self.registers.f.set_z(self.registers.h == 0);
                    self.registers.f.set_n(true);
                    self.registers.f.set_h(will_half_carry_sub_u8(old_value, 1));
                }
                Target::L => {
                    let old_value = self.registers.l;
                    self.registers.l = self.registers.l.wrapping_sub(1);

                    self.registers.f.set_z(self.registers.l == 0);
                    self.registers.f.set_n(true);
                    self.registers.f.set_h(will_half_carry_sub_u8(old_value, 1));
                }
                Target::BC => {
                    self.registers.set_bc(self.registers.bc().wrapping_sub(1));
                }
                Target::DE => {
                    self.registers.set_de(self.registers.de().wrapping_sub(1));
                }
                Target::HL => {
                    self.registers.set_hl(self.registers.hl().wrapping_sub(1));
                }
                Target::SP => {
                    self.registers.sp = self.registers.sp.wrapping_sub(1);
                }
            },
            Operation::DECMEM => {
                let address = self.registers.hl();
                let value = memory.read_u8(address);
                let new_value = value.wrapping_sub(1);

                memory.write_u8(address, new_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(true);
                self.registers.f.set_h(will_half_carry_sub_u8(value, 1))
            }
            Operation::DI => self.ime = false,
            Operation::EI => { /* enabling of ime for EI is delayed one instruction */ }
            Operation::HALT => self.halted = true,
            // TODO: cleanup
            Operation::INC { target } => match target {
                Target::A => {
                    let old_value = self.registers.a;
                    self.registers.a = self.registers.a.wrapping_add(1);

                    self.registers.f.set_z(self.registers.a == 0);
                    self.registers.f.set_n(false);
                    self.registers.f.set_h(will_half_carry_add_u8(old_value, 1));
                }
                Target::B => {
                    let old_value = self.registers.b;
                    self.registers.b = self.registers.b.wrapping_add(1);

                    self.registers.f.set_z(self.registers.b == 0);
                    self.registers.f.set_n(false);
                    self.registers.f.set_h(will_half_carry_add_u8(old_value, 1));
                }
                Target::C => {
                    let old_value = self.registers.c;
                    self.registers.c = self.registers.c.wrapping_add(1);

                    self.registers.f.set_z(self.registers.c == 0);
                    self.registers.f.set_n(false);
                    self.registers.f.set_h(will_half_carry_add_u8(old_value, 1));
                }
                Target::D => {
                    let old_value = self.registers.d;
                    self.registers.d = self.registers.d.wrapping_add(1);

                    self.registers.f.set_z(self.registers.d == 0);
                    self.registers.f.set_n(false);
                    self.registers.f.set_h(will_half_carry_add_u8(old_value, 1));
                }
                Target::E => {
                    let old_value = self.registers.e;
                    self.registers.e = self.registers.e.wrapping_add(1);

                    self.registers.f.set_z(self.registers.e == 0);
                    self.registers.f.set_n(false);
                    self.registers.f.set_h(will_half_carry_add_u8(old_value, 1));
                }
                Target::H => {
                    let old_value = self.registers.h;
                    self.registers.h = self.registers.h.wrapping_add(1);

                    self.registers.f.set_z(self.registers.h == 0);
                    self.registers.f.set_n(false);
                    self.registers.f.set_h(will_half_carry_add_u8(old_value, 1));
                }
                Target::L => {
                    let old_value = self.registers.l;
                    self.registers.l = self.registers.l.wrapping_add(1);

                    self.registers.f.set_z(self.registers.l == 0);
                    self.registers.f.set_n(false);
                    self.registers.f.set_h(will_half_carry_add_u8(old_value, 1));
                }
                Target::BC => self.registers.set_bc(self.registers.bc().wrapping_add(1)),
                Target::DE => self.registers.set_de(self.registers.de().wrapping_add(1)),
                Target::HL => self.registers.set_hl(self.registers.hl().wrapping_add(1)),
                Target::SP => self.registers.sp = self.registers.sp.wrapping_add(1),
            },
            Operation::INCMEM => {
                let address = self.registers.hl();
                let value = memory.read_u8(address);
                let new_value = value.wrapping_add(1);

                memory.write_u8(address, new_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(will_half_carry_add_u8(value, 1))
            }
            Operation::JP { value } => self.registers.pc = value,
            Operation::JPC { jump, value, .. } => {
                if jump {
                    self.registers.pc = value;
                }
            }
            Operation::JPHL => self.registers.pc = self.registers.hl(),
            Operation::JR { offset } => {
                self.registers.pc = self.registers.pc.wrapping_add_signed(offset.into());
            }
            Operation::JRC { offset, jump, .. } => {
                if jump {
                    self.registers.pc = self.registers.pc.wrapping_add_signed(offset.into());
                }
            }
            Operation::LDA { target } => {
                let address = match target {
                    Target16Bit::BC => self.registers.bc(),
                    Target16Bit::DE => self.registers.de(),
                    _ => todo!(),
                };

                memory.write_u8(address, self.registers.a);
            }
            Operation::LDADEC => {
                memory.write_u8(self.registers.hl(), self.registers.a);
                self.registers.set_hl(self.registers.hl().wrapping_sub(1));
            }
            Operation::LDAINC => {
                memory.write_u8(self.registers.hl(), self.registers.a);
                self.registers.set_hl(self.registers.hl().wrapping_add(1));
            }
            Operation::LDAMEM { target } => match target {
                Target16Bit::BC => self.registers.a = memory.read_u8(self.registers.bc()),
                Target16Bit::DE => self.registers.a = memory.read_u8(self.registers.de()),
                _ => todo!(),
            },
            Operation::LDAMEMDEC => {
                self.registers.a = memory.read_u8(self.registers.hl());
                self.registers.set_hl(self.registers.hl().wrapping_sub(1));
            }
            Operation::LDAMEMINC => {
                self.registers.a = memory.read_u8(self.registers.hl());
                self.registers.set_hl(self.registers.hl().wrapping_add(1));
            }
            Operation::LDAOFFSET { value } => {
                let new_value = memory.read_u8(0xFF00 + value as u16);
                self.registers.a = new_value;
            }
            Operation::LDAOFFSETC => {
                tracing::warn!("0xF2 is not correct yet");
                let new_value = memory.read_u8(0xFF00 + self.registers.f.c() as u16);
                self.registers.a = new_value;
            }
            Operation::LDAU16 { address } => {
                memory.write_u8(address, self.registers.a);
            }
            Operation::LDA16 { address, target } => match target {
                Target16Bit::SP => {
                    let low = self.registers.sp as u8;
                    let high = (self.registers.sp >> 8) as u8;

                    memory.write_u8(address, low);
                    memory.write_u8(address + 1, high);
                }
                _ => todo!(),
            },
            Operation::LDHLSP { value } => {
                let sp = self.registers.sp;
                let value = value as i16;

                let new_value = sp.wrapping_add_signed(value);

                self.registers.set_hl(new_value);

                self.registers.f.set_z(false);
                self.registers.f.set_n(false);
                self.registers
                    .f
                    .set_h((sp & 0x000F) + (value as u16 & 0x000F) > 0x000F);
                self.registers
                    .f
                    .set_c((sp & 0x00FF) + (value as u16 & 0x00FF) > 0x00FF);
            }
            Operation::LDMEMA { address } => self.registers.a = memory.read_u8(address),
            Operation::LDMEMREG { store, target } => {
                let value = match target {
                    Target8Bit::A => self.registers.a,
                    Target8Bit::B => self.registers.b,
                    Target8Bit::C => self.registers.c,
                    Target8Bit::D => self.registers.d,
                    Target8Bit::E => self.registers.e,
                    Target8Bit::H => self.registers.h,
                    Target8Bit::L => self.registers.l,
                };

                let address = match store {
                    Target16Bit::HL => self.registers.hl(),
                    _ => panic!("invalid LDMEMREG store: {}", store),
                };

                memory.write_u8(address, value);
            }
            Operation::LDOFFSETA { value } => {
                let address = 0xFF00 + value as u16;
                memory.write_u8(address, self.registers.a);
            }
            Operation::LDOFFSETCA => {
                tracing::warn!("0xE2 is not correct yet");
                let address = 0xFF00 + self.registers.f.c() as u16;
                memory.write_u8(address, self.registers.a);
            }
            Operation::LDREG { store, target } => {
                let target = match target {
                    Target8Bit::A => self.registers.a,
                    Target8Bit::B => self.registers.b,
                    Target8Bit::C => self.registers.c,
                    Target8Bit::D => self.registers.d,
                    Target8Bit::E => self.registers.e,
                    Target8Bit::H => self.registers.h,
                    Target8Bit::L => self.registers.l,
                };

                let store = match store {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                *store = target;
            }
            Operation::LDREGMEM { store, target } => {
                let address = match target {
                    Target16Bit::HL => self.registers.hl(),
                    _ => panic!("invalid LDREGMEM target: {}", target),
                };

                let target = memory.read_u8(address);

                let store = match store {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                *store = target;
            }
            Operation::LDSPHL => self.registers.sp = self.registers.hl(),
            Operation::LDU8 { target, value } => match target {
                Target8Bit::A => self.registers.a = value,
                Target8Bit::B => self.registers.b = value,
                Target8Bit::C => self.registers.c = value,
                Target8Bit::D => self.registers.d = value,
                Target8Bit::E => self.registers.e = value,
                Target8Bit::H => self.registers.h = value,
                Target8Bit::L => self.registers.l = value,
            },
            Operation::LDU8MEM { value } => {
                let address = self.registers.hl();

                memory.write_u8(address, value);
            }
            Operation::LDU16 { target, value } => match target {
                Target16Bit::BC => self.registers.set_bc(value),
                Target16Bit::DE => self.registers.set_de(value),
                Target16Bit::HL => self.registers.set_hl(value),
                Target16Bit::SP => self.registers.sp = value,
            },
            Operation::NOP => {}
            Operation::ORA { target } => {
                let target_value = match target {
                    Target8Bit::A => self.registers.a,
                    Target8Bit::B => self.registers.b,
                    Target8Bit::C => self.registers.c,
                    Target8Bit::D => self.registers.d,
                    Target8Bit::E => self.registers.e,
                    Target8Bit::H => self.registers.h,
                    Target8Bit::L => self.registers.l,
                };

                self.registers.a |= target_value;

                self.registers.f.set_z(self.registers.a == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(false);
            }
            Operation::ORAMEM => {
                let hl = self.registers.hl();

                let mem_value = memory.read_u8(hl);

                self.registers.a |= mem_value;

                self.registers.f.set_z(self.registers.a == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(false);
            }
            Operation::ORAU8 { value } => {
                self.registers.a |= value;

                self.registers.f.set_z(self.registers.a == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(false);
            }
            Operation::POP { target } => {
                let low = memory.read_u8(self.registers.sp) as u16;
                self.registers.sp = self.registers.sp.wrapping_add(1);
                let high = memory.read_u8(self.registers.sp) as u16;
                self.registers.sp = self.registers.sp.wrapping_add(1);

                let new_value = (high << 8) | low;

                match target {
                    PushPopTarget::BC => self.registers.set_bc(new_value),
                    PushPopTarget::DE => self.registers.set_de(new_value),
                    PushPopTarget::HL => self.registers.set_hl(new_value),
                    PushPopTarget::AF => self.registers.set_af(new_value),
                }
            }
            Operation::PREFIX => {}
            Operation::PUSH { target } => {
                let value = match target {
                    PushPopTarget::BC => self.registers.bc(),
                    PushPopTarget::DE => self.registers.de(),
                    PushPopTarget::HL => self.registers.hl(),
                    PushPopTarget::AF => self.registers.af(),
                };

                self.registers.sp = self.registers.sp.wrapping_sub(1);
                memory.write_u8(self.registers.sp, (value >> 8) as u8);

                self.registers.sp = self.registers.sp.wrapping_sub(1);
                memory.write_u8(self.registers.sp, value as u8);
            }
            Operation::RES { bit, target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                *value &= !(1 << bit);
            }
            Operation::RESHL { bit } => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let new_value = value & !(1 << bit);

                memory.write_u8(hl, new_value);
            }
            Operation::RET => {
                let low = memory.read_u8(self.registers.sp) as u16;
                self.registers.sp = self.registers.sp.wrapping_add(1);
                let high = memory.read_u8(self.registers.sp) as u16;
                self.registers.sp = self.registers.sp.wrapping_add(1);

                self.registers.pc = (high << 8) | low;
            }
            Operation::RETC { jump, .. } => {
                if jump {
                    let low = memory.read_u8(self.registers.sp) as u16;
                    self.registers.sp = self.registers.sp.wrapping_add(1);
                    let high = memory.read_u8(self.registers.sp) as u16;
                    self.registers.sp = self.registers.sp.wrapping_add(1);

                    self.registers.pc = (high << 8) | low;
                }
            }
            Operation::RETI => {
                let low = memory.read_u8(self.registers.sp) as u16;
                self.registers.sp = self.registers.sp.wrapping_add(1);
                let high = memory.read_u8(self.registers.sp) as u16;
                self.registers.sp = self.registers.sp.wrapping_add(1);

                self.registers.pc = (high << 8) | low;

                self.ime = true;
            }
            Operation::RL { target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                let carry: u8 = self.registers.f.c().into();
                let will_carry = (*value & (1 << 7)) != 0;

                *value = (*value << 1) | carry;

                self.registers.f.set_z(*value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RLA => {
                let a = self.registers.a;

                let carry: u8 = self.registers.f.c().into();
                let will_carry = (a & (1 << 7)) != 0;

                self.registers.a = (a << 1) | carry;

                self.registers.f.set_z(false);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RLHL => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let carry: u8 = self.registers.f.c().into();
                let will_carry = (value & (1 << 7)) != 0;

                let new_value = (value << 1) | carry;

                memory.write_u8(hl, new_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RLC { target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                let truncated_bit = *value & (1 << 7);
                let will_carry = truncated_bit != 0;

                *value = (*value << 1) | (truncated_bit >> 7);

                self.registers.f.set_z(*value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RLCA => {
                let a = self.registers.a;

                let truncated_bit = a & (1 << 7);
                let will_carry = truncated_bit != 0;

                self.registers.a = (a << 1) | (truncated_bit >> 7);

                self.registers.f.set_z(false);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RLCHL => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let truncated_bit = value & (1 << 7);
                let will_carry = truncated_bit != 0;

                let new_value = (value << 1) | (truncated_bit >> 7);

                memory.write_u8(hl, new_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RR { target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                let carry: u8 = self.registers.f.c().into();
                let will_carry = *value & 1 != 0;

                *value = (*value >> 1) | (carry << 7);

                self.registers.f.set_z(*value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RRA => {
                let a = self.registers.a;

                let carry: u8 = self.registers.f.c().into();
                let will_carry = a & 1 != 0;

                self.registers.a = (a >> 1) | (carry << 7);

                self.registers.f.set_z(false);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RRHL => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let carry: u8 = self.registers.f.c().into();
                let will_carry = value & 1 != 0;

                let new_value = (value >> 1) | (carry << 7);

                memory.write_u8(hl, new_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RRC { target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                let truncated_bit = *value & 1;
                let will_carry = truncated_bit != 0;

                *value = ((*value) >> 1) | (truncated_bit << 7);

                self.registers.f.set_z(*value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RRCA => {
                let a = self.registers.a;

                let truncated_bit = a & 1;
                let will_carry = truncated_bit != 0;

                self.registers.a = ((a) >> 1) | (truncated_bit << 7);

                self.registers.f.set_z(false);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RRCHL => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let truncated_bit = value & 1;
                let will_carry = truncated_bit != 0;

                let new_value = ((value) >> 1) | (truncated_bit << 7);

                memory.write_u8(hl, new_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::RST { value } => {
                let pc = self.registers.pc;

                self.registers.sp = self.registers.sp.wrapping_sub(1);
                memory.write_u8(self.registers.sp, (pc >> 8) as u8);

                self.registers.sp = self.registers.sp.wrapping_sub(1);
                memory.write_u8(self.registers.sp, pc as u8);

                self.registers.pc = value as u16;
            }
            Operation::SBCA { target } => {
                let a = self.registers.a;

                let target_value = match target {
                    Target8Bit::A => self.registers.a,
                    Target8Bit::B => self.registers.b,
                    Target8Bit::C => self.registers.c,
                    Target8Bit::D => self.registers.d,
                    Target8Bit::E => self.registers.e,
                    Target8Bit::H => self.registers.h,
                    Target8Bit::L => self.registers.l,
                };

                let carry_value = self.registers.f.c() as u8;

                let (intermediate, overflowed_intermediate) = a.overflowing_sub(target_value);
                let (new_value, overflowed) = intermediate.overflowing_sub(carry_value);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(true);
                self.registers.f.set_h(
                    (((a & 0x0F) as i32) - ((target_value & 0x0F) as i32) - (carry_value as i32))
                        < 0,
                );
                self.registers
                    .f
                    .set_c(overflowed || overflowed_intermediate);
            }
            Operation::SBCAMEM => {
                let a = self.registers.a;
                let hl = self.registers.hl();

                let mem_value = memory.read_u8(hl);

                let carry_value = self.registers.f.c() as u8;

                let (intermediate, overflowed_intermediate) = a.overflowing_sub(mem_value);
                let (new_value, overflowed) = intermediate.overflowing_sub(carry_value);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(true);
                self.registers.f.set_h(
                    (((a & 0x0F) as i32) - ((mem_value & 0x0F) as i32) - (carry_value as i32)) < 0,
                );
                self.registers
                    .f
                    .set_c(overflowed || overflowed_intermediate);
            }
            Operation::SBCAU8 { value } => {
                let a = self.registers.a;
                let carry_value = self.registers.f.c() as u8;

                let (intermediate, overflowed_intermediate) = a.overflowing_sub(value);
                let (new_value, overflowed) = intermediate.overflowing_sub(carry_value);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(true);
                self.registers.f.set_h(
                    (((a & 0x0F) as i32) - ((value & 0x0F) as i32) - (carry_value as i32)) < 0,
                );
                self.registers
                    .f
                    .set_c(overflowed || overflowed_intermediate);
            }
            Operation::SCF => {
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(true);
            }
            Operation::SET { bit, target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                *value |= 1 << bit;
            }
            Operation::SETHL { bit } => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let new_value = value | (1 << bit);

                memory.write_u8(hl, new_value);
            }
            Operation::SLA { target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                let will_carry = *value & (1 << 7) != 0;

                *value <<= 1;

                self.registers.f.set_z(*value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::SLAHL => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let will_carry = value & (1 << 7) != 0;

                let new_value = value << 1;

                memory.write_u8(hl, new_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::SRA { target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                let will_carry = *value & 1 != 0;
                let bit7 = *value & (1 << 7) != 0;

                *value >>= 1;

                *value = if bit7 {
                    *value | (1 << 7)
                } else {
                    *value & !(1 << 7)
                };

                self.registers.f.set_z(*value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::SRAHL => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let will_carry = value & 1 != 0;
                let bit7 = value & (1 << 7) != 0;

                let mut new_value = value >> 1;

                new_value = if bit7 {
                    new_value | (1 << 7)
                } else {
                    new_value & !(1 << 7)
                };

                memory.write_u8(hl, new_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::SRL { target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                let will_carry = *value & 1 != 0;

                *value >>= 1;

                self.registers.f.set_z(*value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::SRLHL => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let will_carry = value & 1 != 0;

                let new_value = value >> 1;

                memory.write_u8(hl, new_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::STOP => {}
            Operation::SUBA { target } => {
                let a = self.registers.a;

                let target_value = match target {
                    Target8Bit::A => self.registers.a,
                    Target8Bit::B => self.registers.b,
                    Target8Bit::C => self.registers.c,
                    Target8Bit::D => self.registers.d,
                    Target8Bit::E => self.registers.e,
                    Target8Bit::H => self.registers.h,
                    Target8Bit::L => self.registers.l,
                };

                let (new_value, overflowed) = a.overflowing_sub(target_value);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(true);
                self.registers
                    .f
                    .set_h(will_half_carry_sub_u8(a, target_value));
                self.registers.f.set_c(overflowed);
            }
            Operation::SUBAMEM => {
                let a = self.registers.a;
                let hl = self.registers.hl();

                let mem_value = memory.read_u8(hl);

                let (new_value, overflowed) = a.overflowing_sub(mem_value);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(true);
                self.registers.f.set_h(will_half_carry_sub_u8(a, mem_value));
                self.registers.f.set_c(overflowed);
            }
            Operation::SUBAU8 { value } => {
                let a = self.registers.a;

                let (new_value, overflowed) = a.overflowing_sub(value);

                self.registers.a = new_value;

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(true);
                self.registers.f.set_h(will_half_carry_sub_u8(a, value));
                self.registers.f.set_c(overflowed);
            }
            Operation::SWAP { target } => {
                let value = match target {
                    Target8Bit::A => &mut self.registers.a,
                    Target8Bit::B => &mut self.registers.b,
                    Target8Bit::C => &mut self.registers.c,
                    Target8Bit::D => &mut self.registers.d,
                    Target8Bit::E => &mut self.registers.e,
                    Target8Bit::H => &mut self.registers.h,
                    Target8Bit::L => &mut self.registers.l,
                };

                let low = *value >> 4;
                let high = *value << 4;

                *value = high | low;

                self.registers.f.set_z(*value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(false);
            }
            Operation::SWAPHL => {
                let hl = self.registers.hl();
                let value = memory.read_u8(hl);

                let low = value >> 4;
                let high = value << 4;

                let new_value = high | low;

                memory.write_u8(hl, new_value);

                self.registers.f.set_z(new_value == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(false);
            }
            Operation::XORA { target } => {
                let target_value = match target {
                    Target8Bit::A => self.registers.a,
                    Target8Bit::B => self.registers.b,
                    Target8Bit::C => self.registers.c,
                    Target8Bit::D => self.registers.d,
                    Target8Bit::E => self.registers.e,
                    Target8Bit::H => self.registers.h,
                    Target8Bit::L => self.registers.l,
                };

                self.registers.a ^= target_value;

                self.registers.f.set_z(self.registers.a == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(false);
            }
            Operation::XORAMEM => {
                let hl = self.registers.hl();

                let mem_value = memory.read_u8(hl);

                self.registers.a ^= mem_value;

                self.registers.f.set_z(self.registers.a == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(false);
            }
            Operation::XORAU8 { value } => {
                self.registers.a ^= value;

                self.registers.f.set_z(self.registers.a == 0);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(false);
            }
        }
    }
    /// Reads the next two bytes from the given address in memory and combines them into a single
    /// u16 value.
    pub fn read_u16(&self, memory: &dyn Mapper, address: u16) -> u16 {
        let low = memory.read_u8(address);
        let high = memory.read_u8(address + 1);

        ((high as u16) << 8) | low as u16
    }
}

/// Determines if the addition of `b` to `a` will cause a half-carry.
fn will_half_carry_add_u8(a: u8, b: u8) -> bool {
    (a & 0x0F) + (b & 0x0F) > 0x0F
}

/// Determines if the addition of `b` to `a` will cause a half-carry. The half carry is calculated
/// on the 11th bit.
fn will_half_carry_add_u16(a: u16, b: u16) -> bool {
    (a & 0x0FFF) + (b & 0x0FFF) > 0x0FFF
}

/// Determines if the subtraction of `b` from `a` will cause a half-carry.
fn will_half_carry_sub_u8(a: u8, b: u8) -> bool {
    (((a & 0x0F) as i32) - ((b & 0x0F) as i32)) < 0
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::cart::RomOnly;

    struct TestMapper {
        inner: RomOnly,
    }

    impl TestMapper {
        fn new() -> Self {
            Self {
                inner: RomOnly::new(),
            }
        }
    }

    impl Mapper for TestMapper {
        fn read_u8(&self, address: u16) -> u8 {
            self.inner.read_u8(address)
        }
        fn write_u8(&mut self, address: u16, byte: u8) {
            self.inner.write_u8(address, byte);
        }
    }

    #[test]
    fn test_flags_c() {
        let mut flags = Flags::default();
        assert!(!flags.c());

        flags.set_c(true);
        assert!(flags.c());
        assert_eq!(flags.0, 1 << FLAGS_CARRY_BIT_POSITION);

        flags.set_c(false);
        assert!(!flags.c());
    }

    #[test]
    fn test_flags_h() {
        let mut flags = Flags::default();
        assert!(!flags.h());

        flags.set_h(true);
        assert!(flags.h());
        assert_eq!(flags.0, 1 << FLAGS_HALF_CARRY_BIT_POSITION);

        flags.set_h(false);
        assert!(!flags.h());
    }

    #[test]
    fn test_flags_n() {
        let mut flags = Flags::default();
        assert!(!flags.n());

        flags.set_n(true);
        assert!(flags.n());
        assert_eq!(flags.0, 1 << FLAGS_SUBTRACT_BIT_POSITION);

        flags.set_n(false);
        assert!(!flags.n());
    }

    #[test]
    fn test_flags_z() {
        let mut flags = Flags::default();
        assert!(!flags.z());

        flags.set_z(true);
        assert!(flags.z());
        assert_eq!(flags.0, 1 << FLAGS_ZERO_BIT_POSITION);

        flags.set_z(false);
        assert!(!flags.z());
    }

    #[test]
    fn test_mmu_rom_only_read_u16() {
        let mut mapper = TestMapper::new();
        mapper.write_u8(0x1010, 0x01);
        mapper.write_u8(0x1011, 0x01);

        let cpu = Cpu::new();

        let value = cpu.read_u16(&mapper, 0x1010);
        assert_eq!(0x0101, value);
    }
}

#[cfg(test)]
mod json_tests {
    use super::*;

    use crate::cart::RomOnly;

    use serde::Deserialize;
    use std::path::Path;

    struct TestMapper {
        inner: RomOnly,
    }

    impl TestMapper {
        fn new() -> Self {
            Self {
                inner: RomOnly::new(),
            }
        }
    }

    impl Mapper for TestMapper {
        fn read_u8(&self, address: u16) -> u8 {
            self.inner.read_u8(address)
        }
        fn write_u8(&mut self, address: u16, byte: u8) {
            self.inner.write_u8(address, byte);
        }
    }

    /// Contains the state of the cpu and memory at a point in the test lifecycle.
    #[derive(Debug, Deserialize)]
    struct State {
        pc: u16,
        sp: u16,
        a: u8,
        b: u8,
        c: u8,
        d: u8,
        e: u8,
        f: u8,
        h: u8,
        l: u8,
        ime: u8,
        ram: Vec<Vec<u16>>, // TODO: can probably make this better
    }

    /// Contains the input and output data for an instruction op code test.
    #[derive(Debug, Deserialize)]
    struct Test {
        name: String,
        #[serde(rename = "initial")]
        input: State,
        #[serde(rename = "final")]
        output: State,
    }

    /// Reads a JSON file at the given path by joining the value of the `JSON_TESTS_BASE_DIR`
    /// env var. If the env var is not specified then the current directory is used for the value.
    fn read_json_file(path: impl AsRef<Path>) -> anyhow::Result<String> {
        let base_dir = std::env::var("JSON_TESTS_BASE_DIR").unwrap_or(String::from("./"));
        let base_dir = Path::new(&base_dir);

        std::fs::read_to_string(base_dir.join(path)).map_err(|e| e.into())
    }

    /// Deserializes the given JSON into a [`Vec`] of [`Test`].
    fn deserialize_json(json: String) -> anyhow::Result<Vec<Test>> {
        serde_json::from_str(&json).map_err(|e| e.into())
    }

    /// Loads, deserializes and executes all of the tests in the JSON file.
    ///
    /// # Panic
    ///
    /// This function will panic if the JSON file cannot be read or if it cannot be successfully
    /// deserialized into a [`Vec`] of [`Test`].
    fn test_json_file(file_name: &str, instruction_set: InstructionSet) {
        read_json_file(file_name)
            .and_then(deserialize_json)
            .expect("valid test JSON files")
            .into_iter()
            .for_each(|t| execute(t, instruction_set));
    }

    /// Executes a [`Test`] for the given instruction op code.
    fn execute(test: Test, instruction_set: InstructionSet) {
        println!("execute json test {}", test.name);

        let mut cpu = Cpu::new();
        cpu.instruction_set = instruction_set;
        cpu.registers.pc = test.input.pc;
        cpu.registers.sp = test.input.sp;
        cpu.registers.a = test.input.a;
        cpu.registers.b = test.input.b;
        cpu.registers.c = test.input.c;
        cpu.registers.d = test.input.d;
        cpu.registers.e = test.input.e;
        cpu.registers.f = test.input.f.into();
        cpu.registers.h = test.input.h;
        cpu.registers.l = test.input.l;
        cpu.ime = test.input.ime != 0;

        // mimic that we read the 0xCB byte and incremented pc if we are executing a JSON test for
        // a prefixed instruction.
        if cpu.instruction_set == InstructionSet::Prefixed {
            cpu.registers.pc = cpu.registers.pc.wrapping_add(1);
        }

        let mut mapper = TestMapper::new();

        for byte in test.input.ram {
            if byte.len() != 2 {
                panic!("invalid byte data");
            }

            let address = byte[0];
            let value = byte[1] as u8;

            mapper.write_u8(address, value);
        }

        cpu.step(&mut mapper);

        let f_value: u8 = cpu.registers.f.into();

        assert_eq!(test.output.pc, cpu.registers.pc);
        assert_eq!(test.output.sp, cpu.registers.sp);
        assert_eq!(test.output.a, cpu.registers.a);
        assert_eq!(test.output.b, cpu.registers.b);
        assert_eq!(test.output.c, cpu.registers.c);
        assert_eq!(test.output.d, cpu.registers.d);
        assert_eq!(test.output.e, cpu.registers.e);
        assert_eq!(test.output.f, f_value);
        assert_eq!(test.output.h, cpu.registers.h);
        assert_eq!(test.output.l, cpu.registers.l);

        let ime = if cpu.ime { 1 } else { 0 };
        assert_eq!(test.output.ime, ime);

        for byte in test.output.ram {
            if byte.len() != 2 {
                panic!("invalid byte data");
            }

            let address = byte[0];
            let expected = byte[1] as u8;

            let actual = mapper.read_u8(address);

            assert_eq!(expected, actual);
        }
    }

    /// Macro that allows for easily defining a test function that executes a JSON-based test file.
    macro_rules! test_instruction {
        ($name:ident, $file:expr) => {
            #[test]
            #[ignore]
            #[allow(non_snake_case)]
            fn $name() {
                test_json_file($file, InstructionSet::Standard)
            }
        };
    }

    /// Macro that allows for easily defining a test function that executes a JSON-based test file.
    macro_rules! test_prefixed_instruction {
        ($name:ident, $file:expr) => {
            #[test]
            #[ignore]
            #[allow(non_snake_case)]
            fn $name() {
                test_json_file($file, InstructionSet::Prefixed)
            }
        };
    }

    // 0x0x
    test_instruction!(test_00, "00.json");
    test_instruction!(test_01, "01.json");
    test_instruction!(test_02, "02.json");
    test_instruction!(test_03, "03.json");
    test_instruction!(test_04, "04.json");
    test_instruction!(test_05, "05.json");
    test_instruction!(test_06, "06.json");
    test_instruction!(test_07, "07.json");
    test_instruction!(test_08, "08.json");
    test_instruction!(test_09, "09.json");
    test_instruction!(test_0A, "0a.json");
    test_instruction!(test_0B, "0b.json");
    test_instruction!(test_0C, "0c.json");
    test_instruction!(test_0D, "0d.json");
    test_instruction!(test_0E, "0e.json");
    test_instruction!(test_0F, "0f.json");

    // 0x1x
    test_instruction!(test_10, "10.json");
    test_instruction!(test_11, "11.json");
    test_instruction!(test_12, "12.json");
    test_instruction!(test_13, "13.json");
    test_instruction!(test_14, "14.json");
    test_instruction!(test_15, "15.json");
    test_instruction!(test_16, "16.json");
    test_instruction!(test_17, "17.json");
    test_instruction!(test_18, "18.json");
    test_instruction!(test_19, "19.json");
    test_instruction!(test_1A, "1a.json");
    test_instruction!(test_1B, "1b.json");
    test_instruction!(test_1C, "1c.json");
    test_instruction!(test_1D, "1d.json");
    test_instruction!(test_1E, "1e.json");
    test_instruction!(test_1F, "1f.json");

    // 0x2x
    test_instruction!(test_20, "20.json");
    test_instruction!(test_21, "21.json");
    test_instruction!(test_22, "22.json");
    test_instruction!(test_23, "23.json");
    test_instruction!(test_24, "24.json");
    test_instruction!(test_25, "25.json");
    test_instruction!(test_26, "26.json");
    test_instruction!(test_27, "27.json");
    test_instruction!(test_28, "28.json");
    test_instruction!(test_29, "29.json");
    test_instruction!(test_2A, "2a.json");
    test_instruction!(test_2B, "2b.json");
    test_instruction!(test_2C, "2c.json");
    test_instruction!(test_2D, "2d.json");
    test_instruction!(test_2E, "2e.json");
    test_instruction!(test_2F, "2f.json");

    // 0x3x
    test_instruction!(test_30, "30.json");
    test_instruction!(test_31, "31.json");
    test_instruction!(test_32, "32.json");
    test_instruction!(test_33, "33.json");
    test_instruction!(test_34, "34.json");
    test_instruction!(test_35, "35.json");
    test_instruction!(test_36, "36.json");
    test_instruction!(test_37, "37.json");
    test_instruction!(test_38, "38.json");
    test_instruction!(test_39, "39.json");
    test_instruction!(test_3A, "3a.json");
    test_instruction!(test_3B, "3b.json");
    test_instruction!(test_3C, "3c.json");
    test_instruction!(test_3D, "3d.json");
    test_instruction!(test_3E, "3e.json");
    test_instruction!(test_3F, "3f.json");

    // 0x4x
    test_instruction!(test_40, "40.json");
    test_instruction!(test_41, "41.json");
    test_instruction!(test_42, "42.json");
    test_instruction!(test_43, "43.json");
    test_instruction!(test_44, "44.json");
    test_instruction!(test_45, "45.json");
    test_instruction!(test_46, "46.json");
    test_instruction!(test_47, "47.json");
    test_instruction!(test_48, "48.json");
    test_instruction!(test_49, "49.json");
    test_instruction!(test_4A, "4a.json");
    test_instruction!(test_4B, "4b.json");
    test_instruction!(test_4C, "4c.json");
    test_instruction!(test_4D, "4d.json");
    test_instruction!(test_4E, "4e.json");
    test_instruction!(test_4F, "4f.json");

    // 0x5x
    test_instruction!(test_50, "50.json");
    test_instruction!(test_51, "51.json");
    test_instruction!(test_52, "52.json");
    test_instruction!(test_53, "53.json");
    test_instruction!(test_54, "54.json");
    test_instruction!(test_55, "55.json");
    test_instruction!(test_56, "56.json");
    test_instruction!(test_57, "57.json");
    test_instruction!(test_58, "58.json");
    test_instruction!(test_59, "59.json");
    test_instruction!(test_5A, "5a.json");
    test_instruction!(test_5B, "5b.json");
    test_instruction!(test_5C, "5c.json");
    test_instruction!(test_5D, "5d.json");
    test_instruction!(test_5E, "5e.json");
    test_instruction!(test_5F, "5f.json");

    // 0x6x
    test_instruction!(test_60, "60.json");
    test_instruction!(test_61, "61.json");
    test_instruction!(test_62, "62.json");
    test_instruction!(test_63, "63.json");
    test_instruction!(test_64, "64.json");
    test_instruction!(test_65, "65.json");
    test_instruction!(test_66, "66.json");
    test_instruction!(test_67, "67.json");
    test_instruction!(test_68, "68.json");
    test_instruction!(test_69, "69.json");
    test_instruction!(test_6A, "6a.json");
    test_instruction!(test_6B, "6b.json");
    test_instruction!(test_6C, "6c.json");
    test_instruction!(test_6D, "6d.json");
    test_instruction!(test_6E, "6e.json");
    test_instruction!(test_6F, "6f.json");

    // 0x7x
    test_instruction!(test_70, "70.json");
    test_instruction!(test_71, "71.json");
    test_instruction!(test_72, "72.json");
    test_instruction!(test_73, "73.json");
    test_instruction!(test_74, "74.json");
    test_instruction!(test_75, "75.json");
    test_instruction!(test_76, "76.json");
    test_instruction!(test_77, "77.json");
    test_instruction!(test_78, "78.json");
    test_instruction!(test_79, "79.json");
    test_instruction!(test_7A, "7a.json");
    test_instruction!(test_7B, "7b.json");
    test_instruction!(test_7C, "7c.json");
    test_instruction!(test_7D, "7d.json");
    test_instruction!(test_7E, "7e.json");
    test_instruction!(test_7F, "7f.json");

    // 0x8x
    test_instruction!(test_80, "80.json");
    test_instruction!(test_81, "81.json");
    test_instruction!(test_82, "82.json");
    test_instruction!(test_83, "83.json");
    test_instruction!(test_84, "84.json");
    test_instruction!(test_85, "85.json");
    test_instruction!(test_86, "86.json");
    test_instruction!(test_87, "87.json");
    test_instruction!(test_88, "88.json");
    test_instruction!(test_89, "89.json");
    test_instruction!(test_8A, "8a.json");
    test_instruction!(test_8B, "8b.json");
    test_instruction!(test_8C, "8c.json");
    test_instruction!(test_8D, "8d.json");
    test_instruction!(test_8E, "8e.json");
    test_instruction!(test_8F, "8f.json");

    // 0x9x
    test_instruction!(test_90, "90.json");
    test_instruction!(test_91, "91.json");
    test_instruction!(test_92, "92.json");
    test_instruction!(test_93, "93.json");
    test_instruction!(test_94, "94.json");
    test_instruction!(test_95, "95.json");
    test_instruction!(test_96, "96.json");
    test_instruction!(test_97, "97.json");
    test_instruction!(test_98, "98.json");
    test_instruction!(test_99, "99.json");
    test_instruction!(test_9A, "9a.json");
    test_instruction!(test_9B, "9b.json");
    test_instruction!(test_9C, "9c.json");
    test_instruction!(test_9D, "9d.json");
    test_instruction!(test_9E, "9e.json");
    test_instruction!(test_9F, "9f.json");

    // 0xAx
    test_instruction!(test_A0, "a0.json");
    test_instruction!(test_A1, "a1.json");
    test_instruction!(test_A2, "a2.json");
    test_instruction!(test_A3, "a3.json");
    test_instruction!(test_A4, "a4.json");
    test_instruction!(test_A5, "a5.json");
    test_instruction!(test_A6, "a6.json");
    test_instruction!(test_A7, "a7.json");
    test_instruction!(test_A8, "a8.json");
    test_instruction!(test_A9, "a9.json");
    test_instruction!(test_AA, "aa.json");
    test_instruction!(test_AB, "ab.json");
    test_instruction!(test_AC, "ac.json");
    test_instruction!(test_AD, "ad.json");
    test_instruction!(test_AE, "ae.json");
    test_instruction!(test_AF, "af.json");

    // 0xBx
    test_instruction!(test_B0, "b0.json");
    test_instruction!(test_B1, "b1.json");
    test_instruction!(test_B2, "b2.json");
    test_instruction!(test_B3, "b3.json");
    test_instruction!(test_B4, "b4.json");
    test_instruction!(test_B5, "b5.json");
    test_instruction!(test_B6, "b6.json");
    test_instruction!(test_B7, "b7.json");
    test_instruction!(test_B8, "b8.json");
    test_instruction!(test_B9, "b9.json");
    test_instruction!(test_BA, "ba.json");
    test_instruction!(test_BB, "bb.json");
    test_instruction!(test_BC, "bc.json");
    test_instruction!(test_BD, "bd.json");
    test_instruction!(test_BE, "be.json");
    test_instruction!(test_BF, "bf.json");

    // 0xCx
    test_instruction!(test_C0, "c0.json");
    test_instruction!(test_C1, "c1.json");
    test_instruction!(test_C2, "c2.json");
    test_instruction!(test_C3, "c3.json");
    test_instruction!(test_C4, "c4.json");
    test_instruction!(test_C5, "c5.json");
    test_instruction!(test_C6, "c6.json");
    test_instruction!(test_C7, "c7.json");
    test_instruction!(test_C8, "c8.json");
    test_instruction!(test_C9, "c9.json");
    test_instruction!(test_CA, "ca.json");
    test_instruction!(test_CC, "cc.json");
    test_instruction!(test_CD, "cd.json");
    test_instruction!(test_CE, "ce.json");
    test_instruction!(test_CF, "cf.json");

    // 0xDx
    test_instruction!(test_D0, "d0.json");
    test_instruction!(test_D1, "d1.json");
    test_instruction!(test_D2, "d2.json");
    test_instruction!(test_D4, "d4.json");
    test_instruction!(test_D5, "d5.json");
    test_instruction!(test_D6, "d6.json");
    test_instruction!(test_D7, "d7.json");
    test_instruction!(test_D8, "d8.json");
    test_instruction!(test_D9, "d9.json");
    test_instruction!(test_DA, "da.json");
    test_instruction!(test_DC, "dc.json");
    test_instruction!(test_DE, "de.json");
    test_instruction!(test_DF, "df.json");

    // 0xEx
    test_instruction!(test_E0, "e0.json");
    test_instruction!(test_E1, "e1.json");
    //TODO: re-enable after handling the memory mapping for the address range correctly
    //test_instruction!(test_E2, "e2.json");
    test_instruction!(test_E5, "e5.json");
    test_instruction!(test_E6, "e6.json");
    test_instruction!(test_E7, "e7.json");
    test_instruction!(test_E8, "e8.json");
    test_instruction!(test_E9, "e9.json");
    test_instruction!(test_EA, "ea.json");
    test_instruction!(test_EE, "ee.json");
    test_instruction!(test_EF, "ef.json");

    // 0xFx
    test_instruction!(test_F0, "f0.json");
    test_instruction!(test_F1, "f1.json");
    //TODO: re-enable after handling the memory mapping for the address range correctly
    //test_instruction!(test_F2, "f2.json");
    test_instruction!(test_F3, "f3.json");
    test_instruction!(test_F5, "f5.json");
    test_instruction!(test_F6, "f6.json");
    test_instruction!(test_F7, "f7.json");
    test_instruction!(test_F8, "f8.json");
    test_instruction!(test_F9, "f9.json");
    test_instruction!(test_FA, "fa.json");
    test_instruction!(test_FB, "fb.json");
    test_instruction!(test_FE, "fe.json");
    test_instruction!(test_FF, "ff.json");

    // 0xCB0x
    test_prefixed_instruction!(test_CB00, "cb 00.json");
    test_prefixed_instruction!(test_CB01, "cb 01.json");
    test_prefixed_instruction!(test_CB02, "cb 02.json");
    test_prefixed_instruction!(test_CB03, "cb 03.json");
    test_prefixed_instruction!(test_CB04, "cb 04.json");
    test_prefixed_instruction!(test_CB05, "cb 05.json");
    test_prefixed_instruction!(test_CB06, "cb 06.json");
    test_prefixed_instruction!(test_CB07, "cb 07.json");
    test_prefixed_instruction!(test_CB08, "cb 08.json");
    test_prefixed_instruction!(test_CB09, "cb 09.json");
    test_prefixed_instruction!(test_CB0A, "cb 0A.json");
    test_prefixed_instruction!(test_CB0B, "cb 0B.json");
    test_prefixed_instruction!(test_CB0C, "cb 0C.json");
    test_prefixed_instruction!(test_CB0D, "cb 0D.json");
    test_prefixed_instruction!(test_CB0E, "cb 0E.json");
    test_prefixed_instruction!(test_CB0F, "cb 0F.json");

    // 0xCB1x
    test_prefixed_instruction!(test_CB10, "cb 10.json");
    test_prefixed_instruction!(test_CB11, "cb 11.json");
    test_prefixed_instruction!(test_CB12, "cb 12.json");
    test_prefixed_instruction!(test_CB13, "cb 13.json");
    test_prefixed_instruction!(test_CB14, "cb 14.json");
    test_prefixed_instruction!(test_CB15, "cb 15.json");
    test_prefixed_instruction!(test_CB16, "cb 16.json");
    test_prefixed_instruction!(test_CB17, "cb 17.json");
    test_prefixed_instruction!(test_CB18, "cb 18.json");
    test_prefixed_instruction!(test_CB19, "cb 19.json");
    test_prefixed_instruction!(test_CB1A, "cb 1A.json");
    test_prefixed_instruction!(test_CB1B, "cb 1B.json");
    test_prefixed_instruction!(test_CB1C, "cb 1C.json");
    test_prefixed_instruction!(test_CB1D, "cb 1D.json");
    test_prefixed_instruction!(test_CB1E, "cb 1E.json");
    test_prefixed_instruction!(test_CB1F, "cb 1F.json");

    // 0xCB2x
    test_prefixed_instruction!(test_CB20, "cb 20.json");
    test_prefixed_instruction!(test_CB21, "cb 21.json");
    test_prefixed_instruction!(test_CB22, "cb 22.json");
    test_prefixed_instruction!(test_CB23, "cb 23.json");
    test_prefixed_instruction!(test_CB24, "cb 24.json");
    test_prefixed_instruction!(test_CB25, "cb 25.json");
    test_prefixed_instruction!(test_CB26, "cb 26.json");
    test_prefixed_instruction!(test_CB27, "cb 27.json");
    test_prefixed_instruction!(test_CB28, "cb 28.json");
    test_prefixed_instruction!(test_CB29, "cb 29.json");
    test_prefixed_instruction!(test_CB2A, "cb 2A.json");
    test_prefixed_instruction!(test_CB2B, "cb 2B.json");
    test_prefixed_instruction!(test_CB2C, "cb 2C.json");
    test_prefixed_instruction!(test_CB2D, "cb 2D.json");
    test_prefixed_instruction!(test_CB2E, "cb 2E.json");
    test_prefixed_instruction!(test_CB2F, "cb 2F.json");

    // 0xCB3x
    test_prefixed_instruction!(test_CB30, "cb 30.json");
    test_prefixed_instruction!(test_CB31, "cb 31.json");
    test_prefixed_instruction!(test_CB32, "cb 32.json");
    test_prefixed_instruction!(test_CB33, "cb 33.json");
    test_prefixed_instruction!(test_CB34, "cb 34.json");
    test_prefixed_instruction!(test_CB35, "cb 35.json");
    test_prefixed_instruction!(test_CB36, "cb 36.json");
    test_prefixed_instruction!(test_CB37, "cb 37.json");
    test_prefixed_instruction!(test_CB38, "cb 38.json");
    test_prefixed_instruction!(test_CB39, "cb 39.json");
    test_prefixed_instruction!(test_CB3A, "cb 3A.json");
    test_prefixed_instruction!(test_CB3B, "cb 3B.json");
    test_prefixed_instruction!(test_CB3C, "cb 3C.json");
    test_prefixed_instruction!(test_CB3D, "cb 3D.json");
    test_prefixed_instruction!(test_CB3E, "cb 3E.json");
    test_prefixed_instruction!(test_CB3F, "cb 3F.json");

    // 0xCB4x
    test_prefixed_instruction!(test_CB40, "cb 40.json");
    test_prefixed_instruction!(test_CB41, "cb 41.json");
    test_prefixed_instruction!(test_CB42, "cb 42.json");
    test_prefixed_instruction!(test_CB43, "cb 43.json");
    test_prefixed_instruction!(test_CB44, "cb 44.json");
    test_prefixed_instruction!(test_CB45, "cb 45.json");
    test_prefixed_instruction!(test_CB46, "cb 46.json");
    test_prefixed_instruction!(test_CB47, "cb 47.json");
    test_prefixed_instruction!(test_CB48, "cb 48.json");
    test_prefixed_instruction!(test_CB49, "cb 49.json");
    test_prefixed_instruction!(test_CB4A, "cb 4A.json");
    test_prefixed_instruction!(test_CB4B, "cb 4B.json");
    test_prefixed_instruction!(test_CB4C, "cb 4C.json");
    test_prefixed_instruction!(test_CB4D, "cb 4D.json");
    test_prefixed_instruction!(test_CB4E, "cb 4E.json");
    test_prefixed_instruction!(test_CB4F, "cb 4F.json");

    // 0xCB5x
    test_prefixed_instruction!(test_CB50, "cb 50.json");
    test_prefixed_instruction!(test_CB51, "cb 51.json");
    test_prefixed_instruction!(test_CB52, "cb 52.json");
    test_prefixed_instruction!(test_CB53, "cb 53.json");
    test_prefixed_instruction!(test_CB54, "cb 54.json");
    test_prefixed_instruction!(test_CB55, "cb 55.json");
    test_prefixed_instruction!(test_CB56, "cb 56.json");
    test_prefixed_instruction!(test_CB57, "cb 57.json");
    test_prefixed_instruction!(test_CB58, "cb 58.json");
    test_prefixed_instruction!(test_CB59, "cb 59.json");
    test_prefixed_instruction!(test_CB5A, "cb 5A.json");
    test_prefixed_instruction!(test_CB5B, "cb 5B.json");
    test_prefixed_instruction!(test_CB5C, "cb 5C.json");
    test_prefixed_instruction!(test_CB5D, "cb 5D.json");
    test_prefixed_instruction!(test_CB5E, "cb 5E.json");
    test_prefixed_instruction!(test_CB5F, "cb 5F.json");

    // 0xCB6x
    test_prefixed_instruction!(test_CB60, "cb 60.json");
    test_prefixed_instruction!(test_CB61, "cb 61.json");
    test_prefixed_instruction!(test_CB62, "cb 62.json");
    test_prefixed_instruction!(test_CB63, "cb 63.json");
    test_prefixed_instruction!(test_CB64, "cb 64.json");
    test_prefixed_instruction!(test_CB65, "cb 65.json");
    test_prefixed_instruction!(test_CB66, "cb 66.json");
    test_prefixed_instruction!(test_CB67, "cb 67.json");
    test_prefixed_instruction!(test_CB68, "cb 68.json");
    test_prefixed_instruction!(test_CB69, "cb 69.json");
    test_prefixed_instruction!(test_CB6A, "cb 6A.json");
    test_prefixed_instruction!(test_CB6B, "cb 6B.json");
    test_prefixed_instruction!(test_CB6C, "cb 6C.json");
    test_prefixed_instruction!(test_CB6D, "cb 6D.json");
    test_prefixed_instruction!(test_CB6E, "cb 6E.json");
    test_prefixed_instruction!(test_CB6F, "cb 6F.json");

    // 0xCB7x
    test_prefixed_instruction!(test_CB70, "cb 70.json");
    test_prefixed_instruction!(test_CB71, "cb 71.json");
    test_prefixed_instruction!(test_CB72, "cb 72.json");
    test_prefixed_instruction!(test_CB73, "cb 73.json");
    test_prefixed_instruction!(test_CB74, "cb 74.json");
    test_prefixed_instruction!(test_CB75, "cb 75.json");
    test_prefixed_instruction!(test_CB76, "cb 76.json");
    test_prefixed_instruction!(test_CB77, "cb 77.json");
    test_prefixed_instruction!(test_CB78, "cb 78.json");
    test_prefixed_instruction!(test_CB79, "cb 79.json");
    test_prefixed_instruction!(test_CB7A, "cb 7A.json");
    test_prefixed_instruction!(test_CB7B, "cb 7B.json");
    test_prefixed_instruction!(test_CB7C, "cb 7C.json");
    test_prefixed_instruction!(test_CB7D, "cb 7D.json");
    test_prefixed_instruction!(test_CB7E, "cb 7E.json");
    test_prefixed_instruction!(test_CB7F, "cb 7F.json");

    // 0xCB8x
    test_prefixed_instruction!(test_CB80, "cb 80.json");
    test_prefixed_instruction!(test_CB81, "cb 81.json");
    test_prefixed_instruction!(test_CB82, "cb 82.json");
    test_prefixed_instruction!(test_CB83, "cb 83.json");
    test_prefixed_instruction!(test_CB84, "cb 84.json");
    test_prefixed_instruction!(test_CB85, "cb 85.json");
    test_prefixed_instruction!(test_CB86, "cb 86.json");
    test_prefixed_instruction!(test_CB87, "cb 87.json");
    test_prefixed_instruction!(test_CB88, "cb 88.json");
    test_prefixed_instruction!(test_CB89, "cb 89.json");
    test_prefixed_instruction!(test_CB8A, "cb 8A.json");
    test_prefixed_instruction!(test_CB8B, "cb 8B.json");
    test_prefixed_instruction!(test_CB8C, "cb 8C.json");
    test_prefixed_instruction!(test_CB8D, "cb 8D.json");
    test_prefixed_instruction!(test_CB8E, "cb 8E.json");
    test_prefixed_instruction!(test_CB8F, "cb 8F.json");

    // 0xCB9x
    test_prefixed_instruction!(test_CB90, "cb 90.json");
    test_prefixed_instruction!(test_CB91, "cb 91.json");
    test_prefixed_instruction!(test_CB92, "cb 92.json");
    test_prefixed_instruction!(test_CB93, "cb 93.json");
    test_prefixed_instruction!(test_CB94, "cb 94.json");
    test_prefixed_instruction!(test_CB95, "cb 95.json");
    test_prefixed_instruction!(test_CB96, "cb 96.json");
    test_prefixed_instruction!(test_CB97, "cb 97.json");
    test_prefixed_instruction!(test_CB98, "cb 98.json");
    test_prefixed_instruction!(test_CB99, "cb 99.json");
    test_prefixed_instruction!(test_CB9A, "cb 9A.json");
    test_prefixed_instruction!(test_CB9B, "cb 9B.json");
    test_prefixed_instruction!(test_CB9C, "cb 9C.json");
    test_prefixed_instruction!(test_CB9D, "cb 9D.json");
    test_prefixed_instruction!(test_CB9E, "cb 9E.json");
    test_prefixed_instruction!(test_CB9F, "cb 9F.json");

    // 0xCBAx
    test_prefixed_instruction!(test_CBA0, "cb A0.json");
    test_prefixed_instruction!(test_CBA1, "cb A1.json");
    test_prefixed_instruction!(test_CBA2, "cb A2.json");
    test_prefixed_instruction!(test_CBA3, "cb A3.json");
    test_prefixed_instruction!(test_CBA4, "cb A4.json");
    test_prefixed_instruction!(test_CBA5, "cb A5.json");
    test_prefixed_instruction!(test_CBA6, "cb A6.json");
    test_prefixed_instruction!(test_CBA7, "cb A7.json");
    test_prefixed_instruction!(test_CBA8, "cb A8.json");
    test_prefixed_instruction!(test_CBA9, "cb A9.json");
    test_prefixed_instruction!(test_CBAA, "cb AA.json");
    test_prefixed_instruction!(test_CBAB, "cb AB.json");
    test_prefixed_instruction!(test_CBAC, "cb AC.json");
    test_prefixed_instruction!(test_CBAD, "cb AD.json");
    test_prefixed_instruction!(test_CBAE, "cb AE.json");
    test_prefixed_instruction!(test_CBAF, "cb AF.json");

    // 0xCBBx
    test_prefixed_instruction!(test_CBB0, "cb B0.json");
    test_prefixed_instruction!(test_CBB1, "cb B1.json");
    test_prefixed_instruction!(test_CBB2, "cb B2.json");
    test_prefixed_instruction!(test_CBB3, "cb B3.json");
    test_prefixed_instruction!(test_CBB4, "cb B4.json");
    test_prefixed_instruction!(test_CBB5, "cb B5.json");
    test_prefixed_instruction!(test_CBB6, "cb B6.json");
    test_prefixed_instruction!(test_CBB7, "cb B7.json");
    test_prefixed_instruction!(test_CBB8, "cb B8.json");
    test_prefixed_instruction!(test_CBB9, "cb B9.json");
    test_prefixed_instruction!(test_CBBA, "cb BA.json");
    test_prefixed_instruction!(test_CBBB, "cb BB.json");
    test_prefixed_instruction!(test_CBBC, "cb BC.json");
    test_prefixed_instruction!(test_CBBD, "cb BD.json");
    test_prefixed_instruction!(test_CBBE, "cb BE.json");
    test_prefixed_instruction!(test_CBBF, "cb BF.json");

    // 0xCBCx
    test_prefixed_instruction!(test_CBC0, "cb C0.json");
    test_prefixed_instruction!(test_CBC1, "cb C1.json");
    test_prefixed_instruction!(test_CBC2, "cb C2.json");
    test_prefixed_instruction!(test_CBC3, "cb C3.json");
    test_prefixed_instruction!(test_CBC4, "cb C4.json");
    test_prefixed_instruction!(test_CBC5, "cb C5.json");
    test_prefixed_instruction!(test_CBC6, "cb C6.json");
    test_prefixed_instruction!(test_CBC7, "cb C7.json");
    test_prefixed_instruction!(test_CBC8, "cb C8.json");
    test_prefixed_instruction!(test_CBC9, "cb C9.json");
    test_prefixed_instruction!(test_CBCA, "cb CA.json");
    test_prefixed_instruction!(test_CBCB, "cb CB.json");
    test_prefixed_instruction!(test_CBCC, "cb CC.json");
    test_prefixed_instruction!(test_CBCD, "cb CD.json");
    test_prefixed_instruction!(test_CBCE, "cb CE.json");
    test_prefixed_instruction!(test_CBCF, "cb CF.json");

    // 0xCBDx
    test_prefixed_instruction!(test_CBD0, "cb D0.json");
    test_prefixed_instruction!(test_CBD1, "cb D1.json");
    test_prefixed_instruction!(test_CBD2, "cb D2.json");
    test_prefixed_instruction!(test_CBD3, "cb D3.json");
    test_prefixed_instruction!(test_CBD4, "cb D4.json");
    test_prefixed_instruction!(test_CBD5, "cb D5.json");
    test_prefixed_instruction!(test_CBD6, "cb D6.json");
    test_prefixed_instruction!(test_CBD7, "cb D7.json");
    test_prefixed_instruction!(test_CBD8, "cb D8.json");
    test_prefixed_instruction!(test_CBD9, "cb D9.json");
    test_prefixed_instruction!(test_CBDA, "cb DA.json");
    test_prefixed_instruction!(test_CBDB, "cb DB.json");
    test_prefixed_instruction!(test_CBDC, "cb DC.json");
    test_prefixed_instruction!(test_CBDD, "cb DD.json");
    test_prefixed_instruction!(test_CBDE, "cb DE.json");
    test_prefixed_instruction!(test_CBDF, "cb DF.json");

    // 0xCBEx
    test_prefixed_instruction!(test_CBE0, "cb E0.json");
    test_prefixed_instruction!(test_CBE1, "cb E1.json");
    test_prefixed_instruction!(test_CBE2, "cb E2.json");
    test_prefixed_instruction!(test_CBE3, "cb E3.json");
    test_prefixed_instruction!(test_CBE4, "cb E4.json");
    test_prefixed_instruction!(test_CBE5, "cb E5.json");
    test_prefixed_instruction!(test_CBE6, "cb E6.json");
    test_prefixed_instruction!(test_CBE7, "cb E7.json");
    test_prefixed_instruction!(test_CBE8, "cb E8.json");
    test_prefixed_instruction!(test_CBE9, "cb E9.json");
    test_prefixed_instruction!(test_CBEA, "cb EA.json");
    test_prefixed_instruction!(test_CBEB, "cb EB.json");
    test_prefixed_instruction!(test_CBEC, "cb EC.json");
    test_prefixed_instruction!(test_CBED, "cb ED.json");
    test_prefixed_instruction!(test_CBEE, "cb EE.json");
    test_prefixed_instruction!(test_CBEF, "cb EF.json");

    // 0xCBFx
    test_prefixed_instruction!(test_CBF0, "cb F0.json");
    test_prefixed_instruction!(test_CBF1, "cb F1.json");
    test_prefixed_instruction!(test_CBF2, "cb F2.json");
    test_prefixed_instruction!(test_CBF3, "cb F3.json");
    test_prefixed_instruction!(test_CBF4, "cb F4.json");
    test_prefixed_instruction!(test_CBF5, "cb F5.json");
    test_prefixed_instruction!(test_CBF6, "cb F6.json");
    test_prefixed_instruction!(test_CBF7, "cb F7.json");
    test_prefixed_instruction!(test_CBF8, "cb F8.json");
    test_prefixed_instruction!(test_CBF9, "cb F9.json");
    test_prefixed_instruction!(test_CBFA, "cb FA.json");
    test_prefixed_instruction!(test_CBFB, "cb FB.json");
    test_prefixed_instruction!(test_CBFC, "cb FC.json");
    test_prefixed_instruction!(test_CBFD, "cb FD.json");
    test_prefixed_instruction!(test_CBFE, "cb FE.json");
    test_prefixed_instruction!(test_CBFF, "cb FF.json");
}
