use crate::mem::Memory;

use bounded_vec_deque::BoundedVecDeque;
use std::{collections::VecDeque, fmt::Display};

/// Represents the registers on the cpu. Allows for easy manipulation of combined 16-bit registers as well.
#[derive(Clone, Debug, Default)]
struct Registers {
    /// A register.
    a: u8,
    /// B register.
    b: u8,
    /// C register.
    c: u8,
    /// D register.
    d: u8,
    /// E register.
    e: u8,
    /// Special flags register.
    f: Flags,
    /// H register.
    h: u8,
    /// L register.
    l: u8,
    /// Program counter.
    pc: u16,
    /// Srack pointer.
    sp: u16,
}

impl Registers {
    /// Returns the combined value of the a and f registers.
    fn af(&self) -> u16 {
        let f: u8 = self.f.into();

        (self.a as u16) << 8 | f as u16
    }
    /// Sets the values of the a and f registers by treating them as one 16 byte value.
    fn set_af(&mut self, value: u16) {
        self.a = ((value & 0xFF00) >> 8) as u8;
        self.f = ((value & 0xFF) as u8).into();
    }
    /// Returns the combined value of the b and c registers.
    fn bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }
    /// Sets the values of the b and c registers by treating them as one 16 byte value.
    fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0xFF) as u8;
    }
    /// Returns the combined value of the d and e registers.
    fn de(&self) -> u16 {
        (self.d as u16) << 8 | self.e as u16
    }
    /// Sets the values of the d and e registers by treating them as one 16 byte value.
    fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0xFF) as u8;
    }
    /// Returns the combined value of the h and l registers.
    fn hl(&self) -> u16 {
        (self.h as u16) << 8 | self.l as u16
    }
    /// Sets the values of the h and l registers by treating them as one 16 byte value.
    fn set_hl(&mut self, value: u16) {
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
struct Flags(u8);

impl Flags {
    /// Creates a new default [`Flags`].
    fn new() -> Self {
        Self::default()
    }
    /// Retrieves the current status of the carry flag.
    fn c(&self) -> bool {
        (self.0 >> FLAGS_CARRY_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the carry flag.
    fn set_c(&mut self, on: bool) {
        self.set_flag(FLAGS_CARRY_BIT_POSITION, on);
    }
    /// Retrieves the current status of the half carry flag.
    fn h(&self) -> bool {
        (self.0 >> FLAGS_HALF_CARRY_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the half carry flag.
    fn set_h(&mut self, on: bool) {
        self.set_flag(FLAGS_HALF_CARRY_BIT_POSITION, on);
    }
    /// Retrieves the current status of the subtract flag.
    fn n(&self) -> bool {
        (self.0 >> FLAGS_SUBTRACT_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the substract flag.
    fn set_n(&mut self, on: bool) {
        self.set_flag(FLAGS_SUBTRACT_BIT_POSITION, on);
    }
    /// Retrieves the current status of the zero flag.
    fn z(&self) -> bool {
        (self.0 >> FLAGS_ZERO_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the zero flag.
    fn set_z(&mut self, on: bool) {
        self.set_flag(FLAGS_ZERO_BIT_POSITION, on);
    }
    /// Sets the status of the flag at the given position.
    fn set_flag(&mut self, pos: u8, on: bool) {
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
        Self(value)
    }
}

impl From<Flags> for u8 {
    /// Converts the given [`Flags`] into a [`u8`].
    fn from(value: Flags) -> Self {
        value.0
    }
}

/// Enumeration of the flag registers available for the conditional jump relative cpu instruction.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CondJumpTarget {
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
    /// Writes a string representation of the [`CondJumpTarget`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

/// Enumeration of the target registers available to be manipulated by the cpu instructions.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Target {
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
enum Target8Bit {
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
enum Target16Bit {
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

/// Enumeration of the operations the [`Cpu`] is capable of executing.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Operation {
    /// Adds the value in the [`Target16Bit`] register to the value in the HL register and stores it
    /// back to the HL register.
    ADDHL { target: Target16Bit },
    /// Toggles the value of the carry flag.
    CCF,
    /// Take the one's complement (i.e., flips all the bits) of the value in the `A` register.
    CPL,
    /// Adjust the `A` register to a binary-coded decimal (BCD) number after BCD addition and subtraction operations.
    DAA,
    /// Decrements the value in the [`Target`] register by one.
    DEC { target: Target },
    /// Decrements the value at the memory address specified by value of the `HL` register by 1.
    DECMEM,
    /// Increments the value in the [`Target`] register by one.
    INC { target: Target },
    /// Increments the value at the memory address specified by value of the `HL` register by 1.
    INCMEM,
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
    /// Loads the value in the [`Target16Bit`] register and stores it at the address in memory.
    LDA16 { address: u16, target: Target16Bit },
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
    /// Loads the [`u16`] value from memory and stores it in the [`Target16Bit`] register.
    LDU16 { target: Target16Bit, value: u16 },
    /// Loads the [`u8`] value from memory and stores it in the [`Target8Bit`] register.
    LDU8 { target: Target8Bit, value: u8 },
    /// Loads an immediate u8 and then writes it to the memory address pointed to by the value
    /// in the `HL` register.
    LDU8MEM { value: u8 },
    /// No operation.
    NOP,
    /// Prefix op code which causes the subsequent byte to represent a different set of
    /// instructions.
    PREFIX,
    /// Bit rotate the [`Target`] register left by one, through the carry flag.
    RL { target: Target },
    /// Bit rotate the [`Target`] register left by one, not through the carry flag.
    RLC { target: Target },
    /// Bit rotate the [`Target`] register left by one, through the carry flag.
    RR { target: Target },
    /// Bit rotate the [`Target`] register right by one, not through the carry flag.
    RRC { target: Target },
    /// Sets the carry flag.
    SCF,
    /// Stops the system clock and stop mode is entered.
    STOP,
}

impl Display for Operation {
    /// Writes a string representation of the [`Operation`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::ADDHL { target } => f.write_fmt(format_args!("ADD HL, {}", target)),
            Operation::CCF => f.write_str("CCF"),
            Operation::CPL => f.write_str("CPL"),
            Operation::DAA => f.write_str("DAA"),
            Operation::DEC { target } => f.write_fmt(format_args!("DEC {}", target)),
            Operation::DECMEM => f.write_str("DEC [HL]"),
            Operation::INC { target } => f.write_fmt(format_args!("INC {}", target)),
            Operation::INCMEM => f.write_str("INC [HL]"),
            Operation::JR { offset } => f.write_fmt(format_args!("JR {:#4x}", offset)),
            Operation::JRC { target, offset, .. } => {
                f.write_fmt(format_args!("JR {}, {:#4x}", target, offset))
            }
            Operation::LDA { target } => f.write_fmt(format_args!("LD [{}], A", target)),
            Operation::LDADEC => f.write_fmt(format_args!("LD [HL-], A")),
            Operation::LDAINC => f.write_fmt(format_args!("LD [HL+], A")),
            Operation::LDAMEM { target } => f.write_fmt(format_args!("LD A, [{}]", target)),
            Operation::LDAMEMDEC => f.write_str("LD A, [HL-]"),
            Operation::LDAMEMINC => f.write_str("LD A, [HL+]"),
            Operation::LDA16 { address, target } => {
                f.write_fmt(format_args!("LD [{:#6x}] {}", address, target))
            }
            Operation::LDREG { store, target } => {
                f.write_fmt(format_args!("LD {}, {}", store, target))
            }
            Operation::LDREGMEM { store, target } => {
                f.write_fmt(format_args!("LD {}, [{}]", store, target))
            }
            Operation::LDU16 { target, value } => {
                f.write_fmt(format_args!("LD {}, {:#6x}", target, value))
            }
            Operation::LDU8 { target, value } => {
                f.write_fmt(format_args!("LD {}, {:#4x}", target, value))
            }
            Operation::LDU8MEM { value } => f.write_fmt(format_args!("LD [HL], {:#4x}", value)),
            Operation::NOP => f.write_str("NOP"),
            Operation::PREFIX => f.write_str("PREFIX"),
            Operation::RL { target } => match target {
                Target::A => f.write_str("RLA"),
                _ => f.write_fmt(format_args!("RL {}", target)),
            },
            Operation::RLC { target } => match target {
                Target::A => f.write_str("RLCA"),
                _ => f.write_fmt(format_args!("RLC {}", target)),
            },
            Operation::RR { target } => match target {
                Target::A => f.write_str("RRA"),
                _ => f.write_fmt(format_args!("RR {}", target)),
            },
            Operation::RRC { target } => match target {
                Target::A => f.write_str("RRCA"),
                _ => f.write_fmt(format_args!("RRC {}", target)),
            },
            Operation::SCF => f.write_str("SCF"),
            Operation::STOP => f.write_str("STOP"),
        }
    }
}

/// An instruction that is ready to be executed by the [`Cpu`]. It contains not only the
/// [`Operation`] that should be executed by the cpu but also how wide in bytes the instruction
/// is as well as the number of clock ticks it takes to execute.
#[derive(Clone, Debug, Eq, PartialEq)]
struct Instruction {
    /// Number of bytes that make up the instruction.
    num_bytes: u16,
    /// Number of system clock ticks it takes to execute the instruction.
    clock_ticks: u64,
    /// [`Operation`] which should be executed by the cpu.
    operation: Operation,
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
    /// Creates a new instruction that adds the value in the [`Target16Bit`] register to the
    /// value in the `HL` register and stores the result back to the `HL` register.
    fn add_hl(target: Target16Bit) -> Self {
        Self::new(1, 8, Operation::ADDHL { target })
    }
    /// Creates a new instruction that toggles the value of the carry flag.
    fn ccf() -> Self {
        Self::new(1, 4, Operation::CCF)
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
    /// Creates a new jump relative instruction that advances the program counter by the given
    /// offset.
    fn jr(offset: i8) -> Self {
        Self::new(2, 12, Operation::JR { offset })
    }
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
    /// Creates a new instruction which loads the value in the target register and stores it at
    /// the given address in memory.
    fn ld_a16(address: u16, target: Target16Bit) -> Self {
        Self::new(3, 20, Operation::LDA16 { address, target })
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
    /// Creates a new prefix instruction.
    fn prefix() -> Self {
        Self::new(1, 4, Operation::PREFIX)
    }
    /// Creates a new instruction that bit rotates the value in the [`Target`] register left by
    /// one through the carry flag.
    fn rl(target: Target) -> Self {
        Self::new(1, 4, Operation::RL { target })
    }
    /// Creates a new instruction that bit rotates the value in the `A` register left by one
    /// through the carry flag.
    fn rla() -> Self {
        Self::rl(Target::A)
    }
    /// Creates a new instruction that bit rotates the value in the [`Target`] register left by
    /// one, not through the carry flag.
    fn rlc(target: Target) -> Self {
        Self::new(1, 4, Operation::RLC { target })
    }
    /// Creates a new instruction that bit rotates the value in the `A` register left by one, not
    /// through the carry flag.
    fn rlca() -> Self {
        Self::rlc(Target::A)
    }
    /// Creates a new instruction that bit rotates the value in the [`Target`] register right by
    /// one through the carry flag.
    fn rr(target: Target) -> Self {
        Self::new(1, 4, Operation::RR { target })
    }
    /// Creates a new instruction that bit rotates the value in the `A` register right by one
    /// through the carry flag.
    fn rra() -> Self {
        Self::rr(Target::A)
    }
    /// Creates a new instruction that bit rotates the value in the [`Target`] register right by
    /// one, not through the carry flag.
    fn rrc(target: Target) -> Self {
        Self::new(1, 4, Operation::RRC { target })
    }
    /// Creates a new instruction that bit rotates the value in the `A` register right by one, not
    /// through the carry flag.
    fn rrca() -> Self {
        Self::rrc(Target::A)
    }
    /// Creates a new stop instruction that sets the carry flag.
    fn scf() -> Self {
        Self::new(1, 4, Operation::SCF)
    }
    /// Creates a new stop instruction that is akin to [`Operation::NOP`] for the emulator.
    fn stop() -> Self {
        Self::new(1, 4, Operation::STOP)
    }
}

impl Display for Instruction {
    /// Writes a string representation of the [`Instruction`] to the formatter. This implementation
    /// simply defers to the [`Display`] trait implementation for the [`Operation`] owned by the
    /// [`Instruction`].
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.operation.fmt(f)
    }
}

/// Enumerates the different instruction sets that can be used when creating an [`Instruction`]
/// from an op code.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum InstructionSet {
    /// Default instruction set.
    Standard,
    /// Active when the previous instruction executed was `PREFIX` which has op code 0xCB.
    Prefixed,
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
    registers: Registers,
    /// Indicates the instruction set that should be used when decoding an op code.
    instruction_set: InstructionSet,
    /// Tracks the history of [`Instruction`]s that were executed by the cpu.
    history: BoundedVecDeque<Instruction>,
}

/// Default value for the maximum number of instructions stored in the instruction execution
/// history of the [`Cpu`].
const DEFAULT_CPU_MAX_HISTORY: usize = 250;

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
        }
    }
    /// Reads and executes the next instruction based on the current program counter.
    pub fn step(&mut self, memory: &mut Memory) {
        let op_code = memory.read_u8(self.registers.pc);

        let instruction = match self.instruction_set {
            InstructionSet::Standard => self.decode(op_code, memory),
            InstructionSet::Prefixed => self.decode_prefixed(op_code),
        };

        if let Some(instruction) = instruction {
            self.registers.pc = self.registers.pc.wrapping_add(instruction.num_bytes);

            self.execute(&instruction, memory);

            self.instruction_set = if instruction.operation == Operation::PREFIX {
                InstructionSet::Prefixed
            } else {
                InstructionSet::Standard
            };

            self.history.push_front(instruction);
        } else {
            tracing::warn!("unknown instruction: {:#4x}", op_code);
        };
    }
    /// Transforms the given op code into an [`Instruction`] which can be executed by the [`Cpu`].
    fn decode(&self, op_code: u8, memory: &Memory) -> Option<Instruction> {
        tracing::debug!("decode op code {:#4x}", op_code);

        match op_code {
            // 0x0x
            0x00 => Some(Instruction::nop()),
            0x01 => Some(Instruction::ld_u16(
                Target16Bit::BC,
                memory.read_u16(self.registers.pc.wrapping_add(1)),
            )),
            0x02 => Some(Instruction::ld_a(Target16Bit::BC)),
            0x03 => Some(Instruction::inc_u16(Target::BC)),
            0x04 => Some(Instruction::inc(Target::B)),
            0x05 => Some(Instruction::dec(Target::B)),
            0x06 => Some(Instruction::ld_u8(
                Target8Bit::B,
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0x07 => Some(Instruction::rlca()),
            0x08 => Some(Instruction::ld_a16(
                memory.read_u16(self.registers.pc.wrapping_add(1)),
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
            0x0F => Some(Instruction::rrca()),

            // 0x1x
            0x10 => Some(Instruction::stop()),
            0x11 => Some(Instruction::ld_u16(
                Target16Bit::DE,
                memory.read_u16(self.registers.pc.wrapping_add(1)),
            )),
            0x12 => Some(Instruction::ld_a(Target16Bit::DE)),
            0x13 => Some(Instruction::inc_u16(Target::DE)),
            0x14 => Some(Instruction::inc(Target::D)),
            0x15 => Some(Instruction::dec(Target::D)),
            0x16 => Some(Instruction::ld_u8(
                Target8Bit::D,
                memory.read_u8(self.registers.pc.wrapping_add(1)),
            )),
            0x17 => Some(Instruction::rla()),
            0x18 => Some(Instruction::jr(
                memory.read_i8(self.registers.pc.wrapping_add(1)),
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
            0x1F => Some(Instruction::rra()),

            // 0x2x
            0x20 => Some(Instruction::jrc(
                CondJumpTarget::NZ,
                !self.registers.f.z(),
                memory.read_i8(self.registers.pc.wrapping_add(1)),
            )),
            0x21 => Some(Instruction::ld_u16(
                Target16Bit::HL,
                memory.read_u16(self.registers.pc.wrapping_add(1)),
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
                memory.read_i8(self.registers.pc.wrapping_add(1)),
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
                memory.read_i8(self.registers.pc.wrapping_add(1)),
            )),
            0x31 => Some(Instruction::ld_u16(
                Target16Bit::SP,
                memory.read_u16(self.registers.pc.wrapping_add(1)),
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
                memory.read_i8(self.registers.pc.wrapping_add(1)),
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

            // 0xCx
            0xCB => Some(Instruction::prefix()),
            _ => None,
        }
    }
    /// Transforms the given prefixed op code into an [`Instruction`] which can be executed by the
    /// [`Cpu`]. An op code is prefixed if the preceding op code byte was 0xCB.
    fn decode_prefixed(&self, op_code: u8) -> Option<Instruction> {
        tracing::debug!("decode prefixed op code {:#4x}", op_code);
        todo!()
    }
    /// Executes the given [`Instruction`] returning the new program counter value and whether or
    /// not the op code for the next instruction is prefixed.
    fn execute(&mut self, instruction: &Instruction, memory: &mut Memory) {
        tracing::debug!("execute instruction '{}'", instruction);

        match instruction.operation {
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
            Operation::CCF => {
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(!self.registers.f.c());
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

                if (self.registers.f.n()) {
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
            Operation::LDA16 { address, target } => match target {
                Target16Bit::SP => {
                    let low = self.registers.sp as u8;
                    let high = (self.registers.sp >> 8) as u8;

                    memory.write_u8(address, low);
                    memory.write_u8(address + 1, high);
                }
                _ => todo!(),
            },
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
            Operation::PREFIX => {}
            // TODO: cleanup
            Operation::RL { target } => {
                let mut value = match target {
                    Target::A => &mut self.registers.a,
                    _ => panic!("not implemented"),
                };

                let carry: u8 = self.registers.f.c().into();
                let will_carry = (*value & (1 << 7)) != 0;

                *value = (*value << 1) | carry;

                self.registers.f.set_z(false);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            // TODO: cleanup
            Operation::RLC { target } => {
                let mut value = match target {
                    Target::A => &mut self.registers.a,
                    _ => panic!("not implemented"),
                };

                let truncated_bit = *value & (1 << 7);
                let will_carry = truncated_bit != 0;

                *value = (*value << 1) | (truncated_bit >> 7);

                self.registers.f.set_z(false);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            // TODO: cleanup
            Operation::RR { target } => {
                let mut value = match target {
                    Target::A => &mut self.registers.a,
                    _ => panic!("not implemented"),
                };

                let carry: u8 = self.registers.f.c().into();
                let will_carry = *value & 1 != 0;

                *value = (*value >> 1) | (carry << 7);

                self.registers.f.set_z(false);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            // TODO: cleanup
            Operation::RRC { target } => {
                let mut value = match target {
                    Target::A => &mut self.registers.a,
                    _ => panic!("not implemented"),
                };

                let truncated_bit = *value & 1;
                let will_carry = truncated_bit != 0;

                *value = ((*value) >> 1) | (truncated_bit << 7);

                self.registers.f.set_z(false);
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(will_carry);
            }
            Operation::SCF => {
                self.registers.f.set_n(false);
                self.registers.f.set_h(false);
                self.registers.f.set_c(true);
            }
            Operation::STOP => tracing::debug!("ignore stop instruction"),
            _ => todo!(),
        }
    }
}

/// Detrmines if the addtion of `b` to `a` will cause a half-carry.
fn will_half_carry_add_u8(a: u8, b: u8) -> bool {
    (a & 0x0F) + (b & 0x0F) > 0x0F
}

/// Detrmines if the addtion of `b` to `a` will cause a half-carry. The half carry is calculated on
/// the 11th bit.
fn will_half_carry_add_u16(a: u16, b: u16) -> bool {
    (a & 0x0FFF) + (b & 0x0FFF) > 0x0FFF
}

/// Detrmines if the subtraction of `b` from `a` will cause a half-carry.
fn will_half_carry_sub_u8(a: u8, b: u8) -> bool {
    (((a & 0x0F) as i32) - ((b & 0x0F) as i32)) < 0
}

/// Detrmines if the subtraction of `b` from `a` will cause a half-carry. The half carry is
/// calculated on the 11th bit.
fn will_half_carry_sub_u16(a: u16, b: u16) -> bool {
    (((a & 0x0FFF) as i32) - ((b & 0x0FFF) as i32)) < 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_flags_c() {
        let mut flags = Flags::new();
        assert!(!flags.c());

        flags.set_c(true);
        assert!(flags.c());
        assert_eq!(flags.0, 1 << FLAGS_CARRY_BIT_POSITION);

        flags.set_c(false);
        assert!(!flags.c());
    }

    #[test]
    fn test_flags_h() {
        let mut flags = Flags::new();
        assert!(!flags.h());

        flags.set_h(true);
        assert!(flags.h());
        assert_eq!(flags.0, 1 << FLAGS_HALF_CARRY_BIT_POSITION);

        flags.set_h(false);
        assert!(!flags.h());
    }

    #[test]
    fn test_flags_n() {
        let mut flags = Flags::new();
        assert!(!flags.n());

        println!("flags: {:#4x}", flags.0);
        flags.set_n(true);
        assert!(flags.n());
        assert_eq!(flags.0, 1 << FLAGS_SUBTRACT_BIT_POSITION);

        println!("flags: {:#4x}", flags.0);
        flags.set_n(false);
        assert!(!flags.n());
    }

    #[test]
    fn test_flags_z() {
        let mut flags = Flags::new();
        assert!(!flags.z());

        flags.set_z(true);
        assert!(flags.z());
        assert_eq!(flags.0, 1 << FLAGS_ZERO_BIT_POSITION);

        flags.set_z(false);
        assert!(!flags.z());
    }

    #[test]
    fn test_cpu_decode_nop() {
        let op_code: u8 = 0x00;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::NOP, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_u16_bc() {
        let op_code: u8 = 0x01;

        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 1);
            memory.write_u8(0x0002, 0);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::BC,
                    value: 1
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 0);
            memory.write_u8(0x0002, 1);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::BC,
                    value: 256,
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 1);
            memory.write_u8(0x0002, 1);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::BC,
                    value: 257,
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_ld_a_bc() {
        let op_code: u8 = 0x02;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDA {
                target: Target16Bit::BC
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_inc_bc() {
        let op_code: u8 = 0x03;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::BC }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_inc_b() {
        let op_code: u8 = 0x04;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::B }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_dec_b() {
        let op_code: u8 = 0x05;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::B }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_u8_b() {
        let op_code: u8 = 0x06;

        let mut memory = Memory::new();
        memory.write_u8(0x0001, 3);

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(2, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDU8 {
                target: Target8Bit::B,
                value: 3
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_rlca() {
        let op_code: u8 = 0x07;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::RLC { target: Target::A }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_a16_sp() {
        let op_code: u8 = 0x08;

        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 1);
            memory.write_u8(0x0002, 0);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(20, instruction.clock_ticks);
            assert_eq!(
                Operation::LDA16 {
                    address: 1,
                    target: Target16Bit::SP
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 0);
            memory.write_u8(0x0002, 1);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(20, instruction.clock_ticks);
            assert_eq!(
                Operation::LDA16 {
                    address: 256,
                    target: Target16Bit::SP,
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 1);
            memory.write_u8(0x0002, 1);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(20, instruction.clock_ticks);
            assert_eq!(
                Operation::LDA16 {
                    address: 257,
                    target: Target16Bit::SP,
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_add_hl_bc() {
        let op_code: u8 = 0x09;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::ADDHL {
                target: Target16Bit::BC
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_a_mem_bc() {
        let op_code: u8 = 0x0A;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDAMEM {
                target: Target16Bit::BC
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_dec_bc() {
        let op_code: u8 = 0x0B;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::BC }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_inc_c() {
        let op_code: u8 = 0x0C;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::C }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_dec_c() {
        let op_code: u8 = 0x0D;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::C }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_u8_c() {
        let op_code: u8 = 0x0E;

        let mut memory = Memory::new();
        memory.write_u8(1, 20);

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(2, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDU8 {
                target: Target8Bit::C,
                value: 20
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_rrca() {
        let op_code: u8 = 0x0F;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::RRC { target: Target::A }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_stop() {
        let op_code: u8 = 0x10;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::STOP, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_u16_de() {
        let op_code: u8 = 0x11;

        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 1);
            memory.write_u8(0x0002, 0);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::DE,
                    value: 1
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 0);
            memory.write_u8(0x0002, 1);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::DE,
                    value: 256,
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 1);
            memory.write_u8(0x0002, 1);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::DE,
                    value: 257,
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_ld_a_de() {
        let op_code: u8 = 0x12;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDA {
                target: Target16Bit::DE
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_inc_de() {
        let op_code: u8 = 0x13;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::DE }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_inc_d() {
        let op_code: u8 = 0x14;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::D }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_dec_d() {
        let op_code: u8 = 0x15;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::D }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_u8_d() {
        let op_code: u8 = 0x16;

        let mut memory = Memory::new();
        memory.write_u8(1, 20);

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(2, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDU8 {
                target: Target8Bit::D,
                value: 20
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_rla() {
        let op_code: u8 = 0x17;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::RL { target: Target::A }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_jr() {
        let op_code: u8 = 0x18;

        {
            let mut memory = Memory::new();
            memory.write_u8(0x01, 2);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(Operation::JR { offset: 2 }, instruction.operation);
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x01, 0xFF);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(Operation::JR { offset: -1 }, instruction.operation);
        }
    }

    #[test]
    fn test_cpu_decode_add_hl_de() {
        let op_code: u8 = 0x19;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::ADDHL {
                target: Target16Bit::DE
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_a_mem_de() {
        let op_code: u8 = 0x1A;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDAMEM {
                target: Target16Bit::DE
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_dec_de() {
        let op_code: u8 = 0x1B;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::DE }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_inc_e() {
        let op_code: u8 = 0x1C;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::E }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_dec_e() {
        let op_code: u8 = 0x1D;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::E }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_u8_e() {
        let op_code: u8 = 0x1E;

        let mut memory = Memory::new();
        memory.write_u8(1, 20);

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(2, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDU8 {
                target: Target8Bit::E,
                value: 20
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_rra() {
        let op_code: u8 = 0x1F;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::RR { target: Target::A }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_jr_nz() {
        let op_code: u8 = 0x20;

        {
            let mut memory = Memory::new();
            memory.write_u8(1, 9);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::JRC {
                    target: CondJumpTarget::NZ,
                    jump: true,
                    offset: 9
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(1, 9);

            let mut cpu = Cpu::new();
            cpu.registers.f.set_z(true);

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(8, instruction.clock_ticks);
            assert_eq!(
                Operation::JRC {
                    target: CondJumpTarget::NZ,
                    jump: false,
                    offset: 9
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_ld_u16_hl() {
        let op_code: u8 = 0x21;

        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 1);
            memory.write_u8(0x0002, 0);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::HL,
                    value: 1
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 0);
            memory.write_u8(0x0002, 1);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::HL,
                    value: 256,
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 1);
            memory.write_u8(0x0002, 1);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::HL,
                    value: 257,
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_ld_a_inc() {
        let op_code: u8 = 0x22;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::LDAINC, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_inc_hl() {
        let op_code: u8 = 0x23;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::HL }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_inc_h() {
        let op_code: u8 = 0x24;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::H }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_dec_h() {
        let op_code: u8 = 0x25;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::H }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_u8_h() {
        let op_code: u8 = 0x26;

        let mut memory = Memory::new();
        memory.write_u8(1, 20);

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(2, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDU8 {
                target: Target8Bit::H,
                value: 20
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_daa() {
        let op_code: u8 = 0x27;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::DAA, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_jr_z() {
        let op_code: u8 = 0x28;

        {
            let mut memory = Memory::new();
            memory.write_u8(1, 9);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(8, instruction.clock_ticks);
            assert_eq!(
                Operation::JRC {
                    target: CondJumpTarget::Z,
                    jump: false,
                    offset: 9
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(1, 9);

            let mut cpu = Cpu::new();
            cpu.registers.f.set_z(true);

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::JRC {
                    target: CondJumpTarget::Z,
                    jump: true,
                    offset: 9
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_add_hl_hl() {
        let op_code: u8 = 0x29;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::ADDHL {
                target: Target16Bit::HL
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_a_mem_inc() {
        let op_code = 0x2A;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::LDAMEMINC, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_dec_hl() {
        let op_code: u8 = 0x2B;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::HL }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_inc_l() {
        let op_code: u8 = 0x2C;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::L }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_dec_l() {
        let op_code: u8 = 0x2D;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::L }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_u8_l() {
        let op_code: u8 = 0x2E;

        let mut memory = Memory::new();
        memory.write_u8(1, 20);

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(2, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDU8 {
                target: Target8Bit::L,
                value: 20
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_cpl() {
        let op_code: u8 = 0x2F;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::CPL, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_jr_nc() {
        let op_code: u8 = 0x30;

        {
            let mut memory = Memory::new();
            memory.write_u8(1, 9);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::JRC {
                    target: CondJumpTarget::NC,
                    jump: true,
                    offset: 9
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(1, 9);

            let mut cpu = Cpu::new();
            cpu.registers.f.set_c(true);

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(8, instruction.clock_ticks);
            assert_eq!(
                Operation::JRC {
                    target: CondJumpTarget::NC,
                    jump: false,
                    offset: 9
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_ld_u16_sp() {
        let op_code: u8 = 0x31;

        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 1);
            memory.write_u8(0x0002, 0);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::SP,
                    value: 1
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 0);
            memory.write_u8(0x0002, 1);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::SP,
                    value: 256,
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(0x0001, 1);
            memory.write_u8(0x0002, 1);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(3, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::LDU16 {
                    target: Target16Bit::SP,
                    value: 257,
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_ld_a_dec() {
        let op_code: u8 = 0x32;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::LDADEC, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_inc_sp() {
        let op_code: u8 = 0x33;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::SP }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_inc_mem() {
        let op_code: u8 = 0x34;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(12, instruction.clock_ticks);
        assert_eq!(Operation::INCMEM, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_dec_mem() {
        let op_code: u8 = 0x35;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(12, instruction.clock_ticks);
        assert_eq!(Operation::DECMEM, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_u8_mem() {
        let op_code: u8 = 0x36;

        let mut memory = Memory::new();
        memory.write_u8(1, 10);

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(2, instruction.num_bytes);
        assert_eq!(12, instruction.clock_ticks);
        assert_eq!(Operation::LDU8MEM { value: 10 }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_scf() {
        let op_code: u8 = 0x37;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::SCF, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_jr_c() {
        let op_code: u8 = 0x38;

        {
            let mut memory = Memory::new();
            memory.write_u8(1, 9);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(8, instruction.clock_ticks);
            assert_eq!(
                Operation::JRC {
                    target: CondJumpTarget::C,
                    jump: false,
                    offset: 9
                },
                instruction.operation
            );
        }
        {
            let mut memory = Memory::new();
            memory.write_u8(1, 9);

            let mut cpu = Cpu::new();
            cpu.registers.f.set_c(true);

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(
                Operation::JRC {
                    target: CondJumpTarget::C,
                    jump: true,
                    offset: 9
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_add_hl_sp() {
        let op_code: u8 = 0x39;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::ADDHL {
                target: Target16Bit::SP
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_a_mem_dec() {
        let op_code = 0x3A;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::LDAMEMDEC, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_dec_sp() {
        let op_code: u8 = 0x3B;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::SP }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_inc_a() {
        let op_code: u8 = 0x3C;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::INC { target: Target::A }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_dec_a() {
        let op_code: u8 = 0x3D;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::DEC { target: Target::A }, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_u8_a() {
        let op_code: u8 = 0x3E;

        let mut memory = Memory::new();
        memory.write_u8(1, 20);

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(2, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDU8 {
                target: Target8Bit::A,
                value: 20
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ccf() {
        let op_code: u8 = 0x3F;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::CCF, instruction.operation);
    }

    #[test]
    fn test_cpu_decode_ld_reg_b_b() {
        let op_code: u8 = 0x40;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::B,
                target: Target8Bit::B
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_b_c() {
        let op_code: u8 = 0x41;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::B,
                target: Target8Bit::C
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_b_d() {
        let op_code: u8 = 0x42;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::B,
                target: Target8Bit::D
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_b_e() {
        let op_code: u8 = 0x43;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::B,
                target: Target8Bit::E
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_b_h() {
        let op_code: u8 = 0x44;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::B,
                target: Target8Bit::H
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_b_l() {
        let op_code: u8 = 0x45;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::B,
                target: Target8Bit::L
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_mem_b_hl() {
        let op_code: u8 = 0x46;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREGMEM {
                store: Target8Bit::B,
                target: Target16Bit::HL
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_b_a() {
        let op_code: u8 = 0x47;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::B,
                target: Target8Bit::A,
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_c_b() {
        let op_code: u8 = 0x48;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::C,
                target: Target8Bit::B
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_c_c() {
        let op_code: u8 = 0x49;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::C,
                target: Target8Bit::C
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_c_d() {
        let op_code: u8 = 0x4A;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::C,
                target: Target8Bit::D
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_c_e() {
        let op_code: u8 = 0x4B;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::C,
                target: Target8Bit::E
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_c_h() {
        let op_code: u8 = 0x4C;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::C,
                target: Target8Bit::H
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_c_l() {
        let op_code: u8 = 0x4D;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::C,
                target: Target8Bit::L
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_mem_c_hl() {
        let op_code: u8 = 0x4E;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREGMEM {
                store: Target8Bit::C,
                target: Target16Bit::HL
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_c_a() {
        let op_code: u8 = 0x4F;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::C,
                target: Target8Bit::A,
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_d_b() {
        let op_code: u8 = 0x50;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::D,
                target: Target8Bit::B
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_d_c() {
        let op_code: u8 = 0x51;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::D,
                target: Target8Bit::C
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_d_d() {
        let op_code: u8 = 0x52;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::D,
                target: Target8Bit::D
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_d_e() {
        let op_code: u8 = 0x53;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::D,
                target: Target8Bit::E
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_d_h() {
        let op_code: u8 = 0x54;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::D,
                target: Target8Bit::H
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_d_l() {
        let op_code: u8 = 0x55;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::D,
                target: Target8Bit::L
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_mem_d_hl() {
        let op_code: u8 = 0x56;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREGMEM {
                store: Target8Bit::D,
                target: Target16Bit::HL
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_d_a() {
        let op_code: u8 = 0x57;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::D,
                target: Target8Bit::A,
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_e_b() {
        let op_code: u8 = 0x58;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::E,
                target: Target8Bit::B
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_e_c() {
        let op_code: u8 = 0x59;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::E,
                target: Target8Bit::C
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_e_d() {
        let op_code: u8 = 0x5A;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::E,
                target: Target8Bit::D
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_e_e() {
        let op_code: u8 = 0x5B;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::E,
                target: Target8Bit::E
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_e_h() {
        let op_code: u8 = 0x5C;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::E,
                target: Target8Bit::H
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_e_l() {
        let op_code: u8 = 0x5D;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::E,
                target: Target8Bit::L
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_mem_e_hl() {
        let op_code: u8 = 0x5E;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREGMEM {
                store: Target8Bit::E,
                target: Target16Bit::HL
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_e_a() {
        let op_code: u8 = 0x5F;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::E,
                target: Target8Bit::A,
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_h_b() {
        let op_code: u8 = 0x60;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::H,
                target: Target8Bit::B
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_h_c() {
        let op_code: u8 = 0x61;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::H,
                target: Target8Bit::C
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_h_d() {
        let op_code: u8 = 0x62;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::H,
                target: Target8Bit::D
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_h_e() {
        let op_code: u8 = 0x63;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::H,
                target: Target8Bit::E
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_h_h() {
        let op_code: u8 = 0x64;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::H,
                target: Target8Bit::H
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_h_l() {
        let op_code: u8 = 0x65;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::H,
                target: Target8Bit::L
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_mem_h_hl() {
        let op_code: u8 = 0x66;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREGMEM {
                store: Target8Bit::H,
                target: Target16Bit::HL
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_h_a() {
        let op_code: u8 = 0x67;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::H,
                target: Target8Bit::A,
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_l_b() {
        let op_code: u8 = 0x68;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::L,
                target: Target8Bit::B
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_l_c() {
        let op_code: u8 = 0x69;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::L,
                target: Target8Bit::C
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_l_d() {
        let op_code: u8 = 0x6A;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::L,
                target: Target8Bit::D
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_l_e() {
        let op_code: u8 = 0x6B;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::L,
                target: Target8Bit::E
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_l_h() {
        let op_code: u8 = 0x6C;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::L,
                target: Target8Bit::H
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_l_l() {
        let op_code: u8 = 0x6D;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::L,
                target: Target8Bit::L
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_mem_l_hl() {
        let op_code: u8 = 0x6E;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREGMEM {
                store: Target8Bit::L,
                target: Target16Bit::HL
            },
            instruction.operation
        );
    }

    #[test]
    fn test_cpu_decode_ld_reg_l_a() {
        let op_code: u8 = 0x6F;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(
            Operation::LDREG {
                store: Target8Bit::L,
                target: Target8Bit::A,
            },
            instruction.operation
        );
    }
    #[test]
    fn test_cpu_decode_prefix() {
        let op_code: u8 = 0xCB;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::PREFIX, instruction.operation);
    }
}

#[cfg(test)]
mod json_tests {
    use super::*;

    use serde::Deserialize;
    use std::path::Path;

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

    /// Loads, deseralizes and executes all of the tests in the JSON file.
    ///
    /// # Panic
    ///
    /// This function will panic if the JSON file cannot be read or if it cannot be successfully
    /// deserialzied into a [`Vec`] of [`Test`].
    fn test_json_file(file_name: &str, op_code: u8) {
        read_json_file(file_name)
            .and_then(deserialize_json)
            .expect("valid test JSON files")
            .into_iter()
            .for_each(|t| execute(op_code, t));
    }

    /// Executes a [`Test`] for the given instruction op code.
    fn execute(op_code: u8, test: Test) {
        println!("execute json test {}", test.name);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();
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

        for byte in test.input.ram {
            if byte.len() != 2 {
                panic!("invalid byte data");
            }

            let address = byte[0];
            let value = byte[1] as u8;

            memory.write_u8(address, value);
        }

        cpu.step(&mut memory);

        assert_eq!(test.output.pc, cpu.registers.pc);
        assert_eq!(test.output.sp, cpu.registers.sp);
        assert_eq!(test.output.a, cpu.registers.a);
        assert_eq!(test.output.b, cpu.registers.b);
        assert_eq!(test.output.c, cpu.registers.c);
        assert_eq!(test.output.d, cpu.registers.d);
        assert_eq!(test.output.e, cpu.registers.e);
        assert_eq!(test.output.f, <Flags as Into<u8>>::into(cpu.registers.f));
        assert_eq!(test.output.h, cpu.registers.h);
        assert_eq!(test.output.l, cpu.registers.l);

        for byte in test.output.ram {
            if byte.len() != 2 {
                panic!("invalid byte data");
            }

            let address = byte[0];
            let expected = byte[1] as u8;

            let actual = memory.read_u8(address);

            assert_eq!(expected, actual);
        }
    }

    /// Macro that allows for easily defining a test function that executes a JSON-based test file.
    macro_rules! test_instruction {
        ($name:ident, $file:expr, $op:expr) => {
            #[test]
            #[ignore]
            #[allow(non_snake_case)]
            fn $name() {
                test_json_file($file, $op)
            }
        };
    }

    // 0x0x
    test_instruction!(test_00, "00.json", 0x00);
    test_instruction!(test_01, "01.json", 0x01);
    test_instruction!(test_02, "02.json", 0x02);
    test_instruction!(test_03, "03.json", 0x03);
    test_instruction!(test_04, "04.json", 0x04);
    test_instruction!(test_05, "05.json", 0x05);
    test_instruction!(test_06, "06.json", 0x06);
    test_instruction!(test_07, "07.json", 0x07);
    test_instruction!(test_08, "08.json", 0x08);
    test_instruction!(test_09, "09.json", 0x09);
    test_instruction!(test_0A, "0a.json", 0x0A);
    test_instruction!(test_0B, "0b.json", 0x0B);
    test_instruction!(test_0C, "0c.json", 0x0C);
    test_instruction!(test_0D, "0d.json", 0x0D);
    test_instruction!(test_0E, "0e.json", 0x0E);
    test_instruction!(test_0F, "0f.json", 0x0F);

    // 0x1x
    test_instruction!(test_10, "10.json", 0x10);
    test_instruction!(test_11, "11.json", 0x11);
    test_instruction!(test_12, "12.json", 0x12);
    test_instruction!(test_13, "13.json", 0x13);
    test_instruction!(test_14, "14.json", 0x14);
    test_instruction!(test_15, "15.json", 0x15);
    test_instruction!(test_16, "16.json", 0x16);
    test_instruction!(test_17, "17.json", 0x17);
    test_instruction!(test_18, "18.json", 0x18);
    test_instruction!(test_19, "19.json", 0x19);
    test_instruction!(test_1A, "1a.json", 0x1A);
    test_instruction!(test_1B, "1b.json", 0x1B);
    test_instruction!(test_1C, "1c.json", 0x1C);
    test_instruction!(test_1D, "1d.json", 0x1D);
    test_instruction!(test_1E, "1e.json", 0x1E);
    test_instruction!(test_1F, "1f.json", 0x1F);

    // 0x2x
    test_instruction!(test_20, "20.json", 0x20);
    test_instruction!(test_21, "21.json", 0x21);
    test_instruction!(test_22, "22.json", 0x22);
    test_instruction!(test_23, "23.json", 0x23);
    test_instruction!(test_24, "24.json", 0x24);
    test_instruction!(test_25, "25.json", 0x25);
    test_instruction!(test_26, "26.json", 0x26);
    test_instruction!(test_27, "27.json", 0x27);
    test_instruction!(test_28, "28.json", 0x28);
    test_instruction!(test_29, "29.json", 0x29);
    test_instruction!(test_2A, "2a.json", 0x2A);
    test_instruction!(test_2B, "2b.json", 0x2B);
    test_instruction!(test_2C, "2c.json", 0x2C);
    test_instruction!(test_2D, "2d.json", 0x2D);
    test_instruction!(test_2E, "2e.json", 0x2E);
    test_instruction!(test_2F, "2f.json", 0x2F);

    // 0x3x
    test_instruction!(test_30, "30.json", 0x30);
    test_instruction!(test_31, "31.json", 0x31);
    test_instruction!(test_32, "32.json", 0x32);
    test_instruction!(test_33, "33.json", 0x33);
    test_instruction!(test_34, "34.json", 0x34);
    test_instruction!(test_35, "35.json", 0x35);
    test_instruction!(test_36, "36.json", 0x36);
    test_instruction!(test_37, "37.json", 0x37);
    test_instruction!(test_38, "38.json", 0x38);
    test_instruction!(test_39, "39.json", 0x39);
    test_instruction!(test_3A, "3a.json", 0x3A);
    test_instruction!(test_3B, "3b.json", 0x3B);
    test_instruction!(test_3C, "3c.json", 0x3C);
    test_instruction!(test_3D, "3d.json", 0x3D);
    test_instruction!(test_3E, "3e.json", 0x3E);
    test_instruction!(test_3F, "3f.json", 0x3F);

    // 0x4x
    test_instruction!(test_40, "40.json", 0x40);
    test_instruction!(test_41, "41.json", 0x41);
    test_instruction!(test_42, "42.json", 0x42);
    test_instruction!(test_43, "43.json", 0x43);
    test_instruction!(test_44, "44.json", 0x44);
    test_instruction!(test_45, "45.json", 0x45);
    test_instruction!(test_46, "46.json", 0x46);
    test_instruction!(test_47, "47.json", 0x47);
    test_instruction!(test_48, "48.json", 0x48);
    test_instruction!(test_49, "49.json", 0x49);
    test_instruction!(test_4A, "4a.json", 0x4A);
    test_instruction!(test_4B, "4b.json", 0x4B);
    test_instruction!(test_4C, "4c.json", 0x4C);
    test_instruction!(test_4D, "4d.json", 0x4D);
    test_instruction!(test_4E, "4e.json", 0x4E);
    test_instruction!(test_4F, "4f.json", 0x4F);

    // 0x5x
    test_instruction!(test_50, "50.json", 0x50);
    test_instruction!(test_51, "51.json", 0x51);
    test_instruction!(test_52, "52.json", 0x52);
    test_instruction!(test_53, "53.json", 0x53);
    test_instruction!(test_54, "54.json", 0x54);
    test_instruction!(test_55, "55.json", 0x55);
    test_instruction!(test_56, "56.json", 0x56);
    test_instruction!(test_57, "57.json", 0x57);
    test_instruction!(test_58, "58.json", 0x58);
    test_instruction!(test_59, "59.json", 0x59);
    test_instruction!(test_5A, "5a.json", 0x5A);
    test_instruction!(test_5B, "5b.json", 0x5B);
    test_instruction!(test_5C, "5c.json", 0x5C);
    test_instruction!(test_5D, "5d.json", 0x5D);
    test_instruction!(test_5E, "5e.json", 0x5E);
    test_instruction!(test_5F, "5f.json", 0x5F);

    // 0x6x
    test_instruction!(test_60, "60.json", 0x60);
    test_instruction!(test_61, "61.json", 0x61);
    test_instruction!(test_62, "62.json", 0x62);
    test_instruction!(test_63, "63.json", 0x63);
    test_instruction!(test_64, "64.json", 0x64);
    test_instruction!(test_65, "65.json", 0x65);
    test_instruction!(test_66, "66.json", 0x66);
    test_instruction!(test_67, "67.json", 0x67);
    test_instruction!(test_68, "68.json", 0x68);
    test_instruction!(test_69, "69.json", 0x69);
    test_instruction!(test_6A, "6a.json", 0x6A);
    test_instruction!(test_6B, "6b.json", 0x6B);
    test_instruction!(test_6C, "6c.json", 0x6C);
    test_instruction!(test_6D, "6d.json", 0x6D);
    test_instruction!(test_6E, "6e.json", 0x6E);
    test_instruction!(test_6F, "6f.json", 0x6F);
}
