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
    /// Combined AF 16-bit register.
    AF,
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
    /// Adds the value in the [`Target`] register to the value in the HL register and stores it
    /// back to the HL register.
    ADDHL {
        target: Target,
    },
    /// Decrements the value in the [`Target`] register by one.
    DEC {
        target: Target,
    },
    /// Increments the value in the [`Target`] register by one.
    INC {
        target: Target,
    },
    /// Jumps to a relative memory address by advancing the program counter by the offset.
    JR {
        offset: i8,
    },
    /// Conditionally jumps to a relative memory address by advancing the program counter by the
    /// offset based on the state of the [`CondJumpTarget`].
    JRC {
        target: CondJumpTarget,
        jump: bool,
        offset: i8,
    },
    /// Loads the value from the `A` register and stores it in the memory address corresponding to
    /// the value in the [`Target16Bit`] register.
    LDA {
        target: Target16Bit,
    },
    /// Loads the value from memory at the address  `A` register and stores it in the memory address
    /// corresponding to the value of the [`Target8Bit`] register and stores it in the `A` register.
    LDAMEM {
        target: Target16Bit,
    },
    /// Loads the value in the [`Target16Bit`] register and stores it at the address in memory.
    LDA16 {
        address: u16,
        target: Target16Bit,
    },
    /// Loads the [`u16`] value from memory and stores it in the [`Target16Bit`] register.
    LDU16 {
        target: Target16Bit,
        value: u16,
    },
    /// Loads the [`u8`] value from memory and stores it in the [`Target8Bit`] register.
    LDU8 {
        target: Target8Bit,
        value: u8,
    },
    /// No operation.
    NOP,
    /// Prefix op code which causes the subsequent byte to represent a different set of
    /// instructions.
    PREFIX,
    /// Bit rotate the [`Target`] register left by one, through the carry flag.
    RL {
        target: Target,
    },
    /// Bit rotate the [`Target`] register left by one, not through the carry flag.
    RLC {
        target: Target,
    },
    /// Bit rotate the [`Target`] register left by one, through the carry flag.
    RR {
        target: Target,
    },
    /// Bit rotate the [`Target`] register right by one, not through the carry flag.
    RRC {
        target: Target,
    },
    STOP,
}

impl Display for Operation {
    /// Writes a string representation of the [`Operation`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::ADDHL { target } => f.write_fmt(format_args!("ADD HL, {}", target)),
            Operation::DEC { target } => f.write_fmt(format_args!("DEC {}", target)),
            Operation::INC { target } => f.write_fmt(format_args!("INC {}", target)),
            Operation::JR { offset } => f.write_fmt(format_args!("JR {:#4x}", offset)),
            Operation::JRC { target, offset, .. } => {
                f.write_fmt(format_args!("JR {}, {:#4x}", target, offset))
            }
            Operation::LDA { target } => f.write_fmt(format_args!("LD [{}], A", target)),
            Operation::LDAMEM { target } => f.write_fmt(format_args!("LD A, [{}]", target)),
            Operation::LDA16 { address, target } => {
                f.write_fmt(format_args!("LD [{:#6x}] {}", address, target))
            }
            Operation::LDU16 { target, value } => {
                f.write_fmt(format_args!("LD {}, {:#6x}", target, value))
            }
            Operation::LDU8 { target, value } => {
                f.write_fmt(format_args!("LD {}, {:#4x}", target, value))
            }
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
    /// Creates a new instruction that adds the value in the [`Target`] register to the value in
    /// the `HL` register and stores the result back to the `HL` register.
    fn add_hl(target: Target) -> Self {
        Self::new(1, 8, Operation::ADDHL { target })
    }
    /// Creates a new decrement instruction that targets an single 8 bit register.
    fn dec(target: Target) -> Self {
        Self::new(1, 4, Operation::DEC { target })
    }
    /// Creates a new decrement instruction that targets a combined 16 bit register.
    fn dec_u16(target: Target) -> Self {
        Self::new(1, 8, Operation::DEC { target })
    }
    /// Creates a new increment instruction that targets an single 8 bit register.
    fn inc(target: Target) -> Self {
        Self::new(1, 4, Operation::INC { target })
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
    /// Creates a new instruction which loads the the byte from memory at the address corresponding
    /// to the value of the [`Target8Bit`] register and stores it in the `A` register.
    fn ld_a_mem(target: Target16Bit) -> Self {
        Self::new(1, 8, Operation::LDAMEM { target })
    }
    /// Creates a new instruction which loads the value in the target register and stores it at
    /// the given address in memory.
    fn ld_a16(address: u16, target: Target16Bit) -> Self {
        Self::new(3, 20, Operation::LDA16 { address, target })
    }
    /// Creates a new load immediate n8 instruction with the given target 8 bit register and
    /// value to load.
    fn ld_u8(target: Target8Bit, value: u8) -> Self {
        Self::new(2, 8, Operation::LDU8 { target, value })
    }
    /// Creates a new load immediate n16 instruction with the given target 16 bit register and
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
        let op_code = memory.read_byte(self.registers.pc);

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
                memory.read_u16(self.registers.pc + 1),
            )),
            0x02 => Some(Instruction::ld_a(Target16Bit::BC)),
            0x03 => Some(Instruction::inc_u16(Target::BC)),
            0x04 => Some(Instruction::inc(Target::B)),
            0x05 => Some(Instruction::dec(Target::B)),
            0x06 => Some(Instruction::ld_u8(
                Target8Bit::B,
                memory.read_byte(self.registers.pc + 1),
            )),
            0x07 => Some(Instruction::rlca()),
            0x08 => Some(Instruction::ld_a16(
                memory.read_u16(self.registers.pc + 1),
                Target16Bit::SP,
            )),
            0x09 => Some(Instruction::add_hl(Target::BC)),
            0x0A => Some(Instruction::ld_a_mem(Target16Bit::BC)),
            0x0B => Some(Instruction::dec_u16(Target::BC)),
            0x0C => Some(Instruction::inc(Target::C)),
            0x0D => Some(Instruction::dec(Target::C)),
            0x0E => Some(Instruction::ld_u8(
                Target8Bit::C,
                memory.read_byte(self.registers.pc + 1),
            )),
            0x0F => Some(Instruction::rrca()),

            // 0x1x
            0x10 => Some(Instruction::stop()),
            0x11 => Some(Instruction::ld_u16(
                Target16Bit::DE,
                memory.read_u16(self.registers.pc + 1),
            )),
            0x12 => Some(Instruction::ld_a(Target16Bit::DE)),
            0x13 => Some(Instruction::inc_u16(Target::DE)),
            0x14 => Some(Instruction::inc(Target::D)),
            0x15 => Some(Instruction::dec(Target::D)),
            0x16 => Some(Instruction::ld_u8(
                Target8Bit::D,
                memory.read_byte(self.registers.pc + 1),
            )),
            0x17 => Some(Instruction::rla()),
            0x18 => Some(Instruction::jr(memory.read_i8(self.registers.pc + 1))),
            0x19 => Some(Instruction::add_hl(Target::DE)),
            0x1A => Some(Instruction::ld_a_mem(Target16Bit::DE)),
            0x1B => Some(Instruction::dec_u16(Target::DE)),
            0x1C => Some(Instruction::inc(Target::E)),
            0x1D => Some(Instruction::dec(Target::E)),
            0x1E => Some(Instruction::ld_u8(
                Target8Bit::E,
                memory.read_byte(self.registers.pc + 1),
            )),
            0x1F => Some(Instruction::rra()),

            // 0x2x
            0x20 => Some(Instruction::jrc(
                CondJumpTarget::NZ,
                !self.registers.f.z(),
                memory.read_i8(self.registers.pc + 1),
            )),
            0x21 => Some(Instruction::ld_u16(
                Target16Bit::HL,
                memory.read_u16(self.registers.pc + 1),
            )),

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
            // TODO: cleanup
            Operation::ADDHL { target } => match target {
                Target::BC => {
                    let hl = self.registers.hl();
                    let bc = self.registers.bc();

                    let (new_value, overflowed) = hl.overflowing_add(bc);

                    self.registers.set_hl(new_value);

                    self.registers.f.set_n(false);
                    self.registers.f.set_h(will_half_carry_add_u16(hl, bc));
                    self.registers.f.set_c(overflowed);
                }
                Target::DE => {
                    let hl = self.registers.hl();
                    let de = self.registers.de();

                    let (new_value, overflowed) = hl.overflowing_add(de);

                    self.registers.set_hl(new_value);

                    self.registers.f.set_n(false);
                    self.registers.f.set_h(will_half_carry_add_u16(hl, de));
                    self.registers.f.set_c(overflowed);
                }
                _ => todo!(),
            },
            // TODO: cleanup
            Operation::DEC { target } => match target {
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
                Target::BC => {
                    self.registers.set_bc(self.registers.bc().wrapping_sub(1));
                }
                Target::DE => {
                    self.registers.set_de(self.registers.de().wrapping_sub(1));
                }
                _ => panic!("invalid DEC target: {}", target),
            },
            // TODO: cleanup
            Operation::INC { target } => match target {
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
                Target::BC => self.registers.set_bc(self.registers.bc().wrapping_add(1)),
                Target::DE => self.registers.set_de(self.registers.de().wrapping_add(1)),
                _ => panic!("invalid INC target: {}", target),
            },
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

                memory.write_byte(address, self.registers.a);
            }
            Operation::LDAMEM { target } => match target {
                Target16Bit::BC => self.registers.a = memory.read_byte(self.registers.bc()),
                Target16Bit::DE => self.registers.a = memory.read_byte(self.registers.de()),
                _ => todo!(),
            },
            Operation::LDA16 { address, target } => match target {
                Target16Bit::SP => {
                    let low = self.registers.sp as u8;
                    let high = (self.registers.sp >> 8) as u8;

                    memory.write_byte(address, low);
                    memory.write_byte(address + 1, high);
                }
                _ => todo!(),
            },
            Operation::LDU8 { target, value } => match target {
                Target8Bit::B => self.registers.b = value,
                Target8Bit::C => self.registers.c = value,
                Target8Bit::D => self.registers.d = value,
                Target8Bit::E => self.registers.e = value,
                _ => todo!(),
            },
            Operation::LDU16 { target, value } => match target {
                Target16Bit::BC => self.registers.set_bc(value),
                Target16Bit::DE => self.registers.set_de(value),
                Target16Bit::HL => self.registers.set_hl(value),
                _ => todo!(),
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
            memory.write_byte(0x0001, 1);
            memory.write_byte(0x0002, 0);

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
            memory.write_byte(0x0001, 0);
            memory.write_byte(0x0002, 1);

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
            memory.write_byte(0x0001, 1);
            memory.write_byte(0x0002, 1);

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
        memory.write_byte(0x0001, 3);

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
            memory.write_byte(0x0001, 1);
            memory.write_byte(0x0002, 0);

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
            memory.write_byte(0x0001, 0);
            memory.write_byte(0x0002, 1);

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
            memory.write_byte(0x0001, 1);
            memory.write_byte(0x0002, 1);

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
            Operation::ADDHL { target: Target::BC },
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
        memory.write_byte(1, 20);

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
            memory.write_byte(0x0001, 1);
            memory.write_byte(0x0002, 0);

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
            memory.write_byte(0x0001, 0);
            memory.write_byte(0x0002, 1);

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
            memory.write_byte(0x0001, 1);
            memory.write_byte(0x0002, 1);

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
        memory.write_byte(1, 20);

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
            memory.write_byte(0x01, 2);

            let cpu = Cpu::new();

            let instruction = cpu.decode(op_code, &memory).expect("valid op code");
            assert_eq!(2, instruction.num_bytes);
            assert_eq!(12, instruction.clock_ticks);
            assert_eq!(Operation::JR { offset: 2 }, instruction.operation);
        }
        {
            let mut memory = Memory::new();
            memory.write_byte(0x01, 0xFF);

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
            Operation::ADDHL { target: Target::DE },
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
        memory.write_byte(1, 20);

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
            memory.write_byte(1, 9);

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
            memory.write_byte(1, 9);

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
            memory.write_byte(0x0001, 1);
            memory.write_byte(0x0002, 0);

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
            memory.write_byte(0x0001, 0);
            memory.write_byte(0x0002, 1);

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
            memory.write_byte(0x0001, 1);
            memory.write_byte(0x0002, 1);

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
    fn test_cpu_decode_prefix() {
        let op_code: u8 = 0xCB;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(4, instruction.clock_ticks);
        assert_eq!(Operation::PREFIX, instruction.operation);
    }

    #[test]
    fn test_cpu_execute_inc_bc() {
        let instruction = Instruction::inc_u16(Target::BC);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();
        cpu.registers.set_bc(200);

        cpu.execute(&instruction, &mut memory);
        assert_eq!(201, cpu.registers.bc());
    }

    #[test]
    fn test_cpu_execute_inc_b() {
        {
            let instruction = Instruction::inc(Target::B);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.b = 1;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(2, cpu.registers.b);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
        {
            let instruction = Instruction::inc(Target::B);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.b = 0x0F;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x10, cpu.registers.b);
            assert!(!cpu.registers.f.c());
            assert!(cpu.registers.f.h());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
    }

    #[test]
    fn test_cpu_execute_dec_b() {
        {
            let instruction = Instruction::dec(Target::B);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.b = 2;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(1, cpu.registers.b);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
        {
            let instruction = Instruction::dec(Target::B);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.b = 0x10;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x0F, cpu.registers.b);
            assert!(!cpu.registers.f.c());
            assert!(cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
        {
            let instruction = Instruction::dec(Target::B);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.b = 1;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0, cpu.registers.b);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(cpu.registers.f.z());
        }
    }

    #[test]
    fn test_cpu_execute_ld_a_bc() {
        let instruction = Instruction::ld_a(Target16Bit::BC);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();
        cpu.registers.a = 0x22;
        cpu.registers.set_bc(0x0F0F);

        cpu.execute(&instruction, &mut memory);
        assert_eq!(0x22, memory.read_byte(0x0F0F));
    }

    #[test]
    fn test_cpu_execute_ld_u16_bc() {
        let instruction = Instruction::ld_u16(Target16Bit::BC, 0xAE24);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();

        cpu.execute(&instruction, &mut memory);
        assert_eq!(0xAE, cpu.registers.b);
        assert_eq!(0x24, cpu.registers.c);
    }

    #[test]
    fn test_cpu_execute_ld_u8_b() {
        let instruction = Instruction::ld_u8(Target8Bit::B, 0x12);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();

        cpu.execute(&instruction, &mut memory);
        assert_eq!(0x12, cpu.registers.b);
    }

    #[test]
    fn test_cpu_execute_rlca() {
        {
            let instruction = Instruction::rlca();

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.a = 16;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(32, cpu.registers.a);
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.c());
        }
        {
            let instruction = Instruction::rlca();

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.a = 255;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(255, cpu.registers.a);
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.c());
        }
    }

    #[test]
    fn test_cpu_execute_ld_a16_sp() {
        {
            let instruction = Instruction::ld_a16(0, Target16Bit::SP);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.sp = u16::MAX;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0xFF, memory.read_byte(0));
            assert_eq!(0xFF, memory.read_byte(1));
        }
        {
            let instruction = Instruction::ld_a16(0, Target16Bit::SP);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.sp = 0x00FF;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0xFF, memory.read_byte(0));
            assert_eq!(0, memory.read_byte(1));
        }
        {
            let instruction = Instruction::ld_a16(0, Target16Bit::SP);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.sp = 0xFF00;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0, memory.read_byte(0));
            assert_eq!(0xFF, memory.read_byte(1));
        }
        {
            let instruction = Instruction::ld_a16(0, Target16Bit::SP);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.sp = 0x0F0F;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x0F, memory.read_byte(0));
            assert_eq!(0x0F, memory.read_byte(1));
        }
    }

    #[test]
    fn test_cpu_execute_add_hl_bc() {
        {
            let instruction = Instruction::add_hl(Target::BC);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.set_hl(1);
            cpu.registers.set_bc(1);

            cpu.execute(&instruction, &mut memory);
            assert_eq!(2, cpu.registers.hl());
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.c());
        }
        {
            let instruction = Instruction::add_hl(Target::BC);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.set_hl(0x0FFF);
            cpu.registers.set_bc(0x0001);

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x1000, cpu.registers.hl());
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(cpu.registers.f.h());
            assert!(!cpu.registers.f.c());
        }
        {
            let instruction = Instruction::add_hl(Target::BC);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.set_hl(0xFFFE);
            cpu.registers.set_bc(0x0001);

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0xFFFF, cpu.registers.hl());
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.c());
        }
        {
            let instruction = Instruction::add_hl(Target::BC);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.set_hl(0xFFFF);
            cpu.registers.set_bc(0x0001);

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x0000, cpu.registers.hl());
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(cpu.registers.f.h());
            assert!(cpu.registers.f.c());
        }
    }

    #[test]
    fn test_cpu_execute_ld_a_mem_bc() {
        let instruction = Instruction::ld_a_mem(Target16Bit::BC);

        let mut memory = Memory::new();
        memory.write_byte(2, 3);

        let mut cpu = Cpu::new();
        cpu.registers.set_bc(2);

        cpu.execute(&instruction, &mut memory);
        assert_eq!(3, cpu.registers.a);
    }

    #[test]
    fn test_cpu_execute_dec_bc() {
        let instruction = Instruction::dec_u16(Target::BC);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();
        cpu.registers.set_bc(2);

        cpu.execute(&instruction, &mut memory);
        assert_eq!(1, cpu.registers.bc());
    }

    #[test]
    fn test_cpu_execute_inc_c() {
        {
            let instruction = Instruction::inc(Target::C);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.c = 1;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(2, cpu.registers.c);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
        {
            let instruction = Instruction::inc(Target::C);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.c = 0x0F;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x10, cpu.registers.c);
            assert!(!cpu.registers.f.c());
            assert!(cpu.registers.f.h());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
    }

    #[test]
    fn test_cpu_execute_dec_c() {
        {
            let instruction = Instruction::dec(Target::C);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.c = 2;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(1, cpu.registers.c);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
        {
            let instruction = Instruction::dec(Target::C);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.c = 1;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0, cpu.registers.c);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(cpu.registers.f.z());
        }
        {
            let instruction = Instruction::dec(Target::C);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.c = 0x10;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x0F, cpu.registers.c);
            assert!(!cpu.registers.f.c());
            assert!(cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
    }

    #[test]
    fn test_cpu_execute_ld_u8_c() {
        let instruction = Instruction::ld_u8(Target8Bit::C, 0x33);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();

        cpu.execute(&instruction, &mut memory);
        assert_eq!(0x33, cpu.registers.c);
    }

    #[test]
    fn test_cpu_execute_rrca() {
        {
            let instruction = Instruction::rrca();

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.a = 0b00010000;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0b00001000, cpu.registers.a);
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.c());
        }
        {
            let instruction = Instruction::rrca();

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.a = 0b00000001;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0b10000000, cpu.registers.a);
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.c());
        }
    }

    #[test]
    fn test_cpu_execute_ld_u16_de() {
        let instruction = Instruction::ld_u16(Target16Bit::DE, 0xAE24);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();

        cpu.execute(&instruction, &mut memory);
        assert_eq!(0xAE, cpu.registers.d);
        assert_eq!(0x24, cpu.registers.e);
    }

    #[test]
    fn test_cpu_execute_ld_a_de() {
        let instruction = Instruction::ld_a(Target16Bit::DE);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();
        cpu.registers.a = 0x22;
        cpu.registers.set_de(0x0F0F);

        cpu.execute(&instruction, &mut memory);
        assert_eq!(0x22, memory.read_byte(0x0F0F));
    }

    #[test]
    fn test_cpu_execute_inc_de() {
        let instruction = Instruction::inc_u16(Target::DE);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();
        cpu.registers.set_de(200);

        cpu.execute(&instruction, &mut memory);
        assert_eq!(201, cpu.registers.de());
    }

    #[test]
    fn test_cpu_execute_inc_d() {
        {
            let instruction = Instruction::inc(Target::D);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.d = 1;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(2, cpu.registers.d);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
        {
            let instruction = Instruction::inc(Target::D);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.d = 0x0F;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x10, cpu.registers.d);
            assert!(!cpu.registers.f.c());
            assert!(cpu.registers.f.h());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
    }

    #[test]
    fn test_cpu_execute_dec_d() {
        {
            let instruction = Instruction::dec(Target::D);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.d = 2;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(1, cpu.registers.d);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
        {
            let instruction = Instruction::dec(Target::D);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.d = 1;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0, cpu.registers.d);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(cpu.registers.f.z());
        }
        {
            let instruction = Instruction::dec(Target::D);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.d = 0x10;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x0F, cpu.registers.d);
            assert!(!cpu.registers.f.c());
            assert!(cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
    }

    #[test]
    fn test_cpu_execute_ld_u8_d() {
        let instruction = Instruction::ld_u8(Target8Bit::D, 0x33);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();

        cpu.execute(&instruction, &mut memory);
        assert_eq!(0x33, cpu.registers.d);
    }

    #[test]
    fn test_cpu_execute_rla() {
        {
            let instruction = Instruction::rla();

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.a = 16;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(32, cpu.registers.a);
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.c());
        }
        {
            let instruction = Instruction::rla();

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.a = 255;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(254, cpu.registers.a);
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.c());
        }
        {
            let instruction = Instruction::rla();

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.a = 255;
            cpu.registers.f.set_c(true);

            cpu.execute(&instruction, &mut memory);
            assert_eq!(255, cpu.registers.a);
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.c());
        }
    }

    #[test]
    fn test_cpu_execute_jr() {
        {
            let instruction = Instruction::jr(6);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();

            cpu.execute(&instruction, &mut memory);
            assert_eq!(6, cpu.registers.pc);
        }
        {
            let instruction = Instruction::jr(-1);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.pc = 10;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(9, cpu.registers.pc);
        }
    }

    #[test]
    fn test_cpu_execute_add_hl_de() {
        {
            let instruction = Instruction::add_hl(Target::DE);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.set_hl(1);
            cpu.registers.set_de(1);

            cpu.execute(&instruction, &mut memory);
            assert_eq!(2, cpu.registers.hl());
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.c());
        }
        {
            let instruction = Instruction::add_hl(Target::DE);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.set_hl(0x0FFF);
            cpu.registers.set_de(0x0001);

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x1000, cpu.registers.hl());
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(cpu.registers.f.h());
            assert!(!cpu.registers.f.c());
        }
        {
            let instruction = Instruction::add_hl(Target::DE);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.set_hl(0xFFFE);
            cpu.registers.set_de(0x0001);

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0xFFFF, cpu.registers.hl());
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.c());
        }
        {
            let instruction = Instruction::add_hl(Target::DE);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.set_hl(0xFFFF);
            cpu.registers.set_de(0x0001);

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x0000, cpu.registers.hl());
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(cpu.registers.f.h());
            assert!(cpu.registers.f.c());
        }
    }

    #[test]
    fn test_cpu_execute_ld_a_mem_de() {
        let instruction = Instruction::ld_a_mem(Target16Bit::DE);

        let mut memory = Memory::new();
        memory.write_byte(2, 3);

        let mut cpu = Cpu::new();
        cpu.registers.set_de(2);

        cpu.execute(&instruction, &mut memory);
        assert_eq!(3, cpu.registers.a);
    }

    #[test]
    fn test_cpu_execute_dec_de() {
        let instruction = Instruction::dec_u16(Target::DE);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();
        cpu.registers.set_de(2);

        cpu.execute(&instruction, &mut memory);
        assert_eq!(1, cpu.registers.de());
    }

    #[test]
    fn test_cpu_execute_inc_e() {
        {
            let instruction = Instruction::inc(Target::E);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.e = 1;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(2, cpu.registers.e);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
        {
            let instruction = Instruction::inc(Target::E);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.e = 0x0F;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x10, cpu.registers.e);
            assert!(!cpu.registers.f.c());
            assert!(cpu.registers.f.h());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
    }

    #[test]
    fn test_cpu_execute_dec_e() {
        {
            let instruction = Instruction::dec(Target::E);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.e = 2;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(1, cpu.registers.e);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
        {
            let instruction = Instruction::dec(Target::E);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.e = 1;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0, cpu.registers.e);
            assert!(!cpu.registers.f.c());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(cpu.registers.f.z());
        }
        {
            let instruction = Instruction::dec(Target::E);

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.e = 0x10;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0x0F, cpu.registers.e);
            assert!(!cpu.registers.f.c());
            assert!(cpu.registers.f.h());
            assert!(cpu.registers.f.n());
            assert!(!cpu.registers.f.z());
        }
    }

    #[test]
    fn test_cpu_execute_ld_u8_e() {
        let instruction = Instruction::ld_u8(Target8Bit::E, 0x33);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();

        cpu.execute(&instruction, &mut memory);
        assert_eq!(0x33, cpu.registers.e);
    }

    #[test]
    fn test_cpu_execute_rra() {
        {
            let instruction = Instruction::rra();

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.a = 16;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(8, cpu.registers.a);
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(!cpu.registers.f.c());
        }
        {
            let instruction = Instruction::rra();

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.a = 1;

            cpu.execute(&instruction, &mut memory);
            assert_eq!(0, cpu.registers.a);
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.c());
        }
        {
            let instruction = Instruction::rra();

            let mut memory = Memory::new();

            let mut cpu = Cpu::new();
            cpu.registers.a = 1;
            cpu.registers.f.set_c(true);

            cpu.execute(&instruction, &mut memory);
            assert_eq!(128, cpu.registers.a);
            assert!(!cpu.registers.f.z());
            assert!(!cpu.registers.f.n());
            assert!(!cpu.registers.f.h());
            assert!(cpu.registers.f.c());
        }
    }

    #[test]
    fn test_cpu_execute_ld_u16_hl() {
        let instruction = Instruction::ld_u16(Target16Bit::HL, 0xAE24);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();

        cpu.execute(&instruction, &mut memory);
        assert_eq!(0xAE, cpu.registers.h);
        assert_eq!(0x24, cpu.registers.l);
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

            memory.write_byte(address, value);
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

            let actual = memory.read_byte(address);

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
}
