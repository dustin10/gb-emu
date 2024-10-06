use crate::mem::Memory;

use bounded_vec_deque::BoundedVecDeque;
use std::{collections::VecDeque, fmt::Display};

/// Represents the registers on the cpu. Allows for easy manipulation of combined 16-bit registers as well.
#[derive(Clone, Copy, Debug, Default)]
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
        if on {
            self.0 |= 1 << FLAGS_CARRY_BIT_POSITION;
        } else {
            self.0 &= 0 << 1;
        }
    }
    /// Retrieves the current status of the half carry flag.
    fn h(&self) -> bool {
        (self.0 >> FLAGS_HALF_CARRY_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the half carry flag.
    fn set_h(&mut self, on: bool) {
        if on {
            self.0 |= 1 << FLAGS_HALF_CARRY_BIT_POSITION;
        } else {
            self.0 &= 0 << 1;
        }
    }
    /// Retrieves the current status of the subtract flag.
    fn n(&self) -> bool {
        (self.0 >> FLAGS_SUBTRACT_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the substract flag.
    fn set_n(&mut self, on: bool) {
        if on {
            self.0 |= 1 << FLAGS_SUBTRACT_BIT_POSITION;
        } else {
            self.0 &= 0 << 1;
        }
    }
    /// Retrieves the current status of the zero flag.
    fn z(&self) -> bool {
        (self.0 >> FLAGS_ZERO_BIT_POSITION) & 1 != 0
    }
    /// Sets the status of the zero flag.
    fn set_z(&mut self, on: bool) {
        if on {
            self.0 |= 1 << FLAGS_ZERO_BIT_POSITION;
        } else {
            self.0 &= 0 << 1;
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

/// Enumeration of the valid target 8 bit registers for the load instructions.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Load8BitTarget {
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

impl Display for Load8BitTarget {
    /// Writes a string representation of the [`Load8BitTarget`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

/// Enumeration of the valid target 16 bit registers for the load instructions.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Load16BitTarget {
    /// Combined BC 16-bit register.
    BC,
    /// Combined DE 16-bit register.
    DE,
    /// Combined HL 16-bit register.
    HL,
    /// Stack pointer.
    SP,
}

impl Display for Load16BitTarget {
    /// Writes a string representation of the [`Load16BitTarget`] to the formatter.
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
    ADDHL { target: Target },
    /// Decrements the value in the [`Target`] register by one.
    DEC { target: Target },
    /// Increments the value in the [`Target`] register by one.
    INC { target: Target },
    /// Loads the value from the `A` register and stores it in the memory address corresponding to
    /// the value in the [`Load16BitTarget`] register.
    LDA { target: Load16BitTarget },
    /// Loads the value in the [`Load16BitTarget`] register and stores it at the address in memory.
    LDA16 {
        address: u16,
        target: Load16BitTarget,
    },
    /// Loads the [`u16`] value from memory and stores it in the [`Load16BitTarget`] register.
    LDN16 { target: Load16BitTarget, value: u16 },
    /// Loads the [`u8`] value from memory and stores it in the [`Load8BitTarget`] register.
    LDN8 { target: Load8BitTarget, value: u8 },
    /// No operation.
    NOP,
    /// Prefix op code which causes the subsequent byte to represent a different set of
    /// instructions.
    PREFIX,
    /// Bit rotate the A register register left by one, not through the carry flag.
    RLCA,
}

impl Display for Operation {
    /// Writes a string representation of the [`Operation`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::ADDHL { target } => f.write_fmt(format_args!("ADD HL, {}", target)),
            Operation::DEC { target } => f.write_fmt(format_args!("DEC {}", target)),
            Operation::INC { target } => f.write_fmt(format_args!("INC {}", target)),
            Operation::LDA16 { address, target } => {
                f.write_fmt(format_args!("LD [{:#6x}] {}", address, target))
            }
            Operation::LDN16 { target, value } => {
                f.write_fmt(format_args!("LD {}, {:#6x}", target, value))
            }
            Operation::LDN8 { target, value } => {
                f.write_fmt(format_args!("LD {}, {:#4x}", target, value))
            }
            Operation::LDA { target } => f.write_fmt(format_args!("LD [{}], A", target)),
            Operation::NOP => f.write_str("NOP"),
            Operation::PREFIX => f.write_str("PREFIX"),
            Operation::RLCA => f.write_str("RLCA"),
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
    /// Creates a new increment instruction that targets an single 8 bit register.
    fn inc(target: Target) -> Self {
        Self::new(1, 4, Operation::INC { target })
    }
    /// Creates a new increment instruction that targets a combined 16 bit register.
    fn inc_wide(target: Target) -> Self {
        Self::new(1, 8, Operation::INC { target })
    }
    /// Creates a new instruction which loads the the contents of the `A` register into memory at
    /// the address held in the target register.
    fn ld_a(target: Load16BitTarget) -> Self {
        Self::new(1, 8, Operation::LDA { target })
    }
    /// Creates a new instruction which loads the value in the target register and stores it at
    /// the given address in memory.
    fn ld_a16(address: u16, target: Load16BitTarget) -> Self {
        Self::new(3, 20, Operation::LDA16 { address, target })
    }
    /// Creates a new load immediate n8 instruction with the given target 8 bit register and
    /// value to load.
    fn ld_n8(target: Load8BitTarget, value: u8) -> Self {
        Self::new(2, 8, Operation::LDN8 { target, value })
    }
    /// Creates a new load immediate n16 instruction with the given target 16 bit register and
    /// value to load.
    fn ld_n16(target: Load16BitTarget, value: u16) -> Self {
        Self::new(3, 12, Operation::LDN16 { target, value })
    }
    /// Creates a new no op instruction.
    fn nop() -> Self {
        Self::new(1, 4, Operation::NOP)
    }
    /// Creates a new prefix instruction.
    fn prefix() -> Self {
        Self::new(1, 4, Operation::PREFIX)
    }
    /// Creates a new instruction that bit rotates the value in the `A` register left by one, not
    /// through the carry flag.
    fn rlca() -> Self {
        Self::new(1, 4, Operation::RLCA)
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
            let (new_pc, prefix) = self.execute(&instruction, memory);

            self.registers.pc = new_pc;

            self.instruction_set = if prefix {
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
        tracing::debug!("decoding op code {:#4x}", op_code);

        match op_code {
            0x00 => Some(Instruction::nop()),
            0x01 => Some(Instruction::ld_n16(
                Load16BitTarget::BC,
                memory.read_u16(self.registers.pc + 1),
            )),
            0x02 => Some(Instruction::ld_a(Load16BitTarget::BC)),
            0x03 => Some(Instruction::inc_wide(Target::BC)),
            0x04 => Some(Instruction::inc(Target::B)),
            0x05 => Some(Instruction::dec(Target::B)),
            0x06 => Some(Instruction::ld_n8(
                Load8BitTarget::B,
                memory.read_byte(self.registers.pc + 1),
            )),
            0x07 => Some(Instruction::rlca()),
            0x08 => Some(Instruction::ld_a16(
                memory.read_u16(self.registers.pc + 1),
                Load16BitTarget::SP,
            )),
            0x09 => Some(Instruction::add_hl(Target::BC)),
            0xCB => Some(Instruction::prefix()),
            _ => None,
        }
    }
    /// Transforms the given prefixed op code into an [`Instruction`] which can be executed by the
    /// [`Cpu`]. An op code is prefixed if the preceding op code byte was 0xCB.
    fn decode_prefixed(&self, op_code: u8) -> Option<Instruction> {
        tracing::debug!("decoding prefixed op code {:#4x}", op_code);

        todo!()
    }
    /// Executes the given [`Instruction`] returning the new program counter value and whether or
    /// not the op code for the next instruction is prefixed.
    fn execute(&mut self, instruction: &Instruction, memory: &mut Memory) -> (u16, bool) {
        tracing::debug!("executing instruction '{}'", instruction);

        let mut prefix = false;

        match instruction.operation {
            Operation::INC { target } => match target {
                Target::B => {
                    let old_value = self.registers.b;
                    self.registers.b += 1;

                    self.registers.f.set_z(self.registers.b == 0);
                    self.registers.f.set_n(false);
                    self.registers.f.set_h(will_half_carry_add(old_value, 1));
                }
                Target::BC => self.registers.set_bc(self.registers.bc() + 1),
                _ => todo!(),
            },
            Operation::LDA { target } => {
                let address = match target {
                    Load16BitTarget::BC => self.registers.bc(),
                    _ => todo!(),
                };

                memory.write_byte(address, self.registers.a);
            }
            Operation::LDN16 { target, value } => match target {
                Load16BitTarget::BC => self.registers.set_bc(value),
                _ => todo!(),
            },
            Operation::NOP => {}
            Operation::PREFIX => prefix = true,
            _ => todo!(),
        }

        let new_pc = self.registers.pc.wrapping_add(instruction.num_bytes);

        (new_pc, prefix)
    }
}

/// Detrmines if the addtion of `b` to `a` will cause a half-carry.
fn will_half_carry_add(a: u8, b: u8) -> bool {
    (((a & 0x0F) + (b & 0x0F)) & 0x10) == 0x10
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

        flags.set_n(true);
        assert!(flags.n());
        assert_eq!(flags.0, 1 << FLAGS_SUBTRACT_BIT_POSITION);

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
    fn test_cpu_decode_ld_n16_bc() {
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
                Operation::LDN16 {
                    target: Load16BitTarget::BC,
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
                Operation::LDN16 {
                    target: Load16BitTarget::BC,
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
                Operation::LDN16 {
                    target: Load16BitTarget::BC,
                    value: 257,
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_ld_bc_a() {
        let op_code: u8 = 0x02;

        let memory = Memory::new();

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(1, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDA {
                target: Load16BitTarget::BC
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
    fn test_cpu_decode_ld_b_n8() {
        let op_code: u8 = 0x06;

        let mut memory = Memory::new();
        memory.write_byte(0x0001, 3);

        let cpu = Cpu::new();

        let instruction = cpu.decode(op_code, &memory).expect("valid op code");
        assert_eq!(2, instruction.num_bytes);
        assert_eq!(8, instruction.clock_ticks);
        assert_eq!(
            Operation::LDN8 {
                target: Load8BitTarget::B,
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
        assert_eq!(Operation::RLCA, instruction.operation);
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
                    target: Load16BitTarget::SP
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
                    target: Load16BitTarget::SP,
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
                    target: Load16BitTarget::SP,
                },
                instruction.operation
            );
        }
    }

    #[test]
    fn test_cpu_decode_addhl_bc() {
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
        let instruction = Instruction::inc_wide(Target::BC);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();
        cpu.registers.set_bc(200);

        let (new_pc, prefix) = cpu.execute(&instruction, &mut memory);
        assert_eq!(201, cpu.registers.bc());
        assert_eq!(1, new_pc);
        assert!(!prefix);
    }

    #[test]
    fn test_cpu_execute_inc_b() {
        // TODO: tests for zero and half-carry as well as other
    }

    #[test]
    fn test_cpu_execute_ld_a_bc() {
        let instruction = Instruction::ld_a(Load16BitTarget::BC);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();
        cpu.registers.a = 0x22;
        cpu.registers.set_bc(0x0F0F);

        let (new_pc, prefix) = cpu.execute(&instruction, &mut memory);
        assert_eq!(0x22, memory.read_byte(0x0F0F));
        assert_eq!(1, new_pc);
        assert!(!prefix);
    }

    #[test]
    fn test_cpu_execute_ld_n16_bc() {
        let instruction = Instruction::ld_n16(Load16BitTarget::BC, 0xAE24);

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();

        let (new_pc, prefix) = cpu.execute(&instruction, &mut memory);
        assert_eq!(0xAE, cpu.registers.b);
        assert_eq!(0x24, cpu.registers.c);
        assert_eq!(3, new_pc);
        assert!(!prefix);
    }

    #[test]
    fn test_cpu_execute_nop() {
        let instruction = Instruction::nop();

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();

        let (new_pc, prefix) = cpu.execute(&instruction, &mut memory);
        assert_eq!(1, new_pc);
        assert!(!prefix);
    }

    #[test]
    fn test_cpu_execute_prefix() {
        let instruction = Instruction::prefix();

        let mut memory = Memory::new();

        let mut cpu = Cpu::new();

        let (new_pc, prefix) = cpu.execute(&instruction, &mut memory);
        assert_eq!(1, new_pc);
        assert!(prefix);
    }
}
