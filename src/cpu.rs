use crate::mem::Memory;

use std::fmt::Display;

/// Represents the registers on the cpu. Allows for easy manipulation of combined registers.
#[derive(Clone, Copy, Debug, Default)]
struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: Flags,
    h: u8,
    l: u8,
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
/// 76543210
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

/// Enumeration of the target registers available to the cpu instructions.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Debug, Eq, PartialEq)]
enum Target {
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
    SP,
}

impl Display for Target {
    /// Writes a string representation of the [`Target`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?}", self))
    }
}

/// Enumeration of the operations the [`Cpu`] is capable of executing.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Debug, Eq, PartialEq)]
enum Operation {
    /// Adds the value in the [`Target`] register to the value in the HL register and stores it
    /// back to the HL register.
    ADDHL { target: Target },
    /// Decrements the value in the [`Target`] register by one.
    DEC { target: Target },
    /// Increments the value in the [`Target`] register by one.
    INC { target: Target },
    /// Loads the value in the [`Target`] register and stores it at the address in memory.
    LDA16 { address: u16, target: Target },
    /// Loads the [`u16`] value from memory and stores it in the [`Target`] register.
    LDN16 { target: Target, value: u16 },
    /// Loads the [`u8`] value from memory and stores it in the [`Target`] register.
    LDN8 { target: Target, value: u8 },
    /// Loads the value from the A register and stores it in the memory address corresponding to
    /// the value in the [`Target`] register.
    LDA { target: Target },
    /// No operation.
    NOP,
    /// Prefix op-code which causes the subsequent byte to represent a different set of
    /// instructions.
    PREFIX,
    /// Bit rotate the A register register left by 1 not through the carry flag.
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

/// An instruction that is ready to be executed by the [`Cpu`].
#[derive(Clone, Debug, Eq, PartialEq)]
struct Instruction {
    /// Number of bytes that make up the instruction.
    num_bytes: u16,
    /// Number of system clock ticks it takes to execute the instruction.
    clock_ticks: u64,
    /// [`Operation`] which should be executed.
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
}

impl Display for Instruction {
    /// Writes a string representation of the [`Instruction`] to the formatter. This implementation
    /// simply defers to the [`Display`] trait implementation for the [`Operation`] owned by the
    /// [`Instruction`].
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.operation.fmt(f)
    }
}

/// Represents the central processing unit of the Game Boy system. It is responsible for reading,
/// decoding and executing instructions which drive the game.
#[derive(Debug, Default)]
pub struct Cpu {
    /// Registers read and written by the instructions.
    registers: Registers,
    /// Program counter.
    pc: u16,
    /// Stack pointer.
    sp: u16,
    /// Indicates whether the next instruction read was prefixed with 0xCB or not.
    prefixed: bool,
}

impl Cpu {
    /// Creates a new default [`Cpu`].
    pub fn new() -> Self {
        Self::default()
    }
    /// Reads and executes the next instruction based on the current program counter.
    pub fn step(&mut self, memory: &Memory) {
        let op_code = memory.read_byte(self.pc);

        let instruction = if self.prefixed {
            self.decode_prefixed(op_code, memory)
        } else {
            self.decode(op_code, memory)
        };

        if let Some(instruction) = instruction {
            let new_pc = self.execute(&instruction);
            self.pc = new_pc;
        } else {
            tracing::warn!("encountered unknown instruction: 0x{:x}", op_code);
        };
    }
    /// Transforms the given op code into an [`Instruction`] which can be executed by the [`Cpu`].
    fn decode(&self, op_code: u8, memory: &Memory) -> Option<Instruction> {
        match op_code {
            0x00 => Some(Instruction::new(1, 4, Operation::NOP)),
            0x01 => {
                let low = memory.read_byte(self.pc + 1);
                let high = memory.read_byte(self.pc + 2);
                let value = (high as u16) << 8 | low as u16;

                Some(Instruction::new(
                    3,
                    12,
                    Operation::LDN16 {
                        target: Target::BC,
                        value,
                    },
                ))
            }
            0x02 => Some(Instruction::new(
                1,
                8,
                Operation::LDA { target: Target::BC },
            )),
            0x03 => Some(Instruction::new(
                1,
                8,
                Operation::INC { target: Target::BC },
            )),
            0x04 => Some(Instruction::new(1, 4, Operation::INC { target: Target::B })),
            0x05 => Some(Instruction::new(1, 4, Operation::DEC { target: Target::B })),
            0x06 => {
                let value = memory.read_byte(self.pc + 1);

                Some(Instruction::new(
                    2,
                    8,
                    Operation::LDN8 {
                        target: Target::B,
                        value,
                    },
                ))
            }
            0x07 => Some(Instruction::new(1, 4, Operation::RLCA)),
            0x08 => {
                let low = memory.read_byte(self.pc + 1);
                let high = memory.read_byte(self.pc + 2);
                let address = (high as u16) << 8 | low as u16;

                Some(Instruction::new(
                    3,
                    20,
                    Operation::LDA16 {
                        address,
                        target: Target::SP,
                    },
                ))
            }
            0x09 => Some(Instruction::new(
                1,
                8,
                Operation::ADDHL { target: Target::BC },
            )),
            0xCB => Some(Instruction::new(1, 4, Operation::PREFIX)),
            _ => None,
        }
    }
    /// Transforms the given prefixed op code into an [`Instruction`] which can be executed by the
    /// [`Cpu`]. An op code is prefixed if the preceding op code byte was 0xCB.
    fn decode_prefixed(&self, op_code: u8, memory: &Memory) -> Option<Instruction> {
        todo!()
    }
    /// Executes the specified [`Instruction`].
    fn execute(&mut self, instruction: &Instruction) -> u16 {
        tracing::debug!("executing instruction '{:?}'", instruction);

        match instruction.operation {
            Operation::PREFIX => {}
            _ => todo!(),
        }

        self.prefixed = instruction.operation == Operation::PREFIX;
        self.pc.wrapping_add(instruction.num_bytes)
    }
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
    fn test_cpu_decode_ld_bc_n16() {
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
                    target: Target::BC,
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
                    target: Target::BC,
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
                    target: Target::BC,
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
        assert_eq!(Operation::LDA { target: Target::BC }, instruction.operation);
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
                target: Target::B,
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
                    target: Target::SP
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
                    target: Target::SP,
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
                    target: Target::SP,
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
    fn test_execute_prefix() {
        let instruction = Instruction::new(1, 4, Operation::PREFIX);

        let memory = Memory::new();

        let mut cpu = Cpu::new();

        let new_pc = cpu.execute(&instruction);
        assert_eq!(1, new_pc);
        assert!(cpu.prefixed);
    }
}
