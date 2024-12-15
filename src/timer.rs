use crate::mem::Mapper;

/// Memory address of the `DIV` register.
const DIV_ADDRESS: u16 = 0xFF04;

/// Memory address of the `TIMA` register.
const TIMA_ADDRESS: u16 = 0xFF05;

/// Memory address of the `TMA` register.
const TMA_ADDRESS: u16 = 0xFF06;

/// Memory address of the `TAC` register.
const TAC_ADDRESS: u16 = 0xFF07;

/// The [`Timer`] struct manages the state of the timer and divider registers.
#[derive(Debug, Default)]
pub struct Timer {
    /// Holds value of the `DIV` register.
    div: u8,
    /// Holds value of the `TIMA` register. The timer counter.
    tima: u8,
    /// Holds value of the `TMA` register. The timer modulo.
    tma: u8,
    /// Holds value of the `TAC` register. The timer control.
    tac: u8,
}

impl Timer {
    /// Creates a new default [`Timer`].
    pub fn new() -> Self {
        Self::default()
    }
    /// Steps the timer forward the specified number of ticks.
    pub fn step_for(&mut self, _ticks: u8) {
        todo!()
    }
}

impl Mapper for Timer {
    /// Reads the timer-related value mapped to the given address.
    fn read_u8(&self, address: u16) -> u8 {
        tracing::debug!("read timer address: {:#4x}", address);
        match address {
            DIV_ADDRESS => self.div,
            TIMA_ADDRESS => self.tima,
            TMA_ADDRESS => self.tma,
            TAC_ADDRESS => self.tac,
            _ => panic!("not yet implemented"),
        }
    }
    /// Writes the timer-related value mapped to the given address.
    fn write_u8(&mut self, address: u16, byte: u8) {
        tracing::debug!("write timer address: {:#4x} = {:#2x}", address, byte);
        match address {
            DIV_ADDRESS => self.div = 0,
            TIMA_ADDRESS => self.tima = byte,
            TMA_ADDRESS => self.tma = byte,
            TAC_ADDRESS => self.tac = byte,
            _ => panic!("not yet implemented"),
        }
    }
}
