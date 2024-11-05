use crate::mem::Mapper;

/// Memory address of the `DIV` register.
const DIV_ADDRESS: u16 = 0xFF04;

/// The [`Timer`] struct manages the state of the timer and divider registers.
#[derive(Debug, Default)]
pub struct Timer {
    /// Holds value of the `DIV` register.
    div: u8,
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
        match address {
            DIV_ADDRESS => self.div,
            _ => panic!("not yet implemented"),
        }
    }
    /// Writes the timer-related value mapped to the given address.
    fn write_u8(&mut self, address: u16, _byte: u8) {
        match address {
            DIV_ADDRESS => self.div = 0,
            _ => panic!("not yet implemented"),
        }
    }
}
