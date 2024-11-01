use crate::{cart::Mbc, input::Input};

use std::{cell::RefCell, fmt::Display, rc::Rc};

/// Bit position of the VBlank interrupt enabled flag.
const IE_VBLANK_BIT_POSITION: u8 = 0;

/// Bit position of the LCD interrupt enabled flag.
const IE_LCD_BIT_POSITION: u8 = 1;

/// Bit position of the Timer interrupt enabled flag.
const IE_TIMER_BIT_POSITION: u8 = 2;

/// Bit position of the Serial interrupt enabled flag.
const IE_SERIAL_BIT_POSITION: u8 = 3;

/// Bit position of the Joypad interrupt enabled flag.
const IE_JOYPAD_BIT_POSITION: u8 = 4;

/// New type wrapping a [`u8`] that represents the interrupt enabled state of the various
/// subsystems.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct InterruptEnabled(u8);

impl InterruptEnabled {
    /// Returns `true` if the Joypad interrupt flag is enabled.
    pub fn joypad(&self) -> bool {
        self.0 & (1 << IE_JOYPAD_BIT_POSITION) != 0
    }
    /// Returns `true` if the Serial interrupt flag is enabled.
    pub fn serial(&self) -> bool {
        self.0 & (1 << IE_SERIAL_BIT_POSITION) != 0
    }
    /// Returns `true` if the Timer interrupt flag is enabled.
    pub fn timer(&self) -> bool {
        self.0 & (1 << IE_TIMER_BIT_POSITION) != 0
    }
    /// Returns `true` if the LCD interrupt flag is enabled.
    pub fn lcd(&self) -> bool {
        self.0 & (1 << IE_LCD_BIT_POSITION) != 0
    }
    /// Returns `true` if the VBlank interrupt flag is enabled.
    pub fn v_blank(&self) -> bool {
        self.0 & (1 << IE_VBLANK_BIT_POSITION) != 0
    }
}

impl From<u8> for InterruptEnabled {
    /// Converts the given [`u8`] into a [`InterruptEnabled`].
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl From<InterruptEnabled> for u8 {
    /// Converts the given [`InterruptEnabled`] into a [`u8`].
    fn from(value: InterruptEnabled) -> Self {
        value.0
    }
}

impl Display for InterruptEnabled {
    /// Writes a string representation of the [`InterruptEnabled`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// The memory map unit which is responsible for reading and writing the various types of memory
/// available on the GameBoy.
pub struct Mmu {
    /// Memory bank controller from the [`Cartridge`].
    pub mbc: Rc<RefCell<dyn Mbc>>,
    /// [`Input`] that contains state of the GameBoy controller.
    pub input: Rc<RefCell<dyn Input>>,
    /// Flag that determines if interrupts are enabled for various subsystems.
    pub ie: InterruptEnabled,
}

impl Mmu {
    /// Creates a new [`Mmu`].
    pub fn new(mbc: Rc<RefCell<dyn Mbc>>, input: Rc<RefCell<dyn Input>>) -> Self {
        Self {
            mbc,
            input,
            ie: InterruptEnabled::default(),
        }
    }
    /// Reads a single byte from memory at the given address.
    pub fn read_u8(&self, address: u16) -> u8 {
        if self.mbc.borrow().handles_address(address) {
            self.mbc.borrow().read(address)
        } else if self.input.borrow().handles_read(address) {
            self.input.borrow().read()
        } else if address == 0xFF0F {
            self.ie.into()
        } else {
            tracing::warn!("read from unmapped address: {:4x}", address);
            0
        }
    }
    /// Reads the next two bytes from the given address in memory and combines them into a single
    /// u16 value.
    pub fn read_u16(&self, address: u16) -> u16 {
        let low = self.mbc.borrow().read(address);
        let high = self.mbc.borrow().read(address + 1);

        (high as u16) << 8 | low as u16
    }
    /// Reads a single 8 bit signed integer from memory at the given address.
    pub fn read_i8(&self, address: u16) -> i8 {
        self.read_u8(address) as i8
    }
    /// Writes a single byte to memory at the given address.
    pub fn write_u8(&mut self, address: u16, byte: u8) {
        if self.mbc.borrow().handles_address(address) {
            self.mbc.borrow_mut().write(address, byte);
        } else if self.input.borrow().handles_read(address) {
            self.input.borrow_mut().write(byte);
        } else if address == 0xFF0F {
            self.ie = byte.into();
        } else {
            tracing::warn!("write to unmapped address: {:4x}", address);
        }
    }
    /// Writes a block of bytes to memory at the given start address.
    pub fn write_block(&mut self, start_addr: u16, bytes: &[u8]) {
        self.mbc.borrow_mut().write_block(start_addr, bytes);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::cart::RomOnly;

    struct TestInput;

    impl Input for TestInput {
        fn button_down(&mut self, _button: crate::input::Button) {}
        fn button_up(&mut self, _button: crate::input::Button) {}
        fn handles_write(&self, _address: u16) -> bool {
            false
        }
        fn write(&mut self, _byte: u8) {}
        fn handles_read(&self, _address: u16) -> bool {
            false
        }
        fn read(&self) -> u8 {
            0
        }
    }

    fn create_mmu() -> Mmu {
        let mbc = Rc::new(RefCell::new(RomOnly::new()));
        let input = Rc::new(RefCell::new(TestInput));

        Mmu::new(mbc, input)
    }

    #[test]
    fn test_mmu_rom_only_read_u8() {
        let mut mmu = create_mmu();
        mmu.write_u8(0x1010, 200);

        let byte = mmu.read_u8(0x1010);
        assert_eq!(200, byte);
    }

    #[test]
    fn test_mmu_rom_only_read_u16() {
        let mut mmu = create_mmu();
        mmu.write_u8(0x1010, 0x01);
        mmu.write_u8(0x1011, 0x01);

        let value = mmu.read_u16(0x1010);
        assert_eq!(0x0101, value);
    }

    #[test]
    fn test_mmu_rom_only_read_i8() {
        {
            let mut mmu = create_mmu();
            mmu.write_u8(0x1010, 1);

            let value = mmu.read_i8(0x1010);
            assert_eq!(1, value);
        }
        {
            let mut mmu = create_mmu();
            mmu.write_u8(0x1010, 0xFF);

            let value = mmu.read_i8(0x1010);
            assert_eq!(-1, value);
        }
    }

    #[test]
    fn test_mmu_rom_only_write_byte() {
        let mut mmu = create_mmu();
        mmu.write_u8(0x1010, 200);
        assert_eq!(200, mmu.read_u8(0x1010));
    }

    #[test]
    fn test_mmu_rom_only_write_block() {
        let block = [200, 201, 202];

        let mut mmu = create_mmu();
        mmu.write_block(0x1010, &block);

        assert_eq!(200, mmu.read_u8(0x1010));
        assert_eq!(201, mmu.read_u8(0x1011));
        assert_eq!(202, mmu.read_u8(0x1012));
    }

    #[test]
    fn test_interrupt_enabled_joypad() {
        {
            let ie = InterruptEnabled(1 << IE_JOYPAD_BIT_POSITION);
            assert!(ie.joypad());
        }
        {
            let ie = InterruptEnabled(0);
            assert!(!ie.joypad());
        }
    }

    #[test]
    fn test_interrupt_enabled_serial() {
        {
            let ie = InterruptEnabled(1 << IE_SERIAL_BIT_POSITION);
            assert!(ie.serial());
        }
        {
            let ie = InterruptEnabled(0);
            assert!(!ie.serial());
        }
    }

    #[test]
    fn test_interrupt_enabled_timer() {
        {
            let ie = InterruptEnabled(1 << IE_TIMER_BIT_POSITION);
            assert!(ie.timer());
        }
        {
            let ie = InterruptEnabled(0);
            assert!(!ie.timer());
        }
    }

    #[test]
    fn test_interrupt_enabled_lcd() {
        {
            let ie = InterruptEnabled(1 << IE_LCD_BIT_POSITION);
            assert!(ie.lcd());
        }
        {
            let ie = InterruptEnabled(0);
            assert!(!ie.lcd());
        }
    }

    #[test]
    fn test_interrupt_enabled_vblank() {
        {
            let ie = InterruptEnabled(1 << IE_VBLANK_BIT_POSITION);
            assert!(ie.v_blank());
        }
        {
            let ie = InterruptEnabled(0);
            assert!(!ie.v_blank());
        }
    }
}
