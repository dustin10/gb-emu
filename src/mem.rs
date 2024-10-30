use crate::{cart::Mbc, input::Input};

use std::{cell::RefCell, rc::Rc};

/// The memory map unit which is responsible for reading and writing the various types of memory
/// available on the GameBoy.
pub struct Mmu {
    /// Memory bank controller from the [`Cartridge`].
    mbc: Rc<RefCell<dyn Mbc>>,
    /// [`Input`] that contains state of the GameBoy controller.
    input: Rc<RefCell<dyn Input>>,
}

impl Mmu {
    /// Creates a new [`Mmu`].
    pub fn new(mbc: Rc<RefCell<dyn Mbc>>, input: Rc<RefCell<dyn Input>>) -> Self {
        Self { mbc, input }
    }
    /// Reads a single byte from memory at the given address.
    pub fn read_u8(&self, address: u16) -> u8 {
        if self.input.borrow().handles_read(address) {
            self.input.borrow().read()
        } else {
            self.mbc.borrow().read(address)
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
        let value = if self.input.borrow().handles_read(address) {
            self.input.borrow().read()
        } else {
            self.mbc.borrow().read(address)
        };

        value as i8
    }
    /// Writes a single byte to memory at the given address.
    pub fn write_u8(&mut self, address: u16, byte: u8) {
        if self.input.borrow().handles_write(address) {
            self.input.borrow_mut().write(byte);
        } else {
            self.mbc.borrow_mut().write(address, byte);
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

    #[test]
    fn test_mmu_rom_only_read_u8() {
        let mbc = Rc::new(RefCell::new(RomOnly::new()));
        let input = Rc::new(RefCell::new(TestInput));

        let mut mmu = Mmu::new(mbc, input);
        mmu.write_u8(0x1010, 200);

        let byte = mmu.read_u8(0x1010);
        assert_eq!(200, byte);
    }

    #[test]
    fn test_mmu_rom_only_read_u16() {
        let mbc = Rc::new(RefCell::new(RomOnly::new()));
        let input = Rc::new(RefCell::new(TestInput));

        let mut mmu = Mmu::new(mbc, input);
        mmu.write_u8(0x1010, 0x01);
        mmu.write_u8(0x1011, 0x01);

        let value = mmu.read_u16(0x1010);
        assert_eq!(0x0101, value);
    }

    #[test]
    fn test_mmu_rom_only_read_i8() {
        {
            let mbc = Rc::new(RefCell::new(RomOnly::new()));
            let input = Rc::new(RefCell::new(TestInput));

            let mut mmu = Mmu::new(mbc, input);
            mmu.write_u8(0x1010, 1);

            let value = mmu.read_i8(0x1010);
            assert_eq!(1, value);
        }
        {
            let mbc = Rc::new(RefCell::new(RomOnly::new()));
            let input = Rc::new(RefCell::new(TestInput));

            let mut mmu = Mmu::new(mbc, input);
            mmu.write_u8(0x1010, 0xFF);

            let value = mmu.read_i8(0x1010);
            assert_eq!(-1, value);
        }
    }

    #[test]
    fn test_mmu_rom_only_write_byte() {
        let mbc = Rc::new(RefCell::new(RomOnly::new()));
        let input = Rc::new(RefCell::new(TestInput));

        let mut mmu = Mmu::new(mbc, input);

        mmu.write_u8(0x1010, 200);
        assert_eq!(200, mmu.read_u8(0x1010));
    }

    #[test]
    fn test_mmu_rom_only_write_block() {
        let block = [200, 201, 202];

        let mbc = Rc::new(RefCell::new(RomOnly::new()));
        let input = Rc::new(RefCell::new(TestInput));

        let mut mmu = Mmu::new(mbc, input);
        mmu.write_block(0x1010, &block);

        assert_eq!(200, mmu.read_u8(0x1010));
        assert_eq!(201, mmu.read_u8(0x1011));
        assert_eq!(202, mmu.read_u8(0x1012));
    }
}
