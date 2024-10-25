use crate::cart::MBC;

use std::{cell::RefCell, rc::Rc};

pub struct MMU {
    /// Memory bank controller from the [`Cartridge`].
    mbc: Rc<RefCell<dyn MBC>>,
}

impl MMU {
    /// Creates a new [`MMU`].
    pub fn new(mbc: Rc<RefCell<dyn MBC>>) -> Self {
        Self { mbc }
    }
    /// Reads a single byte from memory at the given address.
    pub fn read_u8(&self, address: u16) -> u8 {
        self.mbc.borrow().read(address)
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
        self.mbc.borrow().read(address) as i8
    }
    /// Writes a single byte to memory at the given address.
    pub fn write_u8(&mut self, address: u16, byte: u8) {
        self.mbc.borrow_mut().write(address, byte);
    }
    /// Writes a block of bytes to memory at the given start address.
    pub fn write_block(&mut self, start_addr: u16, bytes: &[u8]) {
        self.mbc.borrow_mut().write_block(start_addr, bytes);
    }
}

#[cfg(test)]
mod tests {
    use crate::cart::RomOnly;

    use super::*;

    #[test]
    fn test_rom_only_read_u8() {
        let mbc = Rc::new(RefCell::new(RomOnly::new()));

        let mut mmu = MMU::new(mbc);
        mmu.write_u8(0x1010, 200);

        let byte = mmu.read_u8(0x1010);
        assert_eq!(200, byte);
    }

    #[test]
    fn test_rom_only_read_u16() {
        let mbc = Rc::new(RefCell::new(RomOnly::new()));

        let mut mmu = MMU::new(mbc);
        mmu.write_u8(0x1010, 0x01);
        mmu.write_u8(0x1011, 0x01);

        let value = mmu.read_u16(0x1010);
        assert_eq!(0x0101, value);
    }

    #[test]
    fn test_rom_only_read_i8() {
        {
            let mbc = Rc::new(RefCell::new(RomOnly::new()));

            let mut mmu = MMU::new(mbc);
            mmu.write_u8(0x1010, 1);

            let value = mmu.read_i8(0x1010);
            assert_eq!(1, value);
        }
        {
            let mbc = Rc::new(RefCell::new(RomOnly::new()));

            let mut mmu = MMU::new(mbc);
            mmu.write_u8(0x1010, 0xFF);

            let value = mmu.read_i8(0x1010);
            assert_eq!(-1, value);
        }
    }

    #[test]
    fn test_rom_only_write_byte() {
        let mbc = Rc::new(RefCell::new(RomOnly::new()));

        let mut mmu = MMU::new(mbc);

        mmu.write_u8(0x1010, 200);
        assert_eq!(200, mmu.read_u8(0x1010));
    }

    #[test]
    fn test_rom_only_write_block() {
        let block = [200, 201, 202];

        let mbc = Rc::new(RefCell::new(RomOnly::new()));

        let mut mmu = MMU::new(mbc);
        mmu.write_block(0x1010, &block);

        assert_eq!(200, mmu.read_u8(0x1010));
        assert_eq!(201, mmu.read_u8(0x1011));
        assert_eq!(202, mmu.read_u8(0x1012));
    }
}
