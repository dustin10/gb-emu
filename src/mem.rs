use crate::{
    cart::Mbc,
    input::{Input, INPUT_ADDRESS},
    ppu::Ppu,
    timer::Timer,
};

use std::{cell::RefCell, fmt::Display, rc::Rc};

/// Array of bytes that make up the boot ROM that was burned directly onto the CPU of the GameBoy.
/// These instructions are read to display the logo and then afterward control is given over to the
/// cartridge to start execution of the game.
const BOOT_ROM: [u8; 256] = [
    0x31, 0xFE, 0xFF, 0xAF, 0x21, 0xFF, 0x9F, 0x32, 0xCB, 0x7C, 0x20, 0xFB, 0x21, 0x26, 0xFF, 0x0E,
    0x11, 0x3E, 0x80, 0x32, 0xE2, 0x0C, 0x3E, 0xF3, 0xE2, 0x32, 0x3E, 0x77, 0x77, 0x3E, 0xFC, 0xE0,
    0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1A, 0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,
    0xFE, 0x34, 0x20, 0xF3, 0x11, 0xD8, 0x00, 0x06, 0x08, 0x1A, 0x13, 0x22, 0x23, 0x05, 0x20, 0xF9,
    0x3E, 0x19, 0xEA, 0x10, 0x99, 0x21, 0x2F, 0x99, 0x0E, 0x0C, 0x3D, 0x28, 0x08, 0x32, 0x0D, 0x20,
    0xF9, 0x2E, 0x0F, 0x18, 0xF3, 0x67, 0x3E, 0x64, 0x57, 0xE0, 0x42, 0x3E, 0x91, 0xE0, 0x40, 0x04,
    0x1E, 0x02, 0x0E, 0x0C, 0xF0, 0x44, 0xFE, 0x90, 0x20, 0xFA, 0x0D, 0x20, 0xF7, 0x1D, 0x20, 0xF2,
    0x0E, 0x13, 0x24, 0x7C, 0x1E, 0x83, 0xFE, 0x62, 0x28, 0x06, 0x1E, 0xC1, 0xFE, 0x64, 0x20, 0x06,
    0x7B, 0xE2, 0x0C, 0x3E, 0x87, 0xE2, 0xF0, 0x42, 0x90, 0xE0, 0x42, 0x15, 0x20, 0xD2, 0x05, 0x20,
    0x4F, 0x16, 0x20, 0x18, 0xCB, 0x4F, 0x06, 0x04, 0xC5, 0xCB, 0x11, 0x17, 0xC1, 0xCB, 0x11, 0x17,
    0x05, 0x20, 0xF5, 0x22, 0x23, 0x22, 0x23, 0xC9, 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
    0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
    0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
    0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E, 0x3C, 0x42, 0xB9, 0xA5, 0xB9, 0xA5, 0x42, 0x3C,
    0x21, 0x04, 0x01, 0x11, 0xA8, 0x00, 0x1A, 0x13, 0xBE, 0x00, 0x00, 0x23, 0x7D, 0xFE, 0x34, 0x20,
    0xF5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xFB, 0x86, 0x00, 0x00, 0x3E, 0x01, 0xE0, 0x50,
];

/// Bit position of the VBlank interrupt flag.
const IF_VBLANK_BIT_POSITION: u8 = 0;

/// Bit position of the LCD interrupt flag.
const IF_LCD_BIT_POSITION: u8 = 1;

/// Bit position of the Timer interrupt flag.
const IF_TIMER_BIT_POSITION: u8 = 2;

/// Bit position of the Serial interrupt flag.
const IF_SERIAL_BIT_POSITION: u8 = 3;

/// Bit position of the Joypad interrupt flag.
const IF_JOYPAD_BIT_POSITION: u8 = 4;

/// New type wrapping a [`u8`] that represents the interrupt flag state of the various
/// subsystems.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct InterruptFlag(u8);

impl InterruptFlag {
    /// Returns `true` if the Joypad interrupt flag is enabled.
    pub fn joypad(&self) -> bool {
        self.0 & (1 << IF_JOYPAD_BIT_POSITION) != 0
    }
    /// Returns `true` if the Serial interrupt flag is enabled.
    pub fn serial(&self) -> bool {
        self.0 & (1 << IF_SERIAL_BIT_POSITION) != 0
    }
    /// Returns `true` if the Timer interrupt flag is enabled.
    pub fn timer(&self) -> bool {
        self.0 & (1 << IF_TIMER_BIT_POSITION) != 0
    }
    /// Returns `true` if the LCD interrupt flag is enabled.
    pub fn lcd(&self) -> bool {
        self.0 & (1 << IF_LCD_BIT_POSITION) != 0
    }
    /// Returns `true` if the VBlank interrupt flag is enabled.
    pub fn v_blank(&self) -> bool {
        self.0 & (1 << IF_VBLANK_BIT_POSITION) != 0
    }
}

impl From<u8> for InterruptFlag {
    /// Converts the given [`u8`] into a [`InterruptFlag`].
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl From<InterruptFlag> for u8 {
    /// Converts the given [`InterruptFlag`] into a [`u8`].
    fn from(value: InterruptFlag) -> Self {
        value.0
    }
}

impl Display for InterruptFlag {
    /// Writes a string representation of the [`InterruptFlag`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// End of the addressable space for the MBC.
const END_MBC_ADDRESS: u16 = 0x7FFF;

/// Memory address of the Interrupt Enable (IE) flag.
const IE_ADDRESS: u16 = 0xFFFF;

/// Memory address of the Interrupt flag (IF).
const IF_ADDRESS: u16 = 0xFF0F;

/// Memory address of the BANK register.
const BANK_REG_ADDRESS: u16 = 0xFF50;

/// Start of the addressable space for VRAM.
const START_VRAM_ADDRESS: u16 = 0x8000;

/// End of the addressable space for VRAM.
const END_VRAM_ADDRESS: u16 = 0x9FFF;

/// Start of the addressable space for OAM.
const START_OAM_ADDRESS: u16 = 0xFE00;

/// End of the addressable space for OAM.
const END_OAM_ADDRESS: u16 = 0xFE9F;

/// Defines a simple interface for reading and writing bytes of memory.
pub trait Mapper {
    /// Reads a single byte from memory at the given address.
    fn read_u8(&self, address: u16) -> u8;
    /// Writes a single byte to memory at the given address.
    fn write_u8(&mut self, address: u16, byte: u8);
}

/// Enumerates the modes that the [`Mmu`] can execute in.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Mode {
    /// Mapping memory for the boot ROM.
    Boot,
    /// Mapping memory for the game on the cartridge.
    Cartridge,
}

impl Display for Mode {
    /// Writes a string representation of the [`Mode`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

impl From<Mode> for u8 {
    // Converts the specified [`Mode`] to a [`u8`].
    fn from(value: Mode) -> Self {
        match value {
            Mode::Boot => 0,
            _ => 1,
        }
    }
}

/// The memory map unit which is responsible for reading and writing the various types of memory
/// available on the GameBoy.
pub struct Mmu {
    /// [`Mode`] that memory is currently being mapped for.
    pub mode: Mode,
    /// [`Mbc`] implementation resolved from the [`Cartridge`] header.
    pub mbc: Rc<RefCell<dyn Mbc>>,
    /// [`Input`] that contains state of the GameBoy controller.
    pub input: Rc<RefCell<Input>>,
    /// [`Ppu`] that contains video memory.
    pub ppu: Rc<RefCell<Ppu>>,
    /// [`Timer`] that manages the state of the timers and divider registers.
    pub timer: Timer,
    /// Flag that determines if interrupts are enabled for various subsystems.
    pub interrupt_enabled: InterruptFlag,
    /// Flag that determines if interrupt handlers are requested for various subsystems.
    pub interrupt_flag: InterruptFlag,
}

impl Mmu {
    /// Creates a new [`Mmu`].
    pub fn new(
        mbc: Rc<RefCell<dyn Mbc>>,
        input: Rc<RefCell<Input>>,
        ppu: Rc<RefCell<Ppu>>,
    ) -> Self {
        Self {
            mode: Mode::Boot,
            mbc,
            input,
            ppu,
            timer: Timer::new(),
            interrupt_enabled: InterruptFlag::default(),
            interrupt_flag: InterruptFlag::default(),
        }
    }
    /// Writes a block of bytes to memory at the given start address.
    pub fn write_block_u8(&mut self, start_addr: u16, bytes: &[u8]) {
        self.mbc.borrow_mut().write_block(start_addr, bytes);
    }
}

impl Mapper for Mmu {
    /// Reads a single byte from memory at the given address.
    fn read_u8(&self, address: u16) -> u8 {
        tracing::debug!("dispatch read memory address: {:#06x}", address);

        match self.mode {
            Mode::Boot => match address {
                0..0xFF => BOOT_ROM[address as usize],
                _ => panic!("invalid boot ROM address read"),
            },
            Mode::Cartridge => match address {
                0..=END_MBC_ADDRESS => self.mbc.borrow().read_u8(address),
                START_VRAM_ADDRESS..=END_VRAM_ADDRESS => self.ppu.borrow().read_u8(address),
                INPUT_ADDRESS => self.input.borrow().read_u8(address),
                IE_ADDRESS => self.interrupt_enabled.into(),
                IF_ADDRESS => self.interrupt_flag.into(),
                BANK_REG_ADDRESS => self.mode.into(),
                START_OAM_ADDRESS..=END_OAM_ADDRESS => self.ppu.borrow().read_u8(address),
                _ => {
                    tracing::warn!("read unmapped memory address: {:#06x}", address);
                    0
                }
            },
        }
    }
    /// Writes a single byte to memory at the given address.
    fn write_u8(&mut self, address: u16, byte: u8) {
        tracing::debug!("dispatching write memory address: {:#06x}", address);

        match address {
            0..=END_MBC_ADDRESS => self.mbc.borrow_mut().write_u8(address, byte),
            START_VRAM_ADDRESS..=END_VRAM_ADDRESS => {
                self.ppu.borrow_mut().write_u8(address, byte);
            }
            INPUT_ADDRESS => self.input.borrow_mut().write_u8(address, byte),
            IE_ADDRESS => self.interrupt_enabled = byte.into(),
            IF_ADDRESS => self.interrupt_flag = byte.into(),
            BANK_REG_ADDRESS => {
                tracing::debug!("mmu mode switched to {}", Mode::Cartridge);
                self.mode = Mode::Cartridge;
            }
            START_OAM_ADDRESS..=END_OAM_ADDRESS => {
                self.ppu.borrow_mut().write_u8(address, byte);
            }
            _ => tracing::warn!("write to unmapped address: {:06x}", address),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::cart::RomOnly;

    fn create_mmu() -> Mmu {
        let mbc = Rc::new(RefCell::new(RomOnly::new()));
        let input = Rc::new(RefCell::new(Input::new()));
        let ppu = Rc::new(RefCell::new(Ppu::new()));

        let mut mmu = Mmu::new(mbc, input, ppu);
        mmu.mode = Mode::Cartridge;

        mmu
    }

    #[test]
    fn test_mmu_rom_only_read_u8() {
        let mut mmu = create_mmu();
        mmu.write_u8(0x1010, 200);

        let byte = mmu.read_u8(0x1010);
        assert_eq!(200, byte);
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
        mmu.write_block_u8(0x1010, &block);

        assert_eq!(200, mmu.read_u8(0x1010));
        assert_eq!(201, mmu.read_u8(0x1011));
        assert_eq!(202, mmu.read_u8(0x1012));
    }

    #[test]
    fn test_interrupt_enabled_joypad() {
        {
            let ie = InterruptFlag(1 << IF_JOYPAD_BIT_POSITION);
            assert!(ie.joypad());
        }
        {
            let ie = InterruptFlag(0);
            assert!(!ie.joypad());
        }
    }

    #[test]
    fn test_interrupt_enabled_serial() {
        {
            let ie = InterruptFlag(1 << IF_SERIAL_BIT_POSITION);
            assert!(ie.serial());
        }
        {
            let ie = InterruptFlag(0);
            assert!(!ie.serial());
        }
    }

    #[test]
    fn test_interrupt_enabled_timer() {
        {
            let ie = InterruptFlag(1 << IF_TIMER_BIT_POSITION);
            assert!(ie.timer());
        }
        {
            let ie = InterruptFlag(0);
            assert!(!ie.timer());
        }
    }

    #[test]
    fn test_interrupt_enabled_lcd() {
        {
            let ie = InterruptFlag(1 << IF_LCD_BIT_POSITION);
            assert!(ie.lcd());
        }
        {
            let ie = InterruptFlag(0);
            assert!(!ie.lcd());
        }
    }

    #[test]
    fn test_interrupt_enabled_vblank() {
        {
            let ie = InterruptFlag(1 << IF_VBLANK_BIT_POSITION);
            assert!(ie.v_blank());
        }
        {
            let ie = InterruptFlag(0);
            assert!(!ie.v_blank());
        }
    }
}
