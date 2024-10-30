use std::{collections::HashMap, fmt::Display};

/// Bit position of the `A` button in the state byte.
const A_BIT_POSITION: u8 = 0;

/// Bit position of the `B` button in the state byte.
const B_BIT_POSITION: u8 = 1;

/// Bit position of the `Select` button in the state byte.
const SELECT_BIT_POSITION: u8 = 2;

/// Bit position of the `Start` button in the state byte.
const START_BIT_POSITION: u8 = 3;

/// Bit position of the `Right` D-Pad button in the state byte.
const RIGHT_BIT_POSITION: u8 = 0;

/// Bit position of the `Left` D-Pad button in the state byte.
const LEFT_BIT_POSITION: u8 = 1;

/// Bit position of the `Up` D-Pad button in the state byte.
const UP_BIT_POSITION: u8 = 2;

/// Bit position of the `Down` D-Pad button in the state byte.
const DOWN_BIT_POSITION: u8 = 3;

/// Bit position of the enable directions reading flag.
const DIRECTIONS_BIT_POSITION: u8 = 4;

/// Bit position of the enable buttons reading flag.
const BUTTONS_BIT_POSITION: u8 = 5;

/// Enumerates the buttons available on the GameBoy controller.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Button {
    Up,
    Down,
    Left,
    Right,
    A,
    B,
    Start,
    Select,
}

impl Display for Button {
    /// Writes a string version of the [`Button`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

/// Enumerates the valid read modes.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ReadMode {
    /// Neither reading of directions or buttons is allowed.
    Neither,
    /// Only reading of buttons is allowed.
    Buttons,
    /// Only reading of directions is allowed.
    Directions,
    /// Both reading of buttons and directions are allowed.
    Both,
}

/// The [`Input`] trait defines the behavior required to handle input for the GameBoy emulator.
pub trait Input {
    /// Marks the specified [`Button`] as being pressed.
    fn button_down(&mut self, button: Button);
    /// Marks the specified [`Button`] as being released.
    fn button_up(&mut self, button: Button);
    /// Determines if the [`Input`] implementation should handle the read at the address.
    fn handles_read(&self, address: u16) -> bool;
    /// Returns the current state of the input as a byte.
    fn read(&self) -> u8;
    /// Determines if the [`Input`] implementation should handle the write to the address.
    fn handles_write(&self, address: u16) -> bool;
    /// Writes the given byte back to the [`Input`] by updating the [`ReadMode`] based on the
    /// values in the relevant bit positions.
    fn write(&mut self, byte: u8);
}

/// Implementation of [`Input`] that represents the GameBoy controller.
#[derive(Debug)]
pub struct Controller {
    /// Current state of the buttons based on user input.
    state: HashMap<Button, bool>,
    /// Current read mode. Used to construct the byte that represents the input state.
    mode: ReadMode,
}

impl Default for Controller {
    /// Creates a default [`Controller`].
    fn default() -> Self {
        let state = HashMap::from([
            (Button::Up, false),
            (Button::Down, false),
            (Button::Left, false),
            (Button::Right, false),
            (Button::A, false),
            (Button::B, false),
            (Button::Start, false),
            (Button::Select, false),
        ]);

        Self {
            state,
            mode: ReadMode::Neither,
        }
    }
}

impl Controller {
    /// Creates a new default [`Input`].
    pub fn new() -> Self {
        Self::default()
    }
}

impl Input for Controller {
    /// Marks the specified [`Button`] as being pressed.
    fn button_down(&mut self, button: Button) {
        tracing::debug!("button down: {}", button);

        let value = self.state.get_mut(&button).expect("key for button exists");
        *value = true;
    }
    /// Marks the specified [`Button`] as being released.
    fn button_up(&mut self, button: Button) {
        tracing::debug!("button up: {}", button);

        let value = self.state.get_mut(&button).expect("key for button exists");
        *value = false;
    }
    /// Determines if the [`Input`] implementation should handle the write to the address.
    fn handles_write(&self, address: u16) -> bool {
        address == 0xFF00
    }
    /// Writes the given byte back to the [`Input`] by updating the [`ReadMode`] based on the
    /// values in the relevant bit positions.
    fn write(&mut self, byte: u8) {
        let directions = byte & (1 << DIRECTIONS_BIT_POSITION) == 0;
        let buttons = byte & (1 << BUTTONS_BIT_POSITION) == 0;

        let mode = match (directions, buttons) {
            (true, true) => ReadMode::Both,
            (true, false) => ReadMode::Directions,
            (false, true) => ReadMode::Buttons,
            (false, false) => ReadMode::Neither,
        };

        self.mode = mode;
    }
    /// Determines if the [`Input`] implementation should handle the read at the address.
    fn handles_read(&self, address: u16) -> bool {
        address == 0xFF00
    }
    /// Reads the current state of the input and returns the byte representation.
    fn read(&self) -> u8 {
        match self.mode {
            ReadMode::Neither | ReadMode::Both => 0x0F,
            ReadMode::Buttons => {
                let mut s: u8 = 0b00011111;

                if *self.state.get(&Button::Start).expect("key exists") {
                    s |= 1 << START_BIT_POSITION;
                } else {
                    s &= !(1 << START_BIT_POSITION);
                }

                if *self.state.get(&Button::Select).expect("key exists") {
                    s |= 1 << SELECT_BIT_POSITION;
                } else {
                    s &= !(1 << SELECT_BIT_POSITION);
                }

                if *self.state.get(&Button::A).expect("key exists") {
                    s |= 1 << A_BIT_POSITION;
                } else {
                    s &= !(1 << A_BIT_POSITION);
                }

                if *self.state.get(&Button::B).expect("key exists") {
                    s |= 1 << B_BIT_POSITION;
                } else {
                    s &= !(1 << B_BIT_POSITION);
                }

                s
            }
            ReadMode::Directions => {
                let mut s: u8 = 0b00101111;

                if *self.state.get(&Button::Up).expect("key exists") {
                    s |= 1 << UP_BIT_POSITION;
                } else {
                    s &= !(1 << UP_BIT_POSITION);
                }

                if *self.state.get(&Button::Down).expect("key exists") {
                    s |= 1 << DOWN_BIT_POSITION;
                } else {
                    s &= !(1 << DOWN_BIT_POSITION);
                }

                if *self.state.get(&Button::Left).expect("key exists") {
                    s |= 1 << LEFT_BIT_POSITION;
                } else {
                    s &= !(1 << LEFT_BIT_POSITION);
                }

                if *self.state.get(&Button::Right).expect("key exists") {
                    s |= 1 << RIGHT_BIT_POSITION;
                } else {
                    s &= !(1 << RIGHT_BIT_POSITION);
                }

                s
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_controller_button_down() {
        let buttons = vec![
            Button::Up,
            Button::Down,
            Button::Left,
            Button::Right,
            Button::A,
            Button::B,
            Button::Start,
            Button::Select,
        ];

        let mut controller = Controller::new();

        buttons.into_iter().for_each(|b| {
            controller.button_down(b);
            assert!(*controller.state.get(&b).expect("key exists"));
        })
    }

    #[test]
    fn test_controller_button_up() {
        let buttons = vec![
            Button::Up,
            Button::Down,
            Button::Left,
            Button::Right,
            Button::A,
            Button::B,
            Button::Start,
            Button::Select,
        ];

        let mut controller = Controller::new();

        buttons.into_iter().for_each(|b| {
            let value = controller.state.get_mut(&b).expect("key exists");
            *value = true;

            controller.button_up(b);
            assert!(!*controller.state.get(&b).expect("key exists"));
        })
    }

    #[test]
    fn test_controller_handles_read() {
        {
            let controller = Controller::new();
            assert!(controller.handles_read(0xFF00));
        }
        {
            let controller = Controller::new();
            assert!(!controller.handles_read(0xA000));
        }
    }

    #[test]
    fn test_controller_read_neither() {
        let mut controller = Controller::new();
        controller.write(0x30);
        assert_eq!(0x0F, controller.read());
    }

    #[test]
    fn test_controller_read_both() {
        let mut controller = Controller::new();
        controller.write(0xF0);
        assert_eq!(0x0F, controller.read());
    }

    #[test]
    fn test_controller_read_directions() {
        {
            let mut controller = Controller::new();
            controller.write(1 << BUTTONS_BIT_POSITION);
            controller.button_down(Button::Right);
            assert_eq!(0b00100001, controller.read());
        }
        {
            let mut controller = Controller::new();
            controller.write(1 << BUTTONS_BIT_POSITION);
            controller.button_down(Button::Left);
            assert_eq!(0b00100010, controller.read());
        }
        {
            let mut controller = Controller::new();
            controller.write(1 << BUTTONS_BIT_POSITION);
            controller.button_down(Button::Up);
            assert_eq!(0b00100100, controller.read());
        }
        {
            let mut controller = Controller::new();
            controller.write(1 << BUTTONS_BIT_POSITION);
            controller.button_down(Button::Down);
            assert_eq!(0b00101000, controller.read());
        }
    }

    #[test]
    fn test_controller_read_buttons() {
        {
            let mut controller = Controller::new();
            controller.write(1 << DIRECTIONS_BIT_POSITION);
            controller.button_down(Button::A);
            assert_eq!(0b00010001, controller.read());
        }
        {
            let mut controller = Controller::new();
            controller.write(1 << DIRECTIONS_BIT_POSITION);
            controller.button_down(Button::B);
            assert_eq!(0b00010010, controller.read());
        }
        {
            let mut controller = Controller::new();
            controller.write(1 << DIRECTIONS_BIT_POSITION);
            controller.button_down(Button::Select);
            assert_eq!(0b00010100, controller.read());
        }
        {
            let mut controller = Controller::new();
            controller.write(1 << DIRECTIONS_BIT_POSITION);
            controller.button_down(Button::Start);
            assert_eq!(0b00011000, controller.read());
        }
    }

    #[test]
    fn test_controller_handles_write() {
        {
            let controller = Controller::new();
            assert!(controller.handles_write(0xFF00));
        }
        {
            let controller = Controller::new();
            assert!(!controller.handles_write(0xA000));
        }
    }
}
