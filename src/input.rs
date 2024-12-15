use crate::mem::Mapper;

use std::{collections::HashMap, fmt::Display};

/// Memory address of the input controller.
pub const INPUT_ADDRESS: u16 = 0xFF00;

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

/// Enumerates the buttons available for input to the GameBoy.
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
    /// Neither reading of directions nor buttons is allowed.
    Neither,
    /// Only reading of buttons is allowed.
    Buttons,
    /// Only reading of directions is allowed.
    Directions,
    /// Both reading of buttons and directions are allowed.
    Both,
}

impl Display for ReadMode {
    /// Writes a string representation of the [`ReadMode`] to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

/// The [`Input`] struct represents the GameBoy joypad and maintains the state of the buttons
/// pressed by the user.
#[derive(Debug)]
pub struct Input {
    /// Current state of the buttons based on user input.
    state: HashMap<Button, bool>,
    /// Current read mode. Used to construct the byte that represents the input state.
    mode: ReadMode,
}

impl Default for Input {
    /// Creates a default [`Input`].
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

impl Input {
    /// Creates a new default [`Input`].
    pub fn new() -> Self {
        Self::default()
    }
    /// Marks the specified [`Button`] as being pressed.
    pub fn button_down(&mut self, button: Button) {
        tracing::debug!("button down: {}", button);

        let value = self.state.get_mut(&button).expect("key for button exists");
        *value = true;
    }
    /// Marks the specified [`Button`] as being released.
    pub fn button_up(&mut self, button: Button) {
        tracing::debug!("button up: {}", button);

        let value = self.state.get_mut(&button).expect("key for button exists");
        *value = false;
    }
    /// Returns the byte that represents the current state of the buttons.
    fn directions_state(&self) -> u8 {
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

        tracing::debug!("joystick button state read: {:#010b}", s);

        s
    }
    /// Returns the byte that represents the current state of the directions.
    fn buttons_state(&self) -> u8 {
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

        tracing::debug!("joystick direction state read: {:#010b}", s);

        s
    }
}

impl Mapper for Input {
    /// Reads the current state of the input and returns the byte representation.
    fn read_u8(&self, address: u16) -> u8 {
        assert_eq!(INPUT_ADDRESS, address);

        match self.mode {
            ReadMode::Neither | ReadMode::Both => 0x0F,
            ReadMode::Buttons => self.buttons_state(),
            ReadMode::Directions => self.directions_state(),
        }
    }
    /// Writes the given byte back to the [`Input`] by updating the [`ReadMode`] based on the
    /// values in the relevant bit positions.
    fn write_u8(&mut self, address: u16, byte: u8) {
        assert_eq!(INPUT_ADDRESS, address);

        let directions = byte & (1 << DIRECTIONS_BIT_POSITION) == 0;
        let buttons = byte & (1 << BUTTONS_BIT_POSITION) == 0;

        let mode = match (directions, buttons) {
            (true, true) => ReadMode::Both,
            (true, false) => ReadMode::Directions,
            (false, true) => ReadMode::Buttons,
            (false, false) => ReadMode::Neither,
        };

        tracing::debug!("input read mode set to {}", mode);

        self.mode = mode;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input_button_down() {
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

        let mut input = Input::new();

        buttons.into_iter().for_each(|b| {
            input.button_down(b);
            assert!(*input.state.get(&b).expect("key exists"));
        })
    }

    #[test]
    fn test_input_button_up() {
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

        let mut input = Input::new();

        buttons.into_iter().for_each(|b| {
            let value = input.state.get_mut(&b).expect("key exists");
            *value = true;

            input.button_up(b);
            assert!(!*input.state.get(&b).expect("key exists"));
        })
    }

    #[test]
    fn test_input_read_neither() {
        let mut input = Input::new();
        input.write_u8(INPUT_ADDRESS, 0x30);
        assert_eq!(0x0F, input.read_u8(INPUT_ADDRESS));
    }

    #[test]
    fn test_input_read_both() {
        let mut input = Input::new();
        input.write_u8(INPUT_ADDRESS, 0xF0);
        assert_eq!(0x0F, input.read_u8(INPUT_ADDRESS));
    }

    #[test]
    fn test_input_read_directions() {
        {
            let mut input = Input::new();
            input.write_u8(INPUT_ADDRESS, 1 << BUTTONS_BIT_POSITION);
            input.button_down(Button::Right);
            assert_eq!(0b00100001, input.read_u8(INPUT_ADDRESS));
        }
        {
            let mut input = Input::new();
            input.write_u8(INPUT_ADDRESS, 1 << BUTTONS_BIT_POSITION);
            input.button_down(Button::Left);
            assert_eq!(0b00100010, input.read_u8(INPUT_ADDRESS));
        }
        {
            let mut input = Input::new();
            input.write_u8(INPUT_ADDRESS, 1 << BUTTONS_BIT_POSITION);
            input.button_down(Button::Up);
            assert_eq!(0b00100100, input.read_u8(INPUT_ADDRESS));
        }
        {
            let mut input = Input::new();
            input.write_u8(INPUT_ADDRESS, 1 << BUTTONS_BIT_POSITION);
            input.button_down(Button::Down);
            assert_eq!(0b00101000, input.read_u8(INPUT_ADDRESS));
        }
    }

    #[test]
    fn test_input_read_buttons() {
        {
            let mut input = Input::new();
            input.write_u8(INPUT_ADDRESS, 1 << DIRECTIONS_BIT_POSITION);
            input.button_down(Button::A);
            assert_eq!(0b00010001, input.read_u8(INPUT_ADDRESS));
        }
        {
            let mut input = Input::new();
            input.write_u8(INPUT_ADDRESS, 1 << DIRECTIONS_BIT_POSITION);
            input.button_down(Button::B);
            assert_eq!(0b00010010, input.read_u8(INPUT_ADDRESS));
        }
        {
            let mut input = Input::new();
            input.write_u8(INPUT_ADDRESS, 1 << DIRECTIONS_BIT_POSITION);
            input.button_down(Button::Select);
            assert_eq!(0b00010100, input.read_u8(INPUT_ADDRESS));
        }
        {
            let mut input = Input::new();
            input.write_u8(INPUT_ADDRESS, 1 << DIRECTIONS_BIT_POSITION);
            input.button_down(Button::Start);
            assert_eq!(0b00011000, input.read_u8(INPUT_ADDRESS));
        }
    }

    #[test]
    #[should_panic]
    fn test_input_read_wrong_address() {
        let input = Input::new();
        input.read_u8(0);
    }

    #[test]
    #[should_panic]
    fn test_input_write_wrong_address() {
        let mut input = Input::new();
        input.write_u8(0, 1 << DIRECTIONS_BIT_POSITION);
    }
}
