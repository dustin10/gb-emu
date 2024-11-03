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
    /// Returns the current state of the input as a byte.
    fn read(&self) -> u8;
    /// Writes the given byte back to the [`Input`] by updating the [`ReadMode`] based on the
    /// values in the relevant bit positions.
    fn write(&mut self, byte: u8);
}

/// Implementation of [`Input`] that represents the GameBoy joypad controller.
#[derive(Debug)]
pub struct Joypad {
    /// Current state of the buttons based on user input.
    state: HashMap<Button, bool>,
    /// Current read mode. Used to construct the byte that represents the input state.
    mode: ReadMode,
}

impl Default for Joypad {
    /// Creates a default [`Joypad`].
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

impl Joypad {
    /// Creates a new default [`Joypad`].
    pub fn new() -> Self {
        Self::default()
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

        s
    }
}

impl Input for Joypad {
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
    /// Reads the current state of the input and returns the byte representation.
    fn read(&self) -> u8 {
        match self.mode {
            ReadMode::Neither | ReadMode::Both => 0x0F,
            ReadMode::Buttons => self.buttons_state(),
            ReadMode::Directions => self.directions_state(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_joypad_button_down() {
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

        let mut joypad = Joypad::new();

        buttons.into_iter().for_each(|b| {
            joypad.button_down(b);
            assert!(*joypad.state.get(&b).expect("key exists"));
        })
    }

    #[test]
    fn test_joypad_button_up() {
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

        let mut joypad = Joypad::new();

        buttons.into_iter().for_each(|b| {
            let value = joypad.state.get_mut(&b).expect("key exists");
            *value = true;

            joypad.button_up(b);
            assert!(!*joypad.state.get(&b).expect("key exists"));
        })
    }

    #[test]
    fn test_joypad_read_neither() {
        let mut joypad = Joypad::new();
        joypad.write(0x30);
        assert_eq!(0x0F, joypad.read());
    }

    #[test]
    fn test_joypad_read_both() {
        let mut joypad = Joypad::new();
        joypad.write(0xF0);
        assert_eq!(0x0F, joypad.read());
    }

    #[test]
    fn test_joypad_read_directions() {
        {
            let mut joypad = Joypad::new();
            joypad.write(1 << BUTTONS_BIT_POSITION);
            joypad.button_down(Button::Right);
            assert_eq!(0b00100001, joypad.read());
        }
        {
            let mut joypad = Joypad::new();
            joypad.write(1 << BUTTONS_BIT_POSITION);
            joypad.button_down(Button::Left);
            assert_eq!(0b00100010, joypad.read());
        }
        {
            let mut joypad = Joypad::new();
            joypad.write(1 << BUTTONS_BIT_POSITION);
            joypad.button_down(Button::Up);
            assert_eq!(0b00100100, joypad.read());
        }
        {
            let mut joypad = Joypad::new();
            joypad.write(1 << BUTTONS_BIT_POSITION);
            joypad.button_down(Button::Down);
            assert_eq!(0b00101000, joypad.read());
        }
    }

    #[test]
    fn test_joypad_read_buttons() {
        {
            let mut joypad = Joypad::new();
            joypad.write(1 << DIRECTIONS_BIT_POSITION);
            joypad.button_down(Button::A);
            assert_eq!(0b00010001, joypad.read());
        }
        {
            let mut joypad = Joypad::new();
            joypad.write(1 << DIRECTIONS_BIT_POSITION);
            joypad.button_down(Button::B);
            assert_eq!(0b00010010, joypad.read());
        }
        {
            let mut joypad = Joypad::new();
            joypad.write(1 << DIRECTIONS_BIT_POSITION);
            joypad.button_down(Button::Select);
            assert_eq!(0b00010100, joypad.read());
        }
        {
            let mut joypad = Joypad::new();
            joypad.write(1 << DIRECTIONS_BIT_POSITION);
            joypad.button_down(Button::Start);
            assert_eq!(0b00011000, joypad.read());
        }
    }
}
