use bounded_vec_deque::BoundedVecDeque;
use chrono::{
    format::{DelayedFormat, StrftimeItems},
    DateTime, Local,
};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    sync::{Arc, Mutex},
};
use tracing::{
    field::{Field, Visit},
    Event, Level, Subscriber,
};
use tracing_subscriber::{layer::Context, Layer};

/// Key for the field containing the message in a tracing event.
const MESSAGE_KEY: &str = "message";

/// Value used in a log when no file name exists in the tracing event metadata.
const NO_FILE_VALUE: &str = "<unknown>";

/// Value used in a log when no line exists in the tracing event metadata.
const NO_LINE_VALUE: u32 = 0;

/// Value used in a log when no message field exists in the tracing event.
const NO_MESSAGE_VALUE: &str = "<none>";

/// Pattern used to format the timestamp that is output in a log.
const DEFAULT_TIMESTAMP_FORMAT: &str = "%FT%T%.3f";

/// Enumerates the supported logging levels for the emulator.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LogLevel {
    TRACE,
    DEBUG,
    INFO,
    WARN,
    ERROR,
}

impl Display for LogLevel {
    /// Writes a string representation of the [`LogLevel`] value to the formatter.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

impl From<Level> for LogLevel {
    /// Converts from an owned [`Level`] to a [`LogLevel`].
    fn from(value: Level) -> Self {
        match value {
            Level::TRACE => LogLevel::TRACE,
            Level::DEBUG => LogLevel::DEBUG,
            Level::INFO => LogLevel::INFO,
            Level::WARN => LogLevel::WARN,
            Level::ERROR => LogLevel::ERROR,
        }
    }
}

impl From<&Level> for LogLevel {
    /// Converts from a [`Level`] reference to a `LogLevel`.
    fn from(value: &Level) -> Self {
        LogLevel::from(*value)
    }
}

/// The [`LogEntry`] struct contains all relevant data collected when a log is emitted by the
/// application.
#[derive(Debug)]
pub struct LogEntry {
    // Level of the emitted log.
    pub level: LogLevel,
    // Timestamp when the log was emitted.
    pub timestamp: DateTime<Local>,
    // Name of the file where the log was emitted.
    pub file: String,
    // Line in the file where the log was emitted.
    pub line: u32,
    // Message value of the emitted log.
    pub message: String,
}

impl LogEntry {
    /// Formats the timestamp of the [`LogEntry`] using the default format string.
    pub fn format_timestamp(&self) -> DelayedFormat<StrftimeItems<'_>> {
        self.timestamp.format(DEFAULT_TIMESTAMP_FORMAT)
    }
}

/// A tracing [`Layer`] implementation which captures logs and buffers them in memory for display
/// in the UI.
#[derive(Debug)]
pub struct MessageCaptureLayer {
    /// Buffered log messages with a bounded size.
    messages: Arc<Mutex<BoundedVecDeque<LogEntry>>>,
}

impl MessageCaptureLayer {
    /// Creates a new [`MessageCaptureLayer`].
    pub fn new(messages: Arc<Mutex<BoundedVecDeque<LogEntry>>>) -> Self {
        Self { messages }
    }
}

impl<S> Layer<S> for MessageCaptureLayer
where
    S: Subscriber,
{
    fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
        let mut visitor = MessageCaptureVisitor::default();
        event.record(&mut visitor);

        let entry = LogEntry {
            level: event.metadata().level().into(),
            timestamp: Local::now(),
            file: event.metadata().file().unwrap_or(NO_FILE_VALUE).to_owned(),
            line: event.metadata().line().unwrap_or(NO_LINE_VALUE),
            message: visitor
                .fields
                .get(MESSAGE_KEY)
                .unwrap_or(&String::from(NO_MESSAGE_VALUE))
                .to_owned(),
        };

        self.messages
            .lock()
            .expect("lock acquired")
            .push_front(entry);
    }
}

/// A [`Visit`] implementation that simply pusehs the [`Debug`] representation of the field into a
/// [`HashMap`].
#[derive(Debug, Default)]
struct MessageCaptureVisitor<'k> {
    /// Contains the field data for a given [`Event`].
    fields: HashMap<&'k str, String>,
}

impl Visit for MessageCaptureVisitor<'_> {
    fn record_debug(&mut self, field: &Field, value: &dyn Debug) {
        self.fields.insert(field.name(), format!("{:?}", value));
    }
}
