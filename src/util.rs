use bounded_vec_deque::BoundedVecDeque;
use chrono::{DateTime, Utc};
use std::{
    collections::HashMap,
    fmt::Debug,
    sync::{Arc, Mutex},
    time::SystemTime,
};
use tracing::{
    field::{Field, Visit},
    Event, Subscriber,
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
const TIMESTAMP_FORMAT: &str = "%FT%T%.3f";

/// A [`Layer`] implementation which captures logs and buffers them in memory for display in the
/// UI.
#[derive(Debug)]
pub struct MessageCaptureLayer {
    /// Buffered log messages with a bounded size.
    messages: Arc<Mutex<BoundedVecDeque<String>>>,
}

impl MessageCaptureLayer {
    /// Creates a new [`MessageCaptureLayer`].
    pub fn new(messages: Arc<Mutex<BoundedVecDeque<String>>>) -> Self {
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

        let now: DateTime<Utc> = SystemTime::now().into();

        let mut messages = self.messages.lock().expect("lock acquired");

        messages.push_front(format!(
            "{} [{}] ({}:{}) - {}",
            now.format(TIMESTAMP_FORMAT),
            event.metadata().level(),
            event.metadata().file().unwrap_or(NO_FILE_VALUE),
            event.metadata().line().unwrap_or(NO_LINE_VALUE),
            visitor
                .fields
                .get(MESSAGE_KEY)
                .unwrap_or(&String::from(NO_MESSAGE_VALUE)),
        ));
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
