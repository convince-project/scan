use crate::{RunOutcome, Time, Val};

/// Trait that handles streaming of traces,
/// e.g., to print them to file.
pub trait Tracer<A> {
    /// Initialize new streaming.
    ///
    /// This method needs to be called once, before calls to [`Self::trace`].
    fn init(&mut self);

    /// Stream a new state of the trace.
    fn trace<I: IntoIterator<Item = Val>>(&mut self, action: &A, time: Time, ports: I);

    /// Finalize and close streaming.
    ///
    /// This method needs to be called at the end of the execution.
    fn finalize(self, outcome: &RunOutcome);
}

// Dummy Tracer that does nothing
impl<A> Tracer<A> for () {
    fn init(&mut self) {}

    fn trace<I: IntoIterator<Item = Val>>(&mut self, _action: &A, _time: Time, _ports: I) {}

    fn finalize(self, _outcome: &RunOutcome) {}
}
