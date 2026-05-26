use crate::{
    RunOutcome, Time, Val,
    channel_system::{Action, Event},
};

/// Trait that handles streaming of traces,
/// e.g., to print them to file.
pub trait Tracer {
    /// Initialize new streaming.
    ///
    /// This method needs to be called once, before calls to [`Self::trace`].
    fn init(&mut self);

    /// Stream a new state of the trace.
    fn trace<I: IntoIterator<Item = Val>>(
        &mut self,
        action: Action,
        event: &Event,
        time: Time,
        ports: I,
    );

    /// Finalize and close streaming.
    ///
    /// This method needs to be called at the end of the execution.
    fn finalize(self, outcome: &RunOutcome);
}
