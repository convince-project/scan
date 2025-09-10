use crate::{Oracle, RunOutcome, Time, Val};
use core::marker::Sync;
use log::trace;
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};

/// Trait that handles streaming of traces,
/// e.g., to print them to file.
pub trait Tracer<A>: Clone + Send + Sync {
    /// Initialize new streaming.
    ///
    /// This method needs to be called once, before calls to [`Self::trace`].
    fn init(&mut self);

    /// Stream a new state of the trace.
    fn trace<'a, I: IntoIterator<Item = &'a Val>>(&mut self, action: &A, time: Time, ports: I);

    /// Finalize and close streaming.
    ///
    /// This method needs to be called at the end of the execution.
    fn finalize(self, outcome: &RunOutcome);
}

// Dummy Tracer that does nothing
impl<A> Tracer<A> for () {
    fn init(&mut self) {}

    fn trace<'a, I: IntoIterator<Item = &'a Val>>(&mut self, _action: &A, _time: Time, _ports: I) {}

    fn finalize(self, _outcome: &RunOutcome) {}
}

/// Trait for types that can execute like a transition system.
///
/// Together with an [`Oracle`], it provides a verifiable system.
pub trait TransitionSystem<Event> {
    /// Performs a (random) transition on the [`TransitionSystem`] and returns the raised `Event`,
    /// unless the execution is terminated and no further events can happen.
    fn transition(&mut self, duration: Time) -> Option<Event>;

    /// Current time of the [`TransitionSystem`] (for timed systems).
    fn time(&self) -> Time;

    /// The current values of the [`TransitionSystem`]'s labels.
    fn labels(&self) -> impl Iterator<Item = bool>;

    /// The current internal state of the [`TransitionSystem`].
    fn state(&self) -> impl Iterator<Item = &Val>;

    /// Runs a single execution of the [`TransitionSystem`] with a given [`Oracle`] and returns a [`RunOutcome`].
    fn experiment<O: Oracle>(
        &mut self,
        duration: Time,
        mut oracle: O,
        running: Arc<AtomicBool>,
    ) -> RunOutcome {
        trace!("new run starting");
        let mut time;
        // reuse vector to avoid allocations
        let mut labels = Vec::new();
        while let Some(_event) = self.transition(duration) {
            labels.clear();
            labels.extend(self.labels());
            time = self.time();
            if time >= duration {
                trace!("run stopped");
                return RunOutcome::Incomplete;
            }
            oracle.update(&labels, time);
            if !running.load(Ordering::Relaxed) {
                trace!("run stopped");
                return RunOutcome::Incomplete;
            } else if oracle.output_guarantees().all(|b| b.is_some()) {
                trace!("run complete early");
                let verified = Vec::from_iter(oracle.output_guarantees().map(Option::unwrap));
                return RunOutcome::Verified(verified);
            }
        }
        trace!("run complete");
        let verified = Vec::from_iter(oracle.final_output_guarantees());
        RunOutcome::Verified(verified)
    }

    /// Runs a single execution of the [`TransitionSystem`] with a given [`Oracle`]
    /// and process the execution trace via the given [`Tracer`].
    fn trace<P, O: Oracle>(&mut self, duration: Time, mut oracle: O, mut tracer: P)
    where
        P: Tracer<Event>,
    {
        // let mut current_len = 0;
        trace!("new run starting");
        let mut time;
        // reuse vector to avoid allocations
        let mut labels = Vec::new();
        tracer.init();
        while let Some(event) = self.transition(duration) {
            // current_len += 1;
            labels.clear();
            labels.extend(self.labels());
            time = self.time();
            if time >= duration {
                trace!("run stopped");
                tracer.finalize(&RunOutcome::Incomplete);
                return;
            }
            tracer.trace(&event, time, self.state());
            oracle.update(&labels, time);
        }
        trace!("run complete");
        let verified = Vec::from_iter(oracle.final_output_guarantees());
        tracer.finalize(&RunOutcome::Verified(verified));
    }
}
