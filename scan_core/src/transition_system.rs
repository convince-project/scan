use crate::{PmtlOracle, RunOutcome, Time, Val};
use log::trace;
use std::{
    error::Error,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
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
    fn finalize(self, outcome: RunOutcome);
}

pub trait TransitionSystem<Event, Err: Error>: Clone + Send + Sync {
    fn transition(&mut self, duration: Time) -> Result<Option<Event>, Err>;

    fn time(&self) -> Time;

    fn labels(&self) -> Vec<bool>;

    fn state(&self) -> impl Iterator<Item = &Val>;

    fn experiment<P>(
        mut self,
        // max_length: usize,
        duration: Time,
        mut oracle: PmtlOracle,
        mut tracer: Option<P>,
        running: Arc<AtomicBool>,
    ) -> Result<RunOutcome, Err>
    where
        P: Tracer<Event>,
    {
        // WARN: without reseeding experiments will not be randomized!
        // self.cs.reseed_rng();
        // let mut current_len = 0;
        trace!("new run starting");
        if let Some(tracer) = tracer.as_mut() {
            tracer.init();
        }
        let result = loop {
            if let Some(event) = self.transition(duration)? {
                // current_len += 1;
                let labels = self.labels();
                let time = self.time();
                if let Some(tracer) = tracer.as_mut() {
                    tracer.trace(&event, time, self.state());
                }
                oracle = oracle.update(&labels, time);
                if !running.load(Ordering::Relaxed) {
                    trace!("run stopped");
                    return Ok(RunOutcome::Incomplete);
                } else if oracle.output_assumes().is_some() {
                    trace!("run undetermined");
                    break RunOutcome::Incomplete;
                } else if let Some(i) = oracle.output_guarantees() {
                    trace!("run fails");
                    break RunOutcome::Fail(i);
                    // } else if current_len >= max_length {
                    //     trace!("run exceeds maximum lenght");
                    //     return Ok(RunOutcome::Incomplete);
                }
            } else {
                trace!("run succeeds");
                break RunOutcome::Success;
            }
        };
        if let Some(tracer) = tracer {
            tracer.finalize(result);
        }
        Ok(result)
    }
}
