use crate::{Oracle, RunOutcome, Time, Val};
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

pub trait TransitionSystem<Event>: Clone + Send + Sync {
    type Err: Error;

    fn transition(&mut self, duration: Time) -> Result<Option<Event>, Self::Err>;

    fn time(&self) -> Time;

    fn labels(&self) -> Vec<bool>;

    fn state(&self) -> impl Iterator<Item = &Val>;

    fn experiment<P, O: Oracle>(
        mut self,
        // max_length: usize,
        duration: Time,
        mut oracle: O,
        mut tracer: Option<P>,
        running: Arc<AtomicBool>,
    ) -> Result<RunOutcome, Self::Err>
    where
        P: Tracer<Event>,
    {
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
                oracle.update(&labels, time);
                if !running.load(Ordering::Relaxed) {
                    trace!("run stopped");
                    return Ok(RunOutcome::Incomplete);
                } else if oracle.output_assumes().any(|f| f.is_some_and(|a| !a)) {
                    trace!("run undetermined");
                    break RunOutcome::Incomplete;
                } else if let Some(i) = oracle
                    .output_guarantees()
                    .enumerate()
                    .find_map(|(i, f)| f.is_some_and(|a| !a).then_some(i))
                {
                    trace!("run fails");
                    break RunOutcome::Fail(i);
                } else if oracle.output_guarantees().all(|f| f.is_some_and(|a| a)) {
                    // Early success if all properties are verified
                    trace!("run succeeds");
                    break RunOutcome::Success;
                }
            } else if oracle.final_output_assumes().any(|f| !f) {
                trace!("run undetermined");
                break RunOutcome::Incomplete;
            } else if let Some(i) = oracle
                .final_output_guarantees()
                .enumerate()
                .find_map(|(i, f)| (!f).then_some(i))
            {
                trace!("run fails");
                break RunOutcome::Fail(i);
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
