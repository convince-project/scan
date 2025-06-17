use clap::Parser;
use scan_core::{Oracle, Scan, Time, Tracer, TransitionSystem};

/// Produce execution traces.
#[derive(Debug, Clone, Parser)]
#[deny(missing_docs)]
pub(crate) struct TraceArgs {
    /// Number of traces to save.
    pub(crate) traces: usize,
    /// Max duration of execution (in model-time).
    #[arg(short, long, default_value_t = 10000)]
    pub(crate) duration: Time,
    /// Run the model execution on a single thread.
    ///
    /// By default, SCAN uses multi-threading.
    /// Use this flag to run the model on a single thread.
    ///
    /// WARNING: only the running of the model is single-threaded.
    /// Other related functionalities, including model parsing and building, etc.,
    /// might still be running multiple threads.
    #[arg(long)]
    pub(crate) single_thread: bool,
    // /// TODO: Do not compress the saved traces.
    // #[arg(long)]
    // pub(crate) uncompressed: bool,
}

impl TraceArgs {
    pub(crate) fn trace<E, Ts, O, Tr>(
        &self,
        // model: String,
        scan: Scan<E, Ts, O>,
        tracer: Tr,
    ) -> anyhow::Result<()>
    where
        Ts: TransitionSystem<E> + 'static,
        E: Clone + Send + Sync + 'static,
        O: Oracle + 'static,
        Tr: Tracer<E> + 'static,
    {
        scan.trace(self.traces, tracer, self.duration, self.single_thread)?;
        Ok(())
    }
}
