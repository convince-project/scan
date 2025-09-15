use anyhow::anyhow;
use clap::Parser;
use scan_core::{Oracle, Scan, Time, Tracer, TransitionSystem};

const ALL_PROPS_ERR: &str =
    "the --all flag is incompatible with individually-specified properties.\n
Examples:
'scan PATH/TO/MODEL trace' executes the model once and writes the trace to disk
'scan PATH/TO/MODEL verify PROPERTY_1 PROPERTY_2' executes the model once and writes the trace to disk, classifying it according to verification outcome of the properties PROPERTY_1 and PROPERTY_2 together over the model
'scan PATH/TO/MODEL verify --all' executes the model once and writes the trace to disk, and classifying it according to verification outcome of all specified properties together over the model";

/// Produce execution traces.
#[derive(Debug, Clone, Parser)]
#[deny(missing_docs)]
pub(crate) struct TraceArgs {
    /// Space-separated list of properties to verify while tracing.
    pub(crate) properties: Vec<String>,
    /// Verify all properties found in the model specification while tracing.
    /// It is equivalent to listing all of the properties.
    #[arg(short, long)]
    pub(crate) all: bool,
    /// Number of traces to save.
    #[arg(long, default_value_t = 1)]
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
    pub(crate) fn validate(&self) -> anyhow::Result<()> {
        if !self.properties.is_empty() && self.all {
            Err(anyhow!(ALL_PROPS_ERR))
        } else {
            Ok(())
        }
    }

    pub(crate) fn trace<'a, D, O, Event, Ts, Tr>(&self, scan: &'a Scan<D, O>, tracer: Tr)
    where
        D: Sync + 'a,
        O: Oracle,
        Ts: TransitionSystem<Event> + From<&'a D>,
        Tr: Tracer<Event>,
    {
        if self.single_thread {
            scan.traces::<Event, Ts, Tr>(self.traces, tracer, self.duration);
        } else {
            scan.par_traces::<Event, Ts, Tr>(self.traces, tracer, self.duration);
        }
    }
}
