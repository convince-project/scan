//! Implementation of *program graphs* (PG) and *channel systems* (CS) formalisms[^1]
//! for use in the SCAN model checker.
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod channel_system;
mod grammar;
mod oracle;
pub mod program_graph;
mod smc;
mod tracer;
mod transition_system;

use flate2::write::GzEncoder;
pub use grammar::*;
use log::{info, trace};
pub use oracle::*;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
pub use smc::*;
use std::{
    fs::File,
    path::PathBuf,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU32, Ordering},
    },
    time::Instant,
};
pub use tracer::Tracer;
pub use transition_system::{Atom, TransitionSystem, TransitionSystemRun};

const TEMP: &str = ".temp";
const SUCCESSES: &str = "successes";
const FAILURES: &str = "failures";

/// The type that represents time.
pub type Time = u32;

/// The possible outcomes of a model execution.
#[derive(Debug, Clone)]
pub enum RunOutcome {
    /// The run was not completed because the execution violated an assume.
    Incomplete,
    /// The run failed by violating the guarantees corresponding to the given index (if any).
    Verified(Vec<bool>),
}

/// The main type to interface with the verification capabilities of SCAN.
/// [`Scan`] holds the model, properties and other data necessary to run the verification process.
/// The type of models and properties is abstracted through the [`Oracle`] trait,
/// to provide a unified interface.
#[derive(Debug, Clone)]
pub struct Scan<O> {
    model: TransitionSystem,
    oracle: O,
    running: Arc<AtomicBool>,
    successes: Arc<AtomicU32>,
    failures: Arc<AtomicU32>,
    violations: Arc<Mutex<Vec<u32>>>,
}

impl<O> Scan<O> {
    /// Create new [`Scan`] object.
    pub fn new(tsd: TransitionSystem, oracle: O) -> Self {
        Scan {
            model: tsd,
            oracle,
            running: Arc::new(AtomicBool::new(false)),
            successes: Arc::new(AtomicU32::new(0)),
            failures: Arc::new(AtomicU32::new(0)),
            violations: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn reset(&self) {
        self.successes.store(0, Ordering::Relaxed);
        self.failures.store(0, Ordering::Relaxed);
        self.violations.lock().unwrap().clear();
        self.running.store(true, Ordering::Relaxed);
    }

    /// Tells whether a verification task is currently running.
    #[inline]
    pub fn running(&self) -> bool {
        self.running.load(Ordering::Relaxed)
    }

    /// Returns the number of successful executions in the current verification run.
    #[inline]
    pub fn successes(&self) -> u32 {
        self.successes.load(Ordering::Relaxed)
    }

    /// Returns the number of failed executions in the current verification run.
    #[inline]
    pub fn failures(&self) -> u32 {
        self.failures.load(Ordering::Relaxed)
    }

    /// Returns a vector where each entry contains the number of violations of the associated property in the current verification run.
    #[inline]
    pub fn violations(&self) -> Vec<u32> {
        self.violations.lock().expect("lock").clone()
    }
}

impl<O: Oracle> Scan<O> {
    fn verification(&self, confidence: f64, precision: f64, duration: Time) {
        let local_successes;
        let local_failures;

        let result =
            self.model
                .new_run()
                .experiment(duration, self.oracle.clone(), self.running.clone());
        if !self.running.load(Ordering::Relaxed) {
            return;
        }
        match result {
            RunOutcome::Verified(guarantees) => {
                if guarantees.iter().all(|b| *b) {
                    local_successes = self.successes.fetch_add(1, Ordering::Relaxed);
                    local_failures = self.failures.load(Ordering::Relaxed);
                    // If all guarantees are satisfied, the execution is successful
                    trace!("runs: {local_successes} successes");
                } else {
                    local_successes = self.successes.load(Ordering::Relaxed);
                    local_failures = self.failures.fetch_add(1, Ordering::Relaxed);
                    let violations = &mut *self.violations.lock().unwrap();
                    violations.resize(violations.len().max(guarantees.len()), 0);
                    guarantees.iter().zip(violations.iter_mut()).for_each(
                        |(success, violations)| {
                            if !success {
                                *violations += 1;
                            }
                        },
                    );
                    // If guarantee is violated, we have found a counter-example!
                    trace!("runs: {local_failures} failures");
                }
            }
            RunOutcome::Incomplete => return,
        }
        let runs = local_successes + local_failures;
        // Avoid division by 0
        let avg = if runs == 0 {
            0.5f64
        } else {
            local_successes as f64 / runs as f64
        };
        if adaptive_bound(avg, confidence, precision) <= runs as f64 {
            info!("adaptive bound satisfied");
            self.running.store(false, Ordering::Relaxed);
        }
    }

    /// Statistically verifies the provided [`TransitionSystem`] using adaptive bound and the given parameters.
    pub fn adaptive(&self, confidence: f64, precision: f64, duration: Time) {
        // Cannot return as a T::Err, don't want to include anyhow in scan_core
        assert!(0f64 < confidence && confidence < 1f64);
        assert!(0f64 < precision && precision < 1f64);

        self.reset();

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        let _ = (0..usize::MAX)
            .map(|_| self.verification(confidence, precision, duration))
            .take_while(|_| self.running.load(Ordering::Relaxed))
            .count();

        let elapsed = start_time.elapsed();
        info!("verification completed in {elapsed:0.2?}");
    }

    /// Produces and saves the traces for the given number of runs,
    /// using the provided [`Tracer`].
    pub fn traces<T>(&self, runs: usize, duration: Time, path: PathBuf, model_data: &T::ModelData)
    where
        T: Tracer<GzEncoder<File>>,
    {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("tracing starting");
        assert!(path.is_dir());
        let start_time = Instant::now();

        (0..runs).for_each(|idx| {
            let mut ts = self.model.new_run();
            let mut path = path.clone();
            let filename = PathBuf::new()
                .with_file_name(format!("{idx:04}"))
                .with_extension(T::EXTENSION);
            path.push(crate::TEMP);
            path.push(&filename);
            path.add_extension("gz");
            let file = File::create_new(&path).expect("create file");
            let writer = flate2::GzBuilder::new()
                .filename(filename.to_str().expect("file name"))
                .comment("Scan-generated execution trace")
                .write(file, flate2::Compression::best());
            let tracer = T::init(writer, model_data);
            ts.trace::<T, _>(
                duration,
                self.oracle.clone(),
                tracer,
                path.clone(),
                model_data,
            )
        });

        let elapsed = start_time.elapsed();
        info!("tracing completed in {elapsed:0.2?}");
    }
}

impl<O> Scan<O>
where
    O: Oracle + Sync,
{
    /// Statistically verifies the provided [`TransitionSystem`] using adaptive bound and the given parameters,
    /// spawning multiple threads.
    pub fn par_adaptive(&self, confidence: f64, precision: f64, duration: Time) {
        assert!(0f64 < confidence && confidence < 1f64);
        assert!(0f64 < precision && precision < 1f64);

        self.reset();

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        let _ = (0..usize::MAX)
            .into_par_iter()
            .map(|_| self.verification(confidence, precision, duration))
            .take_any_while(|_| self.running.load(Ordering::Relaxed))
            .count();

        let elapsed = start_time.elapsed();
        info!("verification completed in {elapsed:0.2?}");
    }

    /// Produces and saves the traces for the given number of runs,
    /// using the provided [`Tracer`],
    /// spawning multiple threads.
    pub fn par_traces<T>(
        &self,
        runs: usize,
        duration: Time,
        path: PathBuf,
        model_data: &T::ModelData,
    ) where
        T: Sync + Tracer<GzEncoder<File>>,
        T::ModelData: Sync,
    {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("tracing starting");
        let start_time = Instant::now();

        (0..runs).into_par_iter().for_each(|idx| {
            let mut ts = self.model.new_run();
            let mut path = path.clone();
            let filename = PathBuf::new()
                .with_file_name(format!("{idx:04}"))
                .with_extension(T::EXTENSION);
            path.push(crate::TEMP);
            path.push(&filename);
            path.add_extension("gz");
            let file = File::create_new(&path).expect("create file");
            let writer = flate2::GzBuilder::new()
                .filename(filename.to_str().expect("file name"))
                .comment("Scan-generated execution trace")
                .write(file, flate2::Compression::best());
            let tracer = T::init(writer, model_data);
            ts.trace::<T, _>(
                duration,
                self.oracle.clone(),
                tracer,
                path.clone(),
                model_data,
            )
        });

        let elapsed = start_time.elapsed();
        info!("tracing completed in {elapsed:0.2?}");
    }
}
