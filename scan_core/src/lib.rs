//! Implementation of *program graphs* (PG) and *channel systems* (CS) formalisms[^1]
//! for use in the SCAN model checker.
//!
//! This crate is part of the [SCAN statistical model checker](https://convince-project.github.io/scan/)
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

pub use grammar::*;
use log::{info, trace};
pub use oracle::*;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
pub use smc::*;
use std::{
    fs::{File, create_dir, create_dir_all, rename},
    path::PathBuf,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU32, Ordering},
    },
    time::Instant,
};
use thiserror::Error;
pub use tracer::{TraceWriter, Tracer};
pub use transition_system::{Atom, TransitionSystem, TransitionSystemRun};

const TEMP: &str = ".temp";
const SUCCESSES: &str = "successes";
const FAILURES: &str = "failures";

/// The type that represents time.
pub type Time = u32;

/// Errors that can be returned by a `[Scan]` method
#[derive(Clone, Copy, Debug, Error)]
pub enum ScanError {
    /// Precision value is out-of-bounds
    #[error("out-of-bounds precision value: {0}")]
    OutOfBoundsPrecision(f64),
    /// Confidence value is out-of-bounds
    #[error("out-of-bounds confidence value: {0}")]
    OutOfBoundsConfidence(f64),
}

/// Final report for a verification run.
#[derive(Debug, Clone)]
pub struct Report {
    /// Total executions run (even partial).
    pub runs: u32,
    /// Successful executions.
    pub successes: u32,
    /// Failed executions.
    pub failures: u32,
    /// Breakdown of violations by property.
    pub violations: Vec<u32>,
}

// The possible outcomes of a model execution:
// - If the run was not completed (because the execution violated an assume), result is `None`.
// - If the run succeeded, or failed by violating the guarantees, result is `Some(violations)`
//   where the `violations` carries which (if any) guarantees were violated.
type RunOutcome = Option<Vec<bool>>;

/// The main type to interface with the verification capabilities of SCAN.
/// [`Scan`] holds the model, properties and other data necessary to run the verification process.
/// The type of properties is abstracted through the [`Oracle`] trait,
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

impl<O: Oracle + Clone> Scan<O> {
    fn verification(&self, confidence: f64, precision: f64, duration: Time) {
        assert!(0f64 < confidence && confidence < 1f64);
        assert!(0f64 < precision && precision < 1f64);

        let result =
            self.model
                .new_run()
                .experiment(duration, self.oracle.clone(), self.running.clone());
        if let Some(guarantees) = result
            && self.running.load(Ordering::Relaxed)
        {
            let local_successes;
            let local_failures;
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
                guarantees
                    .into_iter()
                    .zip(violations.iter_mut())
                    .filter(|(success, _)| !success)
                    .for_each(|(_, violations)| {
                        *violations += 1;
                    });
                // If guarantee is violated, we have found a counter-example!
                trace!("runs: {local_failures} failures");
            }
            let runs = local_successes + local_failures;
            // Division by 0 leads to Inf/NaN, which is not less than any other float number
            // so this actually works as expected even for runs = 0.
            let avg = local_successes as f64 / runs as f64;
            if adaptive_bound(avg, confidence, precision) <= runs as f64 {
                info!("adaptive bound satisfied");
                self.running.store(false, Ordering::Relaxed);
            }
        }
    }

    /// Statistically verifies the provided [`TransitionSystem`] using adaptive bound and the given parameters.
    pub fn adaptive(
        &self,
        confidence: f64,
        precision: f64,
        duration: Time,
    ) -> Result<Report, ScanError> {
        if !(0f64 < confidence && confidence < 1f64) {
            return Err(ScanError::OutOfBoundsConfidence(confidence));
        }
        if !(0f64 < precision && precision < 1f64) {
            return Err(ScanError::OutOfBoundsPrecision(precision));
        }

        self.reset();

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        let runs = (0..)
            .map(|_| self.verification(confidence, precision, duration))
            .take_while(|_| self.running.load(Ordering::Relaxed))
            .count() as u32;

        let elapsed = start_time.elapsed();
        info!("verification completed in {elapsed:0.2?}");
        Ok(Report {
            runs,
            successes: self.successes(),
            failures: self.failures(),
            violations: self.violations(),
        })
    }

    /// Produces and saves the traces for the given number of runs,
    /// using the provided [`Tracer`].
    pub fn traces<T>(&self, runs: usize, duration: Time, path: PathBuf, model_data: &T::ModelData)
    where
        T: Tracer,
    {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("tracing starting");
        let start_time = Instant::now();
        create_traces_dirs_tree(path.clone());

        (0..runs).for_each(|idx| {
            self.trace::<T>(duration, path.clone(), model_data, idx);
        });

        let elapsed = start_time.elapsed();
        info!("tracing completed in {elapsed:0.2?}");
    }

    fn trace<T>(&self, duration: u32, mut path: PathBuf, model_data: &T::ModelData, idx: usize)
    where
        T: Tracer,
    {
        let mut ts = self.model.new_run();
        let filename = PathBuf::new()
            .with_file_name(format!("{idx:04}"))
            .with_extension(T::EXTENSION);
        path.push(TEMP);
        path.push(&filename);
        path.add_extension("gz");
        let file = File::create_new(&path).expect("create file");
        let writer = flate2::GzBuilder::new()
            .filename(filename.to_str().expect("file name"))
            .comment("Scan-generated execution trace")
            .write(file, flate2::Compression::best());
        let tracer = T::init(writer, model_data);
        if let Some(verified) = ts.trace::<T, _>(duration, self.oracle.clone(), tracer, model_data)
        {
            let mut new_path = path.clone();
            // pop file name
            new_path.pop();
            // pop temp folder
            new_path.pop();
            if verified.into_iter().all(|b| b) {
                new_path.push(SUCCESSES);
            } else {
                new_path.push(FAILURES);
            }
            new_path.push(path.file_name().expect("file name"));
            rename(&path, new_path).expect("renaming");
        }
    }
}

impl<O> Scan<O>
where
    O: Oracle + Clone + Sync,
{
    /// Statistically verifies the provided [`TransitionSystem`] using adaptive bound and the given parameters,
    /// spawning multiple threads.
    pub fn par_adaptive(
        &self,
        confidence: f64,
        precision: f64,
        duration: Time,
    ) -> Result<Report, ScanError> {
        if !(0f64 < confidence && confidence < 1f64) {
            return Err(ScanError::OutOfBoundsConfidence(confidence));
        }
        if !(0f64 < precision && precision < 1f64) {
            return Err(ScanError::OutOfBoundsPrecision(precision));
        }

        self.reset();

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        let runs = (0..usize::MAX)
            .into_par_iter()
            .map(|_| self.verification(confidence, precision, duration))
            .take_any_while(|_| self.running.load(Ordering::Relaxed))
            .count() as u32;

        let elapsed = start_time.elapsed();
        info!("verification completed in {elapsed:0.2?}");
        Ok(Report {
            runs,
            successes: self.successes(),
            failures: self.failures(),
            violations: self.violations(),
        })
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
        T: Tracer,
        T::ModelData: Sync,
    {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("tracing starting");
        let start_time = Instant::now();
        create_traces_dirs_tree(path.clone());

        (0..runs).into_par_iter().for_each(|idx| {
            self.trace::<T>(duration, path.clone(), model_data, idx);
        });

        let elapsed = start_time.elapsed();
        info!("tracing completed in {elapsed:0.2?}");
    }
}

fn create_traces_dirs_tree(mut path: PathBuf) {
    create_dir_all(&path).expect("create base dir");
    path.push(TEMP);
    create_dir(&path).expect("create temp dir");
    assert!(path.pop());
    path.push(SUCCESSES);
    create_dir(&path).expect("create successes dir");
    assert!(path.pop());
    path.push(FAILURES);
    create_dir(&path).expect("create failures dir");
}
