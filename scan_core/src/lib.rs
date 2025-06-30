//! Implementation of *program graphs* (PG) and *channel systems* (CS) formalisms[^1]
//! for use in the SCAN model checker.
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod channel_system;
mod grammar;
mod model;
mod mtl;
mod pg_model;
mod pmtl;
pub mod program_graph;
mod smc;
mod transition_system;

pub use grammar::*;
use log::{info, trace};
pub use model::*;
pub use mtl::*;
pub use pg_model::PgModel;
pub use pmtl::*;
use rand::RngCore;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
pub use smc::*;
use std::{
    marker::PhantomData,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, AtomicU32, Ordering},
    },
    time::Instant,
};
pub use transition_system::*;

/// The type that represents time.
pub type Time = u32;

struct DummyRng;

impl RngCore for DummyRng {
    fn next_u32(&mut self) -> u32 {
        panic!("DummyRng should never be called")
    }

    fn next_u64(&mut self) -> u64 {
        panic!("DummyRng should never be called")
    }

    fn fill_bytes(&mut self, dst: &mut [u8]) {
        let _ = dst;
        panic!("DummyRng should never be called")
    }
}

/// The possible outcomes of a model execution.
#[derive(Debug, Clone)]
pub enum RunOutcome {
    /// The run was not completed.
    /// This can happen because:
    ///
    /// - Execution exceeded maximum lenght;
    /// - Execution exceeded maximum duration; or
    /// - Execution violated an assume.
    Incomplete,
    /// The run failed by violating the guarantees corresponding to the given index.
    Verified(Vec<bool>),
}

/// Implementators are induced by a temporal property.
/// They can update their internal state when fed a new state of a trace,
/// and establish whether their corresponding property holds on such trace.
pub trait Oracle: Clone + Send + Sync {
    /// Update the internal state of the [`Oracle`] with the latest state of a temporal trace.
    fn update(&mut self, state: &[bool], time: Time);

    /// Returns the values of the "assume" properties,
    /// if already determined.
    fn output_assumes(&self) -> impl Iterator<Item = Option<bool>>;

    /// Returns the values of the "guarantee" properties,
    /// if already determined.
    fn output_guarantees(&self) -> impl Iterator<Item = Option<bool>>;

    /// As the trace ends, the values of the "assume" properties is determined to be either true or false.
    fn final_output_assumes(&self) -> impl Iterator<Item = bool>;

    /// As the trace ends, the values of the "guarantee" properties is determined to be either true or false.
    fn final_output_guarantees(&self) -> impl Iterator<Item = bool>;
}

/// The main type to interface with the verification capabilities of SCAN.
/// [`Scan`] holds the model, properties and other data necessary to run the verification process.
/// The type of models and properties is abstracted through the [`TransitionSystem`] and [`Oracle`] traits,
/// to provide a unified interface.
#[derive(Clone)]
pub struct Scan<Event, Ts, O>
where
    Ts: TransitionSystem<Event>,
    O: Oracle,
{
    ts: Arc<Ts>,
    oracle: Arc<O>,
    running: Arc<AtomicBool>,
    successes: Arc<AtomicU32>,
    failures: Arc<AtomicU32>,
    violations: Arc<Mutex<Vec<u32>>>,
    _event: PhantomData<Event>,
}

impl<Event, T, O> Scan<Event, T, O>
where
    T: TransitionSystem<Event>,
    O: Oracle,
    Event: Sync,
{
    /// Create a new [`Scan`] object with the given [`TransitionSystem`] and [`Oracle`].
    pub fn new(ts: T, oracle: O) -> Self {
        Self {
            ts: Arc::new(ts),
            oracle: Arc::new(oracle),
            running: Arc::new(AtomicBool::new(false)),
            successes: Arc::new(AtomicU32::new(0)),
            failures: Arc::new(AtomicU32::new(0)),
            violations: Arc::new(Mutex::new(Vec::new())),
            _event: PhantomData,
        }
    }

    /// Tells whether a verification task is currently running.
    pub fn running(&self) -> bool {
        self.running.load(Ordering::Relaxed)
    }

    /// Returns the number of successful executions in the current verification run.
    pub fn successes(&self) -> u32 {
        self.successes.load(Ordering::Relaxed)
    }

    /// Returns the number of failed executions in the current verification run.
    pub fn failures(&self) -> u32 {
        self.failures.load(Ordering::Relaxed)
    }

    /// Returns a vector where each entry contains the number of violations of the associated property in the current verification run.
    pub fn violations(&self) -> Vec<u32> {
        self.violations.lock().expect("lock").clone()
    }

    /// Statistically verifies [`CsModel`] using adaptive bound and the given parameters.
    /// It allows to optionally pass a [`Tracer`] object to record the produced traces,
    /// and a state [`Mutex`] to be updated with the results as they are produced.
    pub fn adaptive(
        &self,
        confidence: f64,
        precision: f64,
        // length: usize,
        duration: Time,
        single_thread: bool,
    ) -> Result<(), T::Err> {
        // Cannot return as a T::Err, don't want to include anyhow in scan_core
        assert!(0f64 < confidence && confidence < 1f64);
        assert!(0f64 < precision && precision < 1f64);

        self.successes.store(0, Ordering::Relaxed);
        self.failures.store(0, Ordering::Relaxed);
        self.violations.lock().unwrap().clear();
        self.running.store(true, Ordering::Relaxed);

        let ts = self.ts.clone();
        let oracle = self.oracle.clone();
        let running = self.running.clone();
        let successes = self.successes.clone();
        let failures = self.failures.clone();
        let violations = self.violations.clone();

        let verification = || {
            let local_successes;
            let local_failures;

            let result = ts.as_ref().clone().experiment(
                duration,
                oracle.as_ref().clone(),
                running.clone(),
            )?;
            if !running.load(Ordering::Relaxed) {
                return Ok(());
            }
            match result {
                RunOutcome::Verified(guarantees) => {
                    if guarantees.iter().all(|b| *b) {
                        local_successes = successes.fetch_add(1, Ordering::Relaxed);
                        local_failures = failures.load(Ordering::Relaxed);
                        // If all guarantees are satisfied, the execution is successful
                        trace!("runs: {local_successes} successes");
                    } else {
                        local_successes = successes.load(Ordering::Relaxed);
                        local_failures = failures.fetch_add(1, Ordering::Relaxed);
                        let violations = &mut *violations.lock().unwrap();
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
                RunOutcome::Incomplete => return Ok(()),
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
                running.store(false, Ordering::Relaxed);
            }
            Ok(())
        };

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        if single_thread {
            (0..usize::MAX)
                .map(|_| verification())
                .take_while(|_| running.load(Ordering::Relaxed))
                .try_for_each(|b| b.map(|_| ()))?;
            // .take_while(|b| *b)
            // .count();
        } else {
            (0..usize::MAX)
                .into_par_iter()
                .map(|_| verification())
                .take_any_while(|_| running.load(Ordering::Relaxed))
                .try_for_each(|b| b.map(|_| ()))?;
        }

        let elapsed = start_time.elapsed();
        info!("verification time elapsed: {elapsed:0.2?}");
        info!("verification terminating");
        Ok(())
    }

    /// Produces and saves the traces for the given number of runs.
    pub fn trace<P>(
        &self,
        runs: usize,
        tracer: P,
        duration: Time,
        single_thread: bool,
    ) -> Result<(), T::Err>
    where
        P: Tracer<Event> + 'static,
    {
        let ts = self.ts.clone();
        let oracle = self.oracle.clone();
        let tracer = tracer.clone();

        let trace = || {
            ts.as_ref()
                .clone()
                .trace(duration, oracle.as_ref().clone(), tracer.clone())
        };

        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("verification starting");
        let start_time = Instant::now();

        if single_thread {
            (0..runs).try_for_each(|_| trace())?;
        } else {
            (0..runs).into_par_iter().try_for_each(|_| trace())?;
        }

        let elapsed = start_time.elapsed();
        info!("verification time elapsed: {elapsed:0.2?}");
        info!("verification terminating");
        Ok(())
    }
}
