//! Implementation of *program graphs* (PG) and *channel systems* (CS) formalisms[^1]
//! for use in the SCAN model checker.
//!
//! [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub mod channel_system;
mod cs_model;
mod dummy_rng;
mod grammar;
mod mtl;
mod oracle;
mod pg_model;
mod pmtl;
pub mod program_graph;
mod smc;
mod transition_system;

use core::marker::Sync;
pub use cs_model::{Atom, CsModel};
use dummy_rng::DummyRng;
pub use grammar::{Expression, Float, Integer, Type, TypeError, Val};
use log::{info, trace};
pub use mtl::{Mtl, MtlOracle};
pub use oracle::Oracle;
pub use pg_model::PgModel;
pub use pmtl::{Pmtl, PmtlOracle};
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
pub use transition_system::{Tracer, TransitionSystem};

/// The type that represents time.
pub type Time = u32;

/// The possible outcomes of a model execution.
#[derive(Debug, Clone)]
pub enum RunOutcome {
    /// The run was not completed.
    /// This can happen because:
    ///
    /// - Execution exceeded maximum length;
    /// - Execution exceeded maximum duration; or
    /// - Execution violated an assume.
    Incomplete,
    /// The run failed by violating the guarantees corresponding to the given index.
    Verified(Vec<bool>),
}

/// An object that can be immutably borrowed to spawn instances of its instance type,
/// provided that the instances have the same lifetime of their [`Definition`].
pub trait Definition {
    /// The instance type, parametrized by the lifetime of its [`Definition`].
    type I<'def>
    where
        Self: 'def;

    /// Immutably borrow the [`Definition`] and spawn a new instance with the same lifetime.
    fn new_instance<'def>(&'def self) -> Self::I<'def>;
}

/// The main type to interface with the verification capabilities of SCAN.
/// [`Scan`] holds the model, properties and other data necessary to run the verification process.
/// The type of models and properties is abstracted through the [`TransitionSystem`] and [`Oracle`] traits,
/// to provide a unified interface.
pub struct Scan<Event, D, O>
where
    D: Definition,
    for<'def> D::I<'def>: TransitionSystem<Event>,
    O: Oracle,
{
    tsd: D,
    oracle: O,
    running: Arc<AtomicBool>,
    successes: Arc<AtomicU32>,
    failures: Arc<AtomicU32>,
    violations: Arc<Mutex<Vec<u32>>>,
    _event: PhantomData<Event>,
}

impl<Event, D, O> Scan<Event, D, O>
where
    D: Definition,
    for<'def> D::I<'def>: TransitionSystem<Event>,
    O: Oracle,
{
    /// Create new [`Scan`] object, based on the given [`Definition`] of a [`TransitionSystem`] and [`Oracle`].
    pub fn new(tsd: D, oracle: O) -> Self {
        Scan {
            tsd,
            oracle,
            running: Arc::new(AtomicBool::new(false)),
            successes: Arc::new(AtomicU32::new(0)),
            failures: Arc::new(AtomicU32::new(0)),
            violations: Arc::new(Mutex::new(Vec::new())),
            _event: PhantomData,
        }
    }

    fn reset(&self) {
        self.successes.store(0, Ordering::Relaxed);
        self.failures.store(0, Ordering::Relaxed);
        self.violations.lock().unwrap().clear();
        self.running.store(true, Ordering::Relaxed);
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

    fn verification(&'_ self, confidence: f64, precision: f64, duration: Time) {
        let local_successes;
        let local_failures;

        let result =
            self.tsd
                .new_instance()
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

    /// Statistically verifies [`CsModel`] using adaptive bound and the given parameters.
    /// It allows to optionally pass a [`Tracer`] object to record the produced traces,
    /// and a state [`Mutex`] to be updated with the results as they are produced.
    pub fn adaptive(&'_ self, confidence: f64, precision: f64, duration: Time) {
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
        info!("verification time elapsed: {elapsed:0.2?}");
        info!("verification terminating");
    }

    fn trace<P>(&'_ self, tracer: P, duration: Time)
    where
        P: Tracer<Event>,
    {
        self.tsd
            .new_instance()
            .trace(duration, self.oracle.clone(), tracer)
    }

    /// Produces and saves the traces for the given number of runs.
    pub fn traces<P>(&'_ self, runs: usize, tracer: P, duration: Time)
    where
        P: Tracer<Event>,
    {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("tracing starting");
        let start_time = Instant::now();

        (0..runs).for_each(|_| self.trace(tracer.clone(), duration));

        let elapsed = start_time.elapsed();
        info!("tracing time elapsed: {elapsed:0.2?}");
        info!("tracing terminating");
    }
}

impl<Event, D, O> Scan<Event, D, O>
where
    D: Definition + Sync,
    for<'def> D::I<'def>: TransitionSystem<Event>,
    O: Oracle,
    Event: Sync,
{
    /// Statistically verifies [`CsModel`] using adaptive bound and the given parameters.
    /// It allows to optionally pass a [`Tracer`] object to record the produced traces,
    /// and a state [`Mutex`] to be updated with the results as they are produced.
    pub fn par_adaptive(&'_ self, confidence: f64, precision: f64, duration: Time) {
        // Cannot return as a T::Err, don't want to include anyhow in scan_core
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
        info!("verification time elapsed: {elapsed:0.2?}");
        info!("verification terminating");
    }

    /// Produces and saves the traces for the given number of runs.
    pub fn par_traces<P>(&'_ self, runs: usize, tracer: P, duration: Time)
    where
        P: Tracer<Event>,
    {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        info!("tracing starting");
        let start_time = Instant::now();

        (0..runs)
            .into_par_iter()
            .for_each(|_| self.trace(tracer.clone(), duration));

        let elapsed = start_time.elapsed();
        info!("tracing time elapsed: {elapsed:0.2?}");
        info!("tracing terminating");
    }
}
