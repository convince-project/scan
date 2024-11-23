use crate::{Pmtl, PmtlOracle, Time};
use log::info;
use rand::prelude::*;
use rayon::prelude::*;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Atom<A: Clone + PartialEq + Eq> {
    Predicate(usize),
    Event(A),
}

// WARN: Representing a trace as a Vec of Vecs may be expensive.
pub type Trace<A> = Vec<(Time, A, Vec<bool>)>;

pub trait Publisher<A> {
    fn init(&mut self);

    fn publish(&mut self, action: &A, time: Time, state: &[bool]);

    fn finalize(self, success: Option<bool>);
}

/// Trait implementing a Transition System (TS), as defined in [^1].
/// As such, it is possible to verify it against MTL specifications.
///
/// [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.
pub trait TransitionSystem: Clone + Send + Sync {
    /// The type of the actions that trigger transitions between states in the TS.
    type Action: Debug + Clone + Eq + Send + Sync + Hash;

    /// The label function of the TS valuates the propositions of its set of propositions for the current state.
    // TODO FIXME: bitset instead of Vec<bool>?
    fn labels(&self) -> Vec<bool>;

    /// The transition relation relates [`Self::Action`]s and post-states that constitutes possible transitions from the current state.
    // fn transitions(self) -> Vec<(Self::Action, Self)>;

    fn monaco_transition<R: Rng>(&mut self, rng: &mut R, max_time: Time) -> Option<Self::Action>;

    fn time(&self) -> Time {
        0
    }

    fn experiment<P, R: Rng>(
        mut self,
        mut oracle: PmtlOracle<Self::Action>,
        mut publisher: Option<P>,
        length: usize,
        duration: Time,
        rng: &mut R,
    ) -> Option<bool>
    where
        P: Publisher<Self::Action>,
    {
        let mut current_len = 0;
        if let Some(publisher) = publisher.as_mut() {
            publisher.init();
        }
        while let Some(action) = self.monaco_transition(rng, duration) {
            current_len += 1;
            let state = self.labels();
            let time = self.time();
            if let Some(publisher) = publisher.as_mut() {
                publisher.publish(&action, time, &state);
            }
            oracle = oracle.update(&action, &state, time);
            match oracle.output() {
                Some(true) => {
                    if current_len >= length {
                        if let Some(publisher) = publisher {
                            publisher.finalize(None);
                        }
                        return None;
                    }
                }
                Some(false) => {
                    if let Some(publisher) = publisher {
                        publisher.finalize(Some(false));
                    }
                    return Some(false);
                }
                None => {
                    if let Some(publisher) = publisher {
                        publisher.finalize(None);
                    }
                    return None;
                }
            }
        }
        if let Some(publisher) = publisher {
            publisher.finalize(Some(true));
        }
        Some(true)
    }

    fn par_adaptive<P>(
        &self,
        guarantees: &[Pmtl<Atom<Self::Action>>],
        assumes: &[Pmtl<Atom<Self::Action>>],
        confidence: f64,
        precision: f64,
        length: usize,
        max_time: Time,
        s: Arc<AtomicU32>,
        f: Arc<AtomicU32>,
        publisher: Option<P>,
    ) where
        P: Publisher<Self::Action> + Clone + Send + Sync,
    {
        let oracle = PmtlOracle::new(assumes, guarantees);
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        (0..usize::MAX)
            .into_par_iter()
            .take_any_while(|_| {
                let mut rng = rand::thread_rng();
                let local_s;
                let local_f;
                if let Some(result) = self.clone().experiment(
                    oracle.clone(),
                    publisher.clone(),
                    length,
                    max_time,
                    &mut rng,
                ) {
                    if result {
                        // If all guarantees are satisfied, the execution is successful
                        local_s = s.fetch_add(1, Ordering::Relaxed) + 1;
                        local_f = f.load(Ordering::Relaxed);
                        info!("runs: {local_s} successes");
                    } else {
                        // If guarantee is violated, we have found a counter-example!
                        local_s = s.load(Ordering::Relaxed);
                        local_f = f.fetch_add(1, Ordering::Relaxed) + 1;
                        info!("runs: {local_f} failures");
                    }
                } else {
                    local_f = f.load(Ordering::Relaxed);
                    local_s = s.load(Ordering::Relaxed);
                }
                let n = local_s + local_f;
                // Avoid division by 0
                let avg = if n == 0 {
                    0.5f64
                } else {
                    local_s as f64 / n as f64
                };
                adaptive_bound(avg, confidence, precision) > n as f64
            })
            .count();
    }
}

// An efficient statistical model checker for nondeterminism and rare events,
// Carlos E. Budde, Pedro R. D’Argenio, Arnd Hartmanns, Sean Sedwards.
// International Journal on Software Tools for Technology Transfer (2020) 22:759–780
// https://doi.org/10.1007/s10009-020-00563-2

/// Computes Okamoto bound for given confidence and precision.
pub fn okamoto_bound(confidence: f64, precision: f64) -> f64 {
    (2f64 / (1f64 - confidence)).ln() / (2f64 * precision.powf(2f64))
}

/// Computes adaptive bound for given confidence, precision and (partial) experimental results.
pub fn adaptive_bound(avg: f64, confidence: f64, precision: f64) -> f64 {
    4f64 * okamoto_bound(confidence, precision)
        * (0.25f64 - ((avg - 0.5f64).abs() - (2f64 * precision / 3f64)).powf(2f64))
}

/// Computes precision for given experimental results and confidence
/// deriving it from adaptive bound through quadratic equation.
pub fn derive_precision(s: u32, f: u32, confidence: f64) -> f64 {
    let n = s + f;
    let avg = s as f64 / n as f64;
    let k = 2f64 * (2f64 / (1f64 - confidence)).ln();
    // Compute quadratic equation coefficients.
    let a = (n as f64) + (4f64 * k / 9f64);
    let b = -4f64 * k * (avg - 0.5f64).abs() / 3f64;
    let c = k * ((avg - 0.5f64).powf(2f64) - 0.25f64);
    // Take (larger positive) quadratic equation solution.
    (-b + (b.powf(2f64) - 4f64 * a * c).sqrt()) / (2f64 * a)
}
