use crate::Time;
use log::info;
use rand::prelude::*;
use rayon::prelude::*;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Atom<A: Clone + PartialEq + Eq> {
    Predicate(usize),
    Event(A),
}

// WARN: Representing a trace as a Vec of Vecs may be expensive.
pub type Trace<A> = Vec<(Time, A, Vec<bool>)>;

pub trait Oracle<A> {
    fn update(&mut self, action: &A, state: &[bool], time: Time) -> bool;
}

pub trait Publisher<A> {
    fn init(&mut self);

    fn publish(&mut self, action: &A, time: Time, state: &[bool]);

    fn finalize(&mut self, success: Option<bool>);
}

/// Trait implementing a Transition System (TS), as defined in [^1].
/// As such, it is possible to verify it against MTL specifications.
///
/// [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.
pub trait TransitionSystem: Clone + Send + Sync {
    /// The type of the actions that trigger transitions between states in the TS.
    type Action: Clone + Eq + Send + Sync;

    /// The label function of the TS valuates the propositions of its set of propositions for the current state.
    // TODO FIXME: bitset instead of Vec<bool>?
    fn labels(&self) -> Vec<bool>;

    /// The transition relation relates [`Self::Action`]s and post-states that constitutes possible transitions from the current state.
    fn transitions(self) -> Vec<(Self::Action, Self)>;

    fn time(&self) -> Time {
        0
    }

    fn experiment<O, P, R: Rng>(
        mut self,
        mut guarantees: Vec<O>,
        mut assumes: Vec<O>,
        mut publisher: Option<P>,
        length: usize,
        rng: &mut R,
    ) -> Option<bool>
    where
        O: Oracle<Self::Action> + Clone,
        P: Publisher<Self::Action>,
    {
        let mut current_len = 0;
        if let Some(publisher) = publisher.as_mut() {
            publisher.init();
        }
        while let Some((action, new_ts)) = self.transitions().choose(rng) {
            current_len += 1;
            let state = new_ts.labels();
            let time = new_ts.time();
            self = new_ts.to_owned();
            if let Some(publisher) = publisher.as_mut() {
                publisher.publish(action, time, &state);
            }
            if assumes
                .iter_mut()
                .map(|o| o.update(action, &state, time))
                .all(|b| b)
            {
                if guarantees
                    .iter_mut()
                    .map(|o| o.update(action, &state, time))
                    .all(|b| b)
                {
                    if current_len >= length {
                        if let Some(publisher) = publisher.as_mut() {
                            publisher.finalize(None);
                        }
                        return None;
                    }
                } else {
                    if let Some(publisher) = publisher.as_mut() {
                        publisher.finalize(Some(false));
                    }
                    return Some(false);
                }
            } else {
                if let Some(publisher) = publisher.as_mut() {
                    publisher.finalize(None);
                }
                return None;
            }
        }
        if let Some(publisher) = publisher.as_mut() {
            publisher.finalize(Some(true));
        }
        Some(true)
    }

    fn par_adaptive<O, P>(
        &self,
        guarantees: &[O],
        assumes: &[O],
        confidence: f64,
        precision: f64,
        length: usize,
        s: Arc<AtomicU32>,
        f: Arc<AtomicU32>,
        publisher: Option<P>,
    ) where
        O: Oracle<Self::Action> + Clone + Send + Sync,
        P: Publisher<Self::Action> + Clone + Send + Sync,
    {
        // WARN FIXME TODO: Implement algorithm for 2.4 Distributed sample generation in Budde et al.
        (0..usize::MAX)
            .into_par_iter()
            .take_any_while(|_| {
                let mut rng = rand::thread_rng();
                let local_s;
                let local_f;
                if let Some(result) = self.clone().experiment(
                    Vec::from(guarantees),
                    Vec::from(assumes),
                    publisher.clone(),
                    length,
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
