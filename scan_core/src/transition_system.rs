use crate::Mtl;
use rayon::prelude::*;

/// Trait implementing a Transition System (TS), as defined in [^1].
/// As such, it is possible to verify it against MTL specifications.
///
/// [^1]: Baier, C., & Katoen, J. (2008). *Principles of model checking*. MIT Press.
pub trait TransitionSystem: Clone + Send {
    /// The type of the actions that trigger transitions between states in the TS.
    type Action: Send;

    /// The label function of the TS valuates the propositions of its set of propositions for the current state.
    // TODO FIXME: bitset instead of Vec<bool>?
    fn labels(&self) -> Vec<bool>;

    /// The transition relation relates [`Self::Action`]s and post-states that constitutes possible transitions from the current state.
    fn transitions(self) -> Vec<(Self::Action, Self)>;

    /// Verifies the TS against the provided [`Mtl`] specifications,
    /// and, when it finds a counterexample, it returns iys execution trace.
    ///
    /// It uses a depth-first, exhaustive search algorithm.
    /// Search is parallelized.
    fn check(self, properties: &[Mtl<usize>]) -> Option<Vec<(Self::Action, Vec<bool>)>> {
        self.transitions()
            .into_par_iter()
            .find_map_first(|(action, ts)| {
                let mut queue = Vec::from([(0, action, ts)]);
                let mut trace = Vec::new();
                while let Some((trace_len, action, ts)) = queue.pop() {
                    assert!(trace_len <= trace.len());
                    trace.truncate(trace_len);
                    let labels = ts.labels();
                    let all_labels_true = labels.iter().all(|b| *b);
                    trace.push((action, labels));
                    // TODO here properties should be checked
                    // For now we just make a simple truth check
                    if all_labels_true {
                        // pop from back and push in front (FIFO queue): width-first-search
                        // WARN: requires memorizing all traces and uses too much memory.

                        // pop from back and push back (stack): depth-first-search
                        // Generate all possible transitions and resulting state.
                        queue.extend(
                            ts.transitions()
                                .into_iter()
                                .map(|(a, ts)| (trace_len + 1, a, ts)),
                        );
                    } else {
                        // If condition is violated, we have found a counter-example!
                        return Some(trace);
                    }
                }
                None
            })
    }
}
