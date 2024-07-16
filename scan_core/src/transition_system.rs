use crate::Mtl;
use rayon::prelude::*;

pub trait TransitionSystem: Clone + Send + Sync {
    type Action: Clone + Send + Sync;

    fn labels(&self) -> Vec<Option<bool>>;

    fn transitions(self) -> Vec<(Self::Action, Self)>;

    fn check(self, properties: &[Mtl<usize>]) -> Option<Vec<(Self::Action, Vec<Option<bool>>)>> {
        self.transitions()
            .into_par_iter()
            .find_map_first(|(action, ts)| {
                let mut queue = Vec::from([(0, action, ts)]);
                let mut trace = Vec::new();
                while let Some((trace_len, action, ts)) = queue.pop() {
                    assert!(trace_len <= trace.len());
                    trace.truncate(trace_len);
                    let labels = ts.labels();
                    let all_labels_true = labels.iter().all(|b| b.unwrap_or(true));
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
