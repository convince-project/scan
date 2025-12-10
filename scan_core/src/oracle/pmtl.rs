mod numset;

use crate::{Oracle, Time};
use numset::NumSet;
use std::hash::Hash;

/// A Past-time Metric Temporal Logic (PMTL) formula.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pmtl<V>
where
    V: Clone,
{
    /// The true formula.
    True,
    /// The false formula.
    False,
    /// An atomic formula.
    Atom(V),
    /// Logical disjunction of a list of formulae.
    And(Vec<Pmtl<V>>),
    /// Logical conjunction of a list of formulae.
    Or(Vec<Pmtl<V>>),
    /// Logical negation of a formula.
    Not(Box<Pmtl<V>>),
    /// Logical implication of a antecedent formula and a consequent formula.
    Implies(Box<(Pmtl<V>, Pmtl<V>)>),
    /// Temporal historical predicate over a formula (with bounds).
    Historically(Box<Pmtl<V>>, Time, Time),
    /// Temporal previously predicate over a formula (with bounds).
    Once(Box<Pmtl<V>>, Time, Time),
    /// Temporal since predicate over a formula (with bounds).
    Since(Box<(Pmtl<V>, Pmtl<V>)>, Time, Time),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ValPmtl {
    True,
    False,
    Atom(usize, bool),
    And(Vec<ValPmtl>),
    Or(Vec<ValPmtl>),
    Not(Box<ValPmtl>),
    Implies(Box<(ValPmtl, ValPmtl)>),
    Historically(Box<ValPmtl>, Time, Time, NumSet),
    Once(Box<ValPmtl>, Time, Time, NumSet),
    Since(Box<(ValPmtl, ValPmtl)>, Time, Time, NumSet),
}

impl From<&Pmtl<usize>> for ValPmtl {
    fn from(value: &Pmtl<usize>) -> Self {
        match value {
            Pmtl::True => ValPmtl::True,
            Pmtl::False => ValPmtl::False,
            Pmtl::Atom(a) => ValPmtl::Atom(*a, false),
            Pmtl::And(pmtls) => ValPmtl::And(pmtls.iter().map(|f| f.into()).collect()),
            Pmtl::Or(pmtls) => ValPmtl::Or(pmtls.iter().map(|f| f.into()).collect()),
            Pmtl::Not(pmtl) => ValPmtl::Not(Box::new(pmtl.as_ref().into())),
            Pmtl::Implies(pmtls) => {
                ValPmtl::Implies(Box::new(((&pmtls.0).into(), (&pmtls.1).into())))
            }
            Pmtl::Historically(pmtl, lb, ub) => {
                ValPmtl::Historically(Box::new(pmtl.as_ref().into()), *lb, *ub, NumSet::empty())
            }
            Pmtl::Once(pmtl, lb, ub) => {
                ValPmtl::Once(Box::new(pmtl.as_ref().into()), *lb, *ub, NumSet::empty())
            }
            Pmtl::Since(pmtls, lb, ub) => ValPmtl::Since(
                Box::new(((&pmtls.0).into(), (&pmtls.1).into())),
                *lb,
                *ub,
                NumSet::empty(),
            ),
        }
    }
}

impl ValPmtl {
    // From Ulus 2024: Online monitoring of metric temporal logic using sequential networks. Link: <http://arxiv.org/abs/1901.00175v2>
    #[inline(always)]
    pub fn output(&self, time: Time) -> bool {
        match self {
            ValPmtl::True => true,
            ValPmtl::False => false,
            ValPmtl::Atom(_atom, b) => *b,
            ValPmtl::And(subs) => subs.iter().all(|f| f.output(time)),
            ValPmtl::Or(subs) => subs.iter().any(|f| f.output(time)),
            ValPmtl::Not(subformula) => !subformula.output(time),
            ValPmtl::Implies(subformulae) => {
                let (lhs, rhs) = subformulae.as_ref();
                rhs.output(time) || !lhs.output(time)
            }
            ValPmtl::Historically(_sub, _lower_bound, _upper_bound, valuation) => {
                !valuation.contains(time)
            }
            ValPmtl::Once(_sub, _lower_bound, _upper_bound, valuation) => valuation.contains(time),
            ValPmtl::Since(_subformulae, _lower_bound, _upper_bound, valuation) => {
                valuation.contains(time)
            }
        }
    }

    pub fn update_state(&mut self, state: &[bool], time: Time) {
        match self {
            ValPmtl::Atom(atom, b) => {
                *b = state[*atom];
            }
            ValPmtl::And(subs) | ValPmtl::Or(subs) => {
                // Make sure all subformulae are updated
                subs.iter_mut().for_each(|f| f.update_state(state, time));
            }
            ValPmtl::Not(subformula) => {
                subformula.update_state(state, time);
            }
            ValPmtl::Implies(subformulae) => {
                let (lhs, rhs) = subformulae.as_mut();
                // Make sure all subformulae are updated
                lhs.update_state(state, time);
                rhs.update_state(state, time);
            }
            ValPmtl::Historically(sub, lower_bound, upper_bound, valuation) => {
                sub.update_state(state, time);
                if !sub.output(time) {
                    valuation.add_interval(
                        lower_bound.saturating_add(time),
                        upper_bound.saturating_add(time),
                    );
                }
            }
            ValPmtl::Once(sub, lower_bound, upper_bound, valuation) => {
                sub.update_state(state, time);
                if sub.output(time) {
                    valuation.add_interval(
                        lower_bound.saturating_add(time),
                        upper_bound.saturating_add(time),
                    );
                }
            }
            ValPmtl::Since(subformulae, lower_bound, upper_bound, valuation) => {
                let (lhs, rhs) = subformulae.as_mut();
                // Make sure all subformulae are updated
                lhs.update_state(state, time);
                let out_lhs = lhs.output(time);
                rhs.update_state(state, time);
                let out_rhs = rhs.output(time);
                if out_lhs {
                    if out_rhs {
                        valuation.add_interval(
                            lower_bound.saturating_add(time),
                            upper_bound.saturating_add(time),
                        );
                    }
                } else if out_rhs {
                    *valuation = NumSet::from_range(
                        lower_bound.saturating_add(time),
                        upper_bound.saturating_add(time),
                    );
                } else {
                    *valuation = NumSet::empty();
                }
            }
            _ => {
                // nothing to do
            }
        }
    }

    pub fn update_time(&mut self, time: Time) {
        match self {
            ValPmtl::And(subs) | ValPmtl::Or(subs) => {
                // Make sure all subformulae are updated
                subs.iter_mut().for_each(|f| f.update_time(time));
            }
            ValPmtl::Not(subformula) => {
                subformula.update_time(time);
            }
            ValPmtl::Implies(subformulae) => {
                let (lhs, rhs) = subformulae.as_mut();
                // Make sure all subformulae are updated
                lhs.update_time(time);
                rhs.update_time(time);
            }
            ValPmtl::Historically(sub, lower_bound, upper_bound, valuation) => {
                sub.update_time(time);
                if !sub.output(time) {
                    valuation.add_interval(
                        lower_bound.saturating_add(time),
                        upper_bound.saturating_add(time),
                    );
                }
            }
            ValPmtl::Once(sub, lower_bound, upper_bound, valuation) => {
                sub.update_time(time);
                if sub.output(time) {
                    valuation.add_interval(
                        lower_bound.saturating_add(time),
                        upper_bound.saturating_add(time),
                    );
                }
            }
            ValPmtl::Since(subformulae, lower_bound, upper_bound, valuation) => {
                let (lhs, rhs) = subformulae.as_mut();
                // Make sure all subformulae are updated
                lhs.update_time(time);
                let out_lhs = lhs.output(time);
                rhs.update_time(time);
                let out_rhs = rhs.output(time);
                if out_lhs {
                    if out_rhs {
                        valuation.add_interval(
                            lower_bound.saturating_add(time),
                            upper_bound.saturating_add(time),
                        );
                    }
                } else if out_rhs {
                    *valuation = NumSet::from_range(
                        lower_bound.saturating_add(time),
                        upper_bound.saturating_add(time),
                    );
                } else {
                    *valuation = NumSet::empty();
                }
            }
            _ => {
                // nothing to do
            }
        }
    }

    // Represents the knowledge that the given formula is always true, or always false, from the current moment.
    pub fn valuation(&self, time: Time) -> Option<bool> {
        match self {
            ValPmtl::True => Some(true),
            ValPmtl::False => Some(false),
            ValPmtl::Atom(_, _) => None,
            ValPmtl::And(items) => items
                .iter()
                .try_fold(true, |b, f| f.valuation(time).map(|v| b && v)),
            ValPmtl::Or(items) => items
                .iter()
                .try_fold(false, |b, f| f.valuation(time).map(|v| b || v)),
            ValPmtl::Not(f) => f.valuation(time).map(|b| !b),
            ValPmtl::Implies(subformulae) => {
                let lhs_val = subformulae.0.valuation(time);
                let rhs_val = subformulae.1.valuation(time);
                if rhs_val.is_some_and(|b| b) || lhs_val.is_some_and(|b| !b) {
                    Some(true)
                } else if rhs_val.is_some_and(|b| !b) && lhs_val.is_some_and(|b| b) {
                    Some(false)
                } else {
                    None
                }
            }
            // Valuation of Historically represents the set of time moments in which we know the formula to be false.
            // If the argument of the operator is know to be always true, then Historically is always true.
            // If the argument of the operator is know to be always false, then Historically is always false provided that the lower time bound is 0.
            ValPmtl::Historically(subformula, lb, _, valuation) => subformula
                .valuation(time)
                .and_then(|v| (*lb == 0 || v).then_some(v))
                .or_else(|| valuation.contains_unbounded_interval(time).then_some(false)),
            // Valuation of Previously represents the set of time moments in which we know the formula to be false.
            // If the argument of the operator is know to be always true, then Previously is always true provided that the lower time bound is 0.
            // If the argument of the operator is know to be always false, then Previously is always false.
            ValPmtl::Once(subformula, lb, _, valuation) => subformula
                .valuation(time)
                .and_then(|v| (*lb == 0 || !v).then_some(v))
                .or_else(|| valuation.contains_unbounded_interval(time).then_some(true)),
            // Valuation of Since represents the set of time moments in which we know the formula to be true if its lhs argument is true from then on.
            // If the rhs argument of the operator is know to be always false, then Since is always false.
            // If the Since valuation is always true and the lhs argument of the operator is know to be always true, then Since is always true.
            // If the Since valuation is always true and the lhs argument of the operator is know to be always false, then Since is always false provided that the lower time bound is 0.
            ValPmtl::Since(subformulae, lb, _, valuation) => {
                if let Some(false) = subformulae.1.valuation(time) {
                    Some(false)
                } else if valuation.contains_unbounded_interval(time) {
                    subformulae
                        .0
                        .valuation(time)
                        .and_then(|v| (*lb == 0 || v).then_some(v))
                } else {
                    None
                }
            }
        }
    }
}

/// An oracle for PMTL properties over timed, dense traces.
#[derive(Debug, Clone)]
pub struct PmtlOracle {
    assumes: Vec<ValPmtl>,
    guarantees: Vec<ValPmtl>,
    time: Time,
}

impl PmtlOracle {
    /// Creates an oracle from assumes and guarantees PMTL formulae.
    pub fn new(assumes: &[Pmtl<usize>], guarantees: &[Pmtl<usize>]) -> Self {
        Self {
            assumes: assumes.iter().map(|f| f.into()).collect(),
            guarantees: guarantees.iter().map(|f| f.into()).collect(),
            time: 0,
        }
    }
}

impl Oracle for PmtlOracle {
    fn output_assumes(&self) -> impl Iterator<Item = Option<bool>> {
        self.assumes.iter().map(|f| f.valuation(self.time))
    }

    fn output_guarantees(&self) -> impl Iterator<Item = Option<bool>> {
        self.guarantees.iter().map(|f| f.valuation(self.time))
    }

    fn final_output_assumes(&self) -> impl Iterator<Item = bool> {
        self.assumes.iter().map(|f| f.output(self.time))
    }

    fn final_output_guarantees(&self) -> impl Iterator<Item = bool> {
        self.guarantees.iter().map(|f| f.output(self.time))
    }

    // From Ulus 2024: Online monitoring of metric temporal logic using sequential networks. Link: <http://arxiv.org/abs/1901.00175v2>
    fn update_state(&mut self, state: &[bool]) {
        self.assumes
            .iter_mut()
            .for_each(|f| f.update_state(state, self.time));
        self.guarantees
            .iter_mut()
            .for_each(|f| f.update_state(state, self.time));
    }

    // From Ulus 2024: Online monitoring of metric temporal logic using sequential networks. Link: <http://arxiv.org/abs/1901.00175v2>
    fn update_time(&mut self, time: Time) {
        assert!(self.time <= time);
        self.assumes.iter_mut().for_each(|f| f.update_time(time));
        self.guarantees.iter_mut().for_each(|f| f.update_time(time));
        self.time = time;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn since_1() {
        let formula = Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(1))), 0, Time::MAX);
        let mut oracle = PmtlOracle::new(&[], &[formula]);
        oracle.update_state(&[false, true]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[false, true]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(1);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[true, true]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(2);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[true, true]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(3);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[true, false]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(4);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[false, false]);
        assert!(oracle.final_output_guarantees().any(|b| !b));
    }

    #[test]
    fn since_2() {
        let formula = Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(1))), 0, 2);
        let mut oracle = PmtlOracle::new(&[], &[formula]);
        oracle.update_state(&[false, true]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[false, true]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(1);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[true, true]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(2);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[true, false]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(3);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[true, false]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(4);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[true, false]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(5);
        assert!(oracle.final_output_guarantees().any(|b| !b));
    }

    #[test]
    fn since_3() {
        let formula = Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(1))), 1, 2);
        let mut oracle = PmtlOracle::new(&[], &[formula]);
        oracle.update_state(&[false, true]);
        assert!(oracle.final_output_guarantees().any(|b| !b));
        oracle.update_state(&[false, true]);
        assert!(oracle.final_output_guarantees().any(|b| !b));
        oracle.update_time(1);
        assert!(oracle.final_output_guarantees().any(|b| !b));
        oracle.update_state(&[true, true]);
        assert!(oracle.final_output_guarantees().any(|b| !b));
        oracle.update_time(2);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[true, false]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(3);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[true, false]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(4);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_state(&[true, false]);
        assert!(oracle.final_output_guarantees().any(|b| b));
        oracle.update_time(5);
        assert!(oracle.final_output_guarantees().any(|b| !b));
    }

    //     #[test]
    //     fn since_4() {
    //         let formula = Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(1))), 1, 2);
    //         let oracle = PmtlOracle::new(&[], &[formula]);
    //         let mut state = oracle.generate();
    //         state.update(&[false, true], 0);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //         state.update(&[false, true], 1);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //         state.update(&[false, true], 2);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //         state.update(&[true, true], 2);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //         state.update(&[true, false], 3);
    //         assert!(state.final_output_guarantees().any(|b| b));
    //         state.update(&[true, false], 4);
    //         assert!(state.final_output_guarantees().any(|b| b));
    //         state.update(&[true, false], 5);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //     }

    //     #[test]
    //     fn historically() {
    //         let formula = Pmtl::Historically(Box::new(Pmtl::Atom(0)), 1, 2);
    //         let oracle = PmtlOracle::new(&[], &[formula]);
    //         let mut state = oracle.generate();
    //         state.update(&[false], 0);
    //         assert!(state.final_output_guarantees().any(|b| b));
    //         state.update(&[false], 0);
    //         assert!(state.final_output_guarantees().any(|b| b));
    //         state.update(&[true], 1);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //         state.update(&[true], 2);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //         state.update(&[true], 3);
    //         assert!(state.final_output_guarantees().any(|b| b));
    //         state.update(&[false], 3);
    //         assert!(state.final_output_guarantees().any(|b| b));
    //         state.update(&[true], 4);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //     }

    //     #[test]
    //     fn previously() {
    //         let formula = Pmtl::Once(Box::new(Pmtl::Atom(0)), 1, 2);
    //         let oracle = PmtlOracle::new(&[], &[formula]);
    //         let mut state = oracle.generate();
    //         state.update(&[false], 0);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //         state.update(&[false], 0);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //         state.update(&[true], 1);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //         state.update(&[false], 2);
    //         assert!(state.final_output_guarantees().any(|b| b));
    //         state.update(&[false], 3);
    //         assert!(state.final_output_guarantees().any(|b| b));
    //         state.update(&[false], 3);
    //         assert!(state.final_output_guarantees().any(|b| b));
    //         state.update(&[true], 4);
    //         assert!(state.final_output_guarantees().any(|b| !b));
    //     }
}
