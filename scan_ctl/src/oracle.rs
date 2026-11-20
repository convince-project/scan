mod numset;

use numset::NumSet;
use scan_core::{Oracle, OracleTree, Time, TimeRange};
use std::{hash::Hash, ops::RangeBounds};

/// A Past-time Metric Temporal Logic (PMTL) formula.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ctl<V>
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
    And(Box<[Ctl<V>]>),
    /// Logical conjunction of a list of formulae.
    Or(Box<[Ctl<V>]>),
    /// Logical negation of a formula.
    Not(Box<Ctl<V>>),
    /// Logical implication of a antecedent formula and a consequent formula.
    Implies(Box<(Ctl<V>, Ctl<V>)>),
    /// Exists path
    Exists(PathCtl<V>),
    /// All paths
    All(PathCtl<V>),
}

/// A Past-time Metric Temporal Logic (PMTL) formula.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathCtl<V>
where
    V: Clone,
{
    /// Temporal historical predicate over a formula (with bounds).
    Historically(Box<Ctl<V>>, TimeRange),
    /// Temporal previously predicate over a formula (with bounds).
    Once(Box<Ctl<V>>, TimeRange),
    /// Temporal since predicate over a formula (with bounds).
    Since(Box<(Ctl<V>, Ctl<V>)>, TimeRange),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ValPathCtl {
    // NumSet represents times qt which formula is surely false
    Historically(Box<ValCtl>, TimeRange, NumSet),
    // NumSet represents times qt which formula is surely true
    Once(Box<ValCtl>, TimeRange, NumSet),
    // NumSet represents times qt which formula might be satisfied
    Since(Box<(ValCtl, ValCtl)>, TimeRange, NumSet),
}

impl From<&PathCtl<usize>> for ValPathCtl {
    fn from(value: &PathCtl<usize>) -> Self {
        match value {
            PathCtl::Historically(pmtl, range) => {
                ValPathCtl::Historically(Box::new(pmtl.as_ref().into()), *range, NumSet::empty())
            }
            PathCtl::Once(pmtl, range) => {
                ValPathCtl::Once(Box::new(pmtl.as_ref().into()), *range, NumSet::empty())
            }
            PathCtl::Since(pmtls, range) => ValPathCtl::Since(
                Box::new(((&pmtls.0).into(), (&pmtls.1).into())),
                *range,
                NumSet::empty(),
            ),
        }
    }
}

impl ValPathCtl {
    // From Ulus 2024: Online monitoring of metric temporal logic using sequential networks. Link: <http://arxiv.org/abs/1901.00175v2>
    pub fn output(&self, time: Time) -> bool {
        match self {
            ValPathCtl::Historically(_sub, _range, valuation) => !valuation.contains(time),
            ValPathCtl::Once(_sub, _range, valuation) => valuation.contains(time),
            ValPathCtl::Since(_subformulae, _range, valuation) => valuation.contains(time),
        }
    }

    pub fn update_state(&mut self, state: &[bool], time: Time) {
        match self {
            ValPathCtl::Historically(sub, range, valuation) => {
                sub.update_state(state, time);
                if !sub.output() {
                    *valuation += range.shift(time);
                }
            }
            ValPathCtl::Once(sub, range, valuation) => {
                sub.update_state(state, time);
                if sub.output() {
                    *valuation += range.shift(time);
                }
            }
            ValPathCtl::Since(subformulae, range, valuation) => {
                let (lhs, rhs) = subformulae.as_mut();
                // Make sure all subformulae are updated
                lhs.update_state(state, time);
                let out_lhs = lhs.output();
                rhs.update_state(state, time);
                let out_rhs = rhs.output();
                if out_lhs {
                    if out_rhs {
                        *valuation += range.shift(time);
                    }
                } else if out_rhs {
                    *valuation = NumSet::from(range.shift(time));
                } else {
                    *valuation = NumSet::empty();
                }
            }
        }
    }

    pub fn update_time(&mut self, time: Time) {
        match self {
            ValPathCtl::Historically(sub, range, valuation) => {
                sub.update_time(time);
                if !sub.output() {
                    *valuation += range.shift(time);
                }
            }
            ValPathCtl::Once(sub, range, valuation) => {
                sub.update_time(time);
                if sub.output() {
                    *valuation += range.shift(time);
                }
            }
            ValPathCtl::Since(subformulae, range, valuation) => {
                let (lhs, rhs) = subformulae.as_mut();
                // Make sure all subformulae are updated
                lhs.update_time(time);
                let out_lhs = lhs.output();
                rhs.update_time(time);
                let out_rhs = rhs.output();
                if out_lhs {
                    if out_rhs {
                        *valuation += range.shift(time);
                    }
                } else if out_rhs {
                    *valuation = NumSet::from(range.shift(time));
                } else {
                    *valuation = NumSet::empty();
                }
            }
        }
    }

    fn close_branch(&mut self, closed_val_path_ctl: ValPathCtl, time: Time) {
        match self {
            ValPathCtl::Historically(val_ctl, _time_range, _num_set)
                if let ValPathCtl::Historically(closed_val_ctl, _, _) = closed_val_path_ctl =>
            {
                val_ctl.close_branch(*closed_val_ctl, time);
            }
            ValPathCtl::Once(val_ctl, _time_range, _num_set)
                if let ValPathCtl::Once(closed_val_ctl, _, _) = closed_val_path_ctl =>
            {
                val_ctl.close_branch(*closed_val_ctl, time);
            }
            ValPathCtl::Since(val_ctls, _time_range, _num_set)
                if let ValPathCtl::Since(closed_val_ctls, _, _) = closed_val_path_ctl =>
            {
                val_ctls.0.close_branch(closed_val_ctls.0, time);
                val_ctls.1.close_branch(closed_val_ctls.1, time);
            }
            _ => {
                panic!("formulae not matching");
            }
        }
    }

    // Represents the knowledge that the given formula is always true, or always false, from the current moment.
    pub fn valuation(&self, time: Time) -> Option<bool> {
        match self {
            // Valuation of Historically represents the set of time moments in which we know the formula to be false.
            // If the argument of the operator is know to be always true, then Historically is always true.
            // If the argument of the operator is know to be always false, then Historically is always false provided that the lower time bound is 0.
            ValPathCtl::Historically(subformula, range, valuation) => subformula
                .valuation(time)
                .and_then(|v| (range.contains(&0) || v).then_some(v))
                .or_else(|| valuation.contains_unbounded_interval(time).then_some(false)),
            // Valuation of Previously represents the set of time moments in which we know the formula to be false.
            // If the argument of the operator is know to be always true, then Previously is always true provided that the lower time bound is 0.
            // If the argument of the operator is know to be always false, then Previously is always false.
            ValPathCtl::Once(subformula, range, valuation) => subformula
                .valuation(time)
                .and_then(|v| (range.contains(&0) || !v).then_some(v))
                .or_else(|| valuation.contains_unbounded_interval(time).then_some(true)),
            // Valuation of Since represents the set of time moments in which we know the formula to be true if its lhs argument is true from then on.
            // If the rhs argument of the operator is know to be always false, then Since is always false.
            // If the Since valuation is always true and the lhs argument of the operator is know to be always true, then Since is always true.
            // If the Since valuation is always true and the lhs argument of the operator is know to be always false, then Since is always false provided that the lower time bound is 0.
            ValPathCtl::Since(subformulae, range, valuation) => {
                if let Some(false) = subformulae.1.valuation(time) {
                    Some(false)
                } else if valuation.contains_unbounded_interval(time) {
                    subformulae
                        .0
                        .valuation(time)
                        .and_then(|v| (range.contains(&0) || v).then_some(v))
                } else {
                    None
                }
            }
        }
    }

    fn branch_end(&mut self, time: u32) {
        match self {
            ValPathCtl::Historically(val_ctl, _, _) => val_ctl.branch_end(time),
            ValPathCtl::Once(val_ctl, _, _) => val_ctl.branch_end(time),
            ValPathCtl::Since(val_ctls, _, _) => {
                val_ctls.0.branch_end(time);
                val_ctls.1.branch_end(time);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ValCtl {
    True,
    False,
    Atom(usize, bool),
    And(Box<[ValCtl]>),
    Or(Box<[ValCtl]>),
    Not(Box<ValCtl>),
    Implies(Box<(ValCtl, ValCtl)>),
    Exists(ValPathCtl, bool),
    All(ValPathCtl, bool),
}

impl From<&Ctl<usize>> for ValCtl {
    fn from(value: &Ctl<usize>) -> Self {
        match value {
            Ctl::True => ValCtl::True,
            Ctl::False => ValCtl::False,
            Ctl::Atom(a) => ValCtl::Atom(*a, false),
            Ctl::And(pmtls) => ValCtl::And(pmtls.iter().map(|f| f.into()).collect()),
            Ctl::Or(pmtls) => ValCtl::Or(pmtls.iter().map(|f| f.into()).collect()),
            Ctl::Not(pmtl) => ValCtl::Not(Box::new(pmtl.as_ref().into())),
            Ctl::Implies(pmtls) => {
                ValCtl::Implies(Box::new(((&pmtls.0).into(), (&pmtls.1).into())))
            }
            Ctl::Exists(path_ctl) => ValCtl::Exists(path_ctl.into(), false),
            Ctl::All(path_ctl) => ValCtl::All(path_ctl.into(), true),
        }
    }
}

impl ValCtl {
    // From Ulus 2024: Online monitoring of metric temporal logic using sequential networks. Link: <http://arxiv.org/abs/1901.00175v2>
    pub fn output(&self) -> bool {
        match self {
            ValCtl::True => true,
            ValCtl::False => false,
            ValCtl::Atom(_atom, b) => *b,
            ValCtl::And(subs) => subs.iter().all(|f| f.output()),
            ValCtl::Or(subs) => subs.iter().any(|f| f.output()),
            ValCtl::Not(subformula) => !subformula.output(),
            ValCtl::Implies(subformulae) => {
                let (lhs, rhs) = subformulae.as_ref();
                rhs.output() || !lhs.output()
            }
            ValCtl::Exists(_val_path_ctl, val) => *val,
            ValCtl::All(_val_path_ctl, val) => *val,
        }
    }

    pub fn update_state(&mut self, state: &[bool], time: Time) {
        match self {
            ValCtl::Atom(atom, b) => {
                *b = state[*atom];
            }
            ValCtl::And(subs) | ValCtl::Or(subs) => {
                // Make sure all subformulae are updated
                subs.iter_mut().for_each(|f| f.update_state(state, time));
            }
            ValCtl::Not(subformula) => {
                subformula.update_state(state, time);
            }
            ValCtl::Implies(subformulae) => {
                let (lhs, rhs) = subformulae.as_mut();
                // Make sure all subformulae are updated
                lhs.update_state(state, time);
                rhs.update_state(state, time);
            }
            ValCtl::Exists(val_path_ctl, val) => {
                val_path_ctl.update_state(state, time);
                if let Some(n_val) = val_path_ctl.valuation(time) {
                    *val |= n_val
                }
            }
            ValCtl::All(val_path_ctl, val) => {
                val_path_ctl.update_state(state, time);
                if let Some(n_val) = val_path_ctl.valuation(time) {
                    *val &= n_val
                }
            }
            _ => {
                // nothing to do
            }
        }
    }

    pub fn update_time(&mut self, time: Time) {
        match self {
            ValCtl::And(subs) | ValCtl::Or(subs) => {
                // Make sure all subformulae are updated
                subs.iter_mut().for_each(|f| f.update_time(time));
            }
            ValCtl::Not(subformula) => {
                subformula.update_time(time);
            }
            ValCtl::Implies(subformulae) => {
                let (lhs, rhs) = subformulae.as_mut();
                // Make sure all subformulae are updated
                lhs.update_time(time);
                rhs.update_time(time);
            }
            ValCtl::Exists(val_path_ctl, val) => {
                val_path_ctl.update_time(time);
                if let Some(n_val) = val_path_ctl.valuation(time) {
                    *val |= n_val
                }
            }
            ValCtl::All(val_path_ctl, val) => {
                val_path_ctl.update_time(time);
                if let Some(n_val) = val_path_ctl.valuation(time) {
                    *val &= n_val
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
            ValCtl::True => Some(true),
            ValCtl::False => Some(false),
            ValCtl::Atom(_, _) => None,
            ValCtl::And(items) => items
                .iter()
                .try_fold(true, |b, f| f.valuation(time).map(|v| b && v)),
            ValCtl::Or(items) => items
                .iter()
                .try_fold(false, |b, f| f.valuation(time).map(|v| b || v)),
            ValCtl::Not(f) => f.valuation(time).map(|b| !b),
            ValCtl::Implies(subformulae) => {
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
            ValCtl::Exists(val_path_ctl, val)
                if let Some(path_val) = val_path_ctl.valuation(time) =>
            {
                Some(*val | path_val)
            }
            ValCtl::Exists(_val_path_ctl, val) => val.then_some(true),
            ValCtl::All(val_path_ctl, val) if let Some(path_val) = val_path_ctl.valuation(time) => {
                Some(*val & path_val)
            }
            ValCtl::All(_val_path_ctl, val) => (!val).then_some(false),
        }
    }

    fn branch_end(&mut self, time: Time) {
        match self {
            ValCtl::And(val_ctls) => {
                for current_formula in val_ctls.iter_mut() {
                    current_formula.branch_end(time)
                }
            }
            ValCtl::Or(val_ctls) => {
                for current_formula in val_ctls.iter_mut() {
                    current_formula.branch_end(time)
                }
            }
            ValCtl::Not(val_ctl) => val_ctl.branch_end(time),
            ValCtl::Implies(val_ctls) => {
                val_ctls.0.branch_end(time);
                val_ctls.1.branch_end(time);
            }
            ValCtl::Exists(val_path_ctl, val) => {
                *val |= val_path_ctl.output(time);
                val_path_ctl.branch_end(time);
            }
            ValCtl::All(val_path_ctl, val) => {
                *val &= val_path_ctl.output(time);
                val_path_ctl.branch_end(time);
            }
            ValCtl::True | ValCtl::False | ValCtl::Atom(_, _) => {
                // nothing to do
            }
        }
    }

    fn close_branch(&mut self, closed_formula: Self, time: Time) {
        match self {
            ValCtl::And(val_ctls) if let ValCtl::And(closed_val_ctls) = closed_formula => {
                for (current_formula, closed_formula) in val_ctls.iter_mut().zip(closed_val_ctls) {
                    current_formula.close_branch(closed_formula, time)
                }
            }
            ValCtl::Or(val_ctls) if let ValCtl::Or(closed_val_ctls) = closed_formula => {
                for (current_formula, closed_formula) in val_ctls.iter_mut().zip(closed_val_ctls) {
                    current_formula.close_branch(closed_formula, time)
                }
            }
            ValCtl::Not(val_ctl) if let ValCtl::Not(closed_val_ctl) = closed_formula => {
                val_ctl.close_branch(*closed_val_ctl, time)
            }
            ValCtl::Implies(val_ctls) if let ValCtl::Implies(closed_val_ctls) = closed_formula => {
                val_ctls.0.close_branch(closed_val_ctls.0, time);
                val_ctls.1.close_branch(closed_val_ctls.1, time);
            }
            ValCtl::Exists(val_path_ctl, val)
                if let ValCtl::Exists(closed_val_ctl, closed_val) = closed_formula =>
            {
                *val |= closed_val;
                val_path_ctl.close_branch(closed_val_ctl, time);
            }
            ValCtl::All(val_path_ctl, val)
                if let ValCtl::All(closed_val_ctl, closed_val) = closed_formula =>
            {
                *val &= closed_val;
                val_path_ctl.close_branch(closed_val_ctl, time);
            }
            ValCtl::True | ValCtl::False | ValCtl::Atom(_, _) => {
                // nothing to do
            }
            _ => {
                unreachable!("formulae should match");
            }
        }
    }
}

/// An oracle for PCTL properties over timed, dense traces.
#[derive(Debug, Clone)]
pub struct CtlOracle {
    assumes: Box<[ValCtl]>,
    guarantees: Box<[ValCtl]>,
    time: Time,
}

impl CtlOracle {
    /// Creates an oracle from assumes and guarantees CMTL formulae.
    pub fn new(assumes: &[Ctl<usize>], guarantees: &[Ctl<usize>]) -> Self {
        Self {
            assumes: assumes.iter().map(|f| f.into()).collect(),
            guarantees: guarantees.iter().map(|f| f.into()).collect(),
            time: 0,
        }
    }
}

impl OracleTree for CtlOracle {
    fn fold_branch(&mut self, closed: Self) {
        for (current_formula, closed_formula) in self.assumes.iter_mut().zip(closed.assumes) {
            current_formula.close_branch(closed_formula, self.time);
        }
        for (current_formula, closed_formula) in self.guarantees.iter_mut().zip(closed.guarantees) {
            current_formula.close_branch(closed_formula, self.time);
        }
    }

    fn end_branch(&mut self) {
        for assume in self.assumes.iter_mut() {
            assume.branch_end(self.time);
        }
        for guarantee in self.guarantees.iter_mut() {
            guarantee.branch_end(self.time);
        }
    }
}

impl Oracle for CtlOracle {
    #[inline]
    fn output_assumes(&self) -> impl Iterator<Item = Option<bool>> {
        self.assumes.iter().map(move |f| f.valuation(self.time))
    }

    #[inline]
    fn output_guarantees(&self) -> impl Iterator<Item = Option<bool>> {
        self.guarantees.iter().map(move |f| f.valuation(self.time))
    }

    #[inline]
    fn final_output_assumes(&self) -> impl Iterator<Item = bool> {
        self.assumes.iter().map(move |f| f.output())
    }

    #[inline]
    fn final_output_guarantees(&self) -> impl Iterator<Item = bool> {
        self.guarantees.iter().map(move |f| f.output())
    }

    // From Ulus 2024: Online monitoring of metric temporal logic using sequential networks. Link: <http://arxiv.org/abs/1901.00175v2>
    fn update_state(&mut self, state: &[bool]) {
        for f in self.assumes.iter_mut() {
            f.update_state(state, self.time)
        }
        for f in self.guarantees.iter_mut() {
            f.update_state(state, self.time)
        }
    }

    // From Ulus 2024: Online monitoring of metric temporal logic using sequential networks. Link: <http://arxiv.org/abs/1901.00175v2>
    fn update_time(&mut self, time: Time) {
        self.time = time;
        for f in self.assumes.iter_mut() {
            f.update_time(time)
        }
        for f in self.guarantees.iter_mut() {
            f.update_time(time)
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn since_1() {
//         let formula = Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(1))), TimeRange::new(..));
//         let mut oracle = PmtlOracle::new(&[], &[formula]);
//         oracle.update_state(&[false, true]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[false, true]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(1);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[true, true]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(2);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[true, true]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(3);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[true, false]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(4);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[false, false]);
//         assert!(oracle.final_output_guarantees().any(|b| !b));
//     }

//     #[test]
//     fn since_2() {
//         let formula = Pmtl::Since(
//             Box::new((Pmtl::Atom(0), Pmtl::Atom(1))),
//             TimeRange::new(0..=2),
//         );
//         let mut oracle = PmtlOracle::new(&[], &[formula]);
//         oracle.update_state(&[false, true]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[false, true]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(1);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[true, true]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(2);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[true, false]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(3);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[true, false]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(4);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[true, false]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(5);
//         assert!(oracle.final_output_guarantees().any(|b| !b));
//     }

//     #[test]
//     fn since_3() {
//         let formula = Pmtl::Since(
//             Box::new((Pmtl::Atom(0), Pmtl::Atom(1))),
//             TimeRange::new(1..=2),
//         );
//         let mut oracle = PmtlOracle::new(&[], &[formula]);
//         oracle.update_state(&[false, true]);
//         assert!(oracle.final_output_guarantees().any(|b| !b));
//         oracle.update_state(&[false, true]);
//         assert!(oracle.final_output_guarantees().any(|b| !b));
//         oracle.update_time(1);
//         assert!(oracle.final_output_guarantees().any(|b| !b));
//         oracle.update_state(&[true, true]);
//         assert!(oracle.final_output_guarantees().any(|b| !b));
//         oracle.update_time(2);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[true, false]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(3);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[true, false]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(4);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_state(&[true, false]);
//         assert!(oracle.final_output_guarantees().any(|b| b));
//         oracle.update_time(5);
//         assert!(oracle.final_output_guarantees().any(|b| !b));
//     }

//     //     #[test]
//     //     fn since_4() {
//     //         let formula = Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(1))), 1, 2);
//     //         let oracle = PmtlOracle::new(&[], &[formula]);
//     //         let mut state = oracle.generate();
//     //         state.update(&[false, true], 0);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //         state.update(&[false, true], 1);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //         state.update(&[false, true], 2);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //         state.update(&[true, true], 2);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //         state.update(&[true, false], 3);
//     //         assert!(state.final_output_guarantees().any(|b| b));
//     //         state.update(&[true, false], 4);
//     //         assert!(state.final_output_guarantees().any(|b| b));
//     //         state.update(&[true, false], 5);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //     }

//     //     #[test]
//     //     fn historically() {
//     //         let formula = Pmtl::Historically(Box::new(Pmtl::Atom(0)), 1, 2);
//     //         let oracle = PmtlOracle::new(&[], &[formula]);
//     //         let mut state = oracle.generate();
//     //         state.update(&[false], 0);
//     //         assert!(state.final_output_guarantees().any(|b| b));
//     //         state.update(&[false], 0);
//     //         assert!(state.final_output_guarantees().any(|b| b));
//     //         state.update(&[true], 1);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //         state.update(&[true], 2);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //         state.update(&[true], 3);
//     //         assert!(state.final_output_guarantees().any(|b| b));
//     //         state.update(&[false], 3);
//     //         assert!(state.final_output_guarantees().any(|b| b));
//     //         state.update(&[true], 4);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //     }

//     //     #[test]
//     //     fn previously() {
//     //         let formula = Pmtl::Once(Box::new(Pmtl::Atom(0)), 1, 2);
//     //         let oracle = PmtlOracle::new(&[], &[formula]);
//     //         let mut state = oracle.generate();
//     //         state.update(&[false], 0);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //         state.update(&[false], 0);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //         state.update(&[true], 1);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //         state.update(&[false], 2);
//     //         assert!(state.final_output_guarantees().any(|b| b));
//     //         state.update(&[false], 3);
//     //         assert!(state.final_output_guarantees().any(|b| b));
//     //         state.update(&[false], 3);
//     //         assert!(state.final_output_guarantees().any(|b| b));
//     //         state.update(&[true], 4);
//     //         assert!(state.final_output_guarantees().any(|b| !b));
//     //     }
// }
