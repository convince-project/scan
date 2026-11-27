mod numset;

use numset::NumSet;
use scan_core::{Oracle, OracleTree, Time, TimeRange};
use std::hash::Hash;

/// A Past-time Metric Temporal Logic (PMTL) formula.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ctlp<V>
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
    And(Box<[Ctlp<V>]>),
    /// Logical conjunction of a list of formulae.
    Or(Box<[Ctlp<V>]>),
    /// Logical negation of a formula.
    Not(Box<Ctlp<V>>),
    /// Logical implication of a antecedent formula and a consequent formula.
    Implies(Box<(Ctlp<V>, Ctlp<V>)>),
    /// Exists path
    Exist(PathCtl<V>),
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
    Historically(Box<Ctlp<V>>, TimeRange),
    /// Temporal previously predicate over a formula (with bounds).
    Once(Box<Ctlp<V>>, TimeRange),
    /// Temporal since predicate over a formula (with bounds).
    Since(Box<(Ctlp<V>, Ctlp<V>)>, TimeRange),
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
                if !sub.output(time) {
                    *valuation += range.shift(time);
                }
            }
            ValPathCtl::Once(sub, range, valuation) => {
                sub.update_state(state, time);
                if sub.output(time) {
                    *valuation += range.shift(time);
                }
            }
            ValPathCtl::Since(subformulae, range, valuation) => {
                let (lhs, rhs) = subformulae.as_mut();
                // Make sure all subformulae are updated
                lhs.update_state(state, time);
                let out_lhs = lhs.output(time);
                rhs.update_state(state, time);
                let out_rhs = rhs.output(time);
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
                if !sub.output(time) {
                    *valuation += range.shift(time);
                }
            }
            ValPathCtl::Once(sub, range, valuation) => {
                sub.update_time(time);
                if sub.output(time) {
                    *valuation += range.shift(time);
                }
            }
            ValPathCtl::Since(subformulae, range, valuation) => {
                let (lhs, rhs) = subformulae.as_mut();
                // Make sure all subformulae are updated
                lhs.update_time(time);
                let out_lhs = lhs.output(time);
                rhs.update_time(time);
                let out_rhs = rhs.output(time);
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

    fn fold_branch(&mut self, closed_val_path_ctl: ValPathCtl, time: Time) {
        match self {
            ValPathCtl::Historically(val_ctl, _time_range, _num_set)
                if let ValPathCtl::Historically(closed_val_ctl, _, _) = closed_val_path_ctl =>
            {
                val_ctl.fold_branch(*closed_val_ctl, time);
            }
            ValPathCtl::Once(val_ctl, _time_range, _num_set)
                if let ValPathCtl::Once(closed_val_ctl, _, _) = closed_val_path_ctl =>
            {
                val_ctl.fold_branch(*closed_val_ctl, time);
            }
            ValPathCtl::Since(val_ctls, _time_range, _num_set)
                if let ValPathCtl::Since(closed_val_ctls, _, _) = closed_val_path_ctl =>
            {
                val_ctls.0.fold_branch(closed_val_ctls.0, time);
                val_ctls.1.fold_branch(closed_val_ctls.1, time);
            }
            _ => {
                panic!("formulae not matching");
            }
        }
        // state formula valuations can change after folding
        // so path formula internal states need to be updated,
        // and update_time is the easiest way to do so
        self.update_time(time);
    }

    // Represents the knowledge that the given formula is always true, or always false, from the current moment.
    pub fn valuation(&self, time: Time) -> Option<bool> {
        match self {
            // Valuation set of Historically represents the set of time moments in which we know the formula to be false.
            ValPathCtl::Historically(_subformula, _rangee, valuation) => {
                valuation.contains_unbounded_interval(time).then_some(false)
            }
            // Valuation set of Previously represents the set of time moments in which we know the formula to be true.
            ValPathCtl::Once(_subformula, _range, valuation) => {
                valuation.contains_unbounded_interval(time).then_some(true)
            }
            // Valuation set of Since represents the set of time moments in which we know the formula to be true if its lhs argument is true from then on.
            ValPathCtl::Since(subformulae, _range, valuation) => (valuation
                .contains_unbounded_interval(time)
                && matches!(subformulae.0.valuation(), Some(true)))
            .then_some(true),
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
    Exist(ValPathCtl, Option<bool>),
    All(ValPathCtl, Option<bool>),
}

impl From<&Ctlp<usize>> for ValCtl {
    fn from(value: &Ctlp<usize>) -> Self {
        match value {
            Ctlp::True => ValCtl::True,
            Ctlp::False => ValCtl::False,
            Ctlp::Atom(a) => ValCtl::Atom(*a, false),
            Ctlp::And(pmtls) => ValCtl::And(pmtls.iter().map(|f| f.into()).collect()),
            Ctlp::Or(pmtls) => ValCtl::Or(pmtls.iter().map(|f| f.into()).collect()),
            Ctlp::Not(pmtl) => ValCtl::Not(Box::new(pmtl.as_ref().into())),
            Ctlp::Implies(pmtls) => {
                ValCtl::Implies(Box::new(((&pmtls.0).into(), (&pmtls.1).into())))
            }
            Ctlp::Exist(path_ctl) => {
                let path_ctl: ValPathCtl = path_ctl.into();
                let val = path_ctl.valuation(0);
                ValCtl::Exist(path_ctl, val)
            }
            Ctlp::All(path_ctl) => {
                let path_ctl: ValPathCtl = path_ctl.into();
                let val = path_ctl.valuation(0);
                ValCtl::All(path_ctl, val)
            }
        }
    }
}

impl ValCtl {
    // From Ulus 2024: Online monitoring of metric temporal logic using sequential networks. Link: <http://arxiv.org/abs/1901.00175v2>
    pub fn output(&self, time: Time) -> bool {
        match self {
            ValCtl::True => true,
            ValCtl::False => false,
            ValCtl::Atom(_atom, b) => *b,
            ValCtl::And(subs) => subs.iter().all(|f| f.output(time)),
            ValCtl::Or(subs) => subs.iter().any(|f| f.output(time)),
            ValCtl::Not(subformula) => !subformula.output(time),
            ValCtl::Implies(subformulae) => {
                let (lhs, rhs) = subformulae.as_ref();
                rhs.output(time) || lhs.output(time)
            }
            ValCtl::Exist(val_path_ctl, _val) => val_path_ctl.output(time),
            ValCtl::All(val_path_ctl, _val) => val_path_ctl.output(time),
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
            ValCtl::Exist(val_path_ctl, val) => {
                val_path_ctl.update_state(state, time);
                if let Some(val_path) = val_path_ctl.valuation(time) {
                    if let Some(val) = val {
                        *val = *val || val_path;
                    } else {
                        *val = Some(val_path);
                    }
                }
            }
            ValCtl::All(val_path_ctl, val) => {
                val_path_ctl.update_state(state, time);
                if let Some(val_path) = val_path_ctl.valuation(time) {
                    if let Some(val) = val {
                        *val = *val && val_path;
                    } else {
                        *val = Some(val_path);
                    }
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
            ValCtl::Exist(val_path_ctl, val) => {
                val_path_ctl.update_time(time);
                if let Some(val_path) = val_path_ctl.valuation(time) {
                    if let Some(val) = val {
                        *val = *val || val_path;
                    } else {
                        *val = Some(val_path);
                    }
                }
            }
            ValCtl::All(val_path_ctl, val) => {
                val_path_ctl.update_time(time);
                if let Some(val_path) = val_path_ctl.valuation(time) {
                    if let Some(val) = val {
                        *val = *val && val_path;
                    } else {
                        *val = Some(val_path);
                    }
                }
            }
            _ => {
                // nothing to do
            }
        }
    }

    // Represents the knowledge that the given formula is always true, or always false, from the current moment.
    pub fn valuation(&self) -> Option<bool> {
        match self {
            ValCtl::True => Some(true),
            ValCtl::False => Some(false),
            ValCtl::Atom(_, _) => None,
            ValCtl::And(items) => items
                .iter()
                .try_fold(true, |b, f| f.valuation().map(|v| b && v)),
            ValCtl::Or(items) => items
                .iter()
                .try_fold(false, |b, f| f.valuation().map(|v| b || v)),
            ValCtl::Not(f) => f.valuation().map(|b| !b),
            ValCtl::Implies(subformulae) => {
                let lhs_val = subformulae.0.valuation();
                let rhs_val = subformulae.1.valuation();
                if rhs_val.is_some_and(|b| b) || lhs_val.is_some_and(|b| !b) {
                    Some(true)
                } else if rhs_val.is_some_and(|b| !b) && lhs_val.is_some_and(|b| b) {
                    Some(false)
                } else {
                    None
                }
            }
            ValCtl::Exist(_val_path_ctl, val) => *val,
            ValCtl::All(_val_path_ctl, val) => *val,
        }
    }

    pub fn final_valuation(&self) -> bool {
        match self {
            ValCtl::True => true,
            ValCtl::False => false,
            ValCtl::Atom(_atom, b) => *b,
            ValCtl::And(subs) => subs.iter().all(|f| f.final_valuation()),
            ValCtl::Or(subs) => subs.iter().any(|f| f.final_valuation()),
            ValCtl::Not(subformula) => !subformula.final_valuation(),
            ValCtl::Implies(subformulae) => {
                let (lhs, rhs) = subformulae.as_ref();
                rhs.final_valuation() || lhs.final_valuation()
            }
            ValCtl::Exist(_val_path_ctl, val) => val.unwrap_or(false),
            ValCtl::All(_val_path_ctl, val) => val.unwrap_or(true),
        }
    }

    fn fold_branch(&mut self, closed_formula: Self, time: Time) {
        match self {
            ValCtl::And(val_ctls) if let ValCtl::And(closed_val_ctls) = closed_formula => {
                for (current_formula, closed_formula) in val_ctls.iter_mut().zip(closed_val_ctls) {
                    current_formula.fold_branch(closed_formula, time)
                }
            }
            ValCtl::Or(val_ctls) if let ValCtl::Or(closed_val_ctls) = closed_formula => {
                for (current_formula, closed_formula) in val_ctls.iter_mut().zip(closed_val_ctls) {
                    current_formula.fold_branch(closed_formula, time)
                }
            }
            ValCtl::Not(val_ctl) if let ValCtl::Not(closed_val_ctl) = closed_formula => {
                val_ctl.fold_branch(*closed_val_ctl, time)
            }
            ValCtl::Implies(val_ctls) if let ValCtl::Implies(closed_val_ctls) = closed_formula => {
                val_ctls.0.fold_branch(closed_val_ctls.0, time);
                val_ctls.1.fold_branch(closed_val_ctls.1, time);
            }
            ValCtl::Exist(val_path_ctl, val)
                if let ValCtl::Exist(closed_val_ctl, closed_val) = closed_formula =>
            {
                let closed_val = closed_val.unwrap_or(false) || closed_val_ctl.output(time);
                val_path_ctl.fold_branch(closed_val_ctl, time);
                if closed_val {
                    *val = Some(true);
                } else {
                    let new_val = val_path_ctl.valuation(time);
                    if new_val.is_some() {
                        *val = new_val;
                    }
                }
            }
            ValCtl::All(val_path_ctl, val)
                if let ValCtl::All(closed_val_ctl, closed_val) = closed_formula =>
            {
                let closed_val = closed_val.unwrap_or(true) && closed_val_ctl.output(time);
                val_path_ctl.fold_branch(closed_val_ctl, time);
                if !closed_val {
                    *val = Some(false);
                } else {
                    let new_val = val_path_ctl.valuation(time);
                    if new_val.is_some() {
                        *val = new_val;
                    }
                }
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
pub struct CtlpOracle {
    assumes: Box<[ValCtl]>,
    guarantees: Box<[ValCtl]>,
    time: Time,
}

impl CtlpOracle {
    /// Creates an oracle from assumes and guarantees CMTL formulae.
    pub fn new(assumes: &[Ctlp<usize>], guarantees: &[Ctlp<usize>]) -> Self {
        Self {
            assumes: assumes.iter().map(|f| f.into()).collect(),
            guarantees: guarantees.iter().map(|f| f.into()).collect(),
            time: 0,
        }
    }
}

impl OracleTree for CtlpOracle {
    fn fold_branch(&mut self, closed: Self) {
        for (current_formula, closed_formula) in self.assumes.iter_mut().zip(closed.assumes) {
            current_formula.fold_branch(closed_formula, self.time);
        }
        for (current_formula, closed_formula) in self.guarantees.iter_mut().zip(closed.guarantees) {
            current_formula.fold_branch(closed_formula, self.time);
        }
    }
}

impl Oracle for CtlpOracle {
    #[inline]
    fn output_assumes(&self) -> impl Iterator<Item = Option<bool>> {
        self.assumes.iter().map(move |f| f.valuation())
    }

    #[inline]
    fn output_guarantees(&self) -> impl Iterator<Item = Option<bool>> {
        self.guarantees.iter().map(move |f| f.valuation())
    }

    #[inline]
    fn final_output_assumes(&self) -> impl Iterator<Item = bool> {
        self.assumes.iter().map(move |f| f.final_valuation())
    }

    #[inline]
    fn final_output_guarantees(&self) -> impl Iterator<Item = bool> {
        self.guarantees.iter().map(move |f| f.final_valuation())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exist_once() {
        let formula = Ctlp::Exist(PathCtl::Once(Box::new(Ctlp::Atom(0)), TimeRange::new(..)));
        let mut initial_oracle = CtlpOracle::new(&[], &[formula]);
        initial_oracle.update_state(&[false]);
        let mut oracle = initial_oracle.clone();
        assert!(oracle.output_guarantees().all(|b| b.is_none()));
        // first branch
        let mut branch_oracle = oracle.clone();
        branch_oracle.update_state(&[false]);
        assert!(branch_oracle.output_guarantees().all(|b| b.is_none()));
        oracle.fold_branch(branch_oracle);
        assert!(oracle.output_guarantees().all(|b| b.is_none()));
        // second branch
        let mut branch_oracle = oracle.clone();
        branch_oracle.update_state(&[true]);
        assert!(
            branch_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| b))
        );
        oracle.fold_branch(branch_oracle);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| b)));
        // third branch
        oracle.update_state(&[false]);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| b)));
        initial_oracle.fold_branch(oracle);
        assert!(
            initial_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| b))
        );
        assert!(initial_oracle.final_output_guarantees().all(|b| b));
    }

    #[test]
    fn exist_historically() {
        let formula = Ctlp::Exist(PathCtl::Historically(
            Box::new(Ctlp::Atom(0)),
            TimeRange::new(..),
        ));
        let mut initial_oracle = CtlpOracle::new(&[], &[formula]);
        initial_oracle.update_state(&[true]);
        let mut oracle = initial_oracle.clone();
        assert!(oracle.output_guarantees().all(|b| b.is_none()));
        // first branch
        let mut branch_oracle = oracle.clone();
        branch_oracle.update_state(&[false]);
        assert!(
            branch_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| !b))
        );
        oracle.fold_branch(branch_oracle);
        assert!(oracle.output_guarantees().all(|b| b.is_none()));
        // second branch
        let mut branch_oracle = oracle.clone();
        branch_oracle.update_state(&[true]);
        assert!(branch_oracle.output_guarantees().all(|b| b.is_none()));
        oracle.fold_branch(branch_oracle);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| b)));
        // third branch
        oracle.update_state(&[false]);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| b)));
        initial_oracle.fold_branch(oracle);
        assert!(
            initial_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| b))
        );
        assert!(initial_oracle.final_output_guarantees().all(|b| b));
    }

    #[test]
    fn all_once() {
        let formula = Ctlp::All(PathCtl::Once(Box::new(Ctlp::Atom(0)), TimeRange::new(..)));
        let mut initial_oracle = CtlpOracle::new(&[], &[formula]);
        initial_oracle.update_state(&[false]);
        let mut oracle = initial_oracle.clone();
        assert!(oracle.output_guarantees().all(|b| b.is_none()));
        // first branch
        let mut branch_oracle = oracle.clone();
        branch_oracle.update_state(&[true]);
        assert!(
            branch_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| b))
        );
        oracle.fold_branch(branch_oracle);
        assert!(oracle.output_guarantees().all(|b| b.is_none()));
        // second branch
        let mut branch_oracle = oracle.clone();
        branch_oracle.update_state(&[false]);
        assert!(branch_oracle.output_guarantees().all(|b| b.is_none()));
        branch_oracle.update_state(&[true]);
        assert!(
            branch_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| b))
        );
        branch_oracle.update_state(&[false]);
        assert!(
            branch_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| b))
        );
        oracle.fold_branch(branch_oracle);
        assert!(oracle.output_guarantees().all(|b| b.is_none()));
        // third branch
        oracle.update_state(&[true]);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| b)));
        oracle.update_state(&[false]);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| b)));
        initial_oracle.fold_branch(oracle);
        assert!(initial_oracle.output_guarantees().all(|b| b.is_none()));
        assert!(initial_oracle.final_output_guarantees().all(|b| b));
    }

    #[test]
    fn all_historically() {
        let formula = Ctlp::All(PathCtl::Historically(
            Box::new(Ctlp::Atom(0)),
            TimeRange::new(..),
        ));
        let mut initial_oracle = CtlpOracle::new(&[], &[formula]);
        initial_oracle.update_state(&[true]);
        let mut oracle = initial_oracle.clone();
        assert!(oracle.output_guarantees().all(|b| b.is_none()));
        // first branch
        let mut branch_oracle = oracle.clone();
        branch_oracle.update_state(&[true]);
        assert!(branch_oracle.output_guarantees().all(|b| b.is_none()));
        oracle.fold_branch(branch_oracle);
        assert!(oracle.output_guarantees().all(|b| b.is_none()));
        // second branch
        let mut branch_oracle = oracle.clone();
        branch_oracle.update_state(&[false]);
        assert!(
            branch_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| !b))
        );
        oracle.fold_branch(branch_oracle);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| !b)));
        // third branch
        oracle.update_state(&[true]);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| !b)));
        initial_oracle.fold_branch(oracle);
        assert!(
            initial_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| !b))
        );
        assert!(initial_oracle.final_output_guarantees().all(|b| !b));
    }

    #[test]
    fn all_historically_exist_once() {
        let formula = Ctlp::All(PathCtl::Historically(
            Box::new(Ctlp::Exist(PathCtl::Once(
                Box::new(Ctlp::Atom(0)),
                TimeRange::new(..),
            ))),
            TimeRange::new(..),
        ));
        let mut initial_oracle = CtlpOracle::new(&[], &[formula]);
        initial_oracle.update_state(&[false]);
        let mut oracle = initial_oracle.clone();
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| !b)));
        // first branch
        let mut branch_oracle = oracle.clone();
        branch_oracle.update_state(&[true]);
        assert!(
            branch_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| !b))
        );
        oracle.fold_branch(branch_oracle);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| !b)));
        // second branch
        oracle.update_state(&[false]);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| !b)));
        initial_oracle.fold_branch(oracle);
        assert!(
            initial_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| !b))
        );
        assert!(initial_oracle.final_output_guarantees().all(|b| !b));
    }

    #[test]
    fn exist_historically_all_once() {
        let formula = Ctlp::Exist(PathCtl::Historically(
            Box::new(Ctlp::All(PathCtl::Once(
                Box::new(Ctlp::Atom(0)),
                TimeRange::new(..),
            ))),
            TimeRange::new(..),
        ));
        let mut initial_oracle = CtlpOracle::new(&[], &[formula]);
        initial_oracle.update_state(&[false]);
        let mut oracle = initial_oracle.clone();
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| !b)));
        // first branch
        let mut branch_oracle = oracle.clone();
        branch_oracle.update_state(&[true]);
        assert!(
            branch_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| !b))
        );
        oracle.fold_branch(branch_oracle);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| !b)));
        // second branch
        oracle.update_state(&[false]);
        assert!(oracle.output_guarantees().all(|b| b.is_some_and(|b| !b)));
        initial_oracle.fold_branch(oracle);
        assert!(
            initial_oracle
                .output_guarantees()
                .all(|b| b.is_some_and(|b| !b))
        );
        assert!(initial_oracle.final_output_guarantees().all(|b| !b));
    }
}
