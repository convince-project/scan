mod numset;

use crate::{Oracle, OracleGenerator, Time};
use numset::NumSet;
use std::collections::HashSet;
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

impl Pmtl<usize> {
    fn is_same(&self, value: &UPmtl) -> bool {
        match value {
            UPmtl::True => matches!(self, &Pmtl::True),
            UPmtl::False => matches!(self, &Pmtl::False),
            UPmtl::Atom(v) => {
                if let Pmtl::Atom(self_v) = self {
                    v == self_v
                } else {
                    false
                }
            }
            UPmtl::And(subs) => {
                if let Pmtl::And(self_subs) = self {
                    subs.len() == self_subs.len()
                        && self_subs
                            .iter()
                            .zip(subs)
                            .all(|(s, (v, _))| s.is_same(v.as_ref()))
                } else {
                    false
                }
            }
            UPmtl::Or(subs) => {
                if let Pmtl::Or(self_subs) = self {
                    subs.len() == self_subs.len()
                        && self_subs
                            .iter()
                            .zip(subs)
                            .all(|(s, (v, _))| s.is_same(v.as_ref()))
                } else {
                    false
                }
            }
            UPmtl::Not((sub, _)) => {
                if let Pmtl::Not(self_sub) = self {
                    self_sub.is_same(sub.as_ref())
                } else {
                    false
                }
            }
            UPmtl::Implies((lhs, _), (rhs, _)) => {
                if let Pmtl::Implies(self_sub) = self {
                    self_sub.0.is_same(lhs.as_ref()) && self_sub.1.is_same(rhs.as_ref())
                } else {
                    false
                }
            }
            UPmtl::Historically((sub, _), lower_bound, upper_bound) => {
                if let Pmtl::Historically(self_sub, self_lower_bound, self_upper_bound) = self {
                    self_sub.is_same(sub.as_ref())
                        && lower_bound == self_lower_bound
                        && upper_bound == self_upper_bound
                } else {
                    false
                }
            }
            UPmtl::Previously((sub, _), lower_bound, upper_bound) => {
                if let Pmtl::Once(self_sub, self_lower_bound, self_upper_bound) = self {
                    self_sub.is_same(sub.as_ref())
                        && lower_bound == self_lower_bound
                        && upper_bound == self_upper_bound
                } else {
                    false
                }
            }
            UPmtl::Since((lhs, _), (rhs, _), lower_bound, upper_bound) => {
                if let Pmtl::Since(self_sub, self_lower_bound, self_upper_bound) = self {
                    self_sub.0.is_same(lhs.as_ref())
                        && self_sub.1.is_same(rhs.as_ref())
                        && lower_bound == self_lower_bound
                        && upper_bound == self_upper_bound
                } else {
                    false
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum UPmtl {
    True,
    False,
    Atom(usize),
    And(Vec<IdxPmtl>),
    Or(Vec<IdxPmtl>),
    Not(IdxPmtl),
    Implies(IdxPmtl, IdxPmtl),
    Historically(IdxPmtl, Time, Time),
    Previously(IdxPmtl, Time, Time),
    Since(IdxPmtl, IdxPmtl, Time, Time),
}

type IdxPmtl = (Box<UPmtl>, usize);

/// An oracle for PMTL properties over timed, dense traces.
#[derive(Debug, Clone)]
pub struct PmtlOracle {
    assumes: Vec<usize>,
    guarantees: Vec<usize>,
    subformulae: Vec<UPmtl>,
}

impl PmtlOracle {
    /// Creates an oracle from assumes and guarantees PMTL formulae.
    pub fn new(assumes: &[Pmtl<usize>], guarantees: &[Pmtl<usize>]) -> Self {
        let set = HashSet::from_iter(
            assumes
                .iter()
                .chain(guarantees)
                .flat_map(|f| f.clone().set_subformulae().into_iter()),
        );
        let subformulae = subformulae(set);
        let assumes = assumes
            .iter()
            .map(|a| {
                subformulae
                    .iter()
                    .position(|f| a.is_same(f))
                    .expect("find assume")
            })
            .collect();
        let guarantees = guarantees
            .iter()
            .map(|g| {
                subformulae
                    .iter()
                    .position(|f| g.is_same(f))
                    .expect("find guarantee")
            })
            .collect();

        Self {
            assumes,
            guarantees,
            subformulae,
        }
    }
}

impl<V> Pmtl<V>
where
    V: Clone + Eq + Hash,
{
    fn set_subformulae(self) -> HashSet<Pmtl<V>> {
        let mut formulae = match &self {
            Pmtl::True | Pmtl::False | Pmtl::Atom(_) => HashSet::new(),
            Pmtl::And(subs) | Pmtl::Or(subs) => HashSet::from_iter(
                subs.iter()
                    .flat_map(|f| f.clone().set_subformulae().into_iter()),
            ),
            Pmtl::Not(subformula)
            | Pmtl::Historically(subformula, _, _)
            | Pmtl::Once(subformula, _, _) => subformula.as_ref().clone().set_subformulae(),
            Pmtl::Implies(subs) | Pmtl::Since(subs, _, _) => {
                let mut formulae = subs.0.clone().set_subformulae();
                formulae.extend(subs.1.clone().set_subformulae());
                formulae
            }
        };
        formulae.insert(self);
        formulae
    }
}

fn subformulae(set: HashSet<Pmtl<usize>>) -> Vec<UPmtl> {
    let mut vec = Vec::from_iter(set);
    vec.sort_unstable_by_key(Pmtl::depth);
    vec.dedup();
    vec.shrink_to_fit();
    let mut idx_vec: Vec<UPmtl> = Vec::new();
    for pmtl in vec {
        let arc_pmtl = match pmtl {
            Pmtl::True => UPmtl::True,
            Pmtl::False => UPmtl::False,
            Pmtl::Atom(p) => UPmtl::Atom(p),
            Pmtl::And(subs) => {
                let mut arc_subs = Vec::new();
                for sub in subs {
                    let idx = idx_vec.iter().position(|f| sub.is_same(f)).expect("index");
                    arc_subs.push((Box::new(idx_vec[idx].clone()), idx));
                }
                UPmtl::And(arc_subs)
            }
            Pmtl::Or(subs) => {
                let mut arc_subs = Vec::new();
                for sub in subs {
                    let idx = idx_vec.iter().position(|f| sub.is_same(f)).expect("index");
                    arc_subs.push((Box::new(idx_vec[idx].clone()), idx));
                }
                UPmtl::Or(arc_subs)
            }
            Pmtl::Not(sub) => {
                let idx = idx_vec.iter().position(|f| sub.is_same(f)).expect("index");
                UPmtl::Not((Box::new(idx_vec[idx].clone()), idx))
            }
            Pmtl::Implies(subs) => {
                let idx_0 = idx_vec
                    .iter()
                    .position(|f| subs.0.is_same(f))
                    .expect("index");
                let idx_1 = idx_vec
                    .iter()
                    .position(|f| subs.1.is_same(f))
                    .expect("index");
                UPmtl::Implies(
                    (Box::new(idx_vec[idx_0].clone()), idx_0),
                    (Box::new(idx_vec[idx_1].clone()), idx_1),
                )
            }
            Pmtl::Historically(sub, lower_bound, upper_bound) => {
                let idx = idx_vec.iter().position(|f| sub.is_same(f)).expect("index");
                UPmtl::Historically(
                    (Box::new(idx_vec[idx].clone()), idx),
                    lower_bound,
                    upper_bound,
                )
            }
            Pmtl::Once(sub, lower_bound, upper_bound) => {
                let idx = idx_vec.iter().position(|f| sub.is_same(f)).expect("index");
                UPmtl::Previously(
                    (Box::new(idx_vec[idx].clone()), idx),
                    lower_bound,
                    upper_bound,
                )
            }
            Pmtl::Since(subs, lower_bound, upper_bound) => {
                let idx_0 = idx_vec
                    .iter()
                    .position(|f| subs.0.is_same(f))
                    .expect("index");
                let idx_1 = idx_vec
                    .iter()
                    .position(|f| subs.1.is_same(f))
                    .expect("index");
                UPmtl::Since(
                    (Box::new(idx_vec[idx_0].clone()), idx_0),
                    (Box::new(idx_vec[idx_1].clone()), idx_1),
                    lower_bound,
                    upper_bound,
                )
            }
        };
        idx_vec.push(arc_pmtl);
    }
    idx_vec
}

impl<V> Pmtl<V>
where
    V: Clone,
{
    fn depth(&self) -> usize {
        match self {
            Pmtl::True | Pmtl::False | Pmtl::Atom(_) => 0,
            Pmtl::And(subs) | Pmtl::Or(subs) => subs.iter().map(Pmtl::depth).max().unwrap_or(0) + 1,
            Pmtl::Not(sub) | Pmtl::Historically(sub, _, _) | Pmtl::Once(sub, _, _) => {
                sub.depth() + 1
            }
            Pmtl::Implies(subs) | Pmtl::Since(subs, _, _) => subs.0.depth().max(subs.1.depth()) + 1,
        }
    }
}

/// Runnable instance of a [`PmtlOracle`] that borrows data from its generator.
#[derive(Debug, Clone)]
pub struct PmtlOracleRun<'a> {
    time: Time,
    assumes: &'a [usize],
    guarantees: &'a [usize],
    subformulae: &'a [UPmtl],
    valuations: Vec<NumSet>,
    outputs: Vec<bool>,
}

impl OracleGenerator for PmtlOracle {
    type O<'a>
        = PmtlOracleRun<'a>
    where
        Self: 'a;

    fn generate<'a>(&'a self) -> Self::O<'a> {
        PmtlOracleRun {
            // WARN: all Hell brakes loose with time: (0, 0)
            time: 0,
            assumes: &self.assumes,
            guarantees: &self.guarantees,
            subformulae: &self.subformulae,
            valuations: vec![NumSet::empty(); self.subformulae.len()],
            outputs: vec![true; self.subformulae.len()],
        }
    }
}

impl<'a> PmtlOracleRun<'a> {
    #[inline(always)]
    fn formula_output(&self, formula: usize) -> bool {
        self.outputs[formula]
    }

    // Represents the knowledge that the given formula is always true, or always false, from the current moment.
    fn formula_valuation(&self, formula: usize) -> Option<bool> {
        match self.subformulae.get(formula).expect("formula") {
            UPmtl::True => Some(true),
            UPmtl::False => Some(false),
            UPmtl::Atom(_) => None,
            UPmtl::And(items) => items
                .iter()
                .map(|(_, f)| self.formula_valuation(*f))
                .try_fold(true, |acc, t| t.map(|b| b && acc)),
            UPmtl::Or(items) => items
                .iter()
                .map(|(_, f)| self.formula_valuation(*f))
                .try_fold(false, |acc, t| t.map(|b| b || acc)),
            UPmtl::Not((_, f)) => self.formula_valuation(*f).map(|b| !b),
            UPmtl::Implies((_, f), (_, g)) => {
                if self.formula_valuation(*g).is_some_and(|b| b)
                    || self.formula_valuation(*f).is_some_and(|b| !b)
                {
                    Some(true)
                } else if self.formula_valuation(*g).is_some_and(|b| !b)
                    && self.formula_valuation(*f).is_some_and(|b| b)
                {
                    Some(false)
                } else {
                    None
                }
            }
            // Valuation of Historically represents the set of time moments in which we know the formula to be false.
            // If the argument of the operator is know to be always true, then Historically is always true.
            // If the argument of the operator is know to be always false, then Historically is always false provided that the lower time bound is 0.
            UPmtl::Historically((_, f), lb, _) => self
                .formula_valuation(*f)
                .and_then(|v| (*lb == 0 || v).then_some(v))
                .or_else(|| {
                    self.valuations
                        .get(formula)
                        .expect("valuation")
                        .contains_interval(self.time, Time::MAX)
                        .then_some(false)
                }),
            // Valuation of Previously represents the set of time moments in which we know the formula to be false.
            // If the argument of the operator is know to be always true, then Previously is always true provided that the lower time bound is 0.
            // If the argument of the operator is know to be always false, then Previously is always false.
            UPmtl::Previously((_, f), lb, _) => self
                .formula_valuation(*f)
                .and_then(|v| (*lb == 0 || !v).then_some(v))
                .or_else(|| {
                    self.valuations
                        .get(formula)
                        .expect("valuation")
                        .contains_interval(self.time, Time::MAX)
                        .then_some(true)
                }),
            // Valuation of Since represents the set of time moments in which we know the formula to be true if its lhs argument is true from then on.
            // If the rhs argument of the operator is know to be always false, then Since is always false.
            // If the Since valuation is always true and the lhs argument of the operator is know to be always true, then Since is always true.
            // If the Since valuation is always true and the lhs argument of the operator is know to be always false, then Since is always false provided that the lower time bound is 0.
            UPmtl::Since((_, lhs), (_, rhs), lb, _) => {
                if self.formula_valuation(*rhs) == Some(false) {
                    Some(false)
                } else if self
                    .valuations
                    .get(formula)
                    .expect("valuation")
                    .contains_interval(self.time, Time::MAX)
                {
                    self.formula_valuation(*lhs)
                        .and_then(|v| (*lb == 0 || v).then_some(v))
                } else {
                    None
                }
            }
        }
    }
}

impl<'a> Oracle for PmtlOracleRun<'a> {
    fn output_assumes(&self) -> impl Iterator<Item = Option<bool>> {
        self.assumes.iter().map(|f| self.formula_valuation(*f))
    }

    fn output_guarantees(&self) -> impl Iterator<Item = Option<bool>> {
        self.guarantees.iter().map(|f| self.formula_valuation(*f))
    }

    fn final_output_assumes(&self) -> impl Iterator<Item = bool> {
        self.assumes.iter().map(|f| self.formula_output(*f))
    }

    fn final_output_guarantees(&self) -> impl Iterator<Item = bool> {
        self.guarantees.iter().map(|f| self.formula_output(*f))
    }

    // From Ulus 2024: Online monitoring of metric temporal logic using sequential networks. Link: <http://arxiv.org/abs/1901.00175v2>
    fn update(&mut self, state: &[bool], time: Time) {
        assert!(self.time <= time);
        self.outputs.clear();
        for (idx, formula) in self.subformulae.iter().enumerate() {
            match formula {
                UPmtl::True => {
                    self.outputs.push(true);
                }
                UPmtl::False => {
                    self.outputs.push(false);
                }
                UPmtl::Atom(atom) => {
                    if state[*atom] {
                        self.outputs.push(true);
                    } else {
                        self.outputs.push(false);
                    }
                }
                UPmtl::And(subs) => {
                    self.outputs
                        .push(subs.iter().all(|(_, i)| self.outputs[*i]));
                }
                UPmtl::Or(subs) => {
                    self.outputs
                        .push(subs.iter().any(|(_, i)| self.outputs[*i]));
                }
                UPmtl::Not(subformula) => {
                    self.outputs.push(!self.outputs[subformula.1]);
                }
                UPmtl::Implies(sub_0, sub_1) => {
                    let out_lhs = self.outputs[sub_0.1];
                    let out_rhs = self.outputs[sub_1.1];
                    self.outputs.push(out_rhs || !out_lhs);
                }
                UPmtl::Historically(sub, lower_bound, upper_bound) => {
                    if !self.outputs[sub.1] {
                        self.valuations[idx] = self.valuations[idx].add_interval(
                            lower_bound.saturating_add(time),
                            upper_bound.saturating_add(time),
                        );
                    }
                    self.outputs.push(!self.valuations[idx].contains(time));
                }
                UPmtl::Previously(sub, lower_bound, upper_bound) => {
                    if self.outputs[sub.1] {
                        self.valuations[idx] = self.valuations[idx].add_interval(
                            lower_bound.saturating_add(time),
                            upper_bound.saturating_add(time),
                        );
                    }
                    self.outputs.push(self.valuations[idx].contains(time));
                }
                UPmtl::Since(sub_0, sub_1, lower_bound, upper_bound) => {
                    if self.outputs[sub_0.1] {
                        if self.outputs[sub_1.1] {
                            self.valuations[idx] = self.valuations[idx].add_interval(
                                lower_bound.saturating_add(time),
                                upper_bound.saturating_add(time),
                            );
                        }
                    } else if self.outputs[sub_1.1] {
                        self.valuations[idx] = NumSet::from_range(
                            lower_bound.saturating_add(time),
                            upper_bound.saturating_add(time),
                        );
                    } else {
                        self.valuations[idx] = NumSet::empty();
                    }
                    self.outputs.push(self.valuations[idx].contains(time));
                }
            }
        }
        self.time = time;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn subformulae_1() {
        let subformulae: Vec<UPmtl> = subformulae(
            Pmtl::Since(Box::new((Pmtl::True, Pmtl::True)), 0, Time::MAX).set_subformulae(),
        );
        assert_eq!(
            subformulae,
            vec![
                UPmtl::True,
                UPmtl::Since(
                    (Box::new(UPmtl::True), 0),
                    (Box::new(UPmtl::True), 0),
                    0,
                    Time::MAX
                ),
            ]
        );
    }

    #[test]
    fn subformulae_2() {
        let subformulae: Vec<UPmtl> = subformulae(
            Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(0))), 0, Time::MAX).set_subformulae(),
        );
        assert_eq!(
            subformulae,
            vec![
                UPmtl::Atom(0),
                UPmtl::Since(
                    (Box::new(UPmtl::Atom(0)), 0),
                    (Box::new(UPmtl::Atom(0)), 0),
                    0,
                    Time::MAX
                ),
            ]
        );
    }

    #[test]
    fn subformulae_3() {
        let subformulae: Vec<UPmtl> = subformulae(
            Pmtl::And(vec![
                Pmtl::Atom(0),
                Pmtl::Not(Box::new(Pmtl::Atom(0))),
                Pmtl::True,
            ])
            .set_subformulae(),
        );
        assert_eq!(subformulae.len(), 4);
        assert!(matches!(subformulae[0], UPmtl::Atom(0) | UPmtl::True));
        assert!(matches!(subformulae[1], UPmtl::Atom(0) | UPmtl::True));
        assert!(matches!(subformulae[2], UPmtl::Not((_, 0 | 1))));
    }

    #[test]
    fn since_1() {
        let formula = Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(1))), 0, Time::MAX);
        let oracle = PmtlOracle::new(&[], &[formula]);
        let mut state = oracle.generate();
        state.update(&[false, true], 0);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[false, true], 1);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, true], 2);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, true], 3);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, false], 4);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[false, false], 5);
        assert!(state.final_output_guarantees().any(|b| !b));
    }

    #[test]
    fn since_2() {
        let formula = Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(1))), 0, 2);
        let oracle = PmtlOracle::new(&[], &[formula]);
        let mut state = oracle.generate();
        state.update(&[false, true], 0);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[false, true], 1);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, true], 2);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, false], 3);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, false], 4);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, false], 5);
        assert!(state.final_output_guarantees().any(|b| !b));
    }

    #[test]
    fn since_3() {
        let formula = Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(1))), 1, 2);
        let oracle = PmtlOracle::new(&[], &[formula]);
        let mut state = oracle.generate();
        state.update(&[false, true], 0);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[false, true], 1);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[true, true], 2);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, false], 3);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, false], 4);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, false], 5);
        assert!(state.final_output_guarantees().any(|b| !b));
    }

    #[test]
    fn since_4() {
        let formula = Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(1))), 1, 2);
        let oracle = PmtlOracle::new(&[], &[formula]);
        let mut state = oracle.generate();
        state.update(&[false, true], 0);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[false, true], 1);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[false, true], 2);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[true, true], 2);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[true, false], 3);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, false], 4);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true, false], 5);
        assert!(state.final_output_guarantees().any(|b| !b));
    }

    #[test]
    fn historically() {
        let formula = Pmtl::Historically(Box::new(Pmtl::Atom(0)), 1, 2);
        let oracle = PmtlOracle::new(&[], &[formula]);
        let mut state = oracle.generate();
        state.update(&[false], 0);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[false], 0);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true], 1);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[true], 2);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[true], 3);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[false], 3);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true], 4);
        assert!(state.final_output_guarantees().any(|b| !b));
    }

    #[test]
    fn previously() {
        let formula = Pmtl::Once(Box::new(Pmtl::Atom(0)), 1, 2);
        let oracle = PmtlOracle::new(&[], &[formula]);
        let mut state = oracle.generate();
        state.update(&[false], 0);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[false], 0);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[true], 1);
        assert!(state.final_output_guarantees().any(|b| !b));
        state.update(&[false], 2);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[false], 3);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[false], 3);
        assert!(state.final_output_guarantees().any(|b| b));
        state.update(&[true], 4);
        assert!(state.final_output_guarantees().any(|b| !b));
    }
}
