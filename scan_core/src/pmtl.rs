mod numset;

use crate::{Atom, Oracle, Time};
use hashbrown::{HashMap, HashSet};
use numset::NumSet;
use std::{hash::Hash, sync::Arc};

type DenseTime = (Time, Time);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pmtl<V>
where
    V: Clone,
{
    True,
    False,
    Atom(V),
    And(Vec<Pmtl<V>>),
    Or(Vec<Pmtl<V>>),
    Not(Arc<Pmtl<V>>),
    Implies(Arc<(Pmtl<V>, Pmtl<V>)>),
    Historically(Arc<Pmtl<V>>, Time, Time),
    Previously(Arc<Pmtl<V>>, Time, Time),
    Since(Arc<(Pmtl<V>, Pmtl<V>)>, Time, Time),
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
            | Pmtl::Previously(subformula, _, _) => subformula.as_ref().clone().set_subformulae(),
            Pmtl::Implies(subs) | Pmtl::Since(subs, _, _) => {
                let mut formulae = subs.0.clone().set_subformulae();
                formulae.extend(subs.1.clone().set_subformulae());
                formulae
            }
        };
        formulae.insert(self);
        formulae
    }

    pub(crate) fn subformulae(self) -> Vec<Pmtl<V>> {
        let set = self.set_subformulae();
        let mut vec = Vec::from_iter(set);
        vec.sort_unstable_by_key(Self::depth);
        vec.shrink_to_fit();
        vec
    }
}

impl<V> Pmtl<V>
where
    V: Clone,
{
    fn depth(&self) -> usize {
        match self {
            Pmtl::True | Pmtl::False | Pmtl::Atom(_) => 0,
            Pmtl::And(subs) | Pmtl::Or(subs) => subs.iter().map(Pmtl::depth).max().unwrap_or(0) + 1,
            Pmtl::Not(sub) | Pmtl::Historically(sub, _, _) | Pmtl::Previously(sub, _, _) => {
                sub.depth() + 1
            }
            Pmtl::Implies(subs) | Pmtl::Since(subs, _, _) => subs.0.depth().max(subs.1.depth()) + 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StateValuationVector<V: Clone + Eq + Hash> {
    time: DenseTime,
    subformulae: Arc<Vec<Pmtl<Atom<V>>>>,
    // The first NumSet represents the valuation,
    // the second NumSet represents the output.
    val_output: HashMap<Pmtl<Atom<V>>, (NumSet, NumSet)>,
}

impl<V: std::fmt::Debug + Clone + Eq + Hash> Oracle<V> for StateValuationVector<V> {
    fn update(&mut self, action: &V, state: &[bool], time: Time) -> bool {
        *self = self.valuation_update(action, state, time);
        self.output(self.subformulae.last().expect("formula"))
    }
}

impl<V: std::fmt::Debug + Clone + Eq + Hash> StateValuationVector<V> {
    pub fn new(formula: Pmtl<Atom<V>>) -> Self {
        let subformulae = Arc::new(formula.subformulae());

        Self {
            // WARN: all Hell brakes loose with time: (0, 0)
            time: (0, 1),
            val_output: HashMap::from_iter(
                subformulae
                    .iter()
                    .cloned()
                    .map(|f| (f, (NumSet::new(), NumSet::new()))),
            ),
            subformulae,
        }
    }

    fn output(&self, formula: &Pmtl<Atom<V>>) -> bool {
        self.val_output
            .get(formula)
            .map(|(_, out)| out.contains(self.time))
            .unwrap()
        // .unwrap_or(false)
    }

    fn valuation_update(&self, event: &V, state: &[bool], time: Time) -> Self {
        assert!(self.time.0 <= time);
        let mut new_valuation = Self {
            time: (time, self.time.1 + 1),
            subformulae: Arc::clone(&self.subformulae),
            val_output: HashMap::with_capacity(self.subformulae.len()),
        };
        for formula in self.subformulae.iter() {
            match formula {
                Pmtl::True => {
                    new_valuation.val_output.insert(
                        formula.clone(),
                        (
                            NumSet::full(),
                            NumSet::from_range(self.time, new_valuation.time),
                        ),
                    );
                }
                Pmtl::False => {
                    new_valuation
                        .val_output
                        .insert(formula.clone(), (NumSet::new(), NumSet::new()));
                }
                Pmtl::Atom(atom) => match atom {
                    Atom::Predicate(p) if state[*p] => {
                        let numset = NumSet::from_range(self.time, new_valuation.time);
                        new_valuation
                            .val_output
                            .insert(formula.clone(), (numset.clone(), numset));
                    }
                    Atom::Event(e) if event == e => {
                        let numset = NumSet::from_range(
                            (new_valuation.time.0, new_valuation.time.1 - 1),
                            new_valuation.time,
                        );
                        new_valuation
                            .val_output
                            .insert(formula.clone(), (numset.clone(), numset));
                    }
                    _ => {
                        new_valuation
                            .val_output
                            .insert(formula.clone(), (NumSet::new(), NumSet::new()));
                    }
                },
                Pmtl::And(subs) => {
                    let nset = NumSet::intersection(
                        subs.iter()
                            .filter_map(|f| new_valuation.val_output.get(f).map(|(_, val)| val))
                            .cloned(),
                    )
                    .simplify();
                    new_valuation
                        .val_output
                        .insert(formula.clone(), (nset.clone(), nset));
                }
                Pmtl::Or(subs) => {
                    let nset = subs
                        .iter()
                        .filter_map(|sub| new_valuation.val_output.get(sub))
                        .fold(NumSet::new(), |mut union, (_, numset)| {
                            union.union(numset);
                            union
                        })
                        .simplify();
                    new_valuation
                        .val_output
                        .insert(formula.clone(), (nset.clone(), nset));
                }
                Pmtl::Not(subformula) => {
                    let mut nset = new_valuation
                        .val_output
                        .get(subformula.as_ref())
                        .expect("nset")
                        .1
                        .clone();
                    nset.complement();
                    nset.cut(self.time, new_valuation.time);
                    nset.simplify();
                    new_valuation
                        .val_output
                        .insert(formula.clone(), (nset.clone(), nset));
                }
                Pmtl::Implies(subs) => {
                    let out_lhs = &new_valuation.val_output.get(&subs.0).expect("output lhs").1;
                    let out_rhs = &new_valuation.val_output.get(&subs.1).expect("output rhs").1;
                    let mut nset = out_lhs.clone();
                    nset.complement();
                    nset.union(out_rhs);
                    nset.cut(self.time, new_valuation.time);
                    nset.simplify();
                    new_valuation
                        .val_output
                        .insert(formula.clone(), (nset.clone(), nset));
                }
                Pmtl::Historically(sub, lower_bound, upper_bound) => {
                    let mut output_sub = new_valuation
                        .val_output
                        .get(sub.as_ref())
                        .expect("nset")
                        .1
                        .clone();
                    output_sub.insert_bound(new_valuation.time);
                    let mut valuation = self.val_output.get(formula).expect("formula").0.clone();
                    let mut partial_lower_bound = self.time;
                    let mut nset_output = NumSet::new();
                    for (partial_upper_bound, out_sub) in
                        output_sub.bounds().iter().filter(|(ub, _)| self.time < *ub)
                    {
                        assert!(partial_lower_bound < *partial_upper_bound);
                        assert!(self.time < *partial_upper_bound);
                        if !*out_sub {
                            let lower_bound = if *lower_bound > 0 {
                                lower_bound
                                    .checked_add(partial_lower_bound.0)
                                    .map(|ub| (ub, 0))
                                    .unwrap_or((Time::MAX, Time::MAX))
                            } else {
                                partial_lower_bound
                            };
                            let upper_bound = upper_bound
                                .checked_add(partial_upper_bound.0)
                                .map(|ub| (ub, Time::MAX))
                                .unwrap_or((Time::MAX, Time::MAX));
                            valuation.add_interval(lower_bound, upper_bound);
                        }
                        let mut to_add = valuation.clone();
                        to_add.cut(partial_lower_bound, *partial_upper_bound);
                        nset_output.union(&to_add);
                        partial_lower_bound = *partial_upper_bound;
                    }
                    nset_output.complement();
                    nset_output.cut(self.time, new_valuation.time);
                    valuation.cut(self.time, (Time::MAX, Time::MAX));
                    new_valuation.val_output.insert(
                        formula.clone(),
                        (valuation.simplify(), nset_output.simplify()),
                    );
                }
                Pmtl::Previously(sub, lower_bound, upper_bound) => {
                    let mut output_sub = new_valuation
                        .val_output
                        .get(sub.as_ref())
                        .expect("nset")
                        .1
                        .clone();
                    output_sub.insert_bound(new_valuation.time);
                    let mut valuation = self.val_output.get(formula).expect("formula").0.clone();
                    let mut partial_lower_bound = self.time;
                    let mut output = NumSet::new();
                    for (partial_upper_bound, out_sub) in
                        output_sub.bounds().iter().filter(|(ub, _)| self.time < *ub)
                    {
                        assert!(partial_lower_bound < *partial_upper_bound);
                        assert!(self.time < *partial_upper_bound);
                        assert!(*partial_upper_bound <= new_valuation.time);
                        if *out_sub {
                            let interval_lower_bound = if *lower_bound > 0 {
                                lower_bound
                                    .checked_add(partial_lower_bound.0)
                                    .map(|ub| (ub, 0))
                                    .unwrap()
                            } else {
                                partial_lower_bound
                            };
                            let interval_upper_bound = upper_bound
                                .checked_add(partial_upper_bound.0)
                                .map(|ub| (ub, Time::MAX))
                                .unwrap_or((Time::MAX, Time::MAX));
                            valuation.add_interval(interval_lower_bound, interval_upper_bound);
                        }
                        let mut to_add = valuation.clone();
                        to_add.cut(partial_lower_bound, *partial_upper_bound);
                        output.union(&to_add);
                        partial_lower_bound = *partial_upper_bound;
                    }
                    valuation.cut(self.time, (Time::MAX, Time::MAX));
                    new_valuation
                        .val_output
                        .insert(formula.clone(), (valuation.simplify(), output.simplify()));
                }
                Pmtl::Since(subs, lower_bound, upper_bound) => {
                    let mut nset_lhs = new_valuation
                        .val_output
                        .get(&subs.0)
                        .expect("nset")
                        .1
                        .clone();
                    let nset_rhs_orig = &new_valuation.val_output.get(&subs.1).expect("nset").1;
                    let mut nset_rhs = nset_rhs_orig.clone();
                    nset_rhs.insert_bound(new_valuation.time);
                    nset_lhs.insert_bound(new_valuation.time);
                    nset_rhs.sync(&nset_lhs);
                    nset_lhs.sync(nset_rhs_orig);
                    let mut valuation = self.val_output.get(formula).expect("formula").0.clone();
                    let mut partial_lower_bound = self.time;
                    let mut nset_output = NumSet::new();
                    for (idx, (partial_upper_bound, out_lhs)) in nset_lhs
                        .bounds()
                        .iter()
                        .enumerate()
                        .filter(|(_, (ub, _))| self.time < *ub)
                    {
                        // since nset_0 and nset_1 are synched:
                        assert_eq!(*partial_upper_bound, nset_rhs.bounds()[idx].0);
                        assert!(partial_lower_bound < *partial_upper_bound);
                        assert!(self.time < *partial_upper_bound);
                        let out_rhs = nset_rhs.bounds()[idx].1;
                        valuation = match (out_lhs, out_rhs) {
                            (true, true) => {
                                let lower_bound = if *lower_bound > 0 {
                                    lower_bound
                                        .checked_add(partial_lower_bound.0)
                                        .map(|ub| (ub, 0))
                                        .unwrap_or((Time::MAX, Time::MAX))
                                } else {
                                    partial_lower_bound
                                };
                                let upper_bound = upper_bound
                                    .checked_add(partial_upper_bound.0)
                                    .map(|ub| (ub, Time::MAX))
                                    .unwrap_or((Time::MAX, Time::MAX));
                                valuation.add_interval(lower_bound, upper_bound);
                                valuation
                            }
                            (true, false) => valuation,
                            (false, true) => {
                                let lower_bound = if *lower_bound > 0 {
                                    lower_bound
                                        .checked_add(partial_upper_bound.0)
                                        .map(|ub| (ub, 0))
                                        .unwrap_or((Time::MAX, Time::MAX))
                                } else {
                                    *partial_upper_bound
                                };
                                let upper_bound = upper_bound
                                    .checked_add(partial_upper_bound.0)
                                    .map(|ub| (ub, Time::MAX))
                                    .unwrap_or((Time::MAX, Time::MAX));
                                NumSet::from_range(lower_bound, upper_bound)
                            }
                            (false, false) => NumSet::new(),
                        };
                        let mut to_add = valuation.clone();
                        to_add.cut(partial_lower_bound, *partial_upper_bound);
                        nset_output.union(&to_add);
                        partial_lower_bound = *partial_upper_bound;
                    }
                    valuation.cut(self.time, (Time::MAX, Time::MAX));
                    new_valuation.val_output.insert(
                        formula.clone(),
                        (valuation.simplify(), nset_output.simplify()),
                    );
                }
            }
        }
        new_valuation
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn subformulae_1() {
        let subformulae: Vec<Pmtl<usize>> =
            Pmtl::Since(Arc::new((Pmtl::True, Pmtl::True)), 0, Time::MAX).subformulae();
        assert_eq!(
            subformulae,
            vec![
                Pmtl::True,
                Pmtl::Since(Arc::new((Pmtl::True, Pmtl::True)), 0, Time::MAX)
            ]
        );
    }

    #[test]
    fn subformulae_2() {
        let subformulae: Vec<Pmtl<usize>> =
            Pmtl::Since(Arc::new((Pmtl::Atom(0), Pmtl::Atom(0))), 0, Time::MAX).subformulae();
        assert_eq!(
            subformulae,
            vec![
                Pmtl::Atom(0),
                Pmtl::Since(Arc::new((Pmtl::Atom(0), Pmtl::Atom(0))), 0, Time::MAX)
            ]
        );
    }

    #[test]
    fn subformulae_3() {
        let subformulae: Vec<Pmtl<usize>> = Pmtl::And(vec![
            Pmtl::Atom(0),
            Pmtl::Not(Arc::new(Pmtl::Atom(0))),
            Pmtl::True,
        ])
        .subformulae();
        assert_eq!(subformulae.len(), 4);
        assert!(matches!(subformulae[0], Pmtl::Atom(0) | Pmtl::True));
        assert!(matches!(subformulae[1], Pmtl::Atom(0) | Pmtl::True));
        assert_eq!(subformulae[2], Pmtl::Not(Arc::new(Pmtl::Atom(0))));
    }

    #[test]
    fn since_1() {
        let formula = Pmtl::Since(
            Arc::new((
                Pmtl::Atom(Atom::Predicate(0)),
                Pmtl::Atom(Atom::Predicate(1)),
            )),
            0,
            Time::MAX,
        );
        let mut state = StateValuationVector::new(formula);
        assert!(!state.update(&0, &[false, true], 0));
        assert!(!state.update(&0, &[false, true], 1));
        assert!(state.update(&0, &[true, true], 2));
        assert!(state.update(&0, &[true, true], 3));
        assert!(state.update(&0, &[true, false], 4));
        assert!(!state.update(&0, &[false, false], 5));
    }

    #[test]
    fn since_2() {
        let formula = Pmtl::Since(
            Arc::new((
                Pmtl::Atom(Atom::Predicate(0)),
                Pmtl::Atom(Atom::Predicate(1)),
            )),
            0,
            2,
        );
        let mut state = StateValuationVector::new(formula);
        assert!(!state.update(&0, &[false, true], 0));
        assert!(!state.update(&0, &[false, true], 1));
        assert!(state.update(&0, &[true, true], 2));
        assert!(state.update(&0, &[true, false], 3));
        assert!(state.update(&0, &[true, false], 4));
        assert!(!state.update(&0, &[true, false], 5));
    }

    #[test]
    fn since_3() {
        let formula = Pmtl::Since(
            Arc::new((
                Pmtl::Atom(Atom::Predicate(0)),
                Pmtl::Atom(Atom::Predicate(1)),
            )),
            1,
            2,
        );
        let mut state = StateValuationVector::new(formula);
        assert!(!state.update(&0, &[false, true], 0));
        assert!(!state.update(&0, &[false, true], 1));
        assert!(state.update(&0, &[true, true], 2));
        assert!(state.update(&0, &[true, false], 3));
        assert!(state.update(&0, &[true, false], 4));
        assert!(!state.update(&0, &[true, false], 5));
    }

    #[test]
    fn since_4() {
        let formula = Pmtl::Since(
            Arc::new((
                Pmtl::Atom(Atom::Predicate(0)),
                Pmtl::Atom(Atom::Predicate(1)),
            )),
            1,
            2,
        );
        let mut state = StateValuationVector::new(formula);
        assert!(!state.update(&0, &[false, true], 0));
        assert!(!state.update(&0, &[false, true], 1));
        assert!(!state.update(&0, &[false, true], 2));
        assert!(!state.update(&0, &[true, true], 2));
        assert!(state.update(&0, &[true, false], 3));
        assert!(state.update(&0, &[true, false], 4));
        assert!(!state.update(&0, &[true, false], 5));
    }

    #[test]
    fn historically() {
        let formula = Pmtl::Historically(Arc::new(Pmtl::Atom(Atom::Predicate(0))), 1, 2);
        let mut state = StateValuationVector::new(formula);
        assert!(state.update(&0, &[false], 0));
        assert!(state.update(&0, &[false], 0));
        assert!(!state.update(&0, &[true], 1));
        assert!(!state.update(&0, &[true], 2));
        assert!(state.update(&0, &[true], 3));
        assert!(state.update(&0, &[false], 3));
        assert!(!state.update(&0, &[true], 4));
    }

    #[test]
    fn previously() {
        let formula = Pmtl::Previously(Arc::new(Pmtl::Atom(Atom::Predicate(0))), 1, 2);
        let mut state = StateValuationVector::new(formula);
        assert!(!state.update(&0, &[false], 0));
        assert!(!state.update(&0, &[false], 0));
        assert!(state.update(&0, &[true], 1));
        assert!(state.update(&0, &[false], 2));
        assert!(state.update(&0, &[false], 3));
        assert!(state.update(&0, &[false], 3));
        assert!(state.update(&0, &[true], 4));
    }
}
