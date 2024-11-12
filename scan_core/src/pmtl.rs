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
    Not(Box<Pmtl<V>>),
    Implies(Box<(Pmtl<V>, Pmtl<V>)>),
    Historically(Box<Pmtl<V>>, Time, Time),
    Previously(Box<Pmtl<V>>, Time, Time),
    Since(Box<(Pmtl<V>, Pmtl<V>)>, Time, Time),
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
                    .flat_map(|f| f.to_owned().set_subformulae().into_iter()),
            ),
            Pmtl::Not(subformula)
            | Pmtl::Historically(subformula, _, _)
            | Pmtl::Previously(subformula, _, _) => subformula.to_owned().set_subformulae(),
            Pmtl::Implies(subs) | Pmtl::Since(subs, _, _) => {
                let mut formulae = subs.0.to_owned().set_subformulae();
                formulae.extend(subs.1.to_owned().set_subformulae());
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
    valuations: HashMap<Pmtl<Atom<V>>, NumSet>,
    output: HashMap<Pmtl<Atom<V>>, NumSet>,
}

impl<V: std::fmt::Debug + Clone + Eq + Hash> Oracle<V> for StateValuationVector<V> {
    fn update(&mut self, action: &V, state: &[bool], time: Time) -> bool {
        *self = self.valuation_update(action, state, time);
        self.output(self.subformulae.last().expect("formula"))
    }
}

impl<V: std::fmt::Debug + Clone + Eq + Hash> StateValuationVector<V> {
    pub fn new(formula: Pmtl<Atom<V>>) -> Self {
        let subformulae: Vec<_> = formula.to_owned().subformulae();
        let subformulae = Arc::new(subformulae);
        assert_eq!(subformulae.last().unwrap(), &formula);

        Self {
            time: (0, 1),
            valuations: HashMap::from_iter(subformulae.iter().cloned().map(|f| (f, NumSet::new()))),
            output: HashMap::from_iter(subformulae.iter().cloned().map(|f| (f, NumSet::new()))),
            subformulae,
        }
    }

    fn output(&self, formula: &Pmtl<Atom<V>>) -> bool {
        self.output
            .get(formula)
            .map(|val| val.contains(self.time))
            .unwrap()
        // .unwrap_or(false)
    }

    fn valuation_update(&self, event: &V, state: &[bool], time: Time) -> Self {
        assert!(self.time.0 <= time);
        let mut new_valuation = Self {
            time: (time, self.time.1 + 1),
            subformulae: self.subformulae.clone(),
            valuations: HashMap::new(),
            output: HashMap::new(),
        };
        for formula in self.subformulae.iter() {
            match formula {
                Pmtl::True => {
                    new_valuation
                        .valuations
                        .insert(formula.to_owned(), NumSet::full());
                    new_valuation.output.insert(
                        formula.to_owned(),
                        NumSet::from_range(self.time, new_valuation.time),
                    );
                }
                Pmtl::False => {
                    new_valuation
                        .valuations
                        .insert(formula.to_owned(), NumSet::new());
                    new_valuation
                        .output
                        .insert(formula.to_owned(), NumSet::new());
                }
                Pmtl::Atom(atom) => match atom {
                    Atom::Predicate(p) if state[*p] => {
                        new_valuation.valuations.insert(
                            formula.to_owned(),
                            NumSet::from_range(self.time, new_valuation.time),
                        );
                        new_valuation.output.insert(
                            formula.to_owned(),
                            NumSet::from_range(self.time, new_valuation.time),
                        );
                    }
                    Atom::Event(e) if event == e => {
                        new_valuation.valuations.insert(
                            formula.to_owned(),
                            NumSet::from_range(
                                (new_valuation.time.0, new_valuation.time.1 - 1),
                                new_valuation.time,
                            ),
                        );
                        new_valuation.output.insert(
                            formula.to_owned(),
                            NumSet::from_range(
                                (new_valuation.time.0, new_valuation.time.1 - 1),
                                new_valuation.time,
                            ),
                        );
                    }
                    _ => {
                        new_valuation
                            .valuations
                            .insert(formula.to_owned(), NumSet::new());
                        new_valuation
                            .output
                            .insert(formula.to_owned(), NumSet::new());
                    }
                },
                Pmtl::And(subs) => {
                    let nset = NumSet::intersection(
                        subs.iter()
                            .map(|f| new_valuation.valuations.get(f).expect("nset"))
                            .cloned(),
                    )
                    .simplify();
                    new_valuation
                        .valuations
                        .insert(formula.to_owned(), nset.to_owned());
                    new_valuation.output.insert(formula.to_owned(), nset);
                }
                Pmtl::Or(subs) => {
                    let mut nset = NumSet::new();
                    for sub in subs {
                        nset.union(new_valuation.valuations.get(sub).expect("nset"));
                    }
                    nset.simplify();
                    new_valuation
                        .valuations
                        .insert(formula.to_owned(), nset.to_owned());
                    new_valuation.output.insert(formula.to_owned(), nset);
                }
                Pmtl::Not(f) => {
                    let mut nset = new_valuation
                        .valuations
                        .get(f.as_ref())
                        .expect("nset")
                        .to_owned();
                    nset.complement();
                    let nset = NumSet::intersection([
                        nset,
                        NumSet::from_range(self.time, new_valuation.time),
                    ]);
                    new_valuation
                        .valuations
                        .insert(formula.to_owned(), nset.to_owned());
                    new_valuation.output.insert(formula.to_owned(), nset);
                }
                Pmtl::Implies(subs) => {
                    let out_lhs = new_valuation.output.get(&subs.0).expect("output lhs");
                    let out_rhs = new_valuation.output.get(&subs.1).expect("output rhs");
                    let mut nset = out_lhs.to_owned();
                    nset.complement();
                    nset.union(out_rhs);
                    new_valuation
                        .valuations
                        .insert(formula.to_owned(), nset.to_owned());
                    new_valuation.output.insert(
                        formula.to_owned(),
                        NumSet::intersection([
                            nset,
                            NumSet::from_range(self.time, new_valuation.time),
                        ]),
                    );
                }
                Pmtl::Historically(sub, lower_bound, upper_bound) => {
                    let mut output_sub = new_valuation
                        .output
                        .get(sub.as_ref())
                        .expect("nset")
                        .to_owned();
                    output_sub.insert_bound(new_valuation.time);
                    let mut valuation = self.valuations.get(formula).expect("formula").clone();
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
                        nset_output.union(&NumSet::intersection([
                            valuation.to_owned(),
                            NumSet::from_range(partial_lower_bound, *partial_upper_bound),
                        ]));
                        partial_lower_bound = *partial_upper_bound;
                    }
                    new_valuation
                        .valuations
                        .insert(formula.to_owned(), valuation.simplify());
                    nset_output.complement();
                    new_valuation.output.insert(
                        formula.to_owned(),
                        NumSet::intersection([
                            nset_output,
                            NumSet::from_range(self.time, new_valuation.time),
                        ])
                        .simplify(),
                    );
                }
                Pmtl::Previously(sub, lower_bound, upper_bound) => {
                    let mut sub_output = new_valuation
                        .output
                        .get(sub.as_ref())
                        .expect("nset")
                        .to_owned();
                    sub_output.insert_bound(new_valuation.time);
                    let mut valuation = self.valuations.get(formula).expect("formula").clone();
                    let mut partial_lower_bound = self.time;
                    let mut output = NumSet::new();
                    for (partial_upper_bound, out_sub) in
                        sub_output.bounds().iter().filter(|(ub, _)| self.time < *ub)
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
                        output.union(&NumSet::intersection([
                            valuation.to_owned(),
                            NumSet::from_range(partial_lower_bound, *partial_upper_bound),
                        ]));
                        partial_lower_bound = *partial_upper_bound;
                    }
                    new_valuation
                        .valuations
                        .insert(formula.to_owned(), valuation.simplify());
                    new_valuation
                        .output
                        .insert(formula.to_owned(), output.simplify());
                }
                Pmtl::Since(subs, lower_bound, upper_bound) => {
                    let mut nset_lhs = new_valuation.output.get(&subs.0).expect("nset").clone();
                    let nset_rhs_orig = new_valuation.output.get(&subs.1).expect("nset");
                    let mut nset_rhs = nset_rhs_orig.clone();
                    nset_rhs.insert_bound(new_valuation.time);
                    nset_lhs.insert_bound(new_valuation.time);
                    nset_rhs.sync(&nset_lhs);
                    nset_lhs.sync(nset_rhs_orig);
                    let mut partial_valuation =
                        self.valuations.get(formula).expect("formula").clone();
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
                        partial_valuation = match (out_lhs, out_rhs) {
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
                                partial_valuation.add_interval(lower_bound, upper_bound);
                                partial_valuation
                            }
                            (true, false) => partial_valuation,
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
                        nset_output.union(&NumSet::intersection([
                            partial_valuation.to_owned(),
                            NumSet::from_range(partial_lower_bound, *partial_upper_bound),
                        ]));
                        partial_lower_bound = *partial_upper_bound;
                    }
                    new_valuation
                        .valuations
                        .insert(formula.to_owned(), partial_valuation.simplify());
                    new_valuation
                        .output
                        .insert(formula.to_owned(), nset_output.simplify());
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
            Pmtl::Since(Box::new((Pmtl::True, Pmtl::True)), 0, Time::MAX).subformulae();
        assert_eq!(
            subformulae,
            vec![
                Pmtl::True,
                Pmtl::Since(Box::new((Pmtl::True, Pmtl::True)), 0, Time::MAX)
            ]
        );
    }

    #[test]
    fn subformulae_2() {
        let subformulae: Vec<Pmtl<usize>> =
            Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(0))), 0, Time::MAX).subformulae();
        assert_eq!(
            subformulae,
            vec![
                Pmtl::Atom(0),
                Pmtl::Since(Box::new((Pmtl::Atom(0), Pmtl::Atom(0))), 0, Time::MAX)
            ]
        );
    }

    #[test]
    fn subformulae_3() {
        let subformulae: Vec<Pmtl<usize>> = Pmtl::And(vec![
            Pmtl::Atom(0),
            Pmtl::Not(Box::new(Pmtl::Atom(0))),
            Pmtl::True,
        ])
        .subformulae();
        assert_eq!(subformulae.len(), 4);
        assert!(matches!(subformulae[0], Pmtl::Atom(0) | Pmtl::True));
        assert!(matches!(subformulae[1], Pmtl::Atom(0) | Pmtl::True));
        assert_eq!(subformulae[2], Pmtl::Not(Box::new(Pmtl::Atom(0))));
    }

    #[test]
    fn since_1() {
        let formula = Pmtl::Since(
            Box::new((
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
            Box::new((
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
            Box::new((
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
            Box::new((
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
        let formula = Pmtl::Historically(Box::new(Pmtl::Atom(Atom::Predicate(0))), 1, 2);
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
        let formula = Pmtl::Previously(Box::new(Pmtl::Atom(Atom::Predicate(0))), 1, 2);
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
