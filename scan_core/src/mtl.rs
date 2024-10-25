use std::{borrow::Borrow, ops::Range};

use crate::Atom;

pub type Time = usize;

#[derive(Debug, Clone)]
pub enum Mtl<V>
where
    V: Clone,
{
    True,
    Atom(V),
    And(Vec<Mtl<V>>),
    Or(Vec<Mtl<V>>),
    Not(Box<Mtl<V>>),
    Implies(Box<(Mtl<V>, Mtl<V>)>),
    Next(Box<Mtl<V>>),
    Until(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    // WeakUntil(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    // Release(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    // WeakRelease(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    Eventually(Box<Mtl<V>>, Option<Range<Time>>),
    Always(Box<Mtl<V>>, Option<Range<Time>>),
}

impl<V> Mtl<V>
where
    V: Clone,
{
    pub fn is_boolean(&self) -> bool {
        match self {
            Mtl::True | Mtl::Atom(_) => true,
            Mtl::And(formulae) | Mtl::Or(formulae) => formulae.iter().all(Self::is_boolean),
            Mtl::Implies(formulae) => formulae.0.is_boolean() && formulae.1.is_boolean(),
            Mtl::Not(formula) => formula.is_boolean(),
            Mtl::Next(_) | Mtl::Until(_, _) | Mtl::Eventually(_, _) | Mtl::Always(_, _) => false,
        }
    }
}

impl<A: Clone + PartialEq> Mtl<Atom<A>> {
    pub fn eval(&self, trace: &[(A, Vec<bool>)]) -> bool {
        match self {
            Mtl::True => true,
            Mtl::Atom(atom) => match atom {
                Atom::Predicate(p) => trace[0].1[*p],
                Atom::Event(e) => *e == trace[0].0,
            },
            Mtl::And(formulae) => formulae.iter().all(|f| f.eval(trace)),
            Mtl::Or(formulae) => formulae.iter().any(|f| f.eval(trace)),
            Mtl::Not(formula) => !formula.eval(trace),
            Mtl::Implies(formulae) => formulae.1.eval(trace) || !formulae.0.eval(trace),
            Mtl::Next(formula) => formula.eval(&trace[1..]),
            Mtl::Until(formulae, _) => {
                let (lhs, rhs) = formulae.borrow();
                for i in 0..trace.len() {
                    if rhs.eval(&trace[i..]) {
                        return true;
                    } else if !lhs.eval(&trace[i..]) {
                        return false;
                    } else {
                        continue;
                    }
                }
                false
            }
            // Reverse order for faster checking
            Mtl::Eventually(formula, _) => {
                (0..trace.len()).rev().any(|t| formula.eval(&trace[t..]))
            }
            Mtl::Always(formula, _) => (0..trace.len()).rev().all(|t| formula.eval(&trace[t..])),
            // Mtl::WeakUntil(_, _) => todo!(),
            // Mtl::Release(_, _) => todo!(),
            // Mtl::WeakRelease(_, _) => todo!(),
        }
    }
}
