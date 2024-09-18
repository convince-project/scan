use std::{borrow::Borrow, ops::Range};

pub type Time = usize;

#[derive(Debug, Clone)]
pub enum Mtl<V>
where
    V: Clone + Eq,
{
    True,
    Atom(V),
    And(Vec<Mtl<V>>),
    // Or(Vec<Mtl<V>>),
    Not(Box<Mtl<V>>),
    // Implies(Box<(Mtl<V>, Mtl<V>)>),
    Next(Box<Mtl<V>>),
    Until(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    // WeakUntil(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    // Release(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    // WeakRelease(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    // Eventually(Box<Mtl<V>>, Option<Range<Time>>),
    // Always(Box<Mtl<V>>, Option<Range<Time>>),
}

impl<V> Mtl<V>
where
    V: Clone + Eq,
{
    pub fn is_boolean(&self) -> bool {
        match self {
            Mtl::True => true,
            Mtl::Atom(_) => true,
            Mtl::And(formulas) => formulas.iter().all(Self::is_boolean),
            Mtl::Not(formula) => formula.is_boolean(),
            Mtl::Next(_) => false,
            Mtl::Until(_, _) => false,
        }
    }
}

impl Mtl<usize> {
    pub fn eval(&self, trace: &[Vec<bool>]) -> bool {
        match self {
            Mtl::True => true,
            Mtl::Atom(id) => trace[0][*id],
            Mtl::And(formulae) => formulae.iter().all(|f| f.eval(trace)),
            // Mtl::Or(formulae) => formulae.iter().any(|f| f.eval(vars)),
            Mtl::Not(formula) => !formula.eval(trace),
            // Mtl::Implies(formulae) => formulae.1.eval(vars) || !formulae.0.eval(vars),
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
            } // Mtl::WeakUntil(_, _) => todo!(),
              // Mtl::Release(_, _) => todo!(),
              // Mtl::WeakRelease(_, _) => todo!(),
              // Mtl::Eventually(_, _) => todo!(),
              // Mtl::Always(_, _) => todo!(),
        }
    }
}
