use std::ops::Range;

pub type Time = usize;

#[derive(Debug, Clone)]
pub enum Mtl<V>
where
    V: Clone + PartialEq + Eq,
{
    Atom(V),
    And(Vec<Mtl<V>>),
    Or(Vec<Mtl<V>>),
    Not(Box<Mtl<V>>),
    Implies(Box<(Mtl<V>, Mtl<V>)>),
    Next(Box<Mtl<V>>),
    Until(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    WeakUntil(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    Release(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    WeakRelease(Box<(Mtl<V>, Mtl<V>)>, Option<Range<Time>>),
    Eventually(Box<Mtl<V>>, Option<Range<Time>>),
    Always(Box<Mtl<V>>, Option<Range<Time>>),
}
