use super::DenseTime;
use crate::Time;
use smallvec::{SmallVec, smallvec};

#[derive(Debug, Clone)]
pub(super) struct NumSet(SmallVec<[(DenseTime, bool); 16]>);

impl NumSet {
    #[inline(always)]
    pub(super) fn new() -> Self {
        Self(SmallVec::new())
    }

    #[inline(always)]
    pub(super) fn full() -> Self {
        Self(smallvec![((Time::MAX, Time::MAX), true)])
    }

    pub(super) fn cut(&mut self, lower_bound: DenseTime, upper_bound: DenseTime) {
        if lower_bound != (0, 0) {
            match self.0.binary_search_by_key(&lower_bound, |(t, _)| *t) {
                Ok(idx) => {
                    self.0[idx].1 = false;
                    let _ = self.0.drain(..idx);
                }
                Err(idx) => {
                    self.0.insert(idx, (lower_bound, false));
                    let _ = self.0.drain(..idx);
                }
            }
        }
        if upper_bound != (Time::MAX, Time::MAX) {
            match self.0.binary_search_by_key(&upper_bound, |(t, _)| *t) {
                Ok(idx) => {
                    self.0.truncate(idx + 1);
                }
                Err(idx) => {
                    let b = self.0.get(idx).map(|(_, b)| *b).unwrap_or(false);
                    self.0.insert(idx, (upper_bound, b));
                    self.0.truncate(idx + 1);
                }
            }
        }
    }

    pub(super) fn from_range(lower_bound: DenseTime, upper_bound: DenseTime) -> Self {
        if lower_bound < upper_bound {
            if lower_bound == (0, 0) {
                Self(smallvec![(upper_bound, true)])
            } else {
                Self(smallvec![(lower_bound, false), (upper_bound, true)])
            }
        } else {
            Self::new()
        }
    }

    #[inline(always)]
    pub(super) fn bounds(&self) -> &[(DenseTime, bool)] {
        &self.0
    }

    pub(super) fn contains(&self, val: DenseTime) -> bool {
        // special case: (0, 0) cannot belong to any (left-open) interval
        val != (0, 0)
            && match self.0.binary_search_by_key(&val, |(bound, _)| *bound) {
                Ok(idx) | Err(idx) => self.0.get(idx).map(|(_, b)| *b).unwrap_or(false),
            }
    }

    pub(super) fn contains_since(&self, val: DenseTime) -> bool {
        // special case: (0, 0) cannot belong to any (left-open) interval
        match self.0.binary_search_by_key(&val, |(bound, _)| *bound) {
            Ok(idx) | Err(idx) => {
                self.0[idx..].iter().all(|(_, b)| *b)
                    && self
                        .0
                        .last()
                        .is_some_and(|(v, _)| *v == (Time::MAX, Time::MAX))
            }
        }
    }

    #[inline(always)]
    fn hinted_insert_bound(&mut self, bound: DenseTime, hint: usize) -> usize {
        match self.0[hint..].binary_search_by_key(&bound, |(bound, _)| *bound) {
            Ok(idx) => idx + hint,
            Err(idx) => {
                let b = self.0.get(idx + hint).map(|(_, b)| *b).unwrap_or(false);
                self.0.insert(idx + hint, (bound, b));
                idx + hint
            }
        }
    }

    #[inline(always)]
    pub(super) fn insert_bound(&mut self, bound: DenseTime) -> usize {
        match self.0.binary_search_by_key(&bound, |(bound, _)| *bound) {
            Ok(idx) => idx,
            Err(idx) => {
                let b = self.0.get(idx).map(|(_, b)| *b).unwrap_or(false);
                self.0.insert(idx, (bound, b));
                idx
            }
        }
    }

    pub(super) fn add_interval(&mut self, lower_bound: DenseTime, upper_bound: DenseTime) {
        if lower_bound >= upper_bound {
            // Nothing to do
        } else if self.0.is_empty() {
            *self = Self::from_range(lower_bound, upper_bound);
        } else if lower_bound == (0, 0) {
            let u_idx = self.insert_bound(upper_bound);
            self.0[..=u_idx].iter_mut().for_each(|(_, b)| *b = true);
        } else {
            let l_idx = self.insert_bound(lower_bound);
            let u_idx = self.hinted_insert_bound(upper_bound, l_idx + 1);
            assert!(l_idx < u_idx);
            self.0[l_idx + 1..=u_idx]
                .iter_mut()
                .for_each(|(_, b)| *b = true);
        }
    }

    pub(super) fn complement(&mut self) {
        if self
            .0
            .last()
            .is_some_and(|(bound, b)| *bound == (Time::MAX, Time::MAX) && *b)
        {
            let _ = self.0.pop();
            self.0.iter_mut().for_each(|(_, b)| *b = !*b);
        } else {
            self.0.iter_mut().for_each(|(_, b)| *b = !*b);
            self.0.push(((Time::MAX, Time::MAX), true));
        }
    }

    pub(super) fn union(&mut self, other: &Self) {
        let mut lower_bound = (0, 0);
        other.0.iter().for_each(|(upper_bound, b)| {
            if *b {
                self.add_interval(lower_bound, *upper_bound);
            }
            lower_bound = *upper_bound;
        });
    }

    pub(super) fn intersection<I: IntoIterator<Item = Self>>(sets: I) -> Self {
        let mut intersection = Self::new();
        for mut set in sets {
            set.complement();
            intersection.union(&set);
        }
        intersection.complement();
        intersection
    }

    pub(super) fn sync(&mut self, other: &Self) {
        let mut hint = 0;
        other.0.iter().for_each(|(bound, _)| {
            hint = self.hinted_insert_bound(*bound, hint) + 1;
        });
    }

    pub(super) fn simplify(&self) -> Self {
        let mut prev_b = false;
        let mut prev_t = (0, 0);
        let mut vec = SmallVec::from_iter(
            self.0
                .iter()
                .filter(|(t, _)| {
                    if prev_t == *t {
                        false
                    } else {
                        prev_t = *t;
                        true
                    }
                })
                .rev()
                .filter(|(_, b)| {
                    if prev_b == *b {
                        false
                    } else {
                        prev_b = *b;
                        true
                    }
                })
                .copied(),
        );
        vec.reverse();
        Self(vec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_range() {
        let set = NumSet::from_range((1, 1), (0, 0));
        assert!(set.bounds().is_empty());

        let set = NumSet::from_range((0, 0), (1, 1));
        assert_eq!(set.bounds(), &[((1, 1), true)]);

        let set = NumSet::from_range((1, 1), (2, 2));
        assert_eq!(set.bounds(), &[((1, 1), false), ((2, 2), true)]);
    }

    #[test]
    fn contains() {
        let set = NumSet::from_range((0, 0), (1, 1));
        assert!(!set.contains((0, 0)));
        assert!(set.contains((0, 1)));
        assert!(set.contains((1, 0)));
        assert!(set.contains((1, 1)));
        assert!(!set.contains((1, 2)));

        let set = NumSet::from_range((1, 1), (2, 2));
        assert!(!set.contains((1, 1)));
        assert!(set.contains((1, 2)));
        assert!(set.contains((2, 1)));
        assert!(set.contains((2, 2)));
        assert!(!set.contains((2, 3)));
    }

    #[test]
    fn insert_bound() {
        let mut set = NumSet::from_range((0, 0), (2, 2));
        set.insert_bound((1, 1));
        assert!(!set.contains((0, 0)));
        assert!(set.contains((0, 1)));
        assert!(set.contains((1, 0)));
        assert!(set.contains((1, 1)));
        assert!(set.contains((1, 2)));
        assert!(set.contains((2, 1)));
        assert!(set.contains((2, 2)));
        assert!(!set.contains((2, 3)));
    }

    #[test]
    fn insert_interval() {
        let mut set = NumSet::from_range((2, 2), (5, 5));
        set.add_interval((0, 0), (1, 1));
        assert_eq!(
            set.bounds(),
            &[((1, 1), true), ((2, 2), false), ((5, 5), true)]
        );

        let mut set = NumSet::from_range((2, 2), (5, 5));
        set.add_interval((1, 1), (3, 3));
        assert_eq!(
            set.bounds(),
            &[
                ((1, 1), false),
                ((2, 2), true),
                ((3, 3), true),
                ((5, 5), true)
            ]
        );

        let mut set = NumSet::from_range((2, 2), (5, 5));
        set.add_interval((3, 3), (4, 4));
        assert_eq!(
            set.bounds(),
            &[
                ((2, 2), false),
                ((3, 3), true),
                ((4, 4), true),
                ((5, 5), true)
            ]
        );

        let mut set = NumSet::from_range((2, 2), (5, 5));
        set.add_interval((3, 3), (5, 5));
        assert_eq!(
            set.bounds(),
            &[((2, 2), false), ((3, 3), true), ((5, 5), true)]
        );

        let mut set = NumSet::from_range((2, 2), (5, 5));
        set.add_interval((3, 3), (6, 6));
        assert_eq!(
            set.bounds(),
            &[
                ((2, 2), false),
                ((3, 3), true),
                ((5, 5), true),
                ((6, 6), true)
            ]
        );
    }

    #[test]
    fn complement() {
        let mut set = NumSet::from_range((2, 2), (3, 3));
        set.complement();
        assert_eq!(
            set.bounds(),
            &[
                ((2, 2), true),
                ((3, 3), false),
                ((Time::MAX, Time::MAX), true)
            ]
        );
        set.complement();
        assert_eq!(set.bounds(), &[((2, 2), false), ((3, 3), true)]);
    }

    #[test]
    fn simplify_1() {
        let mut set = NumSet::from_range((2, 2), (3, 3));
        set.add_interval((1, 1), (4, 4));
        set.add_interval((3, 3), (4, 4));
        assert_eq!(
            set.bounds(),
            &[
                ((1, 1), false),
                ((2, 2), true),
                ((3, 3), true),
                ((4, 4), true)
            ]
        );
        let sset = set.simplify();
        assert_eq!(sset.bounds(), &[((1, 1), false), ((4, 4), true)]);
    }

    #[test]
    fn simplify_2() {
        let mut set = NumSet::from_range((2, 2), (3, 3));
        set.union(&NumSet::from_range((1, 1), (2, 2)));
        assert_eq!(
            set.bounds(),
            &[((1, 1), false), ((2, 2), true), ((3, 3), true),]
        );
        let sset = set.simplify();
        assert_eq!(sset.bounds(), &[((1, 1), false), ((3, 3), true)]);
    }

    #[test]
    fn simplify_3() {
        let set = NumSet::full();
        let sset = set.simplify();
        assert_eq!(sset.bounds(), &[((Time::MAX, Time::MAX), true)]);
    }

    #[test]
    fn sync() {
        let mut set = NumSet::from_range((1, 1), (3, 3));
        let other_set = NumSet::from_range((2, 2), (4, 4));
        set.sync(&other_set);
        assert_eq!(
            set.bounds(),
            &[
                ((1, 1), false),
                ((2, 2), true),
                ((3, 3), true),
                ((4, 4), false),
            ]
        );
        let sset = set.simplify();
        assert_eq!(sset.bounds(), &[((1, 1), false), ((3, 3), true)]);
    }
}
