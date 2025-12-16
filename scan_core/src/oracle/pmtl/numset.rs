use crate::Time;

// Represent union of closed intervals,
// each interval being represented by lower and upper bounds.
// So, `(lower_bound, upper_bound)` represents the closed interval `lower_bound..=upper_bound`.
// We assume that the intervals are always disjoint and non-consecutive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct NumSet(Vec<(Time, Time)>);

impl NumSet {
    #[inline(always)]
    pub(super) fn empty() -> Self {
        Self(Vec::new())
    }

    #[inline(always)]
    fn _full() -> Self {
        Self(vec![(0, Time::MAX)])
    }

    #[inline(always)]
    pub(super) fn from_range(lower_bound: Time, upper_bound: Time) -> Self {
        assert!(lower_bound <= upper_bound);
        Self(vec![(lower_bound, upper_bound)])
    }

    #[inline(always)]
    pub(super) fn contains(&self, t: Time) -> bool {
        match self.0.binary_search_by_key(&t, |&(t, _)| t) {
            Ok(_) => true,
            Err(i) if i > 0 => self.0[i - 1].1 >= t,
            _ => false,
        }
    }

    #[inline(always)]
    fn _contains_interval(&self, lower_bound: Time, upper_bound: Time) -> bool {
        assert!(lower_bound <= upper_bound);
        match self.0.binary_search_by_key(&lower_bound, |&(t, _)| t) {
            Ok(i) => self.0[i].1 >= upper_bound,
            Err(i) if i > 0 => self.0[i - 1].1 >= upper_bound,
            _ => false,
        }
    }

    #[inline(always)]
    pub(super) fn contains_unbounded_interval(&self, lower_bound: Time) -> bool {
        self.0
            .last()
            .is_some_and(|&(l, u)| l <= lower_bound && u == Time::MAX)
    }

    pub(super) fn add_interval(&mut self, lower_bound: Time, upper_bound: Time) {
        assert!(lower_bound <= upper_bound);
        let len = self.0.len();

        match self
            .0
            .binary_search_by_key(&lower_bound.saturating_sub(1), |&(_, t)| t)
        {
            Ok(i) => {
                // self.0[i].1 = lower_bound - 1
                let tail = self.0.split_off(i);
                match tail.binary_search_by_key(&upper_bound.saturating_add(1), |&(t, _)| t) {
                    Ok(j) => {
                        // self.0[j].0 = upper_bound + 1
                        self.0.push((tail[0].0, tail[j].1));
                        self.0.extend_from_slice(&tail[j + 1..]);
                    }
                    Err(j) => {
                        // self.0[j - 1].0 < upper_bound + 1 < self.0[j].0
                        if j == 0 {
                            self.0.push((tail[0].0, upper_bound));
                        } else {
                            self.0.push((tail[0].0, upper_bound.max(tail[j - 1].1)));
                        }
                        self.0.extend_from_slice(&tail[j..]);
                    }
                }
            }
            Err(i) => {
                // self.0[i - 1].1 < lower_bound - 1 < self.0[i].1
                let tail = self.0.split_off(i);

                match tail.binary_search_by_key(&upper_bound.saturating_add(1), |&(t, _)| t) {
                    Ok(j) => {
                        // self.0[j].0 = upper_bound + 1
                        if i == len {
                            self.0.push((lower_bound, tail[j].1));
                        } else {
                            self.0.push((lower_bound.min(tail[i].0), tail[j].1));
                        }
                        self.0.extend_from_slice(&tail[j + 1..]);
                    }
                    Err(j) => {
                        // self.0[j - 1].0 < upper_bound + 1 < self.0[j].0
                        if i == len {
                            if j == 0 {
                                self.0.push((lower_bound, upper_bound));
                            } else {
                                self.0.push((lower_bound, upper_bound.max(tail[j - 1].1)));
                            }
                        } else if j == 0 {
                            self.0.push((lower_bound.min(tail[0].0), upper_bound));
                        } else {
                            self.0
                                .push((lower_bound.min(tail[0].0), upper_bound.max(tail[j - 1].1)));
                        }
                        self.0.extend_from_slice(&tail[j..]);
                    }
                }
            }
        }
        // Self(new)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_interval_1() {
        let mut set = NumSet::empty();
        set.add_interval(0, 1);
        assert_eq!(set, NumSet(vec![(0, 1)]));

        let mut set = NumSet::empty();
        set.add_interval(1, 1);
        assert_eq!(set, NumSet(vec![(1, 1)]));

        let mut set = NumSet::empty();
        set.add_interval(1, Time::MAX);
        assert_eq!(set, NumSet(vec![(1, Time::MAX)]));
    }

    #[test]
    fn add_interval_2() {
        let mut set = NumSet::_full();
        set.add_interval(0, 1);
        assert_eq!(set, NumSet::_full());

        set.add_interval(1, 1);
        assert_eq!(set, NumSet::_full());

        set.add_interval(1, Time::MAX);
        assert_eq!(set, NumSet::_full());
    }

    #[test]
    fn add_interval_3() {
        let mut set = NumSet::from_range(5, 10);
        set.add_interval(0, 1);
        assert_eq!(set, NumSet(vec![(0, 1), (5, 10)]));

        let mut set = NumSet::from_range(5, 10);
        set.add_interval(12, 13);
        assert_eq!(set, NumSet(vec![(5, 10), (12, 13)]));

        let mut set = NumSet::from_range(5, 10);
        set.add_interval(7, 8);
        assert_eq!(set, NumSet(vec![(5, 10)]));

        let mut set = NumSet::from_range(5, 10);
        set.add_interval(3, 12);
        assert_eq!(set, NumSet(vec![(3, 12)]));
    }

    #[test]
    fn add_interval_4() {
        let mut set = NumSet::from_range(5, 10);
        set.add_interval(0, 5);
        assert_eq!(set, NumSet(vec![(0, 10)]));

        let mut set = NumSet::from_range(5, 10);
        set.add_interval(0, 4);
        assert_eq!(set, NumSet(vec![(0, 10)]));

        let mut set = NumSet::from_range(5, 10);
        set.add_interval(10, 15);
        assert_eq!(set, NumSet(vec![(5, 15)]));

        let mut set = NumSet::from_range(5, 10);
        set.add_interval(11, 15);
        assert_eq!(set, NumSet(vec![(5, 15)]));
    }
}
