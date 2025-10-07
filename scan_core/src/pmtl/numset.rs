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
    pub(super) fn contains_interval(&self, lower_bound: Time, upper_bound: Time) -> bool {
        assert!(lower_bound <= upper_bound);
        match self.0.binary_search_by_key(&lower_bound, |&(t, _)| t) {
            Ok(i) => self.0[i].1 >= upper_bound,
            Err(i) if i > 0 => self.0[i - 1].1 >= upper_bound,
            _ => false,
        }
    }

    pub(super) fn add_interval(&self, lower_bound: Time, upper_bound: Time) -> Self {
        assert!(lower_bound <= upper_bound);
        let mut new = Vec::with_capacity(self.0.len() + 1);

        match self
            .0
            .binary_search_by_key(&lower_bound.saturating_sub(1), |&(_, t)| t)
        {
            Ok(i) => {
                // self.0[i].1 = lower_bound - 1
                Vec::extend_from_slice(&mut new, &self.0[..i]);
                match self
                    .0
                    .binary_search_by_key(&upper_bound.saturating_add(1), |&(t, _)| t)
                {
                    Ok(j) => {
                        // self.0[j].0 = upper_bound + 1
                        new.push((self.0[i].0, self.0[j].1));
                        Vec::extend_from_slice(&mut new, &self.0[j + 1..]);
                    }
                    Err(j) => {
                        // self.0[j - 1].0 < upper_bound + 1 < self.0[j].0
                        if j == 0 {
                            new.push((self.0[i].0, upper_bound));
                        } else {
                            new.push((self.0[i].0, upper_bound.max(self.0[j - 1].1)));
                        }
                        Vec::extend_from_slice(&mut new, &self.0[j..]);
                    }
                }
            }
            Err(i) => {
                // self.0[i - 1].1 < lower_bound - 1 < self.0[i].1
                Vec::extend_from_slice(&mut new, &self.0[..i]);

                match self
                    .0
                    .binary_search_by_key(&upper_bound.saturating_add(1), |&(t, _)| t)
                {
                    Ok(j) => {
                        // self.0[j].0 = upper_bound + 1
                        if i == self.0.len() {
                            new.push((lower_bound, self.0[j].1));
                        } else {
                            new.push((lower_bound.min(self.0[i].0), self.0[j].1));
                        }
                        Vec::extend_from_slice(&mut new, &self.0[j + 1..]);
                    }
                    Err(j) => {
                        // self.0[j - 1].0 < upper_bound + 1 < self.0[j].0
                        if i == self.0.len() {
                            if j == 0 {
                                new.push((lower_bound, upper_bound));
                            } else {
                                new.push((lower_bound, upper_bound.max(self.0[j - 1].1)));
                            }
                        } else if j == 0 {
                            new.push((lower_bound.min(self.0[i].0), upper_bound));
                        } else {
                            new.push((
                                lower_bound.min(self.0[i].0),
                                upper_bound.max(self.0[j - 1].1),
                            ));
                        }
                        Vec::extend_from_slice(&mut new, &self.0[j..]);
                    }
                }
            }
        }
        Self(new)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_interval_1() {
        let empty = NumSet::empty();

        let set = empty.add_interval(0, 1);
        assert_eq!(set, NumSet(vec![(0, 1)]));

        let set = empty.add_interval(1, 1);
        assert_eq!(set, NumSet(vec![(1, 1)]));

        let set = empty.add_interval(1, Time::MAX);
        assert_eq!(set, NumSet(vec![(1, Time::MAX)]));
    }

    #[test]
    fn add_interval_2() {
        let full = NumSet::from_range(0, Time::MAX);

        let set = full.add_interval(0, 1);
        assert_eq!(set, full);

        let set = full.add_interval(1, 1);
        assert_eq!(set, full);

        let set = full.add_interval(1, Time::MAX);
        assert_eq!(set, full);
    }

    #[test]
    fn add_interval_3() {
        let interval = NumSet::from_range(5, 10);

        let set = interval.add_interval(0, 1);
        assert_eq!(set, NumSet(vec![(0, 1), (5, 10)]));

        let set = interval.add_interval(12, 13);
        assert_eq!(set, NumSet(vec![(5, 10), (12, 13)]));

        let set = interval.add_interval(7, 8);
        assert_eq!(set, interval);

        let set = interval.add_interval(3, 12);
        assert_eq!(set, NumSet(vec![(3, 12)]));
    }

    #[test]
    fn add_interval_4() {
        let interval = NumSet::from_range(5, 10);

        let set = interval.add_interval(0, 5);
        assert_eq!(set, NumSet(vec![(0, 10)]));

        let set = interval.add_interval(0, 4);
        assert_eq!(set, NumSet(vec![(0, 10)]));

        let set = interval.add_interval(10, 15);
        assert_eq!(set, NumSet(vec![(5, 15)]));

        let set = interval.add_interval(11, 15);
        assert_eq!(set, NumSet(vec![(5, 15)]));
    }
}
