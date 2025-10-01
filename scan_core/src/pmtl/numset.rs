use crate::Time;

// Represent union of closed intervals,
// each interval being represented by lower and upper bounds.
// So, `(lower_bound, upper_bound)` represents the closed interval `lower_bound..=upper_bound`.
// We assume that the intervals are always disjoint and non-consecutive.
#[derive(Debug, Clone)]
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
        match self.0.binary_search_by_key(&lower_bound, |&(t, _)| t) {
            Ok(i) => {
                // self.0[i].0 = lower_bound
                Vec::extend_from_slice(&mut new, &self.0[..i]);
                match self.0.binary_search_by_key(&upper_bound, |&(_, t)| t) {
                    Ok(j) => {
                        // self.0[j].1 = upper_bound
                        new.push((lower_bound, upper_bound));
                        Vec::extend_from_slice(&mut new, &self.0[j + 1..]);
                    }
                    Err(j) if j > 0 && j < self.0.len() => {
                        // self.0[j - 1].1 < upper_bound < self.0[j].1
                        if upper_bound + 1 < self.0[j].0 {
                            // upper_bound and self.0[j].0 are neither the same nor consecutive
                            new.push((lower_bound, upper_bound));
                            Vec::extend_from_slice(&mut new, &self.0[j..]);
                        } else {
                            new.push((lower_bound, self.0[j].1));
                            Vec::extend_from_slice(&mut new, &self.0[j + 1..]);
                        }
                    }
                    Err(j) if j > 0 && j == self.0.len() => {
                        // self.0[self.0.len() - 1].1 < upper_bound
                        new.push((lower_bound, upper_bound));
                    }
                    _ => unreachable!(),
                }
            }
            Err(i @ 1..) => {
                // self.0[i - 1].0 < lower_bound < self.0[i].0
                Vec::extend_from_slice(&mut new, &self.0[..i - 1]);
                match self.0.binary_search_by_key(&upper_bound, |&(_, t)| t) {
                    Ok(j) => {
                        // self.0[j].1 = upper_bound
                        if self.0[i - 1].1 + 1 < lower_bound {
                            new.push(self.0[i - 1]);
                            new.push((lower_bound, self.0[j].1));
                        } else {
                            new.push((self.0[i - 1].0, self.0[j].1));
                        }
                        Vec::extend_from_slice(&mut new, &self.0[j + 1..]);
                    }
                    Err(j) if j > 0 && j < self.0.len() => {
                        // self.0[j - 1].1 < upper_bound < self.0[j].1
                        if self.0[i - 1].1 + 1 < lower_bound {
                            new.push(self.0[i - 1]);
                            if upper_bound + 1 < self.0[j].0 {
                                new.push((lower_bound, upper_bound));
                                Vec::extend_from_slice(&mut new, &self.0[j..]);
                            } else {
                                new.push((lower_bound, self.0[j].1));
                                Vec::extend_from_slice(&mut new, &self.0[j + 1..]);
                            }
                        } else if upper_bound + 1 < self.0[j].0 {
                            new.push((self.0[i - 1].0, upper_bound));
                            Vec::extend_from_slice(&mut new, &self.0[j..]);
                        } else {
                            new.push((self.0[i - 1].0, self.0[j].1));
                            Vec::extend_from_slice(&mut new, &self.0[j + 1..]);
                        }
                    }
                    Err(j) if j > 0 && j == self.0.len() => {
                        // self.0[self.0.len() - 1].1 < upper_bound
                        new.push((self.0[i - 1].0, upper_bound));
                    }
                    _ => unreachable!(),
                }
            }
            Err(0) => {
                // lower_bound < self.0[0].0
                match self.0.binary_search_by_key(&upper_bound, |&(_, t)| t) {
                    Ok(j) => {
                        // self.0[j].1 = upper_bound
                        new.push((lower_bound, self.0[j].1));
                        Vec::extend_from_slice(&mut new, &self.0[j + 1..]);
                    }
                    Err(j @ 1..) => {
                        // self.0[j - 1].1 < upper_bound < self.0[j].1
                        if upper_bound + 1 < self.0[j].0 {
                            new.push((lower_bound, upper_bound));
                            Vec::extend_from_slice(&mut new, &self.0[j..]);
                        } else {
                            new.push((lower_bound, self.0[j].1));
                            Vec::extend_from_slice(&mut new, &self.0[j + 1..]);
                        }
                    }
                    Err(0) => {
                        new.push((lower_bound, upper_bound));
                    }
                }
            }
        }
        Self(new)
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn from_range() {
//         let set = NumSet::from_range((1, 1), 0);
//         assert!(set.bounds().is_empty());

//         let set = NumSet::from_range((0, 0), (1, 1));
//         assert_eq!(set.bounds(), &[((1, 1), true)]);

//         let set = NumSet::from_range((1, 1), (2, 2));
//         assert_eq!(set.bounds(), &[((1, 1), false), ((2, 2), true)]);
//     }

//     #[test]
//     fn contains() {
//         let set = NumSet::from_range((0, 0), (1, 1));
//         assert!(!set.contains((0, 0)));
//         assert!(set.contains((0, 1)));
//         assert!(set.contains((1, 0)));
//         assert!(set.contains((1, 1)));
//         assert!(!set.contains((1, 2)));

//         let set = NumSet::from_range((1, 1), (2, 2));
//         assert!(!set.contains((1, 1)));
//         assert!(set.contains((1, 2)));
//         assert!(set.contains((2, 1)));
//         assert!(set.contains((2, 2)));
//         assert!(!set.contains((2, 3)));
//     }

//     #[test]
//     fn insert_bound() {
//         let mut set = NumSet::from_range((0, 0), (2, 2));
//         set.insert_bound((1, 1));
//         assert!(!set.contains((0, 0)));
//         assert!(set.contains((0, 1)));
//         assert!(set.contains((1, 0)));
//         assert!(set.contains((1, 1)));
//         assert!(set.contains((1, 2)));
//         assert!(set.contains((2, 1)));
//         assert!(set.contains((2, 2)));
//         assert!(!set.contains((2, 3)));
//     }

//     #[test]
//     fn insert_interval() {
//         let mut set = NumSet::from_range((2, 2), (5, 5));
//         set.add_interval((0, 0), (1, 1));
//         assert_eq!(
//             set.bounds(),
//             &[((1, 1), true), ((2, 2), false), ((5, 5), true)]
//         );

//         let mut set = NumSet::from_range((2, 2), (5, 5));
//         set.add_interval((1, 1), (3, 3));
//         assert_eq!(
//             set.bounds(),
//             &[
//                 ((1, 1), false),
//                 ((2, 2), true),
//                 ((3, 3), true),
//                 ((5, 5), true)
//             ]
//         );

//         let mut set = NumSet::from_range((2, 2), (5, 5));
//         set.add_interval((3, 3), (4, 4));
//         assert_eq!(
//             set.bounds(),
//             &[
//                 ((2, 2), false),
//                 ((3, 3), true),
//                 ((4, 4), true),
//                 ((5, 5), true)
//             ]
//         );

//         let mut set = NumSet::from_range((2, 2), (5, 5));
//         set.add_interval((3, 3), (5, 5));
//         assert_eq!(
//             set.bounds(),
//             &[((2, 2), false), ((3, 3), true), ((5, 5), true)]
//         );

//         let mut set = NumSet::from_range((2, 2), (5, 5));
//         set.add_interval((3, 3), (6, 6));
//         assert_eq!(
//             set.bounds(),
//             &[
//                 ((2, 2), false),
//                 ((3, 3), true),
//                 ((5, 5), true),
//                 ((6, 6), true)
//             ]
//         );
//     }

//     #[test]
//     fn complement() {
//         let mut set = NumSet::from_range((2, 2), (3, 3));
//         set.complement();
//         assert_eq!(
//             set.bounds(),
//             &[
//                 ((2, 2), true),
//                 ((3, 3), false),
//                 ((Time::MAX, Time::MAX), true)
//             ]
//         );
//         set.complement();
//         assert_eq!(set.bounds(), &[((2, 2), false), ((3, 3), true)]);
//     }

//     #[test]
//     fn simplify_1() {
//         let mut set = NumSet::from_range((2, 2), (3, 3));
//         set.add_interval((1, 1), (4, 4));
//         set.add_interval((3, 3), (4, 4));
//         assert_eq!(
//             set.bounds(),
//             &[
//                 ((1, 1), false),
//                 ((2, 2), true),
//                 ((3, 3), true),
//                 ((4, 4), true)
//             ]
//         );
//         let sset = set.simplify();
//         assert_eq!(sset.bounds(), &[((1, 1), false), ((4, 4), true)]);
//     }

//     #[test]
//     fn simplify_2() {
//         let mut set = NumSet::from_range((2, 2), (3, 3));
//         set.union(&NumSet::from_range((1, 1), (2, 2)));
//         assert_eq!(
//             set.bounds(),
//             &[((1, 1), false), ((2, 2), true), ((3, 3), true),]
//         );
//         let sset = set.simplify();
//         assert_eq!(sset.bounds(), &[((1, 1), false), ((3, 3), true)]);
//     }

//     #[test]
//     fn simplify_3() {
//         let set = NumSet::full();
//         let sset = set.simplify();
//         assert_eq!(sset.bounds(), &[((Time::MAX, Time::MAX), true)]);
//     }

//     #[test]
//     fn sync() {
//         let mut set = NumSet::from_range((1, 1), (3, 3));
//         let other_set = NumSet::from_range((2, 2), (4, 4));
//         set.sync(&other_set);
//         assert_eq!(
//             set.bounds(),
//             &[
//                 ((1, 1), false),
//                 ((2, 2), true),
//                 ((3, 3), true),
//                 ((4, 4), false),
//             ]
//         );
//         let sset = set.simplify();
//         assert_eq!(sset.bounds(), &[((1, 1), false), ((3, 3), true)]);
//     }
// }
