use std::ops::{Bound, RangeBounds};

/// The type that represents time.
pub type Time = u32;

/// A time constraint given by lower bound and upper bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TimeRange {
    lower_bound: Bound<Time>,
    upper_bound: Bound<Time>,
}

impl RangeBounds<Time> for TimeRange {
    fn start_bound(&self) -> Bound<&Time> {
        self.lower_bound.as_ref()
    }

    fn end_bound(&self) -> Bound<&Time> {
        self.upper_bound.as_ref()
    }
}

impl TimeRange {
    /// Creates new [`TimeRange`] from any range.
    pub fn new<R: RangeBounds<Time>>(range: R) -> Self {
        TimeRange {
            lower_bound: range.start_bound().cloned(),
            upper_bound: range.end_bound().cloned(),
        }
    }

    /// Shift time range in the future for the given delta.
    pub fn shift(&self, delta: Time) -> Self {
        let lower_bound = match self.lower_bound {
            Bound::Included(l) => Bound::Included(l.saturating_add(delta)),
            Bound::Excluded(l) => Bound::Excluded(l.saturating_add(delta)),
            Bound::Unbounded => Bound::Unbounded,
        };
        let upper_bound = match self.upper_bound {
            Bound::Included(r) => Bound::Included(r.saturating_add(delta)),
            Bound::Excluded(r) => Bound::Excluded(r.saturating_add(delta)),
            Bound::Unbounded => Bound::Unbounded,
        };
        TimeRange {
            lower_bound,
            upper_bound,
        }
    }
}
