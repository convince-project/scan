use bumpalo::Bump;

use crate::program_graph::{Action, Transition};

pub(super) struct TransitionsIterator<'a, I: Iterator<Item = &'a (Action, Vec<Transition>)>> {
    iters: &'a mut [I],
    bump: &'a Bump,
}

impl<'a, I: Iterator<Item = &'a (Action, Vec<Transition>)>> TransitionsIterator<'a, I> {
    pub(super) fn new(iters: &'a mut [I], bump: &'a Bump) -> Self {
        Self { iters, bump }
    }
}

impl<'a, I: Iterator<Item = &'a (Action, Vec<Transition>)>> Iterator
    for TransitionsIterator<'a, I>
{
    type Item = (Action, &'a [&'a [Transition]]);

    fn next(&mut self) -> Option<Self::Item> {
        let &(mut first_a, ref first_t) = self.iters[0].next()?;
        let len = self.iters.len();
        let mut vals = bumpalo::vec![in self.bump; first_t.as_slice(); len];

        let mut total = 1;
        let mut i = 0;
        while total < len {
            i = (i + 1) % len;
            let &(next_a, ref next_t) = self.iters[i].find(|(a, _)| *a >= first_a)?;
            vals[i] = next_t.as_slice();
            if next_a > first_a {
                first_a = next_a;
                total = 1;
            } else {
                total += 1;
            }
        }
        Some((first_a, vals.into_bump_slice()))
    }
}
