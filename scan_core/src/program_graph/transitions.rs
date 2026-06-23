use bumpalo::Bump;

use crate::program_graph::{Action, ActionIdx, Transition};

pub(super) struct TransitionsIterator<'a, I: Iterator<Item = &'a (Action, Vec<Transition>)>> {
    iters: &'a mut [I],
    bump: &'a Bump,
    action: Action,
    actions: ActionIdx,
}

impl<'a, I: Iterator<Item = &'a (Action, Vec<Transition>)>> TransitionsIterator<'a, I> {
    pub(super) fn new(iters: &'a mut [I], bump: &'a Bump, actions: ActionIdx) -> Self {
        Self {
            iters,
            bump,
            action: Action(0),
            actions,
        }
    }
}

impl<'a, I: Iterator<Item = &'a (Action, Vec<Transition>)>> Iterator
    for TransitionsIterator<'a, I>
{
    type Item = (Action, &'a [&'a [Transition]]);

    fn next(&mut self) -> Option<Self::Item> {
        if self.action.0 >= self.actions {
            return None;
        }
        let len = self.iters.len();
        let mut total = 0;
        let mut i = 0;
        let mut vals = bumpalo::vec![in self.bump; [].as_slice(); len];
        while total < len {
            let &(next_a, ref next_t) = self.iters[i].find(|&&(a, _)| a.0 >= self.action.0)?;
            vals[i] = next_t.as_slice();
            i = (i + 1) % len;
            total = total * (next_a.0 == self.action.0) as usize + 1;
            self.action = next_a;
        }

        let next_action = self.action;
        self.action.0 += 1;

        Some((next_action, vals.into_bump_slice()))
    }
}
