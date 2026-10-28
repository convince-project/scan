use std::hash::{Hash, Hasher};

use bumpalo::Bump;
use bumpalo::collections::CollectIn;
use fixedbitset::FixedBitSet;
use rand::rngs::SmallRng;
use rand::seq::IteratorRandom;
use rand::{RngExt, SeedableRng};

use crate::channel_system::{
    Action, Channel, ChannelSystemRun, CsError, Event, EventType, Location, Message, PgId,
};
use crate::{Atom, Oracle, Time, TransitionSystem, Val};

/// Transition system model based on a [`ChannelSystem`].
///
/// It is essentially a CS which keeps track of the [`Event`]s produced by the execution
/// and determining a set of predicates.
#[derive(Debug, Clone)]
pub(crate) struct TransitionSystemRun<'def> {
    ts: &'def super::TransitionSystem,
    cs: ChannelSystemRun<'def>,
    vals: Box<[Val]>,
    vals_idx: Box<[usize]>,
}

impl<'def> PartialEq for TransitionSystemRun<'def> {
    fn eq(&self, other: &Self) -> bool {
        self.cs == other.cs && self.vals == other.vals
    }
}

impl<'def> Eq for TransitionSystemRun<'def> {}

impl<'def> Hash for TransitionSystemRun<'def> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cs.hash(state);
        self.vals.hash(state);
    }
}

impl<'def> TransitionSystemRun<'def> {
    pub(crate) fn new(ts: &'def TransitionSystem) -> Self {
        let vals_idx = [0]
            .into_iter()
            .chain(ts.vals.iter().scan(0, |counter, val| {
                *counter += val.len();
                Some(*counter)
            }))
            .collect();
        let vals = ts
            .vals
            .iter()
            .flat_map(|vals| vals.clone().into_boxed_slice())
            .collect();
        TransitionSystemRun {
            ts,
            cs: ts.cs.new_instance(),
            vals,
            vals_idx,
        }
    }

    pub(crate) fn montecarlo_transition<O: Oracle, R: RngExt>(
        &mut self,
        oracle: &mut O,
        pg_list: &mut [PgId],
        rng: &mut R,
        bump: &Bump,
    ) -> Option<Action> {
        // Setting pgs_left as length resets the queue
        let mut pgs_left = pg_list.len();
        let mut montecarlo_action = None;
        while pgs_left > 0 {
            // Select random pg within 0..pgs_left
            let pg_select = rng.random_range(0..pgs_left);
            let pg_id = pg_list[pg_select];
            // Execute randomly chosen transitions on the picked PG until an event is generated,
            // or no more transition is possible
            // NOTE: Special treatment for PGs with single-location state for optimization of this common case.
            // Hopefully it will be possible to treat all cases in a general way eventually.
            montecarlo_action = self.montecarlo_transition_pg(pg_id, oracle, rng, bump);
            if montecarlo_action.is_none() {
                // Swap selected pg with last element of the queue (possibly itself, probably not worth checking)
                // Decrease the length of the queue (so that selected element is removed)
                pgs_left -= 1;
                pg_list.swap(pg_select, pgs_left);
            } else {
                break;
            }
        }
        montecarlo_action
    }

    fn montecarlo_transition_pg<'a, O: Oracle, R: RngExt>(
        &'a mut self,
        pg_id: PgId,
        oracle: &mut O,
        rng: &mut R,
        bump: &'a Bump,
    ) -> Option<Action> {
        let mut rng_extra = SmallRng::from_rng(rng);
        use bumpalo::collections::Vec as BumpVec;

        while let Some((action, post_states)) = self
            .cs
            .possible_transitions_pg(pg_id)
            .expect("pg exists")
            .choose(rng)
            .map(|(action, post_states)| {
                (
                    action,
                    post_states
                        .map(|locs| locs.choose(&mut rng_extra).unwrap())
                        .collect_in::<BumpVec<Location>>(bump)
                        .into_bump_slice(),
                )
            })
        {
            self.transition(oracle, action, post_states, bump)
                .expect("successful transition");
            if self.cs.last_event().is_some() {
                return Some(action);
            }
        }
        None
    }

    #[inline]
    pub(crate) fn time(&self) -> Time {
        self.cs.time()
    }

    #[inline]
    pub(crate) fn time_tick(&mut self) {
        self.cs.wait(1).expect("time error")
    }

    pub(crate) fn labels<'a>(&'a self) -> impl Iterator<Item = bool> + use<'a> {
        let last_event = self.cs.last_event().map(|ev| ev.channel);
        self.ts.predicates.iter().map(move |prop| {
            prop.eval::<SmallRng>(
                &|port| match port {
                    Atom::State(channel, idx) => {
                        let port_idx = self
                            .ts
                            .ports
                            .binary_search(&channel)
                            .expect("port must exist and be initialized");
                        self.vals[self.vals_idx[port_idx] + idx]
                    }
                    Atom::Event(event_channel) => {
                        Val::Boolean(last_event.is_some_and(|channel| channel == event_channel))
                    }
                },
                None,
            )
        })
    }

    #[inline]
    pub(crate) fn state(&self, port: Channel) -> Option<&[Val]> {
        self.ts
            .ports
            .binary_search(&port)
            .ok()
            .map(|idx| &self.vals[self.vals_idx[idx]..self.vals_idx[idx + 1]])
    }

    #[inline]
    pub(crate) fn whole_state(&self) -> &[Val] {
        &self.vals
    }

    pub(crate) fn transition<O: Oracle>(
        &mut self,
        oracle: &mut O,
        action: Action,
        post: &[Location],
        bump: &Bump,
    ) -> Result<(), CsError> {
        self.cs.transition(action, post)?;
        if let Some(event) = self.cs.last_event()
            && let EventType::Send(vals) = event.event_type
            && let Ok(index) = self.ts.ports.binary_search(&event.channel)
        {
            // Since we have to update old values,
            // the vectors are already allocated and their len is always the same.
            // Copying from slice should be faster than cloning.
            self.vals[self.vals_idx[index]..self.vals_idx[index + 1]].copy_from_slice(vals);
            let labels = bumpalo::collections::Vec::from_iter_in(self.labels(), bump);
            oracle.update_state(&labels);
        }
        Ok(())
    }

    pub(crate) fn ample<'a>(&'a self, amples: &'a mut [(FixedBitSet, bool)]) {
        self.ts
            .pg_list
            .iter()
            .zip(amples.iter_mut())
            .filter(|(_pg_id, (_set, invalidate))| *invalidate)
            .for_each(|(pg_id, (set, _))| self.ample_pg(*pg_id, set));

        for i in 0..amples.len() {
            // skip j == i
            for j in 0..amples.len() {
                // only consider invalid j_set's
                if i != j
                    && let Ok([(i_set, _), (j_set, true)]) = amples.get_disjoint_mut([i, j])
                    && j_set.contains(i)
                {
                    let j_bit = j_set[j];
                    j_set.set(i, false);
                    j_set.union_with(i_set);
                    j_set.set(j, j_bit);
                }
            }
        }

        amples
            .iter_mut()
            .for_each(|(_, invalidate)| *invalidate = false);
    }

    fn ample_pg<'a>(&'a self, pg_id: PgId, ample_pg: &'a mut FixedBitSet) {
        ample_pg.clear();
        let mut ample_self = false;
        for action in self.cs.active_actions_pg(pg_id).unwrap() {
            if let Some((channel, message)) = self.ts.cs.communication(action) {
                if self.cs.check_message(channel, message) {
                    ample_self = true;
                    match message {
                        Message::Send if self.ts.ports.contains(&channel) => {
                            // Channel is a port so action is not stutter
                            ample_pg.insert_range(..);
                            break;
                        }
                        Message::Send => {
                            ample_pg.union_with(self.ts.cs.senders_to_set(channel).unwrap());
                            // NOTE: If an execution fragment has a ProbeEmptyQueue action on a channel that is not initially empty,
                            // then it must be preceded by a Receive action;
                            // so, if the channel is not empty and Receive actions belong to the ample set, ProbeEmptyQueue does not need to belong to the ample set.
                            // Otherwise, we add ProbeEmptyQueue actions to the ample set.
                            if self.cs.is_channel_empty(channel)
                                || !ample_pg
                                    .is_superset(self.ts.cs.receivers_from_set(channel).unwrap())
                            {
                                ample_pg
                                    .union_with(self.ts.cs.probe_empty_queue_set(channel).unwrap());
                            }
                        }
                        Message::ProbeEmptyQueue => {
                            // Channel is empty
                            ample_pg.union_with(self.ts.cs.senders_to_set(channel).unwrap());
                        }
                        Message::Receive => {
                            // Channel is not empty
                            ample_pg.union_with(self.ts.cs.receivers_from_set(channel).unwrap());
                            // NOTE: If an execution fragment has a ProbeFullQueue action on a channel that is not initially full,
                            // then it must be preceded by a Send action;
                            // so, if the channel is not full and Send actions belong to the ample set, ProbeFullQueue does not need to belong to the ample set.
                            // Otherwise, we add ProbeFullQueue actions to the ample set.
                            if self.cs.is_channel_full(channel)
                                || !ample_pg
                                    .is_superset(self.ts.cs.senders_to_set(channel).unwrap())
                            {
                                ample_pg
                                    .union_with(self.ts.cs.probe_full_queue_set(channel).unwrap());
                            }
                        }
                        Message::ProbeFullQueue => {
                            // Channel is full
                            ample_pg.union_with(self.ts.cs.receivers_from_set(channel).unwrap());
                        }
                    }
                } else {
                    // non-active action is not added to ample set,
                    // but ample set must contain all actions which could potentially activate it.
                    // Probes (empty/full-queue) never need to be added
                    match message {
                        Message::Send => {
                            ample_pg.union_with(self.ts.cs.receivers_from_set(channel).unwrap());
                            unreachable!("send actions are always possible");
                        }
                        Message::ProbeEmptyQueue => {
                            // Channel is not empty
                            ample_pg.union_with(self.ts.cs.receivers_from_set(channel).unwrap());
                        }
                        Message::Receive => {
                            // Channel is empty
                            ample_pg.union_with(self.ts.cs.senders_to_set(channel).unwrap());
                        }
                        Message::ProbeFullQueue => {
                            // Channel is not full
                            ample_pg.union_with(self.ts.cs.senders_to_set(channel).unwrap());
                        }
                    }
                }
            } else {
                ample_self = true;
            }
        }
        ample_pg.set(u16::from(pg_id) as usize, ample_self);
    }

    #[inline]
    pub(crate) fn fastforward<'a>(&'a mut self, bump: &'a Bump) {
        for &pg_id in &self.ts.pg_list {
            self.fastforward_pg(pg_id, bump);
        }
    }

    // Resolves tsr steps made of non-communication transition and pushes them to the stack
    fn fastforward_pg<'a>(&'a mut self, pg_id: PgId, bump: &'a Bump) {
        loop {
            let choice;
            {
                let transitions = self.cs.possible_transitions_pg(pg_id).unwrap();
                if let Some((len, (action, posts))) = transitions.enumerate().last()
                    && len == 0
                    && self.ts.cs.communication(action).is_none()
                {
                    choice = posts
                        .map(|posts| {
                            posts
                                .enumerate()
                                .last()
                                .and_then(|(len, post)| (len == 0).then_some(post))
                        })
                        .collect_in::<Option<bumpalo::collections::Vec<Location>>>(bump)
                        .map(|posts| (action, posts.into_bump_slice()));
                } else {
                    choice = None;
                }
            }
            if let Some((action, post)) = choice {
                self.cs
                    .transition(action, post)
                    .expect("transition must succeed");
            } else {
                break;
            }
        }
    }

    #[inline]
    pub(crate) fn is_waiting(&self) -> bool {
        self.cs.is_waiting()
    }

    pub(crate) fn possible_transitions_pg(
        &self,
        pg_id: PgId,
    ) -> Result<
        impl Iterator<
            Item = (
                Action,
                impl Iterator<Item = impl Iterator<Item = Location> + Clone>,
            ),
        >,
        CsError,
    > {
        self.cs.possible_transitions_pg(pg_id)
    }

    pub(crate) fn last_event(&self) -> Option<Event<'_>> {
        self.cs.last_event()
    }
}
