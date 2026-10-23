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
    last_event: Option<Channel>,
}

impl<'def> PartialEq for TransitionSystemRun<'def> {
    fn eq(&self, other: &Self) -> bool {
        self.cs == other.cs && self.vals == other.vals && self.last_event == other.last_event
    }
}

impl<'def> Eq for TransitionSystemRun<'def> {}

impl<'def> Hash for TransitionSystemRun<'def> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cs.hash(state);
        self.vals.hash(state);
        self.last_event.hash(state);
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
            last_event: None,
            vals_idx,
        }
    }

    pub(crate) fn montecarlo_transition<'a, O: Oracle, R: RngExt>(
        &mut self,
        oracle: &mut O,
        pg_list: &mut [PgId],
        rng: &mut R,
        bump: &'a Bump,
    ) -> Option<(Action, Event<'a>)> {
        // Setting pgs_left as length resets the queue
        let mut pgs_left = pg_list.len();
        while pgs_left > 0 {
            // Select random pg within 0..pgs_left
            let pg_select = rng.random_range(0..pgs_left);
            let pg_id = pg_list[pg_select];
            // Execute randomly chosen transitions on the picked PG until an event is generated,
            // or no more transition is possible
            // NOTE: Special treatment for PGs with single-location state for optimization of this common case.
            // Hopefully it will be possible to treat all cases in a general way eventually.
            let result = self.montecarlo_transition_pg(pg_id, oracle, rng, bump);
            if let Some((action, event)) = result {
                return Some((action, event));
            } else {
                // Swap selected pg with last element of the queue (possibly itself, probably not worth checking)
                // Decrease the length of the queue (so that selected element is removed)
                pgs_left -= 1;
                pg_list.swap(pg_select, pgs_left);
            }
        }
        None
    }

    fn montecarlo_transition_pg<'a, O: Oracle, R: RngExt>(
        &mut self,
        pg_id: PgId,
        oracle: &mut O,
        rng: &mut R,
        bump: &'a Bump,
    ) -> Option<(Action, Event<'a>)> {
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
            let event = self
                .transition(oracle, action, post_states, bump)
                .expect("successful transition");
            if event.is_some() {
                return event.map(|ev| (action, ev));
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

    pub(crate) fn labels(&self) -> impl Iterator<Item = bool> {
        self.ts.predicates.iter().map(|prop| {
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
                    Atom::Event(event_channel) => Val::Boolean(
                        self.last_event
                            .is_some_and(|channel| channel == event_channel),
                    ),
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

    pub(crate) fn transition<'a, O: Oracle>(
        &mut self,
        oracle: &mut O,
        action: Action,
        post: &[Location],
        bump: &'a Bump,
    ) -> Result<Option<Event<'a>>, CsError> {
        let last_event = self
            .cs
            .transition(action, post, bump)?
            .map(|event| (action, event));
        if let Some((_, event)) = last_event {
            if let EventType::Send(vals) = event.event_type
                && let Ok(index) = self.ts.ports.binary_search(&event.channel)
            {
                // Since we have to update old values,
                // the vectors are already allocated and their len is always the same.
                // Copying from slice should be faster than cloning.
                self.vals[self.vals_idx[index]..self.vals_idx[index + 1]].copy_from_slice(vals);
                let labels = bumpalo::collections::Vec::from_iter_in(self.labels(), bump);
                oracle.update_state(&labels);
            }
            self.last_event = Some(event.channel);
            Ok(Some(event))
        } else {
            self.last_event = None;
            Ok(None)
        }
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
                        }
                        Message::ProbeFullQueue => todo!(),
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
                        Message::ProbeFullQueue => todo!(),
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
                    .transition(action, post, bump)
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
}
