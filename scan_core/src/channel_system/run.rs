use super::{
    Action, Channel, ChannelCapacity, ChannelSystem, CsError, Event, EventType, Location, Message,
    PgId, PgLocation,
};
use crate::{
    Time, Val,
    program_graph::{PgError, ProgramGraphRun},
};
use bumpalo::{Bump, collections::CollectIn};
use rand::rngs::SmallRng;
use std::{
    collections::VecDeque,
    hash::{Hash, Hasher},
};

/// Representation of a CS that can be executed transition-by-transition.
///
/// The structure of the CS cannot be changed,
/// meaning that it is not possible to introduce new PGs or modifying them, or add new channels.
/// Though, this restriction makes it so that cloning the [`ChannelSystem`] is cheap,
/// because only the internal state needs to be duplicated.
#[derive(Debug)]
pub struct ChannelSystemRun<'def> {
    rng: SmallRng,
    time: Time,
    message_queue: Box<[VecDeque<Val>]>,
    program_graphs: Box<[ProgramGraphRun<'def>]>,
    def: &'def ChannelSystem,
    alloc_loc: Vec<PgLocation>,
}

impl<'def> Clone for ChannelSystemRun<'def> {
    fn clone(&self) -> Self {
        Self {
            rng: self.rng.clone(),
            time: self.time,
            message_queue: self.message_queue.clone(),
            program_graphs: self.program_graphs.clone(),
            def: self.def,
            alloc_loc: Vec::with_capacity(self.alloc_loc.len()),
        }
    }
}

impl<'def> PartialEq for ChannelSystemRun<'def> {
    fn eq(&self, other: &Self) -> bool {
        self.rng == other.rng
            && self.time == other.time
            && self.message_queue == other.message_queue
            && self.program_graphs == other.program_graphs
    }
}

impl<'def> Eq for ChannelSystemRun<'def> {}

impl<'def> Hash for ChannelSystemRun<'def> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.time.hash(state);
        self.message_queue.hash(state);
        self.program_graphs.hash(state);
    }
}

impl<'def> ChannelSystemRun<'def> {
    /// Creates a new [`ChannelSystemRun`] which allows to execute the CS as defined.
    ///
    /// The new instance borrows the [`ChannelSystem`] to refer to the CS definition without copying its data,
    /// so that spawning instances is (relatively) inexpensive.
    ///
    /// See also [`ProgramGraphRun::new`].
    pub fn new(cs: &'def ChannelSystem) -> Self {
        let pgs = cs.program_graphs.len() as u16;
        let mut pg_list = Vec::from_iter((0..pgs).map(PgId));
        pg_list.shrink_to_fit();
        let max_locs = cs
            .program_graphs
            .iter()
            .map(|pg| pg.initial_states().len())
            .max()
            .unwrap_or_default();
        ChannelSystemRun {
            rng: rand::make_rng(),
            time: 0,
            program_graphs: cs
                .program_graphs
                .iter()
                .map(|pgdef| pgdef.new_instance())
                .collect(),
            message_queue: cs
                .channels
                .iter()
                .map(|(types, cap)| match cap {
                    ChannelCapacity::Queue(queue) => queue.map_or_else(VecDeque::new, |cap| {
                        VecDeque::with_capacity(types.len() * cap)
                    }),
                    ChannelCapacity::Sink => VecDeque::new(),
                })
                .collect(),
            def: cs,
            alloc_loc: Vec::with_capacity(max_locs),
        }
    }

    /// Returns a reference to the underlying [`ChannelSystem`] defining the execution.
    pub fn def(&self) -> &ChannelSystem {
        self.def
    }

    /// Returns the current time of the CS.
    #[inline]
    pub fn time(&self) -> Time {
        self.time
    }

    /// Returns an immutable reference to the [`ProgramGraphRun`]s of the Channel System associated to the given [`PgId`].
    #[inline]
    pub fn program_graph(&self, pg_id: PgId) -> Result<&ProgramGraphRun<'_>, CsError> {
        self.program_graphs
            .get(pg_id.0 as usize)
            .ok_or(CsError::MissingPg(pg_id))
    }

    /// Iterates over all transitions that can be admitted in the current state.
    ///
    /// An admissible transition is characterized by the PG it executes on, the required action and the post-state
    /// (the pre-state being necessarily the current state of the machine).
    /// The (eventual) guard is guaranteed to be satisfied.
    ///
    /// See also [`ProgramGraphRun::possible_transitions`].
    pub fn possible_transitions(
        &self,
    ) -> impl Iterator<
        Item = (
            PgId,
            Action,
            impl Iterator<Item = impl Iterator<Item = Location> + Clone>,
        ),
    > {
        self.def.program_graph_ids().flat_map(move |pg_id| {
            self.possible_transitions_pg(pg_id)
                .expect("pg exists")
                .map(move |(action, transitions)| (pg_id, action, transitions))
        })
    }

    pub(crate) fn active_actions_pg(
        &self,
        pg_id: PgId,
    ) -> Result<impl Iterator<Item = Action>, CsError> {
        self.program_graph(pg_id).map(move |pg| {
            pg.active_actions()
                .iter()
                .map(move |&action| Action(pg_id, action))
        })
    }

    /// Iterates over all transitions that can be admitted in the current state for the [`ProgramGraph`] associated to the given [`PgId`].
    ///
    /// An admissible transition is characterized by the PG it executes on, the required action and the post-state
    /// (the pre-state being necessarily the current state of the machine).
    /// The (eventual) guard is guaranteed to be satisfied.
    ///
    /// See also [`ProgramGraphRun::possible_transitions`].
    pub fn possible_transitions_pg(
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
        let pg = self.program_graph(pg_id)?;
        Ok(pg
            .possible_transitions()
            .filter(move |&(action, _)| {
                self.def
                    .communication(Action(pg_id, action))
                    .is_none_or(|(channel, message)| self.check_message(channel, message))
            })
            .map(move |(action, post)| {
                (
                    Action(pg_id, action),
                    post.map(move |locs| locs.iter().map(move |loc| Location(pg_id, *loc))),
                )
            }))
    }

    pub(crate) fn check_message(&self, channel: Channel, message: Message) -> bool {
        let channel_idx = channel.0 as usize;
        let (_, capacity) = self.def.channels[channel_idx];
        let len = self.message_queue[channel_idx].len();
        // Channel capacity must never be exceeded!
        // debug_assert!(capacity.is_none_or(|cap| len <= cap));
        // NOTE FIXME currently handshake is unsupported
        // !matches!(capacity, Some(0))
        match capacity {
            ChannelCapacity::Queue(capacity) => match message {
                Message::Send => capacity.is_none_or(|cap| len < cap),
                Message::Receive => len > 0,
                Message::ProbeFullQueue => capacity.is_some_and(|cap| len == cap),
                Message::ProbeEmptyQueue => len == 0,
            },
            ChannelCapacity::Sink => match message {
                Message::Send | Message::ProbeEmptyQueue => true,
                Message::Receive | Message::ProbeFullQueue => false,
            },
        }
    }

    #[inline]
    pub(crate) fn is_channel_empty(&self, channel: Channel) -> Result<bool, CsError> {
        let channel_idx = channel.0 as usize;
        self.message_queue
            .get(channel_idx)
            .ok_or(CsError::MissingChannel(channel))
            .map(|channel| channel.is_empty())
    }

    #[inline]
    pub(crate) fn is_channel_full(&self, channel: Channel) -> Result<bool, CsError> {
        let channel_idx = channel.0 as usize;
        match self.def.channel(channel)?.1 {
            ChannelCapacity::Queue(capacity) => match capacity {
                Some(capacity) => Ok(self.message_queue[channel_idx].len() >= capacity),
                None => Ok(false),
            },
            ChannelCapacity::Sink => Ok(false),
        }
    }

    /// Executes a transition on the given PG characterized by the argument action and post-state.
    ///
    /// Fails if the requested transition is not admissible.
    ///
    /// See also [`ProgramGraphRun::transition`].
    pub fn transition<'a>(
        &mut self,
        action: Action,
        posts: &[Location],
        bump: &'a Bump,
    ) -> Result<Option<Event<'a>>, CsError> {
        use bumpalo::collections::Vec as BumpVec;

        let pg_id = action.0;
        // If action is a communication, check it is legal
        if pg_id.0 >= self.program_graphs.len() as u16 {
            return Err(CsError::MissingPg(pg_id));
        } else if action.0 != pg_id {
            return Err(CsError::ActionNotInPg(action, pg_id));
        } else if let Some(post) = posts.iter().find(|l| l.0 != pg_id) {
            return Err(CsError::LocationNotInPg(*post, pg_id));
        }
        self.alloc_loc.clear();
        self.alloc_loc.extend(posts.iter().map(|post| post.1));
        // If the action is a communication, send/receive the message
        if let Some((channel, message)) = self.def.communication(action) {
            let (_, capacity) = self.def.channels[channel.0 as usize];
            let event_type = match message {
                Message::Send
                    if let ChannelCapacity::Queue(capacity) = capacity
                        && capacity.is_some_and(|cap| {
                            self.message_queue[channel.0 as usize].len() >= cap
                        }) =>
                {
                    return Err(CsError::OutOfCapacity(channel));
                }
                Message::Send => {
                    let vals = self.program_graphs[pg_id.0 as usize]
                        .send(action.1, &self.alloc_loc, &mut self.rng)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))?
                        .collect_in::<BumpVec<_>>(bump)
                        .into_bump_slice();

                    if matches!(capacity, ChannelCapacity::Queue(_)) {
                        self.message_queue[channel.0 as usize].extend(vals);
                    }
                    EventType::Send(vals)
                }
                Message::Receive if self.message_queue[channel.0 as usize].is_empty() => {
                    return Err(CsError::Empty(channel));
                }
                Message::Receive => {
                    let (types, _) = &self.def.channels[channel.0 as usize];
                    let vals = self.message_queue[channel.0 as usize]
                        .drain(..types.len())
                        .collect_in::<bumpalo::collections::Vec<Val>>(bump)
                        .into_bump_slice();
                    self.program_graphs[pg_id.0 as usize]
                        .receive(action.1, &self.alloc_loc, vals.as_ref())
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
                    EventType::Receive(vals)
                }
                Message::ProbeEmptyQueue | Message::ProbeFullQueue
                    if matches!(capacity, ChannelCapacity::Queue(Some(0))) =>
                {
                    return Err(CsError::ProbingHandshakeChannel(channel));
                }
                Message::ProbeEmptyQueue if !self.message_queue[channel.0 as usize].is_empty() => {
                    return Err(CsError::NotEmpty(channel));
                }
                Message::ProbeEmptyQueue => {
                    let _ = self.program_graphs[pg_id.0 as usize]
                        .send(action.1, &self.alloc_loc, &mut self.rng)
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
                    EventType::ProbeEmptyQueue
                }
                Message::ProbeFullQueue => match capacity {
                    ChannelCapacity::Queue(None) => {
                        return Err(CsError::ProbingInfiniteQueue(channel));
                    }
                    ChannelCapacity::Queue(Some(capacity)) => {
                        if self.message_queue[channel.0 as usize].len() >= capacity {
                            let _ = self.program_graphs[pg_id.0 as usize]
                                .send(action.1, &self.alloc_loc, &mut self.rng)
                                .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
                            EventType::ProbeFullQueue
                        } else {
                            return Err(CsError::NotFull(channel));
                        }
                    }
                    ChannelCapacity::Sink => return Err(CsError::NotFull(channel)),
                },
            };
            Ok(Some(Event {
                pg_id,
                channel,
                event_type,
            }))
        } else {
            // Transition the program graph
            self.program_graphs[pg_id.0 as usize]
                .transition(action.1, &self.alloc_loc, &mut self.rng)
                .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
            Ok(None)
        }
    }

    /// Tries waiting for the given delta of time.
    /// Returns error if any of the PG cannot wait due to some time invariant.
    pub fn wait(&mut self, delta: Time) -> Result<(), CsError> {
        if let Some(pg) = self
            .program_graphs
            .iter()
            .position(|pg| !pg.can_wait(delta))
        {
            Err(CsError::ProgramGraph(PgId(pg as u16), PgError::Invariant))
        } else {
            self.program_graphs.iter_mut().for_each(|pg| {
                pg.wait(delta).expect("wait");
            });
            self.time += delta;
            Ok(())
        }
    }

    /// Checks if it is possible to wait a given amount of time-units without violating the time invariants.
    #[inline]
    pub fn can_wait(&self, delta: Time) -> bool {
        self.program_graphs.iter().all(|pg| pg.can_wait(delta))
    }

    /// Returns `true` if there is any transition from the current state that will be unlocked at some point in the future.
    #[inline]
    pub fn is_waiting(&self) -> bool {
        self.can_wait(1) && self.program_graphs.iter().any(|pg| pg.is_waiting())
    }
}
