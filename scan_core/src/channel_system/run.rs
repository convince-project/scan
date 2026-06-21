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
use std::collections::VecDeque;

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
    message_queue: Vec<VecDeque<Val>>,
    program_graphs: Vec<ProgramGraphRun<'def>>,
    def: &'def ChannelSystem,
    bump: Bump,
}

impl<'def> Clone for ChannelSystemRun<'def> {
    fn clone(&self) -> Self {
        Self {
            rng: self.rng.clone(),
            time: self.time,
            message_queue: self.message_queue.clone(),
            program_graphs: self.program_graphs.clone(),
            def: self.def,
            bump: Bump::new(),
        }
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

        ChannelSystemRun {
            rng: rand::make_rng(),
            time: 0,
            program_graphs: Vec::from_iter(
                cs.program_graphs.iter().map(|pgdef| pgdef.new_instance()),
            ),
            message_queue: Vec::from_iter(cs.channels.iter().map(|(types, cap)| match cap {
                ChannelCapacity::Queue(queue) => queue.map_or_else(VecDeque::new, |cap| {
                    VecDeque::with_capacity(types.len() * cap)
                }),
                ChannelCapacity::Sink => VecDeque::new(),
            })),
            def: cs,
            bump: Bump::new(),
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
            impl Iterator<Item = impl Iterator<Item = Location>>,
        ),
    > {
        self.def.program_graph_ids().flat_map(move |pg_id| {
            self.possible_transitions_pg(pg_id)
                .expect("pg exists")
                .map(move |(action, transitions)| (pg_id, action, transitions))
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
        impl Iterator<Item = (Action, impl Iterator<Item = impl Iterator<Item = Location>>)>,
        CsError,
    > {
        self.program_graph(pg_id).map(|pg| {
            pg.possible_transitions().filter_map(move |(action, post)| {
                let action = Action(pg_id, action);
                if let Some((channel, message)) = self.def.communication(pg_id, action.1)
                    && !self.check_message(channel, message)
                {
                    None
                } else {
                    let post = post.map(move |locs| locs.map(move |loc| Location(pg_id, loc)));
                    Some((action, post))
                }
            })
        })
    }

    /// Iterates over all transitions that can be admitted in the current state for the [`ProgramGraph`] associated to the given [`PgId`],
    /// optimized for the special (but common) case in which the state of the PG is given by a single location.
    ///
    /// An admissible transition is characterized by the PG it executes on, the required action and the post-state
    /// (the pre-state being necessarily the current state of the machine).
    /// The (eventual) guard is guaranteed to be satisfied.
    ///
    /// See also [`ProgramGraphRun::nosync_possible_transitions`].
    pub fn nosync_possible_transitions_pg(
        &self,
        pg_id: PgId,
    ) -> Result<impl Iterator<Item = (Action, impl Iterator<Item = Location>)>, CsError> {
        self.program_graph(pg_id)?
            .nosync_possible_transitions()
            .map_err(|err| CsError::ProgramGraph(pg_id, err))
            .map(|transitions| {
                transitions.filter_map(move |(action, post)| {
                    let action = Action(pg_id, action);
                    if let Some((channel, message)) = self.def.communication(pg_id, action.1)
                        && !self.check_message(channel, message)
                    {
                        None
                    } else {
                        let post = post.map(move |loc| Location(pg_id, loc));
                        Some((action, post))
                    }
                })
            })
    }

    fn check_message(&self, channel: Channel, message: Message) -> bool {
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

    /// Executes a transition on the given PG characterized by the argument action and post-state.
    ///
    /// Fails if the requested transition is not admissible.
    ///
    /// See also [`ProgramGraphRun::transition`].
    pub fn transition(
        &mut self,
        pg_id: PgId,
        action: Action,
        post: &[Location],
    ) -> Result<Option<Event>, CsError> {
        use bumpalo::collections::Vec as BumpVec;

        self.bump.reset();

        // If action is a communication, check it is legal
        if pg_id.0 >= self.program_graphs.len() as u16 {
            return Err(CsError::MissingPg(pg_id));
        } else if action.0 != pg_id {
            return Err(CsError::ActionNotInPg(action, pg_id));
        } else if let Some(post) = post.iter().find(|l| l.0 != pg_id) {
            return Err(CsError::LocationNotInPg(*post, pg_id));
        }
        // If the action is a communication, send/receive the message
        if let Some((channel, message)) = self.def.communication(pg_id, action.1) {
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
                        .send(
                            action.1,
                            post.iter()
                                .map(|loc| loc.1)
                                .collect_in::<BumpVec<PgLocation>>(&self.bump)
                                .as_slice(),
                            &mut self.rng,
                        )
                        .map_err(|err| CsError::ProgramGraph(pg_id, err))?;
                    if matches!(capacity, ChannelCapacity::Queue(_)) {
                        self.message_queue[channel.0 as usize].extend(&vals);
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
                        .collect::<Vec<Val>>();
                    self.program_graphs[pg_id.0 as usize]
                        .receive(
                            action.1,
                            post.iter()
                                .map(|loc| loc.1)
                                .collect_in::<BumpVec<PgLocation>>(&self.bump)
                                .as_slice(),
                            vals.as_slice(),
                        )
                        .expect("communication has been verified before");
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
                        .send(
                            action.1,
                            post.iter()
                                .map(|loc| loc.1)
                                .collect_in::<BumpVec<PgLocation>>(&self.bump)
                                .as_slice(),
                            &mut self.rng,
                        )
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
                                .send(
                                    action.1,
                                    post.iter()
                                        .map(|loc| loc.1)
                                        .collect_in::<BumpVec<PgLocation>>(&self.bump)
                                        .as_slice(),
                                    &mut self.rng,
                                )
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
                .transition(
                    action.1,
                    post.iter()
                        .map(|loc| loc.1)
                        .collect_in::<BumpVec<PgLocation>>(&self.bump)
                        .as_slice(),
                    &mut self.rng,
                )
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
}
