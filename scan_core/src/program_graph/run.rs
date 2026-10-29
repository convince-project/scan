use std::{
    hash::{Hash, Hasher},
    mem,
    ops::{Bound, RangeBounds},
    rc::Rc,
};

use rand::{Rng, rngs::SmallRng};

use super::{
    Action, Clock, EPSILON, Effect, Location, LocationIdx, PgError, PgGuard, ProgramGraph,
    Transition, Var,
};
use crate::{Time, Val, program_graph::TimeRange};

/// Representation of a PG that can be executed transition-by-transition.
///
/// The structure of the PG cannot be changed,
/// meaning that it is not possible to introduce new locations, actions, variables, etc.
/// Though, this restriction makes it so that cloning the [`ProgramGraphRun`] is cheap,
/// because only the internal state needs to be duplicated.
#[derive(Debug)]
pub struct ProgramGraphRun<'def> {
    current_states: Box<[Location]>,
    vars: Box<[Val]>,
    clocks: Box<[Time]>,
    def: &'def ProgramGraph,
    // NOTE: cache is represented in sparse matrix format to avoid allocations.
    // transition destinations
    cache_trans: Rc<Vec<Location>>,
    // which transitions refer to the given current state and actions (chunked by states number)
    // so cache_states.len = cache_actions.len * current_states.len + 1 (first index is always 0)
    cache_states: Rc<Vec<usize>>,
    // active actions
    cache_actions: Rc<Vec<Action>>,
    iters: Vec<std::slice::Iter<'def, (Action, Vec<Transition>)>>,
    loc_trans: Vec<&'def [Transition]>,
}

impl<'def> Clone for ProgramGraphRun<'def> {
    fn clone(&self) -> Self {
        Self {
            current_states: self.current_states.clone(),
            vars: self.vars.clone(),
            clocks: self.clocks.clone(),
            def: self.def,
            cache_trans: self.cache_trans.clone(),
            cache_states: self.cache_states.clone(),
            cache_actions: self.cache_actions.clone(),
            iters: Vec::with_capacity(self.current_states.len()),
            loc_trans: vec![[].as_slice(); self.current_states.len()],
        }
    }
}

impl<'def> PartialEq for ProgramGraphRun<'def> {
    // WARN: This does not check that the definitions are the same
    fn eq(&self, other: &Self) -> bool {
        self.current_states == other.current_states
            && self.vars == other.vars
            && self.clocks == other.clocks
    }
}

impl<'def> Eq for ProgramGraphRun<'def> {}

impl<'def> Hash for ProgramGraphRun<'def> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.current_states.hash(state);
        self.vars.hash(state);
        self.clocks.hash(state);
    }
}

impl<'def> ProgramGraphRun<'def> {
    /// Create a new executions from a given PG definition.
    pub fn new(program_graph: &'def ProgramGraph) -> Self {
        let states = program_graph.initial_states.len();
        let max_actions = program_graph
            .locations
            .iter()
            .map(|(actions, _)| actions.len())
            .max()
            .unwrap_or(program_graph.effects.len());
        let mut pg = Self {
            current_states: program_graph.initial_states.clone(),
            vars: program_graph.vars.clone(),
            clocks: vec![0; program_graph.clocks as usize].into(),
            def: program_graph,
            // caching data
            cache_trans: Rc::new(Vec::new()),
            cache_actions: Rc::new(Vec::with_capacity(max_actions)),
            cache_states: Rc::new(Vec::with_capacity(max_actions * states)),
            iters: Vec::with_capacity(program_graph.initial_states.len()),
            loc_trans: vec![[].as_slice(); program_graph.initial_states.len()],
        };
        pg.refresh_cache();
        pg
    }

    /// Returns the current location.
    ///
    /// ```
    /// # use scan_core::program_graph::ProgramGraphBuilder;
    /// // Create a new PG builder
    /// let mut pg_builder = ProgramGraphBuilder::new();
    ///
    /// // The builder is initialized with an initial location
    /// let initial_loc = pg_builder.new_initial_location();
    ///
    /// // Build the PG from its builder
    /// // The builder is always guaranteed to build a well-defined PG and building cannot fail
    /// let pg = pg_builder.build();
    /// let instance = pg.new_instance();
    ///
    /// // Execution starts in the initial location
    /// assert_eq!(instance.current_states().as_slice(), &[initial_loc]);
    /// ```
    #[inline]
    pub fn current_states(&self) -> &[Location] {
        &self.current_states
    }

    /// Iterates over all transitions that can be admitted in the current state.
    ///
    /// An admissible transition is characterized by the required action and the post-state
    /// (the pre-state being necessarily the current state of the machine).
    /// The guard (if any) is guaranteed to be satisfied.
    pub fn possible_transitions(
        &self,
    ) -> impl Iterator<Item = (Action, impl Iterator<Item = &[Location]> + Clone)> {
        let states = self.current_states.len();
        self.cache_actions
            .iter()
            .enumerate()
            .map(move |(action_idx, action)| {
                (
                    *action,
                    self.cache_states[action_idx * states..=(action_idx + 1) * states]
                        .array_windows()
                        .map(|&[transitions_idx, next_transitions_idx]| {
                            &self.cache_trans[transitions_idx..next_transitions_idx]
                        }),
                )
            })
    }

    pub(crate) fn active_actions(&self) -> &[Action] {
        &self.cache_actions
    }

    fn refresh_cache(&mut self) {
        let mut rc_states = Rc::new(Vec::new());
        let mut rc_actions = Rc::new(Vec::new());
        let mut rc_trans = Rc::new(Vec::new());
        if Rc::strong_count(&self.cache_states) == 1 {
            mem::swap(&mut rc_states, &mut self.cache_states);
            mem::swap(&mut rc_actions, &mut self.cache_actions);
            mem::swap(&mut rc_trans, &mut self.cache_trans);
        };
        let states = Rc::get_mut(&mut rc_states).expect("Rc strong count is 1");
        states.clear();
        states.push(0);
        let actions = Rc::get_mut(&mut rc_actions).expect("Rc strong count is 1");
        actions.clear();
        let trans = Rc::get_mut(&mut rc_trans).expect("Rc strong count is 1");
        trans.clear();

        self.iters.clear();
        self.iters.extend(
            self.current_states
                .iter()
                .map(|loc| self.def.locations[loc.0 as usize].0.iter()),
        );

        let len = self.def.initial_states.len();
        let mut action = Action(0);
        'l: loop {
            let mut total = 0;
            let mut i = 0;
            while total < len {
                if let Some(&(next_a, ref next_t)) = self.iters[i].find(|(a, _)| a.0 >= action.0) {
                    self.loc_trans[i] = next_t.as_slice();
                    i = (i + 1) % len;
                    total = total * (next_a.0 == action.0) as usize + 1;
                    action = next_a;
                } else {
                    break 'l;
                }
            }
            // found next possible action
            let initial_num_trans = trans.len();
            for state_trans in &self.loc_trans {
                let num_trans = trans.len();
                trans.extend(
                    state_trans
                        .iter()
                        .filter(|(post_state, guard, constraints)| {
                            self.check_transition(action, *post_state, guard.as_ref(), constraints)
                        })
                        .map(|(post_state, ..)| *post_state),
                );
                let next_num_trans = trans.len();
                if next_num_trans > num_trans {
                    // only add state if it has some active transition
                    states.push(next_num_trans);
                } else {
                    // if one state has no active transitions for an action,
                    // remove all transitions for that action.
                    states.truncate(actions.len() * len + 1);
                    trans.truncate(*states.last().unwrap());
                    continue 'l;
                }
            }
            // if all states have active transitions for an action,
            // add action to list of possible actions.
            assert!(trans.len() > initial_num_trans);
            actions.push(action);
        }
        assert_eq!(states.len(), actions.len() * len + 1);

        self.cache_states = rc_states;
        self.cache_actions = rc_actions;
        self.cache_trans = rc_trans;
    }

    #[inline]
    fn check_transition(
        &self,
        action: Action,
        post_state: Location,
        guard: Option<&PgGuard>,
        constraints: &[(Clock, TimeRange)],
    ) -> bool {
        let (_, ref invariants) = self.def.locations[post_state.0 as usize];
        if action != EPSILON
            && let Effect::Effects(_, ref resets) = self.def.effects[action.0 as usize]
        {
            self.active_transition(guard, constraints, invariants, resets)
        } else {
            self.active_autonomous_transition(guard, constraints, invariants)
        }
    }

    fn active_transition(
        &self,
        guard: Option<&PgGuard>,
        constraints: &[(Clock, TimeRange)],
        invariants: &[(Clock, TimeRange)],
        resets: &[Clock],
    ) -> bool {
        guard.is_none_or(|guard| guard.eval::<SmallRng>(&|var| self.vars[var.0 as usize], None))
            && constraints.iter().all(|(c, range)| {
                let time = self.clocks[c.0 as usize];
                range.contains(&time)
            })
            && invariants.iter().all(|(c, range)| {
                // branchless expression
                let time = resets.binary_search(c).is_err() as Time * self.clocks[c.0 as usize];
                range.contains(&time)
            })
    }

    #[inline]
    fn active_autonomous_transition(
        &self,
        guard: Option<&PgGuard>,
        constraints: &[(Clock, TimeRange)],
        invariants: &[(Clock, TimeRange)],
    ) -> bool {
        guard.is_none_or(|guard| guard.eval::<SmallRng>(&|var| self.vars[var.0 as usize], None))
            && constraints.iter().chain(invariants).all(|(c, range)| {
                let time = self.clocks[c.0 as usize];
                range.contains(&time)
            })
    }

    fn active_transitions(
        &self,
        action: Action,
        post_states: &[Location],
        resets: &[Clock],
    ) -> bool {
        self.current_states
            .iter()
            .zip(post_states)
            .all(|(current_state, post_state)| {
                self.def
                    .guards(*current_state, action, *post_state)
                    .any(|(guard, constraints)| {
                        self.active_transition(
                            guard,
                            constraints,
                            &self.def.locations[post_state.0 as usize].1,
                            resets,
                        )
                    })
            })
    }

    fn active_autonomous_transitions(&self, post_states: &[Location]) -> bool {
        self.current_states
            .iter()
            .zip(post_states)
            .all(|(current_state, post_state)| {
                self.def
                    .guards(*current_state, EPSILON, *post_state)
                    .any(|(guard, constraints)| {
                        self.active_autonomous_transition(
                            guard,
                            constraints,
                            &self.def.locations[post_state.0 as usize].1,
                        )
                    })
            })
    }

    /// Executes a transition characterized by the argument action and post-state.
    ///
    /// Fails if the requested transition is not admissible,
    /// or if the post-location time invariants are violated.
    pub fn transition<R: Rng>(
        &mut self,
        action: Action,
        post_states: &[Location],
        rng: &mut R,
    ) -> Result<(), PgError> {
        if post_states.len() != self.current_states.len() {
            return Err(PgError::MismatchingPostStates);
        }
        if let Some(ps) = post_states
            .iter()
            .find(|ps| ps.0 >= self.def.locations.len() as LocationIdx)
        {
            return Err(PgError::MissingLocation(*ps));
        }
        if action == EPSILON {
            if !self.active_autonomous_transitions(post_states) {
                return Err(PgError::UnsatisfiedGuard);
            }
        } else if action.0 >= self.def.effects.len() as LocationIdx {
            return Err(PgError::MissingAction(action));
        } else if let Effect::Effects(ref effects, ref resets) = self.def.effects[action.0 as usize]
        {
            if self.active_transitions(action, post_states, resets) {
                effects.iter().for_each(|(var, effect)| {
                    self.vars[var.0 as usize] =
                        effect.eval(&|var| self.vars[var.0 as usize], Some(rng))
                });
                resets
                    .iter()
                    .for_each(|clock| self.clocks[clock.0 as usize] = 0);
            } else {
                return Err(PgError::UnsatisfiedGuard);
            }
        } else {
            return Err(PgError::Communication(action));
        }
        self.current_states.copy_from_slice(post_states);
        self.refresh_cache();
        Ok(())
    }

    /// Checks if it is possible to wait a given amount of time-units without violating the time invariants.
    #[inline]
    pub fn can_wait(&self, delta: Time) -> bool {
        self.current_states
            .iter()
            .flat_map(|current_state| self.def.locations[current_state.0 as usize].1.iter())
            .all(|(c, range)| {
                // Invariants need to be satisfied during the whole wait.
                let start_time = self.clocks[c.0 as usize];
                let end_time = start_time + delta;
                // range.contains(&start_time) &&
                range.contains(&end_time)
            })
    }

    /// Waits a given amount of time-units.
    ///
    /// Returns error if the waiting would violate the current location's time invariant (if any).
    #[inline]
    pub fn wait(&mut self, delta: Time) -> Result<(), PgError> {
        if self.can_wait(delta) {
            self.clocks.iter_mut().for_each(|t| *t += delta);
            self.refresh_cache();
            Ok(())
        } else {
            Err(PgError::Invariant)
        }
    }

    pub(crate) fn send<R: Rng>(
        &mut self,
        action: Action,
        post_states: &[Location],
        rng: &mut R,
    ) -> Result<impl Iterator<Item = Val>, PgError> {
        if action == EPSILON {
            Err(PgError::NotSend(action))
        } else if post_states.len() != self.current_states.len() {
            // WARN FIXME: This error is not appropriate here
            Err(PgError::Sync)
        } else if self.active_transitions(action, post_states, &[]) {
            if let Effect::Send(effects) = &self.def.effects[action.0 as usize] {
                self.current_states.copy_from_slice(post_states);
                self.refresh_cache();
                Ok(effects
                    .iter()
                    .map(|effect| effect.eval(&|var| self.vars[var.0 as usize], Some(rng))))
            } else {
                Err(PgError::NotSend(action))
            }
        } else {
            Err(PgError::UnsatisfiedGuard)
        }
    }

    pub(crate) fn receive(
        &mut self,
        action: Action,
        post_states: &[Location],
        vals: &[Val],
    ) -> Result<(), PgError> {
        if action == EPSILON {
            Err(PgError::NotReceive(action))
        } else if self.active_transitions(action, post_states, &[]) {
            if let Effect::Receive(ref vars) = self.def.effects[action.0 as usize] {
                // let var_content = self.vars.get_mut(var.0 as usize).expect("variable exists");
                if vars.len() == vals.len()
                    && vals.iter().zip(vars).all(|(val, var)| {
                        self.vars
                            .get(var.0 as usize)
                            .expect("variable exists")
                            .r#type()
                            == val.r#type()
                    })
                {
                    vals.iter().zip(vars).for_each(|(&val, var)| {
                        *self.vars.get_mut(var.0 as usize).expect("variable exists") = val
                    });
                    self.current_states.copy_from_slice(post_states);
                    self.refresh_cache();
                    Ok(())
                } else {
                    Err(PgError::TypeMismatch)
                }
            } else {
                Err(PgError::NotReceive(action))
            }
        } else {
            Err(PgError::UnsatisfiedGuard)
        }
    }

    /// Returns `true` if there is any transition from the current state that will be unlocked at some point in the future,
    /// either because of a temporal guard on the transition becoming true, or a time invariant on the post-location becoming true.
    pub fn is_waiting(&self) -> bool {
        let unsatisfied_lower_bound = |(c, range): &(Clock, TimeRange)| {
            let time = self.clocks[c.0 as usize];
            let bound = range.start_bound();
            match bound {
                Bound::Included(b) => time < *b,
                Bound::Excluded(b) => time <= *b,
                Bound::Unbounded => false,
            }
        };
        let satisfied_upper_bound = |(c, range): &(Clock, TimeRange)| {
            let time = self.clocks[c.0 as usize];
            let bound = range.end_bound();
            match bound {
                Bound::Included(b) => time <= *b,
                Bound::Excluded(b) => time < *b,
                Bound::Unbounded => true,
            }
        };
        // NOTE WARN: active_actions is wrong
        self.current_states.iter().all(|state| {
            let (transitions, _) = &self.def.locations[state.0 as usize];
            transitions
                .iter()
                .any(|(_action, transitions)|
                    transitions.iter()
                    .any(move |(post_state, guard, constraints)| {
                        let invariants = self.def.locations[post_state.0 as usize].1.as_slice();
                        // Check guard
                        guard.as_ref().is_none_or(|guard| guard.eval::<SmallRng>(&|var: Var| self.vars[var.0 as usize], None))
                        // Check time constraints
                        && ((constraints.iter().any(unsatisfied_lower_bound)
                            && constraints.iter().all(satisfied_upper_bound))
                            // Check time invariants
                            || (invariants.iter().any(unsatisfied_lower_bound)
                                && invariants.iter().all(satisfied_upper_bound)))
                    }))
        })
    }
}
