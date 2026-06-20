use std::ops::RangeBounds;

use bumpalo::{Bump, collections::CollectIn};
use rand::{Rng, rngs::SmallRng};

use super::{
    Action, Clock, EPSILON, Effect, Location, LocationIdx, PgError, PgGuard, ProgramGraph,
    Transition, TransitionsIterator, Var,
};
use crate::{BooleanExpr, Time, Val, program_graph::TimeRange};

/// Representation of a PG that can be executed transition-by-transition.
///
/// The structure of the PG cannot be changed,
/// meaning that it is not possible to introduce new locations, actions, variables, etc.
/// Though, this restriction makes it so that cloning the [`ProgramGraphRun`] is cheap,
/// because only the internal state needs to be duplicated.
#[derive(Debug)]
pub struct ProgramGraphRun<'def> {
    current_states: Vec<Location>,
    vars: Vec<Val>,
    clocks: Vec<Time>,
    def: &'def ProgramGraph,
    bump: Bump,
}

impl<'def> Clone for ProgramGraphRun<'def> {
    fn clone(&self) -> Self {
        Self {
            current_states: self.current_states.clone(),
            vars: self.vars.clone(),
            clocks: self.clocks.clone(),
            def: self.def,
            bump: Bump::new(),
        }
    }
}

impl<'def> ProgramGraphRun<'def> {
    /// Create a new executions from a given PG definition.
    pub fn new(program_graph: &'def ProgramGraph) -> Self {
        Self {
            current_states: program_graph.initial_states.clone(),
            vars: program_graph.vars.clone(),
            clocks: vec![0; program_graph.clocks as usize],
            def: program_graph,
            bump: Bump::new(),
        }
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

    #[inline]
    fn transitions<'a>(
        &'a self,
    ) -> TransitionsIterator<'a, impl Iterator<Item = &'a (Action, Vec<Transition>)>> {
        let iters = self
            .current_states
            .iter()
            .map(|loc| self.def.locations[loc.0 as usize].0.iter())
            .collect_in::<bumpalo::collections::Vec<_>>(&self.bump)
            .into_bump_slice_mut();
        TransitionsIterator::new(iters, &self.bump)
    }

    /// Iterates over all transitions that can be admitted in the current state.
    ///
    /// An admissible transition is characterized by the required action and the post-state
    /// (the pre-state being necessarily the current state of the machine).
    /// The guard (if any) is guaranteed to be satisfied.
    pub fn possible_transitions(
        &self,
    ) -> impl Iterator<Item = (Action, impl Iterator<Item = impl Iterator<Item = Location>>)> {
        self.transitions().map(move |(action, loc_transitions)| {
            (
                action,
                loc_transitions.iter().map(move |transitions| {
                    transitions
                        .iter()
                        .filter(move |(post_state, guard, constraints)| {
                            self.check_transition(action, *post_state, guard.as_ref(), constraints)
                        })
                        .map(|(post_state, ..)| *post_state)
                }),
            )
        })
    }

    /// Iterates over all transitions that can be admitted in the current state,
    /// optimized for the special (but common) case in which the state is given by a single location.
    ///
    /// An admissible transition is characterized by the required action and the post-state
    /// (the pre-state being necessarily the current state of the machine).
    /// The guard (if any) is guaranteed to be satisfied.
    ///
    /// Returns error if the state is not given by a single location.
    pub fn nosync_possible_transitions(
        &self,
    ) -> Result<impl Iterator<Item = (Action, impl Iterator<Item = Location>)>, PgError> {
        if self.current_states.len() == 1 {
            let current_loc = self.current_states[0];
            Ok(self.def.locations[current_loc.0 as usize].0.iter().map(
                move |(action, transitions)| {
                    (
                        *action,
                        transitions
                            .iter()
                            .filter(move |(post_state, guard, constraints)| {
                                self.check_transition(
                                    *action,
                                    *post_state,
                                    guard.as_ref(),
                                    constraints,
                                )
                            })
                            .map(|(post_state, ..)| *post_state),
                    )
                },
            ))
        } else {
            Err(PgError::Sync)
        }
    }

    fn check_transition(
        &self,
        action: Action,
        post_state: Location,
        guard: Option<&BooleanExpr<Var>>,
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
        guard
            .is_none_or(|guard| guard.eval::<SmallRng>(&|var| self.vars[var.0 as usize], &mut None))
            && constraints.iter().all(|(c, range)| {
                let time = self.clocks[c.0 as usize];
                range.contains(&time)
            })
            && invariants.iter().all(|(c, range)| {
                let time = if resets.binary_search(c).is_ok() {
                    0
                } else {
                    self.clocks[c.0 as usize]
                };
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
        guard
            .is_none_or(|guard| guard.eval::<SmallRng>(&|var| self.vars[var.0 as usize], &mut None))
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
        self.bump.reset();
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
                let rng = &mut Some(rng);
                effects.iter().for_each(|(var, effect)| {
                    self.vars[var.0 as usize] = effect.eval(&|var| self.vars[var.0 as usize], rng)
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
                range.contains(&start_time) && range.contains(&end_time)
            })
    }

    /// Waits a given amount of time-units.
    ///
    /// Returns error if the waiting would violate the current location's time invariant (if any).
    #[inline]
    pub fn wait(&mut self, delta: Time) -> Result<(), PgError> {
        self.bump.reset();
        if self.can_wait(delta) {
            self.clocks.iter_mut().for_each(|t| *t += delta);
            Ok(())
        } else {
            Err(PgError::Invariant)
        }
    }

    pub(crate) fn send<'a, R: Rng>(
        &'a mut self,
        action: Action,
        post_states: &[Location],
        rng: &'a mut R,
    ) -> Result<Vec<Val>, PgError> {
        self.bump.reset();
        if action == EPSILON {
            Err(PgError::NotSend(action))
        } else if self.active_transitions(action, post_states, &[]) {
            if let Effect::Send(effects) = &self.def.effects[action.0 as usize] {
                let rng = &mut Some(rng);
                let vals = effects
                    .iter()
                    .map(|effect| effect.eval(&|var| self.vars[var.0 as usize], rng))
                    .collect();
                self.current_states.copy_from_slice(post_states);
                Ok(vals)
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
        self.bump.reset();
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
                    // self.current_states = post_states;
                    // self.update_buf();
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
}
