//! Implementation of the PG model of computation.
//!
//! A _Program Graph_ is given by:
//!
//! - a finite set `L` of _locations_;
//! - a finite set `A` of _actions_;
//! - a finite set `V` of _typed variables_;
//! - a _transition relation_ that associates pairs of locations (pre-location and post-location) and an action with a Boolean expression (the _guard_ of the transition);
//! - for each actions, a set of _effects_, i.e., a variable `x` from `V` and an expression in the variables of `V` of the same type as `x`.
//!
//! The state of a PG is given by its current location and the value assigned to each variable.
//! The PG's state evolves by non-deterministically choosing a transition whose pre-state is the current state,
//! and whose guard expression evaluates to `true`.
//! Then, the post-state of the chosen transition becomes the current state of the PG.
//! Finally, the effects of the transition's associated action are applied in order,
//! by assigning each effect's variable the value of the effect's expression evaluation.
//!
//! A PG is represented by a [`ProgramGraph`] and defined through a [`ProgramGraphBuilder`],
//! by adding, one at a time, new locations, actions, effects, guards and transitions.
//! Then, the [`ProgramGraph`] is built from the [`ProgramGraphBuilder`]
//! and can be executed by performing transitions,
//! though the structure of the PG itself can no longer be altered.
//!
//! ```
//! # use scan_core::program_graph::{ProgramGraphBuilder, Location};
//! // Create a new PG builder
//! let mut pg_builder = ProgramGraphBuilder::new();
//!
//! // The builder is initialized with an initial location
//! let initial_loc = pg_builder.new_initial_location();
//!
//! // Create a new action
//! let action = pg_builder.new_action();
//!
//! // Create a new location
//! let post_loc = pg_builder.new_location();
//!
//! // Add a transition (the guard is optional, and None is equivalent to the guard always being true)
//! let result = pg_builder.add_transition(initial_loc, action, post_loc, None);
//!
//! // This can only fail if the builder does not recognize either the locations
//! // or the action defining the transition
//! result.expect("both the initial location and the action belong to the PG");
//!
//! // Build the PG from its builder
//! // The builder is always guaranteed to build a well-defined PG and building cannot fail
//! let pg = pg_builder.build();
//! let mut instance = pg.new_instance();
//!
//! // Execution starts in the initial location
//! assert_eq!(instance.current_states().as_slice(), &[initial_loc]);
//!
//! // Compute the possible transitions on the PG
//! {
//!     let mut iter = instance .possible_transitions();
//!     let (act, mut trans) = iter.next().unwrap();
//!     assert_eq!(act, action);
//!     let post_locs: Vec<Location> = trans.next().unwrap().collect();
//!     assert_eq!(post_locs, vec![post_loc]);
//!     assert!(iter.next().is_none());
//! }
//!
//! // Perform a transition
//! # use rand::{Rng, SeedableRng};
//! # use rand::rngs::SmallRng;
//! let mut rng: SmallRng = rand::make_rng();
//! let result = instance .transition(action, &[post_loc], &mut rng);
//!
//! // Performing a transition can fail, in particular, if the transition was not allowed
//! result.expect("The transition from the initial location onto itself is possible");
//!
//! // There are no more possible transitions
//! assert!(instance.possible_transitions().next().is_none());
//!
//! // Attempting to transition results in an error
//! instance.transition(action, &[post_loc], &mut rng).expect_err("The transition is not possible");
//! ```

mod builder;

use crate::{Time, grammar::*};
pub use builder::*;
use bumpalo::{Bump, collections::CollectIn};
use rand::{Rng, rngs::SmallRng};
use thiserror::Error;

/// The index for [`Location`]s in a [`ProgramGraph`].
pub type LocationIdx = u32;

/// An indexing object for locations in a PG.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ProgramGraphBuilder`] or [`ProgramGraph`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location(LocationIdx);

/// The index for [`Action`]s in a [`ProgramGraph`].
pub type ActionIdx = u32;

/// An indexing object for actions in a PG.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ProgramGraphBuilder`] or [`ProgramGraph`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]

pub struct Action(ActionIdx);

impl From<Action> for ActionIdx {
    #[inline]
    fn from(val: Action) -> Self {
        val.0
    }
}

/// Epsilon action to enable autonomous transitions.
/// It cannot have effects.
pub(crate) const EPSILON: Action = Action(ActionIdx::MAX);

/// An indexing object for typed variables in a PG.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ProgramGraphBuilder`] or [`ProgramGraph`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Var(u16);

/// An indexing object for clocks in a PG.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ProgramGraphBuilder`] or [`ProgramGraph`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Clock(u16);

/// A time constraint given by a clock and, optionally, a lower bound and/or an upper bound.
pub type TimeConstraint = (Clock, Option<Time>, Option<Time>);

/// An expression using PG's [`Var`] as variables.
pub type PgExpression = Expression<Var>;

/// A Boolean expression over [`Var`] variables.
pub type PgGuard = BooleanExpr<Var>;

/// The error type for operations with [`ProgramGraphBuilder`]s and [`ProgramGraph`]s.
#[derive(Debug, Clone, Copy, Error)]
pub enum PgError {
    /// There is no such action in the PG.
    #[error("action {0:?} does not belong to this program graph")]
    MissingAction(Action),
    /// There is no such clock in the PG.
    #[error("clock {0:?} does not belong to this program graph")]
    MissingClock(Clock),
    /// There is no such location in the PG.
    #[error("location {0:?} does not belong to this program graph")]
    MissingLocation(Location),
    /// There is no such variable in the PG.
    #[error("location {0:?} does not belong to this program graph")]
    MissingVar(Var),
    /// The PG does not allow this transition.
    #[error("there is no such transition")]
    MissingTransition,
    /// Types that should be matching are not,
    /// or are not compatible with each other.
    #[error("type mismatch")]
    TypeMismatch,
    /// Transition's guard is not satisfied.
    #[error("the guard has not been satisfied")]
    UnsatisfiedGuard,
    /// The tuple has no component for such index.
    #[error("the tuple has no {0} component")]
    MissingComponent(usize),
    /// Cannot add effects to a Receive action.
    #[error("cannot add effects to a Receive action")]
    EffectOnReceive,
    /// Cannot add effects to a Send action.
    #[error("cannot add effects to a Send action")]
    EffectOnSend,
    /// This action is a communication (either Send or Receive).
    #[error("{0:?} is a communication (either Send or Receive)")]
    Communication(Action),
    /// Mismatching (i.e., wrong number) post states of transition.
    #[error("Mismatching (i.e., wrong number) post states of transition")]
    MismatchingPostStates,
    /// The action is a not a Send communication.
    #[error("{0:?} is a not a Send communication")]
    NotSend(Action),
    /// The action is a not a Receive communication.
    #[error("{0:?} is a not a Receive communication")]
    NotReceive(Action),
    /// The epsilon action has no effects.
    #[error("The epsilon action has no effects")]
    NoEffects,
    /// A time invariant is not satisfied.
    #[error("A time invariant is not satisfied")]
    Invariant,
    /// A type error
    #[error("type error")]
    Type(#[source] TypeError),
}

#[derive(Debug, Clone)]
enum Effect {
    // NOTE: Could use a SmallVec for clock resets
    Effects(Vec<(Var, Expression<Var>)>, Vec<Clock>),
    Send(Vec<Expression<Var>>),
    Receive(Vec<Var>),
}

type LocationData = (Vec<(Action, Vec<Transition>)>, Vec<TimeConstraint>);

/// A definition object for a PG.
/// It represents the abstract definition of a PG.
///
/// The only way to produce a [`ProgramGraph`] is through a [`ProgramGraphBuilder`].
/// This guarantees that there are no type errors involved in the definition of action's effects and transitions' guards,
/// and thus the PG will always be in a consistent state.
///
/// The only way to execute the [`ProgramGraph`] is to generate a new [`ProgramGraphRun`] through [`ProgramGraph::new_instance`].
/// The [`ProgramGraphRun`] cannot outlive its [`ProgramGraph`], as it holds references to it.
/// This allows to cheaply generate multiple [`ProgramGraphRun`]s from the same [`ProgramGraph`].
///
/// Example:
///
/// ```
/// # use scan_core::program_graph::ProgramGraphBuilder;
/// # use rand::rngs::SmallRng;
/// # use rand::SeedableRng;
/// // Create and populate a PG builder object
/// let mut pg_builder = ProgramGraphBuilder::new();
/// let initial = pg_builder.new_initial_location();
/// pg_builder.add_autonomous_transition(initial, initial, None).expect("add transition");
///
/// // Build the builder object to get a PG definition object.
/// let pg_def = pg_builder.build();
///
/// // Instantiate a PG with the previously built definition.
/// let mut pg = pg_def.new_instance();
///
/// // Perform the (unique) active transition available.
/// let (e, mut post_locs) = pg.possible_transitions().last().expect("autonomous transition");
/// let post_loc = post_locs.last().expect("post location").last().expect("post location");
/// assert_eq!(post_loc, initial);
/// let mut rng: SmallRng = rand::make_rng();
/// pg.transition(e, &[initial], &mut rng).expect("transition is active");
/// ```
#[derive(Debug, Clone)]
pub struct ProgramGraph {
    initial_states: Vec<Location>,
    effects: Vec<Effect>,
    locations: Vec<LocationData>,
    // Time invariants of each location
    vars: Vec<Val>,
    // Number of clocks
    clocks: u16,
}

impl ProgramGraph {
    /// Creates a new [`ProgramGraphRun`] which allows to execute the PG as defined.
    ///
    /// The new instance borrows the caller to refer to the PG definition without copying its data,
    /// so that spawning instances is (relatively) inexpensive.
    pub fn new_instance<'def>(&'def self) -> ProgramGraphRun<'def> {
        ProgramGraphRun {
            current_states: self.initial_states.clone(),
            vars: self.vars.clone(),
            def: self,
            clocks: vec![0; self.clocks as usize],
            bump: Bump::new(),
        }
    }

    // Returns transition's guard.
    // Panics if the pre- or post-state do not exist.
    #[inline]
    fn guards(
        &self,
        pre_state: Location,
        action: Action,
        post_state: Location,
    ) -> impl Iterator<Item = (Option<&PgGuard>, &[TimeConstraint])> {
        let a_transitions = self.locations[pre_state.0 as usize].0.as_slice();
        a_transitions
            .binary_search_by_key(&action, |&(a, ..)| a)
            .into_iter()
            .flat_map(move |transitions_idx| {
                let post_idx_lb = a_transitions[transitions_idx]
                    .1
                    .partition_point(|&(p, ..)| p < post_state);
                a_transitions[transitions_idx].1[post_idx_lb..]
                    .iter()
                    .take_while(move |&&(p, ..)| p == post_state)
                    .map(|(_, g, c)| (g.as_ref(), c.as_slice()))
            })
    }
}

// WARN: This iterator allocates a [`Vec`] every time `next` is called.
// To avoid this, one way would be to allocate a vector at creation and return a reference to it
// for every iteration of next, but this requires "streaming iterators" or analogous solution.
// TODO: Find a way to implement preallocation (or no allocation).
struct TransitionsIterator<'a, I: Iterator<Item = (Action, &'a [Transition])>> {
    iters: bumpalo::collections::Vec<'a, I>,
    bump: &'a Bump,
}

impl<'a, I: Iterator<Item = (Action, &'a [Transition])>> Iterator for TransitionsIterator<'a, I> {
    type Item = (Action, bumpalo::collections::vec::Vec<'a, &'a [Transition]>);

    fn next(&mut self) -> Option<Self::Item> {
        let len = self.iters.len();
        let (mut first_a, first_t) = self.iters.first_mut().and_then(|it| it.next())?;
        let mut vals = bumpalo::vec![in self.bump; first_t; len];

        let mut total = 1;
        let mut i = 0;
        while total < len {
            i = (i + 1) % len;
            let (next_a, next_t) = self
                .iters
                .get_mut(i)
                .expect("i < len")
                .find(|(a, _)| *a >= first_a)?;
            if next_a > first_a {
                first_a = next_a;
                total = 1;
            } else {
                total += 1;
            }
            vals[i] = next_t;
        }
        Some((first_a, vals))
    }
}

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
    ) -> TransitionsIterator<'a, impl Iterator<Item = (Action, &'a [Transition])>> {
        let iters = self
            .current_states
            .iter()
            .map(|loc| {
                self.def.locations[loc.0 as usize]
                    .0
                    .iter()
                    .map(|(action, transitions)| (*action, transitions.as_slice()))
            })
            .collect_in::<bumpalo::collections::Vec<_>>(&self.bump);
        TransitionsIterator {
            iters,
            bump: &self.bump,
        }
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
                loc_transitions
                    .into_bump_slice()
                    .iter()
                    .map(move |transitions| {
                        transitions
                            .iter()
                            .filter_map(move |(post_state, guard, constraints)| {
                                self.check_transition(
                                    action,
                                    *post_state,
                                    guard.as_ref(),
                                    constraints,
                                )
                            })
                    }),
            )
        })
    }

    pub(crate) fn nosync_possible_transitions(
        &self,
    ) -> impl Iterator<Item = (Action, impl Iterator<Item = Location>)> {
        assert_eq!(self.current_states.len(), 1);
        let current_loc = self.current_states[0];
        self.def.locations[current_loc.0 as usize]
            .0
            .iter()
            .map(move |(action, transitions)| {
                (
                    *action,
                    transitions
                        .iter()
                        .filter_map(move |(post_state, guard, constraints)| {
                            self.check_transition(*action, *post_state, guard.as_ref(), constraints)
                        }),
                )
            })
    }

    #[inline]
    fn check_transition(
        &self,
        action: Action,
        post_state: Location,
        guard: Option<&BooleanExpr<Var>>,
        constraints: &[TimeConstraint],
    ) -> Option<Location> {
        let (_, ref invariants) = self.def.locations[post_state.0 as usize];
        if action != EPSILON
            && let Effect::Effects(_, ref resets) = self.def.effects[action.0 as usize]
        {
            self.active_transition(guard, constraints, invariants, resets)
        } else {
            self.active_autonomous_transition(guard, constraints, invariants)
        }
        .then_some(post_state)
    }

    #[inline]
    fn active_transition(
        &self,
        guard: Option<&PgGuard>,
        constraints: &[TimeConstraint],
        invariants: &[TimeConstraint],
        resets: &[Clock],
    ) -> bool {
        guard
            .is_none_or(|guard| guard.eval::<SmallRng>(&|var| self.vars[var.0 as usize], &mut None))
            && constraints.iter().all(|(c, l, u)| {
                let time = self.clocks[c.0 as usize];
                l.is_none_or(|l| l <= time) && u.is_none_or(|u| time < u)
            })
            && invariants.iter().all(|(c, l, u)| {
                let time = if resets.binary_search(c).is_ok() {
                    0
                } else {
                    self.clocks[c.0 as usize]
                };
                l.is_none_or(|l| l <= time) && u.is_none_or(|u| time < u)
            })
    }

    #[inline]
    fn active_autonomous_transition(
        &self,
        guard: Option<&PgGuard>,
        constraints: &[TimeConstraint],
        invariants: &[TimeConstraint],
    ) -> bool {
        guard
            .is_none_or(|guard| guard.eval::<SmallRng>(&|var| self.vars[var.0 as usize], &mut None))
            && constraints.iter().chain(invariants).all(|(c, l, u)| {
                let time = self.clocks[c.0 as usize];
                l.is_none_or(|l| l <= time) && u.is_none_or(|u| time < u)
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
        self.bump.reset();
        Ok(())
    }

    /// Checks if it is possible to wait a given amount of time-units without violating the time invariants.
    #[inline]
    pub fn can_wait(&self, delta: Time) -> bool {
        self.current_states
            .iter()
            .flat_map(|current_state| self.def.locations[current_state.0 as usize].1.iter())
            .all(|(c, l, u)| {
                // Invariants need to be satisfied during the whole wait.
                let start_time = self.clocks[c.0 as usize];
                let end_time = start_time + delta;
                l.is_none_or(|l| l <= start_time) && u.is_none_or(|u| end_time < u)
            })
    }

    /// Waits a given amount of time-units.
    ///
    /// Returns error if the waiting would violate the current location's time invariant (if any).
    #[inline]
    pub fn wait(&mut self, delta: Time) -> Result<(), PgError> {
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

#[cfg(test)]
mod tests {
    use rand::SeedableRng;
    use rand::rngs::SmallRng;

    use super::*;

    #[test]
    fn wait() {
        let mut builder = ProgramGraphBuilder::new();
        let _ = builder.new_initial_location();
        let pg_def = builder.build();
        let mut pg = pg_def.new_instance();
        assert_eq!(pg.possible_transitions().count(), 0);
        pg.wait(1).expect("wait 1 time unit");
    }

    #[test]
    fn transition() {
        let mut builder = ProgramGraphBuilder::new();
        let initial = builder.new_initial_location();
        let r#final = builder.new_location();
        builder
            .add_autonomous_transition(initial, r#final, None)
            .expect("add transition");
        let pg_def = builder.build();
        let mut pg = pg_def.new_instance();
        assert_eq!(pg.current_states(), &[initial]);
        {
            let mut possible_transitions = pg.possible_transitions();
            assert!(
                possible_transitions
                    .next()
                    .is_some_and(|(a, _)| a == EPSILON),
            );
            assert!(possible_transitions.next().is_none());
        }
        let mut rng = SmallRng::from_seed([0; 32]);
        pg.transition(EPSILON, &[r#final], &mut rng)
            .expect("transition to final");
        assert_eq!(pg.current_states(), &[r#final]);
        assert_eq!(pg.possible_transitions().count(), 0);
    }

    #[test]
    fn guard() {
        let mut builder = ProgramGraphBuilder::new();
        let initial = builder.new_initial_location();
        let r#final = builder.new_location();
        builder
            .add_autonomous_transition(initial, r#final, Some(BooleanExpr::Const(true)))
            .expect("add transition");
        builder
            .add_autonomous_transition(r#final, initial, Some(BooleanExpr::Const(false)))
            .expect("add transition");
        let pg_def = builder.build();
        let mut pg = pg_def.new_instance();
        let mut rng = SmallRng::from_seed([0; 32]);
        // It is possible to transition from initial to final
        pg.transition(EPSILON, &[r#final], &mut rng)
            .expect("transition to final");
        assert_eq!(pg.current_states(), &[r#final]);
        // It is not possible to transition from final to initial
        let mut possible_transitions = pg.possible_transitions();
        let (next_action, mut next_locations) = possible_transitions.next().unwrap();
        assert_eq!(next_action, EPSILON);
        let mut next_location = next_locations.next().unwrap();
        assert!(next_location.next().is_none());
        assert!(possible_transitions.next().is_none());
    }

    #[test]
    fn effect() {
        const TRESHOLD: Natural = 3;
        let mut builder = ProgramGraphBuilder::new();
        let initial = builder.new_initial_location();
        let r#final = builder.new_location();
        let var = builder.new_var(Val::from(0 as Natural));
        let action = builder.new_action();
        builder
            .add_effect(
                action,
                var,
                Expression::Natural(NaturalExpr::Var(var) + NaturalExpr::Const(1)),
            )
            .expect("add effect");
        builder
            .add_transition(
                initial,
                action,
                initial,
                Some(
                    Expression::from_var(var, Type::Natural)
                        .less_than(Expression::from(TRESHOLD))
                        .expect("boolean expression"),
                ),
            )
            .expect("add transition");
        builder
            .add_autonomous_transition(
                initial,
                r#final,
                Some(
                    Expression::from_var(var, Type::Natural)
                        .equal_to(Expression::from(TRESHOLD))
                        .expect("boolean expression"),
                ),
            )
            .expect("add transition");
        let pg_def = builder.build();
        let mut pg = pg_def.new_instance();
        let mut rng = SmallRng::from_seed([0; 32]);
        for _ in 0..TRESHOLD {
            assert_eq!(pg.current_states(), &[initial]);
            // It is not possible to transition from initial to final
            pg.transition(EPSILON, &[r#final], &mut rng)
                .expect_err("transition to final not possible");
            // It is possible to transition from initial to initial
            pg.transition(action, &[initial], &mut rng)
                .expect("transition to initial");
        }
        assert_eq!(pg.current_states(), &[initial]);
        // It is not possible to transition from initial to initial
        pg.transition(action, &[initial], &mut rng)
            .expect_err("transition to initial not possible");
        // It is possible to transition from initial to final
        pg.transition(EPSILON, &[r#final], &mut rng)
            .expect("transition to final");
        assert_eq!(pg.current_states(), &[r#final]);
    }

    #[test]
    fn program_graph() -> Result<(), PgError> {
        // Create Program Graph
        let mut builder = ProgramGraphBuilder::new();
        // Variables
        let mut rng = SmallRng::from_seed([0; 32]);
        let battery = builder.new_var(Val::from(0i64));
        // Locations
        let initial = builder.new_initial_location();
        let left = builder.new_location();
        let center = builder.new_location();
        let right = builder.new_location();
        // Actions
        let initialize = builder.new_action();
        builder.add_effect(initialize, battery, PgExpression::from(3i64))?;
        let move_left = builder.new_action();
        let discharge = PgExpression::Integer(IntegerExpr::Var(battery) + IntegerExpr::from(-1));
        builder.add_effect(move_left, battery, discharge.clone())?;
        let move_right = builder.new_action();
        builder.add_effect(move_right, battery, discharge)?;
        // Guards
        let out_of_charge =
            BooleanExpr::IntGreater(IntegerExpr::Var(battery), IntegerExpr::from(0i64));
        // Program graph definition
        builder.add_transition(initial, initialize, center, None)?;
        builder.add_transition(left, move_right, center, Some(out_of_charge.clone()))?;
        builder.add_transition(center, move_right, right, Some(out_of_charge.clone()))?;
        builder.add_transition(right, move_left, center, Some(out_of_charge.clone()))?;
        builder.add_transition(center, move_left, left, Some(out_of_charge))?;
        // Execution
        let pg_def = builder.build();
        let mut pg = pg_def.new_instance();
        assert_eq!(pg.possible_transitions().count(), 1);
        pg.transition(initialize, &[center], &mut rng)
            .expect("initialize");
        assert_eq!(pg.possible_transitions().count(), 2);
        pg.transition(move_right, &[right], &mut rng)
            .expect("move right");
        assert_eq!(pg.possible_transitions().count(), 1);
        pg.transition(move_right, &[right], &mut rng)
            .expect_err("already right");
        assert_eq!(pg.possible_transitions().count(), 1);
        pg.transition(move_left, &[center], &mut rng)
            .expect("move left");
        assert_eq!(pg.possible_transitions().count(), 2);
        pg.transition(move_left, &[left], &mut rng)
            .expect("move left");
        assert!(
            pg.possible_transitions()
                .next()
                .unwrap()
                .1
                .next()
                .unwrap()
                .next()
                .is_none()
        );
        pg.transition(move_left, &[left], &mut rng)
            .expect_err("battery = 0");
        Ok(())
    }
}
