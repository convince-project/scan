//! Implementation of the PG model of computation.
//!
//! A _Program Graph_ is given by:
//!
//! - a finite set `L` of _locations_;
//! - a finite set `A` of _actions_;
//! - a finite set `V` of _typed variebles_;
//! - a _transition relation_ that associates pairs of locations (pre-location and post-location) and an action with a Boolean expression (the _guard_ of the transition);
//! - for each actions, a set of _effects_, i.e., a variabe `x` from `V` and an expression in the variables of `V` of the same type as `x`.
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
//! # use smallvec::smallvec;
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
//! let mut pg = pg_builder.build();
//!
//! // Execution starts in the initial location
//! assert_eq!(pg.current_states().as_slice(), &[initial_loc]);
//!
//! // Compute the possible transitions on the PG
//! {
//!     let mut iter = pg.possible_transitions();
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
//! let mut rng = SmallRng::from_os_rng();
//! let result = pg.transition(action, &[post_loc], &mut rng);
//!
//! // Performing a transition can fail, in particular, if the transition was not allowed
//! result.expect("The transition from the initial location onto itself is possible");
//!
//! // There are no more possible transitions
//! assert!(pg.possible_transitions().next().is_none());
//!
//! // Attempting to transition results in an error
//! pg.transition(action, &[post_loc], &mut rng).expect_err("The transition is not possible");
//! ```

mod builder;

use crate::{DummyRng, Time, grammar::*};
pub use builder::*;
use rand::Rng;
use smallvec::SmallVec;
use std::{collections::BTreeSet, sync::Arc};
use thiserror::Error;

/// An indexing object for locations in a PG.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ProgramGraphBuilder`] or [`ProgramGraph`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location(u16);

/// An indexing object for actions in a PG.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ProgramGraphBuilder`] or [`ProgramGraph`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Action(u16);

/// Epsilon action to enable autonomous transitions.
/// It cannot have effects.
const EPSILON: Action = Action(u16::MAX);

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

#[derive(Debug)]
enum FnEffect<R: Rng> {
    // NOTE: Could use a SmallVec for clock resets
    Effects(Vec<(Var, FnExpression<Var, R>)>, Vec<Clock>),
    Send(FnExpression<Var, R>),
    Receive(Var),
}

type Guard = FnExpression<Var, DummyRng>;

type Transition = (Action, Location, Option<Guard>, Vec<TimeConstraint>);

#[derive(Debug)]
struct ProgramGraphDef<R: Rng> {
    effects: Vec<FnEffect<R>>,
    locations: Vec<(Vec<Transition>, Vec<TimeConstraint>, BTreeSet<Action>)>,
}

impl<R: Rng> ProgramGraphDef<R> {
    // Returns transition's guard.
    // Panics if the pre- or post-state do not exist.
    // Returns error if the transition does not exist.
    #[inline(always)]
    fn guards(
        &self,
        pre_state: Location,
        action: Action,
        post_state: Location,
    ) -> impl Iterator<Item = (&Option<Guard>, &Vec<TimeConstraint>)> {
        let (transitions, ..) = &self.locations[pre_state.0 as usize];
        let part = transitions.partition_point(|(a, p, ..)| (*a, *p) < (action, post_state));
        transitions[part..]
            .iter()
            .take_while(move |(a, p, ..)| (*a, *p) == (action, post_state))
            .map(|(_, _, g, c)| (g, c))
    }
}

/// Representation of a PG that can be executed transition-by-transition.
///
/// The structure of the PG cannot be changed,
/// meaning that it is not possible to introduce new locations, actions, variables, etc.
/// Though, this restriction makes it so that cloning the [`ProgramGraph`] is cheap,
/// because only the internal state needs to be duplicated.
///
/// The only way to produce a [`ProgramGraph`] is through a [`ProgramGraphBuilder`].
/// This guarantees that there are no type errors involved in the definition of action's effects and transitions' guards,
/// and thus the PG will always be in a consistent state.
#[derive(Clone)]
pub struct ProgramGraph<R: Rng> {
    current_states: SmallVec<[Location; 8]>,
    vars: Vec<Val>,
    clocks: Vec<Time>,
    def: Arc<ProgramGraphDef<R>>,
    buf: BTreeSet<Action>,
}

impl<R: Rng> ProgramGraph<R> {
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
    /// # use rand::rngs::SmallRng;
    /// let mut pg = pg_builder.build::<SmallRng>();
    ///
    /// // Execution starts in the initial location
    /// assert_eq!(pg.current_states().as_slice(), &[initial_loc]);
    /// ```
    #[inline(always)]
    pub fn current_states(&self) -> &SmallVec<[Location; 8]> {
        &self.current_states
    }

    #[inline(always)]
    fn update_buf(&mut self) {
        if self.current_states.len() > 1 {
            self.buf = &self.def.locations[self.current_states[0].0 as usize].2
                & &self.def.locations[self.current_states[1].0 as usize].2;
            for loc in &self.current_states[2..] {
                self.buf = &self.buf & &self.def.locations[loc.0 as usize].2;
            }
        }
    }

    /// Iterates over all transitions that can be admitted in the current state.
    ///
    /// An admittable transition is characterized by the required action and the post-state
    /// (the pre-state being necessarily the current state of the machine).
    /// The guard (if any) is guaranteed to be satisfied.
    pub fn possible_transitions(
        &self,
    ) -> impl Iterator<
        Item = (
            Action,
            impl Iterator<Item = impl Iterator<Item = Location> + use<'_, R>> + use<'_, R>,
        ),
    > + use<'_, R> {
        if self.current_states.len() == 1 {
            &self.def.locations[self.current_states[0].0 as usize].2
        } else {
            &self.buf
        }
        .iter()
        .map(|action| (*action, self.possible_transitions_action(*action)))
    }

    #[inline(always)]
    fn possible_transitions_action(
        &self,
        action: Action,
    ) -> impl Iterator<Item = impl Iterator<Item = Location> + use<'_, R>> + use<'_, R> {
        self.current_states
            .iter()
            .map(move |loc| self.possible_transitions_action_loc(action, *loc))
    }

    fn possible_transitions_action_loc(
        &self,
        action: Action,
        current_state: Location,
    ) -> impl Iterator<Item = Location> + use<'_, R> {
        let ppoint = self.def.locations[current_state.0 as usize]
            .0
            .partition_point(|(a, ..)| *a < action);
        let mut last_post_state: Option<Location> = None;
        self.def.locations[current_state.0 as usize].0[ppoint..]
            .iter()
            .take_while(move |(a, ..)| *a == action)
            .filter_map(move |(_, post_state, guard, constraints)| {
                // post_states could be duplicated waistfully
                if last_post_state.is_some_and(|s| s == *post_state) {
                    return None;
                }
                let (_, ref invariants, _) = self.def.locations[post_state.0 as usize];
                if if action == EPSILON {
                    self.active_autonomous_transition(guard.as_ref(), constraints, invariants)
                } else {
                    match self.def.effects[action.0 as usize] {
                        FnEffect::Effects(_, ref resets) => {
                            self.active_transition(guard.as_ref(), constraints, invariants, resets)
                        }
                        FnEffect::Send(_) | FnEffect::Receive(_) => self
                            .active_autonomous_transition(guard.as_ref(), constraints, invariants),
                    }
                } {
                    last_post_state = Some(*post_state);
                    last_post_state
                } else {
                    None
                }
            })
    }

    fn active_transition(
        &self,
        guard: Option<&Guard>,
        constraints: &[TimeConstraint],
        invariants: &[TimeConstraint],
        resets: &[Clock],
    ) -> bool {
        guard.is_none_or(|guard| {
            // TODO FIXME: is there a way to avoid creating a dummy RNG?
            if let Val::Boolean(pass) =
                guard.eval(&|var| self.vars[var.0 as usize].clone(), &mut DummyRng)
            {
                pass
            } else {
                panic!("guard is not a boolean");
            }
        }) && constraints.iter().all(|(c, l, u)| {
            let time = self.clocks[c.0 as usize];
            l.is_none_or(|l| l <= time) && u.is_none_or(|u| time < u)
        }) && invariants.iter().all(|(c, l, u)| {
            let time = if resets.binary_search(c).is_ok() {
                0
            } else {
                self.clocks[c.0 as usize]
            };
            l.is_none_or(|l| l <= time) && u.is_none_or(|u| time < u)
        })
    }

    fn active_autonomous_transition(
        &self,
        guard: Option<&Guard>,
        constraints: &[TimeConstraint],
        invariants: &[TimeConstraint],
    ) -> bool {
        guard.is_none_or(|guard| {
            // TODO FIXME: is there a way to avoid creating a dummy RNG?
            if let Val::Boolean(pass) =
                guard.eval(&|var| self.vars[var.0 as usize].clone(), &mut DummyRng)
            {
                pass
            } else {
                panic!("guard is not a boolean");
            }
        }) && constraints.iter().chain(invariants).all(|(c, l, u)| {
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
                            guard.as_ref(),
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
                            guard.as_ref(),
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
    pub fn transition(
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
            .find(|ps| ps.0 >= self.def.locations.len() as u16)
        {
            return Err(PgError::MissingLocation(*ps));
        }
        if action == EPSILON {
            if !self.active_autonomous_transitions(post_states) {
                return Err(PgError::UnsatisfiedGuard);
            }
        } else if action.0 >= self.def.effects.len() as u16 {
            return Err(PgError::MissingAction(action));
        } else if let FnEffect::Effects(ref effects, ref resets) =
            self.def.effects[action.0 as usize]
        {
            if self.active_transitions(action, post_states, resets) {
                effects.iter().for_each(|(var, effect)| {
                    self.vars[var.0 as usize] =
                        effect.eval(&|var| self.vars[var.0 as usize].clone(), rng)
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
        self.update_buf();
        Ok(())
    }

    /// Checks if it is possible to wait a given amount of time-units without violating the time invariants.
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
    pub fn wait(&mut self, delta: Time) -> Result<(), PgError> {
        if self.can_wait(delta) {
            self.clocks.iter_mut().for_each(|t| *t += delta);
            Ok(())
        } else {
            Err(PgError::Invariant)
        }
    }

    pub(crate) fn send<'a>(
        &'a mut self,
        action: Action,
        post_states: &[Location],
        rng: &'a mut R,
    ) -> Result<Val, PgError> {
        if action == EPSILON {
            Err(PgError::NotSend(action))
        } else if self.active_transitions(action, post_states, &[]) {
            if let FnEffect::Send(effect) = &self.def.effects[action.0 as usize] {
                let val = effect.eval(&|var| self.vars[var.0 as usize].clone(), rng);
                self.current_states.copy_from_slice(post_states);
                // self.current_states = post_states;
                self.update_buf();
                Ok(val)
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
        val: Val,
    ) -> Result<(), PgError> {
        if action == EPSILON {
            Err(PgError::NotReceive(action))
        } else if self.active_transitions(action, post_states, &[]) {
            if let FnEffect::Receive(var) = self.def.effects[action.0 as usize] {
                let var_content = self.vars.get_mut(var.0 as usize).expect("variable exists");
                if var_content.r#type() == val.r#type() {
                    *var_content = val;
                    self.current_states.copy_from_slice(post_states);
                    // self.current_states = post_states;
                    self.update_buf();
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
    use rand::rngs::mock::StepRng;

    use super::*;

    #[test]
    fn wait() {
        let mut builder = ProgramGraphBuilder::new();
        let _ = builder.new_initial_location();
        let mut pg = builder.build::<SmallRng>();
        assert_eq!(pg.possible_transitions().count(), 0);
        pg.wait(1).expect("wait 1 time unit");
    }

    #[test]
    fn transitions() {
        let mut builder = ProgramGraphBuilder::new();
        let initial = builder.new_initial_location();
        let r#final = builder.new_location();
        let action = builder.new_action();
        builder
            .add_transition(initial, action, r#final, None)
            .expect("add transition");
        let mut pg = builder.build();
        assert_eq!(pg.current_states().as_slice(), &[initial]);
        // assert_eq!(
        //     pg.possible_transitions().collect::<Vec<_>>(),
        //     vec![(
        //         action,
        //         SmallVec::<[_; 4]>::from(vec![SmallVec::<[_; 8]>::from(vec![r#final])])
        //     )]
        // );
        let mut rng = SmallRng::from_seed([0; 32]);
        pg.transition(action, &[r#final], &mut rng)
            .expect("transition to final");
        assert_eq!(pg.possible_transitions().count(), 0);
    }

    #[test]
    fn program_graph() -> Result<(), PgError> {
        // Create Program Graph
        let mut builder = ProgramGraphBuilder::new();
        let mut rng = StepRng::new(0, 1);
        // Variables
        let battery = builder.new_var_with_rng(Expression::Const(Val::Integer(0)), &mut rng)?;
        // Locations
        let initial = builder.new_initial_location();
        let left = builder.new_location();
        let center = builder.new_location();
        let right = builder.new_location();
        // Actions
        let initialize = builder.new_action();
        builder.add_effect(initialize, battery, PgExpression::Const(Val::Integer(3)))?;
        let move_left = builder.new_action();
        let discharge = PgExpression::Sum(vec![
            PgExpression::Var(battery, Type::Integer),
            PgExpression::Const(Val::Integer(-1)),
        ]);
        builder.add_effect(move_left, battery, discharge.clone())?;
        let move_right = builder.new_action();
        builder.add_effect(move_right, battery, discharge)?;
        // Guards
        let out_of_charge = PgExpression::Greater(Box::new((
            PgExpression::Var(battery, Type::Integer),
            PgExpression::Const(Val::Integer(0)),
        )));
        // Program graph definition
        builder.add_transition(initial, initialize, center, None)?;
        builder.add_transition(left, move_right, center, Some(out_of_charge.clone()))?;
        builder.add_transition(center, move_right, right, Some(out_of_charge.clone()))?;
        builder.add_transition(right, move_left, center, Some(out_of_charge.clone()))?;
        builder.add_transition(center, move_left, left, Some(out_of_charge))?;
        // Execution
        let mut pg = builder.build();
        assert_eq!(pg.possible_transitions().count(), 1);
        let mut rng = SmallRng::from_seed([0; 32]);
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
