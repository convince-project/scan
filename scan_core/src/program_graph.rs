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
mod run;
mod transitions;

use crate::{TimeRange, grammar::*};
pub use builder::*;
pub use run::ProgramGraphRun;
use thiserror::Error;
use transitions::TransitionsIterator;

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

/// An expression using PG's [`Var`] as variables.
pub type PgExpression = Expression<Var>;

/// A Boolean expression over [`Var`] variables.
type PgGuard = BooleanExpr<Var>;

#[derive(Debug, Clone)]
enum Effect {
    Effects(Vec<(Var, Expression<Var>)>, Vec<Clock>),
    Send(Vec<Expression<Var>>),
    Receive(Vec<Var>),
}

type Transition = (Location, Option<BooleanExpr<Var>>, Vec<(Clock, TimeRange)>);

type LocationData = (Vec<(Action, Vec<Transition>)>, Vec<(Clock, TimeRange)>);

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
    /// Program graph is a synchronous composition
    #[error("synchronous composition")]
    Sync,
    /// A type error
    #[error("type error")]
    Type(#[source] TypeError),
}

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
        ProgramGraphRun::new(self)
    }

    // Returns transition's guard.
    // Panics if the pre- or post-state do not exist.
    #[inline]
    fn guards(
        &self,
        pre_state: Location,
        action: Action,
        post_state: Location,
    ) -> impl Iterator<Item = (Option<&PgGuard>, &[(Clock, TimeRange)])> {
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
