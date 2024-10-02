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
//! # use scan_core::program_graph::ProgramGraphBuilder;
//! // Create a new PG builder
//! let mut pg_builder = ProgramGraphBuilder::new();
//!
//! // The builder is initialized with an initial location
//! let initial_loc = pg_builder.initial_location();
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
//! assert_eq!(pg.current_location(), initial_loc);
//!
//! // Compute the possible transitions on the PG
//! assert_eq!(Vec::from_iter(pg.possible_transitions()), vec![(action, post_loc)]);
//!
//! // Perform a transition
//! let result = pg.transition(action, post_loc);
//!
//! // Performing a transition can fail, in particular, if the transition was not allowed
//! result.expect("The transition from the initial location onto itself is possible");
//!
//! // There are no more possible transitions
//! assert!(pg.possible_transitions().next().is_none());
//!
//! // Attempting to transition results in an error
//! pg.transition(action, post_loc).expect_err("The transition is not possible");
//! ```

mod builder;

pub use builder::*;

// TODO: use fast hasher (?)
use super::grammar::*;
use std::{collections::HashMap, sync::Arc};
use thiserror::Error;

/// An indexing object for locations in a PG.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ProgramGraphBuilder`] or [`ProgramGraph`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Location(usize);

/// An indexing object for actions in a PG.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ProgramGraphBuilder`] or [`ProgramGraph`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Action(usize);

/// Epsilon action to enable autonomous transitions.
const EPSILON: Action = Action(usize::MAX);

/// An indexing object for typed variables in a PG.
///
/// These cannot be directly created or manipulated,
/// but have to be generated and/or provided by a [`ProgramGraphBuilder`] or [`ProgramGraph`].
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Var(usize);

impl ValsContainer<Var> for Vec<Val> {
    fn value(&self, var: Var) -> Option<Val> {
        self.get(var.0).cloned()
    }
}

/// An expression using PG's [`Var`] as variables.
pub type PgExpression = Expression<Var>;

/// The error type for operations with [`ProgramGraphBuilder`]s and [`ProgramGraph`]s.
#[derive(Debug, Clone, Error)]
pub enum PgError {
    /// The expression is badly typed.
    #[error("malformed expression {0:?}")]
    BadExpression(PgExpression),
    /// There is no such action in the PG.
    #[error("action {0:?} does not belong to this program graph")]
    MissingAction(Action),
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
    /// The action is a not a Send communication.
    #[error("{0:?} is a not a Send communication")]
    NotSend(Action),
    /// The action is a not a Receive communication.
    #[error("{0:?} is a not a Receive communication")]
    NotReceive(Action),
    /// The epsilon action has no effects.
    #[error("The epsilon action has no effects")]
    EpsilonEffects,
}

// type FnExpr = Box<dyn Fn(&[Val]) -> Val + Send + Sync>;

// struct FnExpression(FnExpr);

// impl std::fmt::Debug for FnExpression {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "Expression")
//     }
// }

// impl FnExpression {
//     fn eval(&self, vals: &[Val]) -> Val {
//         self.0(vals)
//     }
// }

#[derive(Debug)]
enum FnEffect {
    // TODO: use SmallVec optimization
    // NOTE: SmallVec here would not appear in public API
    Effects(Vec<(Var, FnExpression<Vec<Val>>)>),
    Send(FnExpression<Vec<Val>>),
    Receive(Var),
}

// QUESTION: is there a better/more efficient representation?
type Transitions = HashMap<(Action, Location), Option<FnExpression<Vec<Val>>>>;

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
#[derive(Clone, Debug)]
pub struct ProgramGraph {
    current_location: Location,
    vars: Vec<Val>,
    effects: Arc<Vec<FnEffect>>,
    transitions: Arc<Vec<Transitions>>,
}

impl ProgramGraph {
    /// Returns the current location.
    ///
    /// ```
    /// # use scan_core::program_graph::ProgramGraphBuilder;
    /// // Create a new PG builder
    /// let mut pg_builder = ProgramGraphBuilder::new();
    ///
    /// // The builder is initialized with an initial location
    /// let initial_loc = pg_builder.initial_location();
    ///
    /// // Build the PG from its builder
    /// // The builder is always guaranteed to build a well-defined PG and building cannot fail
    /// let mut pg = pg_builder.build();
    ///
    /// // Execution starts in the initial location
    /// assert_eq!(pg.current_location(), initial_loc);
    /// ```
    pub fn current_location(&self) -> Location {
        self.current_location
    }

    /// Iterates over all transitions that can be admitted in the current state.
    ///
    /// An admittable transition is characterized by the required action and the post-state
    /// (the pre-state being necessarily the current state of the machine).
    /// The guard (if any) is guaranteed to be satisfied.
    pub fn possible_transitions(&self) -> impl Iterator<Item = (Action, Location)> + '_ {
        self.transitions[self.current_location.0]
            .iter()
            .filter_map(|((action, post), guard)| {
                if let Some(guard) = guard {
                    if let Some(Val::Boolean(pass)) = guard.eval(&self.vars) {
                        if pass {
                            Some((*action, *post))
                        } else {
                            None
                        }
                    } else {
                        panic!("guard is not a boolean");
                    }
                } else {
                    Some((*action, *post))
                }
            })
    }

    fn satisfies_guard(&self, action: Action, post_state: Location) -> Result<(), PgError> {
        let guard = self.transitions[self.current_location.0]
            .get(&(action, post_state))
            .ok_or(PgError::MissingTransition)?;
        if guard.as_ref().map_or(true, |guard| {
            if let Val::Boolean(pass) = guard.eval(&self.vars).expect("evaluation must succeed") {
                pass
            } else {
                panic!("guard is not a boolean");
            }
        }) {
            Ok(())
        } else {
            Err(PgError::UnsatisfiedGuard)
        }
    }

    /// Executes a transition characterized by the argument action and post-state.
    ///
    /// Fails if the requested transition is not admissible.
    pub fn transition(&mut self, action: Action, post_state: Location) -> Result<(), PgError> {
        self.satisfies_guard(action, post_state)?;
        if action != EPSILON {
            if let FnEffect::Effects(effects) = &self.effects[action.0] {
                for (var, effect) in effects {
                    self.vars[var.0] = effect.eval(&self.vars).expect("evaluation must succeed");
                }
            } else {
                return Err(PgError::Communication(action));
            }
        }
        self.current_location = post_state;
        Ok(())
    }

    pub(crate) fn send(&mut self, action: Action, post_state: Location) -> Result<Val, PgError> {
        self.satisfies_guard(action, post_state)?;
        if let FnEffect::Send(effect) = &self.effects[action.0] {
            let val = effect.eval(&self.vars).expect("evaluation must succeed");
            self.current_location = post_state;
            Ok(val)
        } else {
            Err(PgError::NotSend(action))
        }
    }

    pub(crate) fn receive(
        &mut self,
        action: Action,
        post_state: Location,
        val: Val,
    ) -> Result<Val, PgError> {
        self.satisfies_guard(action, post_state)?;
        if let FnEffect::Receive(var) = &self.effects[action.0] {
            let var_content = self
                .vars
                .get_mut(var.0)
                .ok_or_else(|| PgError::MissingVar(var.to_owned()))?;
            if var_content.r#type() == val.r#type() {
                let previous_val = var_content.clone();
                *var_content = val;
                self.current_location = post_state;
                Ok(previous_val)
            } else {
                Err(PgError::TypeMismatch)
            }
        } else {
            Err(PgError::NotReceive(action))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transitions() -> Result<(), PgError> {
        let mut builder = ProgramGraphBuilder::new();
        let initial = builder.initial_location();
        let r#final = builder.new_location();
        let action = builder.new_action();
        builder.add_transition(initial, action, r#final, None)?;
        let mut pg = builder.build();
        assert_eq!(
            pg.possible_transitions().collect::<Vec<_>>(),
            vec![(action, r#final)]
        );
        pg.transition(action, r#final)?;
        assert_eq!(pg.possible_transitions().count(), 0);
        Ok(())
    }

    #[test]
    fn program_graph() -> Result<(), PgError> {
        // Create Program Graph
        let mut builder = ProgramGraphBuilder::new();
        // Variables
        let battery = builder.new_var(Expression::Const(Val::Integer(0)))?;
        // Locations
        let initial = builder.initial_location();
        let left = builder.new_location();
        let center = builder.new_location();
        let right = builder.new_location();
        // Actions
        let initialize = builder.new_action();
        builder.add_effect(initialize, battery, PgExpression::Const(Val::Integer(3)))?;
        let move_left = builder.new_action();
        builder.add_effect(
            move_left,
            battery,
            PgExpression::Sum(vec![
                PgExpression::Var(battery),
                // PgExpression::Opposite(Box::new(PgExpression::Const(Val::Integer(1)))),
                PgExpression::Const(Val::Integer(-1)),
            ]),
        )?;
        let move_right = builder.new_action();
        builder.add_effect(
            move_right,
            battery,
            PgExpression::Sum(vec![
                PgExpression::Var(battery),
                // PgExpression::Opposite(Box::new(PgExpression::Const(Val::Integer(1)))),
                PgExpression::Const(Val::Integer(-1)),
            ]),
        )?;
        // Guards
        let out_of_charge = PgExpression::Greater(Box::new((
            PgExpression::Var(battery),
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
        pg.transition(initialize, center)?;
        assert_eq!(pg.possible_transitions().count(), 2);
        pg.transition(move_right, right)?;
        assert_eq!(pg.possible_transitions().count(), 1);
        pg.transition(move_right, right).expect_err("already right");
        assert_eq!(pg.possible_transitions().count(), 1);
        pg.transition(move_left, center)?;
        assert_eq!(pg.possible_transitions().count(), 2);
        pg.transition(move_left, left)?;
        assert_eq!(pg.possible_transitions().count(), 0);
        pg.transition(move_left, left).expect_err("battery = 0");
        Ok(())
    }
}
