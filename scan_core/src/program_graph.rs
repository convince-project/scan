//! Implementation of the PG model of computation.
//!
//! A PG is defined through a [`ProgramGraphBuilder`],
//! by adding new locations, actions, effects, guards and transitions.
//! Then, a [`ProgramGraph`] is built from the [`ProgramGraphBuilder`]
//! and can be executed by performing transitions,
//! though the definition of the PG itself can no longer be altered.

// TODO: use fast hasher (?)
use super::grammar::*;
use std::{collections::HashMap, rc::Rc};
use thiserror::Error;

// Use of "Newtype" pattern to define different types of indexes.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Location(usize);

// Use of "Newtype" pattern to define different types of indexes.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Action(usize);

// Use of "Newtype" pattern to define different types of indexes.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Var(usize);

pub type PgExpression = Expression<Var>;

#[derive(Debug, Clone, Error)]
pub enum PgError {
    #[error("malformed expression {0:?}")]
    BadExpression(PgExpression),
    #[error("action {0:?} does not belong to this program graph")]
    MissingAction(Action),
    #[error("location {0:?} does not belong to this program graph")]
    MissingLocation(Location),
    #[error("type mismatch")]
    TypeMismatch,
    #[error("location {0:?} does not belong to this program graph")]
    NonExistingVar(Var),
    #[error("There is no such transition")]
    NoTransition,
    #[error("The guard has not been satisfied")]
    UnsatisfiedGuard,
    #[error("the tuple has no {0} component")]
    MissingComponent(usize),
}

#[derive(Debug, Clone)]
pub struct ProgramGraphBuilder {
    // Effects are indexed by actions
    effects: Vec<Vec<(Var, PgExpression)>>,
    // Transitions are indexed by locations
    // We can assume there is at most one condition by logical disjunction
    transitions: Vec<HashMap<(Action, Location), Option<PgExpression>>>,
    vars: Vec<Type>,
}

impl Default for ProgramGraphBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramGraphBuilder {
    const INITIAL_LOCATION: Location = Location(0);

    pub fn new() -> Self {
        let mut pgb = Self {
            effects: Vec::new(),
            transitions: Vec::new(),
            vars: Vec::new(),
        };
        // Create an initial location and make sure it is equal to the constant `Self::INITIAL_LOCATION`
        // This is the simplest way to make sure the state of the builder is always consistent
        let initial_location = pgb.new_location();
        assert_eq!(initial_location, Self::INITIAL_LOCATION);
        pgb
    }

    pub fn var_type(&self, var: Var) -> Result<Type, PgError> {
        self.vars
            .get(var.0)
            .ok_or(PgError::NonExistingVar(var))
            .cloned()
    }

    pub fn new_var(&mut self, var_type: Type) -> Var {
        let idx = self.vars.len();
        self.vars.push(var_type);
        Var(idx)
    }

    pub fn new_action(&mut self) -> Action {
        // Actions are indexed progressively
        let idx = self.effects.len();
        self.effects.push(Vec::new());
        Action(idx)
    }

    pub fn add_effect(
        &mut self,
        action: Action,
        var: Var,
        effect: PgExpression,
    ) -> Result<(), PgError> {
        let var_type = self
            .vars
            .get(var.0)
            .ok_or_else(|| PgError::NonExistingVar(var.to_owned()))?;
        if *var_type == self.r#type(&effect)? {
            self.effects
                .get_mut(action.0)
                .ok_or(PgError::MissingAction(action))
                .map(|effects| effects.push((var, effect)))
        } else {
            Err(PgError::TypeMismatch)
        }
    }

    pub fn new_location(&mut self) -> Location {
        // Locations are indexed progressively
        let idx = self.transitions.len();
        self.transitions.push(HashMap::new());
        Location(idx)
    }

    pub fn add_transition(
        &mut self,
        pre: Location,
        action: Action,
        post: Location,
        guard: Option<PgExpression>,
    ) -> Result<(), PgError> {
        // Check 'pre' and 'post' locations exists
        if self.transitions.len() <= pre.0 {
            Err(PgError::MissingLocation(pre))
        } else if self.transitions.len() <= post.0 {
            Err(PgError::MissingLocation(post))
        } else if self.effects.len() <= action.0 {
            // Check 'action' exists
            Err(PgError::MissingAction(action))
        } else if guard.is_some() && !matches!(self.r#type(guard.as_ref().unwrap())?, Type::Boolean)
        {
            Err(PgError::TypeMismatch)
        } else {
            let _ = self.transitions[pre.0]
                .entry((action, post))
                .and_modify(|previous_guard| {
                    if let Some(guard) = guard.to_owned() {
                        if let Some(previous_guard) = previous_guard {
                            if let PgExpression::Or(exprs) = previous_guard {
                                exprs.push(guard.to_owned());
                            } else {
                                *previous_guard = PgExpression::Or(vec![
                                    previous_guard.to_owned(),
                                    guard.to_owned(),
                                ]);
                            }
                        } else {
                            *previous_guard = Some(guard);
                        }
                    }
                })
                .or_insert(guard);
            Ok(())
        }
    }

    pub fn r#type(&self, expr: &PgExpression) -> Result<Type, PgError> {
        match expr {
            PgExpression::Boolean(_) => Ok(Type::Boolean),
            PgExpression::Integer(_) => Ok(Type::Integer),
            PgExpression::Tuple(tuple) => Ok(Type::Product(
                tuple
                    .iter()
                    .map(|e| self.r#type(e))
                    .collect::<Result<Vec<Type>, PgError>>()?,
            )),
            PgExpression::Var(var) => self
                .vars
                .get(var.0)
                .cloned()
                .ok_or_else(|| PgError::NonExistingVar(var.to_owned())),
            PgExpression::And(props) | PgExpression::Or(props) => {
                if props
                    .iter()
                    .map(|prop| self.r#type(prop))
                    .collect::<Result<Vec<Type>, PgError>>()?
                    .iter()
                    .all(|prop| matches!(prop, Type::Boolean))
                {
                    Ok(Type::Boolean)
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Implies(props) => {
                if matches!(self.r#type(&props.0)?, Type::Boolean)
                    && matches!(self.r#type(&props.1)?, Type::Boolean)
                {
                    Ok(Type::Boolean)
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Not(prop) => {
                if matches!(self.r#type(prop)?, Type::Boolean) {
                    Ok(Type::Boolean)
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Opposite(expr) => {
                if matches!(self.r#type(expr)?, Type::Integer) {
                    Ok(Type::Integer)
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Sum(exprs) | PgExpression::Mult(exprs) => {
                if exprs
                    .iter()
                    .map(|expr| self.r#type(expr))
                    .collect::<Result<Vec<Type>, PgError>>()?
                    .iter()
                    .all(|expr| matches!(expr, Type::Integer))
                {
                    Ok(Type::Integer)
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Equal(exprs)
            | PgExpression::Greater(exprs)
            | PgExpression::GreaterEq(exprs)
            | PgExpression::Less(exprs)
            | PgExpression::LessEq(exprs) => {
                if matches!(self.r#type(&exprs.0)?, Type::Integer)
                    && matches!(self.r#type(&exprs.1)?, Type::Integer)
                {
                    Ok(Type::Boolean)
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Component(index, expr) => {
                if let Type::Product(components) = self.r#type(expr)? {
                    components
                        .get(*index)
                        .cloned()
                        .ok_or(PgError::MissingComponent(*index))
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
        }
    }

    pub fn build(mut self) -> ProgramGraph {
        // Since vectors of effects and transitions will become unmutable,
        // they should be shrunk to take as little space as possible
        self.effects.iter_mut().for_each(Vec::shrink_to_fit);
        self.effects.shrink_to_fit();
        self.transitions.shrink_to_fit();
        // Vars are not going to be unmutable,
        // but their number will be constant anyway
        self.vars.shrink_to_fit();
        // Build program graph
        ProgramGraph {
            current_location: Self::INITIAL_LOCATION,
            vars: self.vars.iter().map(Type::default_value).collect(),
            effects: Rc::new(self.effects),
            transitions: Rc::new(self.transitions),
        }
    }

    pub fn initial_location(&self) -> Location {
        Self::INITIAL_LOCATION
    }
}

#[derive(Debug, Clone)]
pub struct ProgramGraph {
    current_location: Location,
    vars: Vec<Val>,
    // TODO: use SmallVec optimization
    effects: Rc<Vec<Vec<(Var, PgExpression)>>>,
    transitions: Rc<Vec<HashMap<(Action, Location), Option<PgExpression>>>>,
}

impl ProgramGraph {
    pub fn possible_transitions(&self) -> impl Iterator<Item = (Action, Location)> + '_ {
        self.transitions[self.current_location.0]
            .iter()
            .filter_map(|((action, post), guard)| {
                if let Some(guard) = guard {
                    if let Val::Boolean(pass) = self.eval(guard).expect("guard must evaluate") {
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

    pub fn transition(&mut self, action: Action, post_state: Location) -> Result<(), PgError> {
        let guard = self.transitions[self.current_location.0]
            .get(&(action, post_state))
            .ok_or(PgError::NoTransition)?;
        if guard.as_ref().map_or(true, |guard| {
            if let Val::Boolean(pass) = self.eval(guard).expect("guard must evaluate") {
                pass
            } else {
                panic!("guard is not a boolean");
            }
        }) {
            for (var, effect) in &self.effects[action.0] {
                // Not using the 'Self::assign' method because:
                // - borrow checker
                // - effects are validated before, so no need to type-check again
                self.vars[var.0] = self
                    .eval(effect)
                    .expect("effect has already been validated");
            }
            self.current_location = post_state;
            Ok(())
        } else {
            Err(PgError::UnsatisfiedGuard)
        }
    }

    pub(super) fn eval(&self, expr: &PgExpression) -> Result<Val, PgError> {
        match expr {
            PgExpression::Boolean(b) => Ok(Val::Boolean(*b)),
            PgExpression::Integer(i) => Ok(Val::Integer(*i)),
            PgExpression::Var(var) => self
                .vars
                .get(var.0)
                .ok_or_else(|| PgError::NonExistingVar(var.to_owned()))
                .cloned(),
            PgExpression::Tuple(entries) => entries
                .iter()
                .map(|e| self.eval(e))
                .collect::<Result<Vec<Val>, PgError>>()
                .map(Val::Tuple),
            PgExpression::Component(index, expr) => {
                if let Val::Tuple(components) = self.eval(expr)? {
                    components
                        .get(*index)
                        .cloned()
                        .ok_or(PgError::MissingComponent(*index))
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::And(props) => Ok(Val::Boolean(
                props
                    .iter()
                    .map(|prop| {
                        if let Val::Boolean(val) = self.eval(prop)? {
                            Ok(val)
                        } else {
                            Err(PgError::TypeMismatch)
                        }
                    })
                    .collect::<Result<Vec<bool>, PgError>>()?
                    .iter()
                    .all(|val| *val),
            )),
            PgExpression::Or(props) => Ok(Val::Boolean(
                props
                    .iter()
                    .map(|prop| {
                        if let Val::Boolean(val) = self.eval(prop)? {
                            Ok(val)
                        } else {
                            Err(PgError::TypeMismatch)
                        }
                    })
                    .collect::<Result<Vec<bool>, PgError>>()?
                    .iter()
                    .any(|val| *val),
            )),
            PgExpression::Implies(props) => {
                if let (Val::Boolean(lhs), Val::Boolean(rhs)) =
                    (self.eval(&props.0)?, self.eval(&props.1)?)
                {
                    Ok(Val::Boolean(rhs || !lhs))
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Not(prop) => {
                if let Val::Boolean(arg) = self.eval(prop)? {
                    Ok(Val::Boolean(!arg))
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Opposite(expr) => {
                if let Val::Integer(arg) = self.eval(expr)? {
                    Ok(Val::Integer(-arg))
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Sum(exprs) => Ok(Val::Integer(
                exprs
                    .iter()
                    .map(|prop| {
                        if let Val::Integer(val) = self.eval(prop)? {
                            Ok(val)
                        } else {
                            Err(PgError::TypeMismatch)
                        }
                    })
                    .collect::<Result<Vec<Integer>, PgError>>()?
                    .iter()
                    .sum(),
            )),
            PgExpression::Mult(exprs) => Ok(Val::Integer(
                exprs
                    .iter()
                    .map(|prop| {
                        if let Val::Integer(val) = self.eval(prop)? {
                            Ok(val)
                        } else {
                            Err(PgError::TypeMismatch)
                        }
                    })
                    .collect::<Result<Vec<Integer>, PgError>>()?
                    .iter()
                    .fold(1, |tot, val| tot * *val),
            )),
            PgExpression::Equal(exprs) => {
                if let (Val::Integer(lhs), Val::Integer(rhs)) =
                    (self.eval(&exprs.0)?, self.eval(&exprs.1)?)
                {
                    Ok(Val::Boolean(lhs == rhs))
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Greater(exprs) => {
                if let (Val::Integer(lhs), Val::Integer(rhs)) =
                    (self.eval(&exprs.0)?, self.eval(&exprs.1)?)
                {
                    Ok(Val::Boolean(lhs > rhs))
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::GreaterEq(exprs) => {
                if let (Val::Integer(lhs), Val::Integer(rhs)) =
                    (self.eval(&exprs.0)?, self.eval(&exprs.1)?)
                {
                    Ok(Val::Boolean(lhs >= rhs))
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::Less(exprs) => {
                if let (Val::Integer(lhs), Val::Integer(rhs)) =
                    (self.eval(&exprs.0)?, self.eval(&exprs.1)?)
                {
                    Ok(Val::Boolean(lhs < rhs))
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
            PgExpression::LessEq(exprs) => {
                if let (Val::Integer(lhs), Val::Integer(rhs)) =
                    (self.eval(&exprs.0)?, self.eval(&exprs.1)?)
                {
                    Ok(Val::Boolean(lhs <= rhs))
                } else {
                    Err(PgError::TypeMismatch)
                }
            }
        }
    }

    pub(super) fn assign(&mut self, var: Var, val: Val) -> Result<Val, PgError> {
        let var_content = self
            .vars
            .get_mut(var.0)
            .ok_or_else(|| PgError::NonExistingVar(var.to_owned()))?;
        if var_content.r#type() == val.r#type() {
            let previous_val = var_content.clone();
            *var_content = val;
            Ok(previous_val)
        } else {
            Err(PgError::TypeMismatch)
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
        let battery = builder.new_var(Type::Integer);
        // Locations
        let initial = builder.initial_location();
        let left = builder.new_location();
        let center = builder.new_location();
        let right = builder.new_location();
        // Actions
        let initialize = builder.new_action();
        builder.add_effect(initialize, battery, PgExpression::Integer(3))?;
        let move_left = builder.new_action();
        builder.add_effect(
            move_left,
            battery,
            PgExpression::Sum(vec![
                PgExpression::Var(battery),
                // PgExpression::Opposite(Box::new(PgExpression::Const(Val::Integer(1)))),
                PgExpression::Integer(-1),
            ]),
        )?;
        let move_right = builder.new_action();
        builder.add_effect(
            move_right,
            battery,
            PgExpression::Sum(vec![
                PgExpression::Var(battery),
                // PgExpression::Opposite(Box::new(PgExpression::Const(Val::Integer(1)))),
                PgExpression::Integer(-1),
            ]),
        )?;
        // Guards
        let out_of_charge = PgExpression::Greater(Box::new((
            PgExpression::Var(battery),
            PgExpression::Integer(0),
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
