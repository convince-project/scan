use super::*;
use crate::grammar::*;
use log::info;
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
enum Effect {
    Effects(Vec<(Var, PgExpression)>),
    Send(PgExpression),
    Receive(Var),
}

impl From<Effect> for FnEffect {
    fn from(value: Effect) -> Self {
        match value {
            Effect::Effects(effects) => FnEffect::Effects(
                effects
                    .into_iter()
                    .map(|(var, expr)| -> (Var, FnExpression) { (var, expr.into()) })
                    .collect(),
            ),
            Effect::Send(msg) => FnEffect::Send(msg.into()),
            Effect::Receive(var) => FnEffect::Receive(var),
        }
    }
}

// WARN: Can produce FnExpression's that will panic when computed if passed a badly-typed expresstion.
// NOTE: There is no way to ensure a correct conversion a-priori because we don't know the type of variables here.
// TODO: This should probably become a method in ProgramGraphBuilder that does proper checks.
impl From<PgExpression> for FnExpression {
    fn from(value: PgExpression) -> Self {
        FnExpression(match value {
            PgExpression::Const(val) => Box::new(move |_| val.to_owned()),
            PgExpression::Var(var) => Box::new(move |vars: &[Val]| vars[var.0].to_owned()),
            PgExpression::Tuple(exprs) => {
                let exprs: Vec<FnExpression> = exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars: &[Val]| {
                    Val::Tuple(exprs.iter().map(|expr| expr.eval(vars)).collect::<Vec<_>>())
                })
            }
            PgExpression::Component(index, expr) => {
                let expr = Into::<FnExpression>::into(*expr).0;
                Box::new(move |vars: &[Val]| {
                    if let Val::Tuple(vals) = expr(vars) {
                        vals[index].to_owned()
                    } else {
                        panic!();
                    }
                })
            }
            PgExpression::And(exprs) => {
                let exprs: Vec<FnExpression> = exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars: &[Val]| {
                    Val::Boolean(exprs.iter().all(|expr| {
                        if let Val::Boolean(b) = expr.eval(vars) {
                            b
                        } else {
                            panic!()
                        }
                    }))
                })
            }
            PgExpression::Or(exprs) => {
                let exprs: Vec<FnExpression> = exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars: &[Val]| {
                    Val::Boolean(exprs.iter().any(|expr| {
                        if let Val::Boolean(b) = expr.eval(vars) {
                            b
                        } else {
                            panic!()
                        }
                    }))
                })
            }
            PgExpression::Implies(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Boolean(lhs), Val::Boolean(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(rhs || !lhs)
                    } else {
                        panic!()
                    }
                })
            }
            PgExpression::Not(expr) => {
                let expr = FnExpression::from(*expr);
                Box::new(move |vars: &[Val]| {
                    if let Val::Boolean(b) = expr.eval(vars) {
                        Val::Boolean(!b)
                    } else {
                        panic!()
                    }
                })
            }
            PgExpression::Opposite(expr) => {
                let expr = FnExpression::from(*expr);
                Box::new(move |vars: &[Val]| {
                    if let Val::Integer(i) = expr.eval(vars) {
                        Val::Integer(-i)
                    } else {
                        panic!()
                    }
                })
            }
            PgExpression::Sum(exprs) => {
                let exprs: Vec<FnExpression> = exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars: &[Val]| {
                    Val::Integer(
                        exprs
                            .iter()
                            .map(|expr| {
                                if let Val::Integer(i) = expr.eval(vars) {
                                    i
                                } else {
                                    panic!()
                                }
                            })
                            .sum(),
                    )
                })
            }
            PgExpression::Mult(exprs) => {
                let exprs: Vec<FnExpression> = exprs.into_iter().map(FnExpression::from).collect();
                Box::new(move |vars: &[Val]| {
                    Val::Integer(
                        exprs
                            .iter()
                            .map(|expr| {
                                if let Val::Integer(i) = expr.eval(vars) {
                                    i
                                } else {
                                    panic!()
                                }
                            })
                            .product(),
                    )
                })
            }
            PgExpression::Equal(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(lhs == rhs)
                    } else {
                        panic!()
                    }
                })
            }
            PgExpression::Greater(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(lhs > rhs)
                    } else {
                        panic!()
                    }
                })
            }
            PgExpression::GreaterEq(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(lhs >= rhs)
                    } else {
                        panic!()
                    }
                })
            }
            PgExpression::Less(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(lhs < rhs)
                    } else {
                        panic!()
                    }
                })
            }
            PgExpression::LessEq(exprs) => {
                let (lhs, rhs) = *exprs;
                let lhs = FnExpression::from(lhs);
                let rhs = FnExpression::from(rhs);
                Box::new(move |vars: &[Val]| {
                    if let (Val::Integer(lhs), Val::Integer(rhs)) = (lhs.eval(vars), rhs.eval(vars))
                    {
                        Val::Boolean(lhs <= rhs)
                    } else {
                        panic!()
                    }
                })
            }
        })
    }
}

/// Defines and builds a PG.
#[derive(Debug, Clone)]
pub struct ProgramGraphBuilder {
    // Effects are indexed by actions
    effects: Vec<Effect>,
    // Transitions are indexed by locations
    // We can assume there is at most one condition by logical disjunction
    transitions: Vec<HashMap<(Action, Location), Option<PgExpression>>>,
    vars: Vec<Val>,
}

impl Default for ProgramGraphBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramGraphBuilder {
    const INITIAL_LOCATION: Location = Location(0);

    /// Creates a new [`ProgramGraphBuilder`].
    /// At creation, this will only have the inital location with no variables, no actions and no transitions.
    /// The initial location can be retreived by [`ProgramGraphBuilder::initial_location`]
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

    /// Gets the initial location of the PG.
    /// This is created toghether with the [`ProgramGraphBuilder`] by default.
    pub fn initial_location(&self) -> Location {
        Self::INITIAL_LOCATION
    }

    // Gets the type of a variable.
    pub(crate) fn var_type(&self, var: Var) -> Result<Type, PgError> {
        self.vars
            .get(var.0)
            .map(Val::r#type)
            .ok_or(PgError::MissingVar(var))
    }

    /// Adds a new variable with the given initial value (and the inferred type) to the PG.
    ///
    /// It fails if the expression giving the initial value of the variable is well-typed.
    ///
    /// ```
    /// # use scan_core::program_graph::{PgExpression, ProgramGraphBuilder};
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // Create a new action
    /// let action = pg_builder.new_action();
    ///
    /// // Create a new variable
    /// pg_builder
    ///     .new_var(PgExpression::And(vec![PgExpression::from(0)]))
    ///     .expect_err("expression is badly-typed");
    /// ```
    pub fn new_var(&mut self, init: PgExpression) -> Result<Var, PgError> {
        let idx = self.vars.len();
        // We check the type to make sure the expression is well-formed
        let _ = self.r#type(&init)?;
        let val = FnExpression::from(init).eval(&self.vars);
        self.vars.push(val);
        Ok(Var(idx))
    }

    /// Adds a new action to the PG.
    pub fn new_action(&mut self) -> Action {
        // Actions are indexed progressively
        let idx = self.effects.len();
        self.effects.push(Effect::Effects(Vec::new()));
        Action(idx)
    }

    /// Adds an effect to the given action.
    /// Requires specifying which variable is assigned the value of which expression whenever the action triggers a transition.
    ///
    /// It fails if the type of the variable and that of the expression do not match.
    ///
    /// ```
    /// # use scan_core::program_graph::{PgExpression, ProgramGraphBuilder};
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // Create a new action
    /// let action = pg_builder.new_action();
    ///
    /// // Create a new variable
    /// let var = pg_builder.new_var(PgExpression::from(true)).expect("expression is well-typed");
    ///
    /// // Add an effect to the action
    /// pg_builder
    ///     .add_effect(action, var, PgExpression::from(1))
    ///     .expect_err("var is of type bool but expression is of type integer");
    /// ```
    pub fn add_effect(
        &mut self,
        action: Action,
        var: Var,
        effect: PgExpression,
    ) -> Result<(), PgError> {
        let var_type = self
            .vars
            .get(var.0)
            .map(Val::r#type)
            .ok_or_else(|| PgError::MissingVar(var.to_owned()))?;
        if var_type == self.r#type(&effect)? {
            match self
                .effects
                .get_mut(action.0)
                .ok_or(PgError::MissingAction(action))?
            {
                Effect::Effects(effects) => {
                    effects.push((var, effect));
                    Ok(())
                }
                Effect::Send(_) => Err(PgError::EffectOnSend),
                Effect::Receive(_) => Err(PgError::EffectOnReceive),
            }
        } else {
            Err(PgError::TypeMismatch)
        }
    }

    pub(crate) fn new_send(&mut self, msg: PgExpression) -> Result<Action, PgError> {
        // Actions are indexed progressively
        let _ = self.r#type(&msg)?;
        let idx = self.effects.len();
        self.effects.push(Effect::Send(msg));
        Ok(Action(idx))
    }

    pub(crate) fn new_receive(&mut self, var: Var) -> Result<Action, PgError> {
        if self.vars.len() <= var.0 {
            Err(PgError::MissingVar(var.to_owned()))
        } else {
            // Actions are indexed progressively
            let idx = self.effects.len();
            self.effects.push(Effect::Receive(var));
            Ok(Action(idx))
        }
    }

    /// Adds a new location to the PG.
    pub fn new_location(&mut self) -> Location {
        // Locations are indexed progressively
        let idx = self.transitions.len();
        self.transitions.push(HashMap::new());
        Location(idx)
    }

    /// Adds a transition to the PG.
    /// Requires specifying:
    ///
    /// - state pre-transition,
    /// - action triggering the transition,
    /// - state post-transition, and
    /// - (optionally) boolean expression guarding the transition.
    ///
    /// Fails if the provided guard is not a boolean expression.
    ///
    /// ```
    /// # use scan_core::program_graph::{PgExpression, ProgramGraphBuilder};
    /// # let mut pg_builder = ProgramGraphBuilder::new();
    /// // The builder is initialized with an initial location
    /// let initial_loc = pg_builder.initial_location();
    ///
    /// // Create a new action
    /// let action = pg_builder.new_action();
    ///
    /// // Add a transition
    /// pg_builder
    ///     .add_transition(initial_loc, action, initial_loc, Some(PgExpression::from(1)))
    ///     .expect_err("the guard expression is not boolean");
    /// ```
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

    // Computes the type of an expression.
    // Fails if the expression is badly typed,
    // e.g., if variables in it have type incompatible with the expression.
    pub(crate) fn r#type(&self, expr: &PgExpression) -> Result<Type, PgError> {
        match expr {
            PgExpression::Const(val) => Ok(val.r#type()),
            PgExpression::Tuple(tuple) => Ok(Type::Product(
                tuple
                    .iter()
                    .map(|e| self.r#type(e))
                    .collect::<Result<Vec<Type>, PgError>>()?,
            )),
            PgExpression::Var(var) => self
                .vars
                .get(var.0)
                .map(Val::r#type)
                .ok_or_else(|| PgError::MissingVar(var.to_owned())),
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

    /// Produces a [`ProgramGraph`] defined by the [`ProgramGraphBuilder`]'s data and consuming it.
    ///
    /// Since the construction of the builder is already checked ad every step,
    /// this method cannot fail.
    pub fn build(mut self) -> ProgramGraph {
        // Since vectors of effects and transitions will become unmutable,
        // they should be shrunk to take as little space as possible
        self.effects.shrink_to_fit();
        self.transitions.shrink_to_fit();
        // Vars are not going to be unmutable,
        // but their number will be constant anyway
        self.vars.shrink_to_fit();
        // Build program graph
        info!(
            "create Program Graph with:\n{} locations\n{} actions\n{} vars",
            self.transitions.len(),
            self.effects.len(),
            self.vars.len()
        );
        ProgramGraph {
            current_location: Self::INITIAL_LOCATION,
            vars: self.vars,
            effects: Rc::new(self.effects.into_iter().map(FnEffect::from).collect()),
            transitions: Rc::new(
                self.transitions
                    .into_iter()
                    .map(|effects| {
                        effects
                            .into_iter()
                            .map(|(p, expr)| (p, expr.map(FnExpression::from)))
                            .collect()
                    })
                    .collect(),
            ),
        }
    }
}
