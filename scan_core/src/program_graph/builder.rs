use super::{
    Action, Clock, FnEffect, FnExpression, Location, PgError, PgExpression, ProgramGraph,
    TimeConstraint, Var, EPSILON, TIME, WAIT,
};
use crate::{
    grammar::{Type, Val},
    program_graph::ProgramGraphDef,
    Integer,
};
// use ahash::{AHashMap as HashMap, AHashSet as HashSet};
use hashbrown::{HashMap, HashSet};
use log::info;
use std::sync::Arc;

#[derive(Debug, Clone)]
enum Effect {
    Effects(Vec<(Var, PgExpression)>),
    Send(PgExpression),
    Receive(Var),
}

impl From<Effect> for FnEffect {
    fn from(value: Effect) -> Self {
        match value {
            Effect::Effects(effects) => {
                let mut effects = effects
                    .into_iter()
                    .map(|(var, expr)| -> (Var, FnExpression<Var>) {
                        (var, FnExpression::<Var>::from(expr))
                    })
                    .collect::<Vec<_>>();
                effects.shrink_to_fit();
                FnEffect::Effects(effects)
            }
            Effect::Send(msg) => FnEffect::Send(msg.into()),
            Effect::Receive(var) => FnEffect::Receive(var),
        }
    }
}

/// Defines and builds a PG.
#[derive(Debug, Clone)]
pub struct ProgramGraphBuilder {
    // Effects are indexed by actions
    effects: Vec<Effect>,
    guards: HashMap<(Location, Action, Location), PgExpression>,
    // Transitions are indexed by locations
    // We can assume there is at most one condition by logical disjunction
    transitions: HashSet<(Location, Action, Location)>,
    indexed_transitions: Vec<Vec<(Action, Location)>>,
    vars: Vec<Val>,
}

impl Default for ProgramGraphBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramGraphBuilder {
    const INITIAL: Location = Location(0);

    /// Creates a new [`ProgramGraphBuilder`].
    /// At creation, this will only have the inital location with no variables, no actions and no transitions.
    /// The initial location can be retreived by [`ProgramGraphBuilder::initial_location`]
    pub fn new() -> Self {
        let mut pgb = Self {
            effects: Vec::new(),
            guards: HashMap::new(),
            transitions: HashSet::new(),
            vars: Vec::new(),
            indexed_transitions: Vec::new(),
        };
        // Create an initial location and make sure it is equal to the constant `Self::INITIAL_LOCATION`
        // This is the simplest way to make sure the state of the builder is always consistent
        let initial_location = pgb.new_location();
        assert_eq!(initial_location, Self::INITIAL);

        let time = pgb.new_clock();
        assert_eq!(time, TIME);

        pgb
    }

    /// Gets the initial location of the PG.
    /// This is created toghether with the [`ProgramGraphBuilder`] by default.
    pub fn initial_location(&self) -> Location {
        Self::INITIAL
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
    /// It fails if the expression giving the initial value of the variable is not well-typed.
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
        let _ = init.r#type().map_err(PgError::Type)?;
        init.context(&|var| self.vars.get(var.0).map(Val::r#type))
            .map_err(PgError::Type)?;
        let val = FnExpression::from(init).eval(&|var| self.vars[var.0].clone());
        self.vars.push(val);
        Ok(Var(idx))
    }

    pub fn new_clock(&mut self) -> Clock {
        let idx = self.vars.len();
        self.vars.push(Val::Integer(0));
        Clock(idx)
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
        if action == EPSILON || action == WAIT {
            return Err(PgError::NoEffects);
        }
        effect
            .context(&|var| self.vars.get(var.0).map(Val::r#type))
            .map_err(PgError::Type)?;
        let var_type = self
            .vars
            .get(var.0)
            .map(Val::r#type)
            .ok_or_else(|| PgError::MissingVar(var.to_owned()))?;
        if var_type == effect.r#type().map_err(PgError::Type)? {
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

    pub fn reset_clock(&mut self, action: Action, clock: Clock) -> Result<(), PgError> {
        if clock == TIME {
            // return an error
            Err(PgError::TimeClock)
        } else {
            self.add_effect(
                action,
                Var(clock.0),
                PgExpression::Var(Var(TIME.0), Type::Integer),
            )
        }
    }

    pub(crate) fn new_send(&mut self, msg: PgExpression) -> Result<Action, PgError> {
        // Check message is well-typed
        msg.context(&|var| self.vars.get(var.0).map(Val::r#type))
            .map_err(PgError::Type)?;
        let _ = msg.r#type().map_err(PgError::Type)?;
        // Actions are indexed progressively
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
        let idx = self.indexed_transitions.len();
        self.indexed_transitions.push(Vec::new());
        let loc = Location(idx);
        self.add_transition(loc, WAIT, loc, None)
            .expect("add wait transition");
        loc
    }

    /// TODO
    pub fn new_timed_location(&mut self, invariants: &[TimeConstraint]) -> Location {
        // Locations are indexed progressively
        let idx = self.indexed_transitions.len();
        self.indexed_transitions.push(Vec::new());
        let loc = Location(idx);
        self.add_timed_transition(loc, WAIT, loc, None, invariants)
            .expect("add wait transition");
        loc
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
    ///     .add_transition(initial_loc, action, initial_loc, None)
    ///     .expect("this transition can be added");
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
        if self.indexed_transitions.len() <= pre.0 {
            Err(PgError::MissingLocation(pre))
        } else if self.indexed_transitions.len() <= post.0 {
            Err(PgError::MissingLocation(post))
        } else if action != EPSILON && action != WAIT && self.effects.len() <= action.0 {
            // Check 'action' exists
            Err(PgError::MissingAction(action))
        } else if guard
            .as_ref()
            .is_some_and(|guard| !matches!(guard.r#type(), Ok(Type::Boolean)))
        {
            Err(PgError::TypeMismatch)
        } else {
            if let Some(guard) = guard {
                guard
                    .context(&|var| self.vars.get(var.0).map(Val::r#type))
                    .map_err(PgError::Type)?;
                let _ = self
                    .guards
                    .entry((pre, action, post))
                    .and_modify(|previous_guard| {
                        if let PgExpression::Or(exprs) = previous_guard {
                            exprs.push(guard.to_owned());
                        } else {
                            *previous_guard =
                                PgExpression::Or(vec![previous_guard.to_owned(), guard.to_owned()]);
                        }
                    })
                    .or_insert(guard);
            }
            if self.transitions.insert((pre, action, post)) {
                self.indexed_transitions[pre.0].push((action, post));
            }
            Ok(())
        }
    }

    /// TODO
    pub fn add_timed_transition(
        &mut self,
        pre: Location,
        action: Action,
        post: Location,
        guard: Option<PgExpression>,
        constraints: &[TimeConstraint],
    ) -> Result<(), PgError> {
        let time_constraints = constraints
            .iter()
            .flat_map(|(clock, lower_bound, upper_bound)| {
                let lower_bound = lower_bound.map(|lower_bound| {
                    PgExpression::LessEq(Box::new((
                        PgExpression::Const(Val::Integer(lower_bound as Integer)),
                        PgExpression::Sum(vec![
                            PgExpression::Var(Var(TIME.0), Type::Integer),
                            PgExpression::Opposite(Box::new(PgExpression::Var(
                                Var(clock.0),
                                Type::Integer,
                            ))),
                        ]),
                    )))
                });
                let upper_bound = upper_bound.map(|upper_bound| {
                    PgExpression::LessEq(Box::new((
                        PgExpression::Sum(vec![
                            PgExpression::Var(Var(TIME.0), Type::Integer),
                            PgExpression::Opposite(Box::new(PgExpression::Var(
                                Var(clock.0),
                                Type::Integer,
                            ))),
                        ]),
                        PgExpression::Const(Val::Integer(upper_bound as Integer)),
                    )))
                });
                lower_bound.into_iter().chain(upper_bound)
            });

        let guard = PgExpression::And(time_constraints.chain(guard).collect());

        self.add_transition(pre, action, post, Some(guard))
    }

    /// Adds an autonomous transition to the PG, i.e., a transition enabled by the epsilon action.
    /// Requires specifying:
    ///
    /// - state pre-transition,
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
    /// // Add a transition
    /// pg_builder
    ///     .add_autonomous_transition(initial_loc, initial_loc, None)
    ///     .expect("this autonomous transition can be added");
    /// pg_builder
    ///     .add_autonomous_transition(initial_loc, initial_loc, Some(PgExpression::from(1)))
    ///     .expect_err("the guard expression is not boolean");
    /// ```
    pub fn add_autonomous_transition(
        &mut self,
        pre: Location,
        post: Location,
        guard: Option<PgExpression>,
    ) -> Result<(), PgError> {
        self.add_transition(pre, EPSILON, post, guard)
    }

    pub fn add_autonomous_timed_transition(
        &mut self,
        pre: Location,
        post: Location,
        guard: Option<PgExpression>,
        constraints: &[TimeConstraint],
    ) -> Result<(), PgError> {
        self.add_timed_transition(pre, EPSILON, post, guard, constraints)
    }

    /// Produces a [`ProgramGraph`] defined by the [`ProgramGraphBuilder`]'s data and consuming it.
    ///
    /// Since the construction of the builder is already checked ad every step,
    /// this method cannot fail.
    pub fn build(mut self) -> ProgramGraph {
        // Since vectors of effects and transitions will become unmutable,
        // they should be shrunk to take as little space as possible
        self.effects.shrink_to_fit();
        self.guards.shrink_to_fit();
        self.transitions.shrink_to_fit();
        self.indexed_transitions.shrink_to_fit();
        self.indexed_transitions
            .iter_mut()
            .for_each(|v| v.shrink_to_fit());
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
        let def = ProgramGraphDef {
            effects: self.effects.into_iter().map(FnEffect::from).collect(),
            guards: self
                .guards
                .into_iter()
                .map(|(t, g)| (t, FnExpression::from(g)))
                .collect(),
            transitions: self.transitions,
            indexed_transitions: self.indexed_transitions,
        };
        ProgramGraph {
            current_location: Self::INITIAL,
            vars: self.vars,
            def: Arc::new(def),
        }
    }
}
