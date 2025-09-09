use rand::{Rng, SeedableRng};

use crate::{
    Definition, DummyRng, TransitionSystem, Val,
    grammar::FnExpression,
    program_graph::{
        Action, PgError, PgExpression, ProgramGraph, ProgramGraphBuilder, ProgramGraphRun, Var,
    },
};

/// A [`ProgramGraph`]-based model implementing [`TransitionSystem`] with synchronous concurrency.
pub struct PgModel<R: Rng> {
    pg: ProgramGraph<R>,
    global_vars: Vec<Var>,
    predicates: Vec<FnExpression<Var, DummyRng>>,
}

impl<R: Rng + 'static> PgModel<R> {
    /// Create a new [`PgModel`] from the given [`ProgramGraph`] and predicates over its internal state.
    pub fn new(
        pg: ProgramGraphBuilder<R>,
        global_vars: Vec<Var>,
        predicates: Vec<PgExpression>,
    ) -> Self {
        let pg = pg.build();
        let predicates = predicates
            .into_iter()
            .map(Into::<FnExpression<Var, DummyRng>>::into)
            .collect();
        Self {
            pg,
            global_vars,
            predicates,
        }
    }
}

impl<R: Rng + SeedableRng + Clone + Send + Sync> Definition for PgModel<R> {
    type I<'def>
        = PgModelRun<'def, R>
    where
        R: 'def;

    fn new_instance<'def>(&'def self) -> PgModelRun<'def, R> {
        PgModelRun {
            pg: self.pg.new_instance(),
            rng: R::from_os_rng(),
            global_vars: &self.global_vars,
            predicates: &self.predicates,
        }
    }
}

/// A model based on a single [`ProgramGraph`],
/// with predicates over the PG's variables.
pub struct PgModelRun<'def, R: Rng> {
    pg: ProgramGraphRun<'def, R>,
    rng: R,
    global_vars: &'def [Var],
    predicates: &'def [FnExpression<Var, DummyRng>],
}

impl<'def, R: Rng + SeedableRng + Clone + Send + Sync> TransitionSystem<Action>
    for PgModelRun<'def, R>
{
    type Err = PgError;

    fn transition(&mut self, _duration: crate::Time) -> Result<Option<Action>, PgError> {
        Ok(self.pg.montecarlo(&mut self.rng))
    }

    fn time(&self) -> crate::Time {
        0
    }

    fn labels(&self) -> impl Iterator<Item = bool> {
        self.predicates.iter().map(|p| {
            if let Val::Boolean(b) = self.pg.eval(p) {
                b
            } else {
                panic!("non-bool pred")
            }
        })
    }

    fn state(&self) -> impl Iterator<Item = &Val> {
        self.global_vars
            .as_ref()
            .iter()
            .map(|var| self.pg.val(*var).expect("value"))
    }
}
