use rand::{SeedableRng, rngs::SmallRng};

use crate::{
    Expression, Time, TransitionSystem, Val,
    program_graph::{
        Action, PgExpression, ProgramGraph, ProgramGraphBuilder, ProgramGraphRun, Var,
    },
    transition_system::TransitionSystemGenerator,
};

/// A [`ProgramGraph`]-based model implementing [`TransitionSystem`] with synchronous concurrency.
#[derive(Debug, Clone)]
pub struct PgModel {
    pg: ProgramGraph,
    global_vars: Vec<Var>,
    predicates: Vec<Expression<Var>>,
}

impl PgModel {
    /// Create a new [`PgModel`] from the given [`ProgramGraph`] and predicates over its internal state.
    pub fn new(
        pg: ProgramGraphBuilder,
        global_vars: Vec<Var>,
        predicates: Vec<PgExpression>,
    ) -> Self {
        let pg = pg.build();
        Self {
            pg,
            global_vars,
            predicates,
        }
    }
}

impl TransitionSystemGenerator for PgModel {
    type Ts<'a>
        = PgModelRun<'a>
    where
        Self: 'a;

    fn generate<'a>(&'a self) -> Self::Ts<'a> {
        PgModelRun {
            pg: self.pg.new_instance(),
            rng: SmallRng::from_os_rng(),
            global_vars: &self.global_vars,
            predicates: &self.predicates,
            time: 0,
        }
    }
}

/// A model based on a single [`ProgramGraph`],
/// with predicates over the PG's variables.
#[derive(Debug, Clone)]
pub struct PgModelRun<'def> {
    pg: ProgramGraphRun<'def>,
    rng: SmallRng,
    global_vars: &'def [Var],
    predicates: &'def [Expression<Var>],
    time: Time,
}

impl<'def> TransitionSystem for PgModelRun<'def> {
    type Event = Action;

    fn transition(&mut self) -> Option<Action> {
        self.pg.montecarlo(&mut self.rng)
    }

    fn time(&self) -> Time {
        self.time
    }

    fn time_tick(&mut self) {
        self.time += 1;
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

    fn state(&self) -> impl Iterator<Item = Val> {
        self.global_vars
            .as_ref()
            .iter()
            .map(|var| self.pg.val(*var).expect("value"))
    }
}
