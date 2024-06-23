use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use scan_core::program_graph::*;
use scan_core::*;

#[inline(always)]
fn run_to_completion(mut pg: ProgramGraph) {
    while let Some((action, post)) = pg.possible_transitions().last() {
        assert!(pg.transition(action, post).is_ok());
    }
    // pg.possible_transitions().last()
}

#[inline(always)]
fn simple_pg() -> ProgramGraph {
    let mut pg = ProgramGraphBuilder::new();
    let pre = pg.initial_location();
    let action = pg.new_action();
    let post = pg.new_location();
    pg.add_transition(pre, action, post, None).unwrap();
    pg.build()
}

#[inline(always)]
fn condition_pg() -> ProgramGraph {
    let mut pg = ProgramGraphBuilder::new();
    let pre = pg.initial_location();
    let action = pg.new_action();
    let post = pg.new_location();
    pg.add_transition(
        pre,
        action,
        post,
        Some(Expression::Implies(Box::new((
            Expression::LessEq(Box::new((
                Expression::Sum(vec![
                    Expression::Integer(1),
                    Expression::Integer(2),
                    Expression::Integer(3),
                    Expression::Integer(4),
                    Expression::Integer(5),
                ]),
                Expression::Integer(100),
            ))),
            Expression::Greater(Box::new((Expression::Integer(5), Expression::Integer(6)))),
        )))),
    )
    .unwrap();
    pg.build()
}

#[inline(always)]
fn long_pg() -> ProgramGraph {
    let mut pg = ProgramGraphBuilder::new();
    let mut pre = pg.initial_location();
    let action = pg.new_action();
    for _ in 0..10 {
        let post = pg.new_location();
        pg.add_transition(pre, action, post, None).unwrap();
        pre = post;
    }
    pg.build()
}

#[inline(always)]
fn counter_pg() -> ProgramGraph {
    let mut pg = ProgramGraphBuilder::new();
    let initial = pg.initial_location();
    let action = pg.new_action();
    let var = pg.new_var(Type::Integer);
    pg.add_effect(
        action,
        var,
        Expression::Sum(vec![Expression::Var(var), Expression::Integer(1)]),
    )
    .unwrap();
    for counter in 0..10 {
        let guard = Expression::Equal(Box::new((
            Expression::Var(var),
            Expression::Integer(counter),
        )));
        pg.add_transition(initial, action, initial, Some(guard))
            .unwrap();
    }
    pg.build()
}

fn possible_transitions(c: &mut Criterion) {
    let pgs = [
        (simple_pg(), "simple pg"),
        (condition_pg(), "condition pg"),
        (long_pg(), "long pg"),
        (counter_pg(), "counter pg"),
    ];
    for (pg, name) in pgs.iter() {
        c.bench_with_input(
            BenchmarkId::new("possible transitions", name),
            pg,
            |b, pg| {
                b.iter(|| pg.possible_transitions().count());
            },
        );
    }
}

fn run(c: &mut Criterion) {
    let pgs = [
        (simple_pg(), "simple pg"),
        (condition_pg(), "condition pg"),
        (long_pg(), "long pg"),
        (counter_pg(), "counter pg"),
    ];
    for (pg, name) in pgs.iter() {
        c.bench_with_input(
            BenchmarkId::new("execute to termination", name),
            pg,
            |b, pg| {
                b.iter(|| run_to_completion(pg.clone()));
            },
        );
    }
}

criterion_group!(benches, possible_transitions, run);
criterion_main!(benches);
