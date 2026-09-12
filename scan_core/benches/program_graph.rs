use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use rand::rngs::SmallRng;
use rand::{SeedableRng, make_rng};
use scan_core::program_graph::*;
use scan_core::*;

fn autonomous_transition(c: &mut Criterion) {
    let mut pg = ProgramGraphBuilder::new();
    let pre = pg.new_initial_location();
    let post = pg.new_location();
    pg.add_autonomous_transition(pre, post, None).unwrap();
    let pg = pg.build();
    let mut run = pg.new_instance();
    c.bench_function("possible transitions", |b| {
        b.iter(|| {
            run.possible_transitions().for_each(|(_, trs)| {
                trs.for_each(|post| {
                    let _ = post.count();
                })
            })
        })
    });
    c.bench_function("nosync possible transitions", |b| {
        b.iter(|| {
            run.nosync_possible_transitions()
                .unwrap()
                .for_each(|(_, post)| {
                    let _ = post.count();
                })
        })
    });
    let mut rng: SmallRng = make_rng();
    let (action, _) = run.nosync_possible_transitions().unwrap().next().unwrap();
    c.bench_function("transition", |b| {
        b.iter(|| run.transition(action, &[post], &mut rng))
    });
}

fn guard(c: &mut Criterion) {
    let mut pg = ProgramGraphBuilder::new();
    let pre = pg.new_initial_location();
    let post = pg.new_location();
    // The goal is to measure transition time, not expr-evaluation time, so we use trivial guard
    pg.add_autonomous_transition(pre, post, Some(BooleanExpr::Const(true)))
        .unwrap();
    let pg = pg.build();
    let mut run = pg.new_instance();
    c.bench_function("possible transitions with guard", |b| {
        b.iter(|| {
            run.possible_transitions().for_each(|(_, trs)| {
                trs.for_each(|post| {
                    let _ = post.count();
                })
            })
        })
    });
    c.bench_function("nosync possible transitions with guard", |b| {
        b.iter(|| {
            run.nosync_possible_transitions()
                .unwrap()
                .for_each(|(_, post)| {
                    let _ = post.count();
                })
        })
    });
    let mut rng: SmallRng = make_rng();
    let (action, _) = run.nosync_possible_transitions().unwrap().next().unwrap();
    c.bench_function("transition with guard", |b| {
        b.iter(|| run.transition(action, &[post], &mut rng))
    });
}

fn effect(c: &mut Criterion) {
    let mut pg = ProgramGraphBuilder::new();
    let pre = pg.new_initial_location();
    let post = pg.new_location();
    let var = pg.new_var(Val::from(false));
    let action = pg.new_action();
    pg.add_effect(action, var, Expression::from(true)).unwrap();
    // The goal is to measure transition time, not expr-evaluation time, so we use trivial guard
    pg.add_transition(pre, action, post, None).unwrap();
    let pg = pg.build();
    let mut run = pg.new_instance();
    c.bench_function("possible transitions with effect", |b| {
        b.iter(|| {
            run.possible_transitions().for_each(|(_, trs)| {
                trs.for_each(|post| {
                    let _ = post.count();
                })
            })
        })
    });
    c.bench_function("nosync possible transitions with effect", |b| {
        b.iter(|| {
            run.nosync_possible_transitions()
                .unwrap()
                .for_each(|(_, post)| {
                    let _ = post.count();
                })
        })
    });
    let mut rng: SmallRng = make_rng();
    let (action, _) = run.nosync_possible_transitions().unwrap().next().unwrap();
    c.bench_function("transition with effect", |b| {
        b.iter(|| run.transition(action, &[post], &mut rng))
    });
}

criterion_group!(small_benches, autonomous_transition, guard, effect);
criterion_main!(small_benches);

#[inline(always)]
fn run_to_completion(pg: &mut ProgramGraphRun) {
    let mut rng = SmallRng::from_seed([0; 32]);

    while let Some((action, post)) = pg
        .possible_transitions()
        .filter_map(|(a, iter)| {
            iter.into_iter()
                .map(|v| v.last())
                .collect::<Option<Vec<_>>>()
                .map(|l| (a, l))
        })
        .last()
    {
        assert!(pg.transition(action, post.as_slice(), &mut rng).is_ok());
    }
}

#[inline(always)]
fn simple_pg() -> ProgramGraphBuilder {
    let mut pg = ProgramGraphBuilder::new();
    let pre = pg.new_initial_location();
    let action = pg.new_action();
    let post = pg.new_location();
    pg.add_transition(pre, action, post, None).unwrap();
    pg
}

#[inline(always)]
fn condition_pg() -> ProgramGraphBuilder {
    let mut pg = ProgramGraphBuilder::new();
    let pre = pg.new_initial_location();
    let action = pg.new_action();
    let post = pg.new_location();
    pg.add_transition(
        pre,
        action,
        post,
        Some(BooleanExpr::Implies(Box::new((
            Expression::less_than_or_equal_to(
                ((((Expression::from(Val::Integer(1)) + Expression::from(Val::Integer(2)))
                    .unwrap()
                    + Expression::from(Val::Integer(3)))
                .unwrap()
                    + Expression::from(Val::Integer(4)))
                .unwrap()
                    + Expression::from(Val::Integer(5)))
                .unwrap(),
                Expression::from(Val::Integer(100)),
            )
            .unwrap(),
            Expression::greater_than(
                Expression::from(Val::Integer(5)),
                Expression::from(Val::Integer(6)),
            )
            .unwrap(),
        )))),
    )
    .unwrap();
    pg
}

#[inline(always)]
fn long_pg() -> ProgramGraphBuilder {
    let mut pg = ProgramGraphBuilder::new();
    let mut pre = pg.new_initial_location();
    let action = pg.new_action();
    for _ in 0..10 {
        let post = pg.new_location();
        pg.add_transition(pre, action, post, None).unwrap();
        pre = post;
    }
    pg
}

#[inline(always)]
fn counter_pg() -> ProgramGraphBuilder {
    let mut pg = ProgramGraphBuilder::new();
    let initial = pg.new_initial_location();
    let action = pg.new_action();
    let var = pg.new_var(Val::Integer(0));
    pg.add_effect(
        action,
        var,
        (Expression::from_var(var, Type::Integer) + Expression::from(Val::Integer(1))).unwrap(),
    )
    .unwrap();
    for counter in 0..10 {
        let guard = Expression::equal_to(
            Expression::from_var(var, Type::Integer),
            Expression::from(Val::Integer(counter)),
        )
        .unwrap();
        pg.add_transition(initial, action, initial, Some(guard))
            .unwrap();
    }
    pg
}

fn possible_transitions(c: &mut Criterion) {
    let pgs = [
        (simple_pg(), "simple pg"),
        (condition_pg(), "condition pg"),
        (long_pg(), "long pg"),
        (counter_pg(), "counter pg"),
    ];
    for (pg, name) in pgs.into_iter() {
        let pg = pg.build();
        let pg = pg.new_instance();
        c.bench_with_input(
            BenchmarkId::new("possible transitions", name),
            &pg,
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
    for (pg, name) in pgs.into_iter() {
        let pg = pg.build();
        c.bench_with_input(
            BenchmarkId::new("execute to termination", name),
            &pg,
            |b, pg| {
                let mut pg = pg.new_instance();
                b.iter(|| run_to_completion(&mut pg));
            },
        );
    }
}

// criterion_group!(benches, possible_transitions, run);
// criterion_main!(benches);
