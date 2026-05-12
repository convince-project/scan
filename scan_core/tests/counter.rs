use scan_core::{channel_system::ChannelSystemBuilder, *};

#[test]
fn counter_pg() {
    let mut cs = ChannelSystemBuilder::new();
    let pg = cs.new_program_graph();
    let initial = cs.new_initial_location(pg).expect("new initial location");
    let action = cs.new_action(pg).expect("new action");
    let var = cs.new_var(pg, Val::Integer(0)).expect("new var");
    cs.add_effect(
        pg,
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
        cs.add_transition(pg, initial, action, initial, Some(guard))
            .unwrap();
    }
    let cs = TransitionSystem::new(cs);
    let mut pg: TransitionSystemRun = cs.new_run();
    pg.transition();
    while pg.last_event().is_some() {
        pg.transition();
    }
}
