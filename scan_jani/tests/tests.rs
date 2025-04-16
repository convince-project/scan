use std::path::Path;

const MAXSTEP: usize = 1000;

#[test]
fn jani_test() {
    test(Path::new("./tests/test.jani"))
}

#[test]
fn jani_test2() {
    test(Path::new("./tests/test2.jani"))
}

#[test]
fn sync() {
    test(Path::new("./tests/sync.jani"))
}

#[test]
fn die() {
    test(Path::new("./tests/die.jani"))
}

#[test]
fn dining_crypt3() {
    test(Path::new("./tests/dining_crypt3.jani"))
}

#[test]
fn ij_3() {
    test(Path::new("./tests/ij.3.jani"))
}

fn test(path: &Path) {
    let mut model = scan_jani::parse(path).unwrap().0.channel_system().clone();
    let mut steps = 0;
    assert!(model.possible_transitions().count() > 0);
    while let Some((pg_id, action, post)) = model
        .possible_transitions()
        .filter_map(|(pg, a, iter)| {
            iter.into_iter()
                .map(|v| v.last())
                .collect::<Option<Vec<_>>>()
                .map(|l| (pg, a, l))
        })
        .last()
    {
        steps += 1;
        assert!(steps < MAXSTEP, "step limit reached");
        assert!(model.transition(pg_id, action, post.as_slice()).is_ok());
    }
    assert!(steps > 0, "no transitions executed");
}
