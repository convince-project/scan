use std::path::Path;

#[test]
fn jani_test() {
    test(Path::new("./tests/test.jani"))
}

#[test]
fn jani_test2() {
    test(Path::new("./tests/test2.jani"))
}

#[test]
fn workflow() {
    test(Path::new("./tests/workflow.jani"))
}

#[test]
fn brp() {
    test(Path::new("./tests/brp.v1.jani"))
}

#[test]
fn crowds() {
    test(Path::new("./tests/crowds.v1.jani"))
}

#[test]
fn leader_sync_3_2() {
    test(Path::new("./tests/leader_sync.3-2.v1.jani"))
}

#[test]
fn leader_sync_5_4() {
    test(Path::new("./tests/leader_sync.5-4.v1.jani"))
}

fn test(path: &Path) {
    let (scan, ..) = scan_jani::load(path, &[]).expect("load");
    scan.adaptive(0.95, 0.01, 10000, true)
        .expect("verification");
}
