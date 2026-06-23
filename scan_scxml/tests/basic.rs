use std::path::Path;

#[test]
fn fsm() {
    test(Path::new("./tests/assets/test_fsm/model.xml"))
}

#[test]
fn datamodel() {
    test(Path::new("./tests/assets/test_datamodel/model.xml"))
}

#[test]
fn enumdata() {
    test(Path::new("./tests/assets/test_enumdata/model.xml"))
}

#[test]
fn send() {
    test(Path::new("./tests/assets/test_send/model.xml"))
}

#[test]
fn send_triangle() {
    test(Path::new("./tests/assets/test_send_triangle/model.xml"))
}

#[test]
fn send_onentry() {
    test(Path::new("./tests/assets/test_send_onentry/model.xml"))
}

#[test]
fn origin() {
    test(Path::new("./tests/assets/test_origin/model.xml"))
}

#[test]
fn origin_location() {
    test(Path::new("./tests/assets/test_origin_location/model.xml"))
}

#[test]
fn param() {
    test(Path::new("./tests/assets/test_param/model.xml"))
}

#[test]
fn param_triangle() {
    test(Path::new("./tests/assets/test_param_triangle/model.xml"))
}

#[test]
fn param_tennis() {
    test(Path::new("./tests/assets/test_param_tennis/model.xml"))
}

#[test]
fn conditional() {
    test(Path::new("./tests/assets/test_if/model.xml"))
}

#[test]
fn elif() {
    test(Path::new("./tests/assets/test_elif/model.xml"))
}

fn test(path: &Path) {
    let (scan, ..) = scan_scxml::load(path, &[], true).expect("load model");
    let _ = scan.adaptive(0.95, 0.01).expect("run verification");
}
