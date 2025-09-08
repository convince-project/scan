pub use scan_promela::builder::*;
pub use scan_promela::*;

// helper functions for tests
pub fn name(id: &str) -> Name {
    Name::new(id.into())
}
pub fn varref(id: &str) -> Varref {
    Varref::new(name(id), None)
}
pub fn ivar_test(id: &str) -> Ivar {
    Ivar::new(name(id), None, None)
}
pub fn const_i(n: i32) -> AnyExpr {
    AnyExpr::new_const(Const::Number(n))
}
pub fn decl_int(id: &str) -> OneDecl {
    OneDecl::var_decl_no_visible(Typename::Int, vec![ivar_test(id)])
}
