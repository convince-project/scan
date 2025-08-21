use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

lrlex_mod!("main/spinv.l");
lrpar_mod!("main/spinv4.y");

#[path = "builder.rs"]
pub mod builder;
pub use builder::*;

pub use crate::spinv4_y::*;
