use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

lrlex_mod!("spinv.l");
lrpar_mod!("spinv4.y");

#[path = "builder.rs"]
pub mod builder;
pub use builder::*;

pub use crate::spinv4_y::*;
