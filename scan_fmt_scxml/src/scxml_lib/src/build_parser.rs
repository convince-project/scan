pub mod scxml;
pub mod data;
pub mod state;
pub mod transition;
pub mod executable;
pub mod send;
pub mod i_f;
pub mod param;
pub mod utils;

pub use scxml::Scxml;
pub use data::Data;
pub use state::State;
pub use transition::Transition;
pub use executable::Executable;
pub use send::Send;
pub use i_f::If;
pub use param::Param;
pub use utils::controll_expression;



use crate::build_tree::tree;
use boa_interner::Interner;

pub fn build_parser(s:tree::Tree,interner:&mut Interner)->Scxml{
    //let mut interner_c= Interner::new();
    //s.stamp();
    let process_list_c  = Scxml::build_scxml(s,interner);
    process_list_c.clone()  
}