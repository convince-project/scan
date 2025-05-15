//The lib file is the main file of our library the build function is the one that will be used by scan
pub mod build_tree;
pub mod build_parser;

use std::{path::PathBuf};

use boa_interner::Interner;
pub use build_tree::build_tree;
pub use build_parser::{build_parser,Scxml};


pub fn build(s:PathBuf,interner:&mut Interner)->Scxml{
    //Build the Tree class based on the directory s
    let collection_trees = build_tree(s);
    //Build Scxml class based on Tree class
    let collection_parser = build_parser(collection_trees,interner);
    collection_parser
}
