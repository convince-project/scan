/*build_tree is the main function in building trees.
It takes as input a string corresponding to the path to a  file and returns a Tree class
That corresponds to the path of the files and the corresponding tree  */

pub mod state_conteiner;
pub mod attribute_value;
pub mod state;
pub mod tree;

use std::io::{self, BufRead};
use std::env;
use std::path::{Path, PathBuf};
use std::fs::File;
use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

lrlex_mod!("calc.l");
lrpar_mod!("calc.y");

pub fn build_tree(s:PathBuf)->tree::Tree{
    let mut is_start = true;
    let attributes_start_state: Vec<attribute_value::AttributeValue> = vec![];
    let start_state = state::State::new("Start".to_string(),attributes_start_state);
    let children: Vec<tree::Tree> = vec![];
    let mut root = tree::Tree::new(0,-1,start_state,children);
    
    let lexerdef = calc_l::lexerdef();
    let path = s;
    if let Some(extension) = path.extension() {
        if extension == "scxml" {
            let mut count = 0;
            let mut cur_node: i32 = 0;
            if let Ok(lines) = read_lines(path.display().to_string()) {
                for line in lines.flatten() {
                    let lt = line.trim(); //Remove whitespace before and after the string
                    if !lt.is_empty(){
                        match lt {
                            lt =>{
                                // Now we create a lexer with the `lexer` method with which
                                // we can lex an input.
                                let lexer = lexerdef.lexer(&lt);
                                // Pass the lexer to the parser and lex and parse the input.
                                let (res, errs) = calc_y::parse(&lexer);
                                for e in errs {
                                    println!("{}", e.pp(&lexer, &calc_y::token_epp));
                                }
                                match res {
                                    //Here I put an if to ensure that when it finds a string "" it doesn't print it
                                    Some(Ok(r)) =>{
                                        if is_start==true{
                                            is_start = false;
                                            root.set_value(r.get_value());
                                            count = count+1;
                                        }
                                        else{
                                        if r.get_value().get_name()=="elseif"{
                                            if root.get_value_specific_node(cur_node).get_name()=="if"{
                                                cur_node = root.insert_node( r.get_value(), cur_node, count,false);
                                            }else if root.get_value_specific_node(cur_node).get_name()=="elseif"{
                                                cur_node = root.get_father_id(cur_node);
                                                cur_node = root.insert_node( r.get_value(), cur_node, count,false);
                                            }else{
                                                print!("ERROR elseif need to be a children of if");
                                                std::process::exit(0);
                                            }
                                        }else if r.get_value().get_name()=="else"{
                                            if root.get_value_specific_node(cur_node).get_name()=="if"{
                                                cur_node = root.insert_node( r.get_value(), cur_node, count,false);
                                            }else if root.get_value_specific_node(cur_node).get_name()=="elseif"{
                                                cur_node = root.get_father_id(cur_node);
                                                cur_node = root.insert_node( r.get_value(), cur_node, count,false);
                                            }else{
                                                print!("ERROR elseif need to be a children of if");
                                                std::process::exit(0);
                                            }
                                        }
                                        else if r.get_value().get_name()=="CLOSE"{
                                            if root.get_value_specific_node(cur_node).get_name()=="if"{
                                                cur_node = root.insert_node( r.get_value(), cur_node, count,r.get_cond());
                                            }else if root.get_value_specific_node(cur_node).get_name()=="elseif"{
                                                cur_node = root.get_father_id(cur_node);
                                                cur_node = root.insert_node( r.get_value(), cur_node, count,r.get_cond());
                                            }else if root.get_value_specific_node(cur_node).get_name()=="else"{
                                                cur_node = root.get_father_id(cur_node);
                                                cur_node = root.insert_node( r.get_value(), cur_node, count,r.get_cond())
                                            }else{
                                                cur_node = root.insert_node( r.get_value(), cur_node, count,r.get_cond());
                                            }
                                        }
                                        else{
                                            cur_node = root.insert_node( r.get_value(), cur_node, count,r.get_cond()); 
                                        }
                                        count =count+1;
                                        }
                                    },
                                    _ => eprintln!("Unable to evaluate expression.")
                                }
                            }
                        }
                    }
                }
            }
            else{
                println!("The file is missed or the path is incorrec.") 
            }
        }
        else{
            println!("Attention the file {} must have scxml extension",path.display());
        }  
    }
    root
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}