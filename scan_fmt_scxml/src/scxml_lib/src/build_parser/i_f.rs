//If class
use crate::build_tree::tree;
use crate::build_parser::{Executable,controll_expression};

use boa_ast::StatementListItem;
use boa_ast::scope::Scope;

use log::{trace};

#[derive(Debug, Clone)]
pub struct If {
    pub(crate) r#elif: Vec<(boa_ast::Expression, Vec<Executable>)>,
    pub(crate) r#else: Vec<Executable>,
}

impl If {
    pub fn new(
        r#elif_c: Vec<(boa_ast::Expression, Vec<Executable>)>, r#else_c: Vec<Executable>
    ) -> Self {
        If { 
            r#elif: r#elif_c, 
            r#else: r#else_c, 
        }
    }

   pub fn build_if(s:tree::Tree,
        interner: &mut boa_interner::Interner)->If{
        trace!(target: "parser", "start build if");
        //I initialize the class attributes
        let mut cond_c="".to_string();
        let mut r#elif_c: Vec<(boa_ast::Expression, Vec<Executable>)> = Vec::new();
        let mut r#else_c: Vec<Executable>= Vec::new();
        for atr in s.get_value().get_attribute_list(){
            let cur_atr = atr.get_name();
            match cur_atr {
                "cond" => {
                    cond_c = atr.get_value().to_string();
                    cond_c = controll_expression(cond_c);
                }
                _key => {
                }
            }
        }
        //Check if cond is present and set it
        let cond = if cond_c.is_empty(){
            None
        }else{
            Some(cond_c) 
        };
        let cond = if let Some(cond) = cond {
            if let StatementListItem::Statement(boa_ast::Statement::Expression(cond)) =
                boa_parser::Parser::new(boa_parser::Source::from_bytes(&cond))
                    .parse_script(&Scope::new_global(),interner)
                    .expect("hope this works")
                    .statements()
                    .first()
                    .expect("hopefully there is a statement")
                    .to_owned()
            {
                cond
            } else {
                print!("ERROR if condition is missing");
                std::process::exit(0);
            }
        } else {
            print!("ERROR if condition is missing");
            std::process::exit(0);
        };
        let cur_cond = cond;
        let mut cur_exec:Vec<Executable> = Vec::new();
        let mut is_if = true;
        let mut is_else = false;
        //check child of if tag
        for child in s.get_children(){
            let cur_child = child.get_value();
            match cur_child.get_name(){
                "raise" =>{
                    cur_exec.push(Executable::build_execut_raise(child));
                }
                "assign" =>{
                    cur_exec.push(Executable::build_execut_assign(child, interner));
                }
                "if" =>{
                    cur_exec.push(Executable::build_execut_if(child, interner));
                }
                "send" =>{
                    cur_exec.push(Executable::build_execut_send(child, interner));
                }
                "elseif"=>{
                    //chect that elseif is not after an else
                    if is_else == false{
                        if is_if == true{
                            r#elif_c.push((cur_cond.clone(),cur_exec.clone()));
                            is_if =false;
                        }
                        let mut cond_cr="".to_string();
                        for atr in cur_child.get_attribute_list(){
                            let cur_atr = atr.get_name();
                            match cur_atr {
                                "cond" => {
                                    cond_cr = atr.get_value().to_string();
                                    cond_cr = controll_expression(cond_cr);
                                }
                                _key => {
                                }
                            }
                        }
                        let c_cr = if cond_cr.is_empty(){
                            None
                        }else{
                            Some(cond_cr) 
                        };
                        let c_cr = if let Some(c_cr) = c_cr {
                            if let StatementListItem::Statement(boa_ast::Statement::Expression(c_cr)) =
                                boa_parser::Parser::new(boa_parser::Source::from_bytes(&c_cr))
                                    .parse_script(&Scope::new_global(),interner)
                                    .expect("hope this works")
                                    .statements()
                                    .first()
                                    .expect("hopefully there is a statement")
                                    .to_owned()
                            {
                                c_cr
                            } else {
                                print!("ERROR transition");
                                std::process::exit(0);
                            }
                        } else {
                            print!("ERROR transition");
                            std::process::exit(0);
                        };
                        r#elif_c.push((c_cr,If::build_elseif(child, interner)));
                    }
                    else{
                        print!("ERROR there is an elseif after an else");
                        std::process::exit(0);
                    }
                }
                "else"=>{
                    //check if there are more than one else
                    if is_else == false{
                        r#else_c = If::build_else(child,interner);
                        is_else = true;
                    }else{
                        print!("ERROR else must be one");
                        std::process::exit(0); 
                    }
                }
                _key => {
                }
            } 
        }
        if is_if == true{
            r#elif_c.push((cur_cond.clone(),cur_exec.clone()));
        }
        let r#if = If::new(r#elif_c,r#else_c/*,false*/);
        trace!(target: "parser", "end build if");
        return r#if;
    }

    fn build_elseif(s:tree::Tree,interner: &mut boa_interner::Interner)->Vec<Executable>{
        let mut exec:Vec<Executable> = Vec::new();
        for child in s.get_children(){
            let cur_child = child.get_value();
            match cur_child.get_name(){
                "raise" =>{
                    exec.push(Executable::build_execut_raise(child));
                }
                "assign" =>{
                    exec.push(Executable::build_execut_assign(child, interner));
                }
                "if" =>{
                    exec.push(Executable::build_execut_if(child, interner));
                }
                "send"=>{
                    exec.push(Executable::build_execut_send(child, interner));
                }
                _key => {
                }
            } 
        }
        return exec;
    }

    fn build_else(s:tree::Tree,interner: &mut boa_interner::Interner)->Vec<Executable>{
        let mut exec:Vec<Executable> = Vec::new();
        for child in s.get_children(){
            let cur_child = child.get_value();
            match cur_child.get_name(){
                "raise" =>{
                    exec.push(Executable::build_execut_raise(child));
                }
                "assign" =>{
                    exec.push(Executable::build_execut_assign(child, interner));
                }
                "if" =>{
                    exec.push(Executable::build_execut_if(child, interner));
                }
                "send"=>{
                    exec.push(Executable::build_execut_send(child, interner));
                }
                _key => {
                }
            } 
        }
        return exec;
    }
     //These sets and get are needed for the builder
    pub fn get_r_elif(&self)-> Vec<(boa_ast::Expression, Vec<Executable>)>{
        self.r#elif.clone()
    }
    pub fn get_r_else(&self)-> Vec<Executable>{
        self.r#else.clone()
    }
    //This part is not used by scan, it is only used to test the library and prints the class elements to the screen.
    pub fn stamp(&self){
        print!("State=IF\n"); 
        for (c,e)in self.r#elif.clone(){
            print!("State=Elseif\n");
            println!("cond={:?}",c);  
            for value in e{
                match  value{
                    Executable::Assign { location, expr }   => {
                        println!("State=Assign");
                        println!("location: {}",location);
                        println!("expr={:?}",expr);
                    },
                    Executable::Raise { event }=> {
                        println!("State=Raise");
                        println!("event: {}",event);
                    },
                    Executable::Send(send)=>{
                        send.stamp();
                    }
                    Executable::If(r#if)=>{
                        r#if.stamp();
                    }
                }
            }
        }
        for value in self.r#else.clone(){
            print!("State=Else\n");
            match value {
                Executable::Assign { location, expr }   => {
                    println!("State=Assign");
                    println!("location: {}",location);
                    println!("expr={:?}",expr);
                },
                Executable::Raise { event }=> {
                    println!("State=Raise");
                    println!("event: {}",event);
                },
                Executable::Send(send)=>{
                    send.stamp();
                }
                Executable::If(r#if)=>{
                    r#if.stamp();
                }
            }
        }
    }
}