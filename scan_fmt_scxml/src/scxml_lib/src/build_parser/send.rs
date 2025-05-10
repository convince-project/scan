use crate::build_tree::tree;
use crate::build_parser::{Param,controll_expression};
use boa_ast::StatementListItem;
use boa_ast::scope::Scope;

use log::{trace};

#[derive(Debug, Clone)]
pub enum Target {
    Id(String),
    Expr(boa_ast::Expression),
}

#[derive(Debug, Clone)]
pub struct Send {
    pub(crate) event: String,
    pub(crate) target: Option<Target>,
    pub(crate) delay: Option<u32>,
    pub(crate) params: Vec<Param>,
}

impl Send {
    pub fn new(
        event_c: String,target_c: Option<Target>,delay_c: Option<u32>,params_c: Vec<Param>
    ) -> Self {
        Send { 
            event: event_c, 
            target: target_c, 
            delay: delay_c, 
            params: params_c 
        }
    }

    pub fn build_send(s:tree::Tree,
        interner: &mut boa_interner::Interner)->Send{
        trace!(target: "parser", "start build send");
        //I initialize the class attributes
        let mut event_c="".to_string();
        let mut target_c="".to_string();
        let mut targetexpr_c="".to_string();
        let mut delay_c="".to_string();
        let mut params_c:Vec<Param> = Vec::new();
        for atr in s.get_value().get_attribute_list(){
            let cur_atr = atr.get_name();
            match cur_atr {
                "event" => {
                    event_c = atr.get_value().to_string();
                }
                "target" =>{
                    target_c = atr.get_value().to_string();
                }
                "targetexpr" =>{
                    targetexpr_c = atr.get_value().to_string();
                    targetexpr_c = controll_expression(targetexpr_c);
                }
                "delay" =>{
                    delay_c = atr.get_value().to_string();
                }
                _key => {
                }
            }
        }
        let event_o = event_c;
        let delay_cc= match delay_c.parse().ok(){
            n => n
        };
        let target = if target_c.is_empty()|| target_c=="none"{
            None
        }else{
            Some(target_c) 
        };
        let targetexpr = if targetexpr_c.is_empty() || targetexpr_c=="none"{
            None
        }else{
            Some(targetexpr_c) 
        };
        let target = if let Some(target) = target {
                Some(Target::Id(target))
        } else if let Some(targetexpr) = targetexpr {
                if let StatementListItem::Statement(boa_ast::Statement::Expression(targetexpr)) =
                    boa_parser::Parser::new(boa_parser::Source::from_bytes(&targetexpr))
                        .parse_script(&Scope::new_global(),interner)
                        .expect("hope this works")
                        .statements()
                        .first()
                        .expect("hopefully there is a statement")
                        .to_owned()
                {
                    Some(Target::Expr(targetexpr))
                } else {
                    print!("ERROR send");
                    std::process::exit(0);
                }
        } else {
            None
        };
        for child in s.get_children(){
            let cur_child = child.get_value();
            match cur_child.get_name(){
                "param" =>{
                    params_c.push(Param::build_param(child,
                        //omg_type,
                        interner));
                }
                _key => {
                }
            } 
        }
        let send = Send::new(event_o,target,delay_cc,params_c);
        trace!(target: "parser", "end build send");
        return send;
    }
    //These sets and get are needed for the builder
    pub fn get_event(&self)-> String{
        self.event.clone()
    }
    pub fn get_target(&self)-> Option<Target>{
        self.target.clone()
    }
    pub fn get_delay(&self)-> Option<u32>{
        self.delay.clone()
    }
    pub fn get_params(&self)-> Vec<Param>{
        self.params.clone()
    }

    //This part is not used by scan, it is only used to test the library and prints the class elements to the screen.
    pub fn stamp(&self){
        print!("State=Send\n");
        print!("event={}\n",self.event); 
        while let Some(value) = self.target.clone(){
            match value {
                Target::Id(id)=>{
                    println!("id: {}",id);
                },
                Target::Expr(expr)=>{
                    println!("expr={:?}",expr);
                }
            }
        }
        match self.delay.clone() {
            Some(value) =>println!("delay={}", value),
            None =>println!("No Delay"),
        }
        for value in self.params.clone(){
            value.stamp();
        }
    }
}