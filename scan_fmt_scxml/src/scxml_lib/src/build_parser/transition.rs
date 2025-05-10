use crate::build_tree::tree;
use crate::build_parser::{Executable,controll_expression};

use boa_ast::StatementListItem;
use boa_ast::scope::Scope;

use log::{trace};

#[derive(Debug, Clone)]
pub struct Transition {
    pub(crate) event: Option<String>,
    pub(crate) target: String,
    pub(crate) cond: Option<boa_ast::Expression>,
    pub(crate) effects: Vec<Executable>,
}

impl Transition {
    pub fn new(
        event_c:Option<String>,target_c:String,cond_c:Option<boa_ast::Expression>,effects_c:Vec<Executable>) -> Self{
        Transition { 
            event: event_c, 
            target: target_c, 
            cond: cond_c, 
            effects: effects_c 
        }
    }

    pub fn build_transition(s:tree::Tree,interner: &mut boa_interner::Interner)->Transition{
        trace!(target: "parser", "start build transition");
        //I initialize the class attributes
        let mut event_c: String = "".to_string();
        let mut target_c: String = "".to_string();
        let mut cond_c:String = "".to_string();
        let mut effect_c:Vec<Executable> = Vec::new();
        for atr in s.get_value().get_attribute_list(){
            let cur_atr = atr.get_name();
            match cur_atr {
                "event" => {
                    event_c = atr.get_value().to_string();
                }
                "target" =>{
                    target_c = atr.get_value().to_string();
                }
                "cond" =>{
                    cond_c = atr.get_value().to_string();
                    cond_c = controll_expression(cond_c);
                }
                _key => {
                }
            }
        }
        let event_o =  if event_c.is_empty()||event_c=="none"{
            None
        }else{
            Some(event_c)
        };
        let target_o = target_c;
        let cond = if cond_c.is_empty()||cond_c=="none"{
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
                Some(cond)
            } else {
                print!("ERROR transition");
                std::process::exit(0);
            }
        } else {
            None
        };
        //check children of the class
        for child in s.get_children(){
            let cur_child = child.get_value();
            match cur_child.get_name(){
                "raise" =>{
                    effect_c.push(Executable::build_execut_raise(child));
                }
                "assign" =>{
                    effect_c.push(Executable::build_execut_assign(child, interner));
                }
                "if" =>{
                    effect_c.push(Executable::build_execut_if(child, interner));
                }
                "send" =>{
                    effect_c.push(Executable::build_execut_send(child, interner));
                }
                _key => {
                }
            } 
        }
        let transition = Transition::new(event_o,target_o,cond,effect_c);
        trace!(target: "parser", "end build transition");
        return transition;
    }

    //These sets and get are needed for the builder
    pub fn get_event(&self)-> Option<String>{
        self.event.clone()
    }
    pub fn get_target(&self)-> String{
        self.target.clone()
    }
    pub fn get_cond(&self)-> Option<boa_ast::Expression>{
        self.cond.clone()
    }
    pub fn get_effects(&self)-> Vec<Executable>{
        self.effects.clone()
    }
    //This part is not used by scan, it is only used to test the library and prints the class elements to the screen.
     pub fn stamp(&self){
        print!("Transition\n");
        match self.event.clone() {
            Some(value) =>println!("event={}", value),
            None =>println!("The `no event."),
        }
        print!("target={}\n",self.target);
        match self.cond.clone(){
            Some(value) => println!("cond={:?}",value),
            None =>println!("The `no cond."),
        }
        for value in self.effects.clone(){
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
            //value.stamp();
        }
    }   
}