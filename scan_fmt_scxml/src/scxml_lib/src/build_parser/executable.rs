//Executable class
use crate::build_tree::tree;
use crate::build_parser::{Send,If,controll_expression};

use boa_ast::StatementListItem;
use boa_ast::scope::Scope;

use log::{trace};


#[derive(Debug, Clone)]
pub enum Executable {
    Assign {
        location: String,
        expr: boa_ast::Expression,
    },
    Raise {
        event: String,
    },
    Send(Send),
    If(If),
}

impl Executable {
    pub fn new_raise(event_c:String) -> Self {
        Executable::Raise { event: event_c }
    }
    pub fn new_assign(
        location_c:String,expr_c: boa_ast::Expression
    ) -> Self {
        Executable::Assign { 
            location: location_c, 
            expr: expr_c 
        }
    }
    pub fn new_send(send_c:Send)->Self{
        Executable::Send(send_c)
    }
    pub fn new_if(if_c:If)->Self{
        Executable::If(if_c)
    }

    pub fn build_execut_raise(s:tree::Tree)->Executable{
        trace!(target: "parser", "start build raise");
        //I initialize the class attributes
        let mut event_c="".to_string();
        for atr in s.get_value().get_attribute_list(){
            let cur_atr = atr.get_name();
            match cur_atr {
                "event" => {
                    event_c = atr.get_value().to_string();
                }
                _key => {
                }
            }
        }
        let event_o = event_c;
        let execute = Executable::new_raise(event_o);
        trace!(target: "parser", "end build raise");
        return execute;
    }
    
    pub fn build_execut_assign(s:tree::Tree,interner: &mut boa_interner::Interner)->Executable{
        trace!(target: "parser", "start build assign");
        //I initialize the class attributes
        let mut location_c="".to_string();
        let mut expr_c="".to_string();
        let expr_o;
        for atr in s.get_value().get_attribute_list(){
            let cur_atr = atr.get_name();
            match cur_atr {
                "location" => {
                    location_c = atr.get_value().to_string();
                }
                "expr" => {
                    expr_c = atr.get_value().to_string();
                    expr_c = controll_expression(expr_c);
                }
                _key => {
                }
            }
        }
        let location_o = location_c;
        let expr = expr_c;
        //This part convert String in an Expression
        let statement = boa_parser::Parser::new(boa_parser::Source::from_bytes(&expr))
                .parse_script(&Scope::new_global(),interner)
                .expect("hope this works")
                .statements()
                .first()
                .expect("hopefully there is a statement")
                .to_owned();
        match statement {
                StatementListItem::Statement(boa_ast::Statement::Expression(expr)) => {
                    expr_o = expr;
                }
                _ => {print!("ERROR executable");
                     std::process::exit(0);
                }
        }
        let execute = Executable::new_assign(location_o,expr_o);
        trace!(target: "parser", "end build assign");
        return execute;
    }

    pub fn build_execut_send(s:tree::Tree,interner: &mut boa_interner::Interner)->Executable{
        let send_c = Send::build_send(s, interner);
        let execute = Executable::new_send(send_c);
        return execute;
    }

    pub fn build_execut_if(s:tree::Tree,interner: &mut boa_interner::Interner)->Executable{
        let if_c = If::build_if(s, interner);
        let execute = Executable::new_if(if_c);
        return execute;
    }

}