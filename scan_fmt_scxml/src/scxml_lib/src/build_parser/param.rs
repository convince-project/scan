//Class Param 
use crate::build_tree::tree;
use crate::build_parser::controll_expression;
use boa_ast::{Expression as BoaExpression, StatementListItem};
use boa_ast::scope::Scope;

use log::{trace};

#[derive(Debug, Clone)]
pub struct Param {
    pub(crate) name: String,
    pub(crate) omg_type: Option<String>,
    pub(crate) expr: BoaExpression,
}

impl Param {
    pub fn new(
        name_c: String,omg_type_c: Option<String>, expr_c: BoaExpression,
    ) -> Self {
        Param { 
            name: name_c, 
            omg_type: omg_type_c, 
            expr: expr_c 
        }
    }

    pub fn build_param(s:tree::Tree, 
        interner: &mut boa_interner::Interner)->Param{
        trace!(target: "parser", "start build param");
        //I initialize the class attributes
        let mut name_c="".to_string();
        let mut location_c="".to_string();
        let mut expr_c="".to_string();
        let mut type_c="".to_string();
        for atr in s.get_value().get_attribute_list(){
            let cur_atr = atr.get_name();
            match cur_atr {
                "name" => {
                    name_c = atr.get_value().to_string();
                }
                "location" =>{
                    location_c = atr.get_value().to_string();
                }
                "expr" =>{
                    expr_c = atr.get_value().to_string();
                    expr_c = controll_expression(expr_c);
                }
                "type" =>{
                    type_c = atr.get_value().to_string();
                }
                _key => {
                }
            }
        }
        let name_o = name_c;
        let omg_type_o = if type_c.is_empty()||type_c=="none"{
            None
        }else{
            Some(type_c) 
        };
        let expr = if expr_c.is_empty()||expr_c=="none"{
            location_c
        }else{
            expr_c 
        };
        let expr= if let StatementListItem::Statement(boa_ast::Statement::Expression(expr)) =
            boa_parser::Parser::new(boa_parser::Source::from_bytes(&expr))
                .parse_script(&Scope::new_global(),interner)
                .expect("hope this works")
                .statements()
                .first()
                .expect("hopefully there is a statement")
                .to_owned()
        {
            expr
        } else {
            print!("ERROR param");
            std::process::exit(0);
        };
        let param = Param::new(name_o,omg_type_o,expr);
        trace!(target: "parser", "end build param");
        return param;
    }
    //These sets and get are needed for the builder
    pub fn get_name(&self)-> String{
        self.name.clone()
    }
    pub fn get_omg_type(&self)-> Option<String>{
        self.omg_type.clone()
    }
    pub fn get_expr(&self)-> BoaExpression{
        self.expr.clone()
    }
    pub fn set_omg_type(&mut self,o_t:Option<String>){
        self.omg_type = o_t;
    }
    //This part is not used by scan, it is only used to test the library and prints the class elements to the screen.
    pub fn stamp(&self){
        print!("State=Param\n");
        print!("name={}\n",self.name); 
        match self.omg_type.clone() {
            Some(value) =>println!("type={}", value),
            None =>println!("No Type"),
        }
        println!("expr: {:?}",self.expr);
    }

}