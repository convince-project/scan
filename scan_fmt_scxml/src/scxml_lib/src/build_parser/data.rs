//Data class
use crate::build_tree::tree;
use crate::build_parser::controll_expression;
use boa_ast::StatementListItem;
use boa_ast::scope::Scope;
use log::{trace};

#[derive(Debug, Clone)]
pub struct Data {
    pub(crate) id: String,
    pub(crate) expression: Option<boa_ast::Expression>,
    pub(crate) omg_type: String,
}

impl Data {
    fn new(
        id_c:String,expression_c:Option<boa_ast::Expression>,omg_type_c:String
    ) -> Self {
        Data { 
            id: id_c, 
            expression: expression_c, 
            omg_type: omg_type_c 
        }
    }

    pub fn build_data(s:tree::Tree,
        interner: &mut boa_interner::Interner)->Data{
        trace!(target: "parser", "start build data");
        //I initialize the class attributes
        let mut id_c = "".to_string();
        let mut omg_type_c= "".to_string();
        let mut expr_c= "".to_string();
        for atr in s.get_value().get_attribute_list(){
            let cur_atr = atr.get_name();
            match cur_atr {
                "id" => {
                    id_c = atr.get_value().to_string();
                }
                "expr" =>{
                    expr_c = atr.get_value().to_string();
                    expr_c = controll_expression(expr_c);

                }
                "type" =>{
                    omg_type_c = atr.get_value().to_string();
                }
                _key => {
                }
            }
        }
        //Check if the expression exists or not
        let expr_o = if expr_c.is_empty()||expr_c=="none"{
            None
        }else{
            Some(expr_c)
        };
        //This part convert String in an Expression
        let expression = if let Some(expression) = expr_o {
            if let StatementListItem::Statement(boa_ast::Statement::Expression(expression)) =
                boa_parser::Parser::new(boa_parser::Source::from_bytes(&expression))
                    .parse_script(&Scope::new_global(),interner)
                    .expect("hope this works")
                    .statements()
                    .first()
                    .expect("hopefully there is a statement")
                    .to_owned()
            {
                Some(expression)
            } else {
                print!("ERROR data");
                std::process::exit(0);
            }
        } else {
            None
        };
        let data = Data::new(id_c,expression,omg_type_c);
        trace!(target: "parser", "end build data"); 
        return data;
    }
    //These sets and get are needed for the builder
    pub fn get_id(&self)-> String{
        self.id.clone()
    }
    pub fn get_expression(&self)-> Option<boa_ast::Expression>{
        self.expression.clone()
    }
    pub fn get_omg_type(&self)-> String{
        self.omg_type.clone()
    }
    //This part is not used by scan, it is only used to test the library and prints the class elements to the screen.
    pub fn stamp(&self){
        print!("State=Data\n");
        print!("id={}\n",self.id);
        match self.expression.clone() {
            Some(value) =>println!("expression={:?}", value),
            None =>println!("No Expression"),
        }
        println!("type={}",self.omg_type);
    }
}