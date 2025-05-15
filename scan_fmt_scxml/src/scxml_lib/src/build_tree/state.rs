use crate::build_tree::attribute_value::AttributeValue;

//State class contains the state of the xml language( name is the name of state and attribute_list the list of attributes)
#[derive(Debug,Clone)]
pub struct State {
    name: String,
    attribute_list: Vec<AttributeValue>,
}
//I define the functions of the State class 
//new is the constructor while stamp prints the value of the state to the screen in the form state=name attribute_list[ name_atr=value ...]
impl State{
    pub fn new(value_n:String,value_al:Vec<AttributeValue>) -> Self {
        State { 
            name: value_n, 
            attribute_list: value_al
        }

    }
    pub fn set_name(&mut self, value_n: String) { 
        self.name = value_n; 
    }
    pub fn set_attrbute_list(&mut self, value_al: Vec<AttributeValue>) { 
        self.attribute_list = value_al; 
    }
    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn get_attribute_list(&self) -> Vec<AttributeValue> {
        self.attribute_list.to_owned()
    }
    pub fn stamp(&self){
        print!("state={}\n",self.name);
        print!("attributes:[ ");
        for a in &self.attribute_list{
            a.stamp();
            print!(" ");
        }
        print!("]");

    }
}