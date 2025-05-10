//AttributeValue class contains the attributes of the xml language( name is the name of attribute and value is the value)
#[derive(Debug,Clone)]
pub struct AttributeValue {
   name: String,
   value: String,
}
//I define the functions of the AttributeValue class 
//stamp prints the value of the attribute to the screen in the form name=value
impl AttributeValue {
    pub fn new(n:String,v:String) -> Self {
        AttributeValue { 
            name:n, 
            value:v
        }
    }
    pub fn set_name(&mut self, n: String) { 
        self.name = n; 
    }
    pub fn set_value(&mut self, v: String) { 
        self.value = v; 
    }
    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn get_value(&self) -> &str {
        &self.value
    }
    
    pub fn stamp(&self){
        print!("{}={}",self.name,self.value);
    }
}