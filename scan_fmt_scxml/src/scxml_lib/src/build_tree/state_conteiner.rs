use crate::build_tree::state::State;

//StateConteiner class contains the state of the xml language and a variable bool that is usefull to define if state is closed or not
#[derive(Debug,Clone)]
pub struct StateConteiner {
    value: State,
    cond: bool,
}
//I define the functions of the State class 
//new is the constructor
impl StateConteiner{
    pub fn new(v:State,c:bool) -> Self {
        StateConteiner { 
            value:v, 
            cond: c
        }
    }
    pub fn set_value(&mut self, v: State) { 
        self.value = v; 
    }
    pub fn set_cond(&mut self, c: bool) { 
        self.cond = c; 
    }
    pub fn get_value(&self) -> State {
        self.value.to_owned()
    }
    pub fn get_cond(&self) -> bool {
        self.cond
    }   
}