//Scxml class
use crate::build_tree::tree;
use crate::build_parser::{State,Data};
use std::collections::HashMap;

use log::{info, trace};

#[derive(Debug, Clone)]
pub struct Scxml {
    pub(crate) id: String,
    pub(crate) initial: String,
    pub(crate) datamodel: Vec<Data>,
    pub(crate) states: HashMap<String,State>,
}

impl Scxml {
    pub fn new(id_c:String,initial_c:String,datamodel_c:Vec<Data>,states_c:HashMap<String,State>) -> Self {
        Scxml{
            id:id_c,
            initial : initial_c,
            datamodel : datamodel_c,
            states : states_c
        }
    }

    pub fn build_scxml(s:tree::Tree,
        interner: &mut boa_interner::Interner
        )->Self{
        info!(target: "parser", "parsing fsm");
        trace!(target: "parser", "start build scxml");   
        let mut id_c = "".to_string();
        let mut initial_c = "".to_string();
        let mut datamodel_c:Vec<Data> = Vec::new();
        let mut state_c:HashMap<String,State> = HashMap::new();
        //I initialize the class attributes
        for atr in s.get_value().get_attribute_list(){
            let cur_atr = atr.get_name();
            match cur_atr {
                "name" => {
                    id_c = atr.get_value().to_string();
                }
                "initial" => {
                        initial_c = atr.get_value().to_string();
                }
                _key => {
                }
            }
        }
        //check children of the class
        for child in s.get_children(){
            let cur_child = child.get_value();
            match cur_child.get_name(){
                "state" =>{
                    let mut id_s="".to_string();
                    for atr in child.get_value().get_attribute_list(){
                        let cur_atr = atr.get_name();
                        match cur_atr {
                            "id" => {
                                id_s = atr.get_value().to_string();
                            }
                            _key => {
                            }
                        }
                    }
                    state_c.insert(id_s, State::build_state(child,
                        interner
                    ));
                }
                "datamodel" =>{
                    datamodel_c= Self::build_datamodel(child,
                        interner);
                }
                _key => {
                }
            } 
        }
        let scxml = Scxml::new(id_c,initial_c,datamodel_c,state_c);
        trace!(target: "parser", "end build scxml");
        return scxml;
    }

    //Function for check the children of datamodel state (that are data state) 
    pub fn build_datamodel(s:tree::Tree,interner: &mut boa_interner::Interner)->Vec<Data>{
        trace!(target: "parser", "start build datamodel"); 
        let mut vec_data_c:Vec<Data> = Vec::new();
        for child in s.get_children(){
            let cur_child = child.get_value();
            match cur_child.get_name(){
                "data" =>{
                    vec_data_c.push(Data::build_data(child,
                        //omg_type,
                        interner));
                }
                _key => {
                }
            } 
        }
        trace!(target: "parser", "end build datamodel"); 
        return vec_data_c;
    }

    //These sets and get are needed for the builder
    pub fn get_id(&self) -> String{
        self.id.clone()
    }
    pub fn get_initial(&self) -> String{
        self.initial.clone()
    }
    pub fn get_datamodel(&self) -> Vec<Data>{
        self.datamodel.clone()
    }
    pub fn get_states(&self) -> HashMap<String,State>{
        self.states.clone()
    }

    //This part is not used by scan, it is only used to test the library and prints the class elements to the screen.
    pub fn stamp(&self){
        print!("State=Scxml\n");
        print!("id={}\n",self.id);
        print!("initial={}\n",self.initial);
        for data in self.datamodel.clone(){
            data.stamp();
        }
        for(_key,value) in self.states.clone(){
            value.stamp();
        }
    }
}