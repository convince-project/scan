/*State class */
use crate::build_tree::tree;
use crate::build_parser::{Transition,Executable};

use log::{trace};

#[derive(Debug, Clone)]
pub struct State {
    pub(crate) id: String,
    pub(crate) transitions: Vec<Transition>,
    pub(crate) on_entry: Vec<Executable>,
    pub(crate) on_exit: Vec<Executable>,
}

impl State {
    pub fn new(id_c:String,transition_c:Vec<Transition>,on_entry_c:Vec<Executable>,on_exit_c:Vec<Executable>) -> Self {
        State{
            id:id_c,
            transitions : transition_c,
            on_entry : on_entry_c,
            on_exit : on_exit_c
        }
    }

    pub fn build_state(s:tree::Tree,
        //omg_type: Option<String>,
        interner: &mut boa_interner::Interner
        )->State{
        trace!(target: "parser", "start build state"); 
        //I initialize the class attributes
        let mut id_c ="".to_string();
        let mut transition_c: Vec<Transition> = Vec::new();
        let mut on_entry_c:Vec<Executable> = Vec::new();
        let mut on_exit_c:Vec<Executable> = Vec::new();
        for atr in s.get_value().get_attribute_list(){
            let cur_atr = atr.get_name();
            match cur_atr {
                "id" => {
                    id_c = atr.get_value().to_string();
                }
                _key => {
                }
            }
        }
        //Set children of the class
        for child in s.get_children(){
            let cur_child = child.get_value();
            match cur_child.get_name(){
                "transition" =>{
                   transition_c.push(Transition::build_transition(child, interner))
                }
                "onentry" =>{
                    let cur_one_entry= State::build_on_entry(child,interner);
                    for exec in cur_one_entry{
                        on_entry_c.push(exec);
                    }
                }
                "onexit" =>{
                    let cur_one_exit= State::build_on_exit(child,interner);
                    for exec in cur_one_exit{
                        on_exit_c.push(exec);
                    }
                }
                _key => {
                }
            } 
        }
        let state = State::new(id_c,transition_c,on_entry_c,on_exit_c);
        trace!(target: "parser", "end build state");
        return state;
    }

    //Funzion for create On_Entry array 
    fn build_on_entry(s:tree::Tree,interner: &mut boa_interner::Interner)->Vec<Executable>{
        trace!(target: "parser", "start build on_entry");
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
        trace!(target: "parser", "end build on_entry");
        return exec;
    }
    //function for creating On_Exit array
    fn build_on_exit(s:tree::Tree,interner: &mut boa_interner::Interner)->Vec<Executable>{
        trace!(target: "parser", "start build on_exit");
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
        trace!(target: "parser", "end build on_exit");
        return exec;
    }

    //These sets and get are needed for the builder
    pub fn get_id(&self)-> String{
        self.id.clone()
    }
    pub fn get_transitions(&self)-> Vec<Transition>{
        self.transitions.clone()
    }
    pub fn get_on_entry(&self)-> Vec<Executable>{
        self.on_entry.clone()
    }
    pub fn get_on_exit(&self)-> Vec<Executable>{
        self.on_exit.clone()
    }
    //This part is not used by scan, it is only used to test the library and prints the class elements to the screen.
    pub fn stamp(&self){
        print!("State=State\n");
        print!("id={}\n",self.id);
        for value in self.transitions.clone(){
            value.stamp();
        }
        for value in self.on_entry.clone(){
            print!("State=OnEntry\n");
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
        for value in self.on_exit.clone(){
            print!("State=OnExit\n");
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