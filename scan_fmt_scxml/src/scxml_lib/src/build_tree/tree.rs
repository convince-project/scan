use crate::build_tree::state::State;
use crate::build_tree::attribute_value::AttributeValue;

#[derive(Debug,Clone)]
pub struct Tree{
    value:State,
    id:i32,
    father_id:i32,
    children:Vec<Tree>,
}
//I define the functions of the Tree class
//new is the constructor
impl Tree{
    pub fn new(i:i32,fi:i32,v:State,c:Vec<Tree>)->Self{
        Tree{
           value:v,
           id:i,
           father_id:fi,
           children:c
        }
    }
    pub fn get_father_id(&self,i:i32)->i32{
        let mut cur_node: i32 = 0;
         if self.id == i{
            cur_node = self.father_id;
         }else{
             for ch in &mut self.get_children(){
                 cur_node = ch.get_father_id(i);
             }
         }
        cur_node
    }
    pub fn get_value_specific_node(&self,i:i32)->State{
        let attributes_void: Vec<AttributeValue> = vec![];
        let mut cur_node = State::new("NO".to_string(),attributes_void);
        if self.id == i{
           cur_node = self.value.clone();
        }else{
            for ch in &mut self.get_children(){
                cur_node = ch.get_value_specific_node(i);
            }
        }
       cur_node
    }
    pub fn get_value(&self) -> State {
        self.value.to_owned()
    }
    pub fn get_children(&self) -> Vec<Tree> {
        self.children.to_owned()
    }
    pub fn set_value(&mut self, v: State) { 
        self.value = v; 
    }
    pub fn insert_node(& mut self,v: State,i:i32,r:i32,b:bool)->i32{
        let mut cur_node: i32 = 0;
         if self.id == i{
             if v.get_name()=="CLOSE" {
                  cur_node = self.father_id;
             }else if v.get_name()=="COMMENT"{
                  cur_node = self.id;
             }
             else{
                 let children: Vec<Tree> = vec![];
                 let node = Tree::new(r,i,v,children);
                 self.children.push(node);
                 if b==false{
                     cur_node = r;
                 }else{
                     cur_node = i;
                 }
             }
         }else{
             for ch in &mut self.children{
                 cur_node = ch.insert_node(v.clone(), i, r,b);
             }
         }
        cur_node
     }
    //Function that stamp the Tree
    pub fn stamp(&self){
        self.value.stamp();
        print!("\n");
        for v in self.children.clone(){
            v.stamp();
        }
    }    
}