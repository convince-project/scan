 //Main file is only for us to test the library, it will not be used by scan
use std::{io::self, path::PathBuf};
use boa_interner::Interner;
use scxml_lib::build;

/*The main of the project takes as input a folder containing the files.
As a test the user is asked to enter the path to a file.
An Interner is created.
The file is finally passed to the build function contained within lib.rs and the created Scxml class is printed on the screen.*/
fn main() {
    //Asks the user to enter a directory
    let mut guess= String::new();
    io::stdin()
        .read_line(&mut guess)
        .expect("Failed to read line");
    let dir = guess.trim();    
    let path = PathBuf::from(dir);
    let mut interner_c= Interner::new();
    let test = build(path,&mut interner_c);
    test.stamp();
}

