%start Expr
%%
//In Expr I inform the program that there can be three types of expressions
//In the < Try > case, using Try I tell them to take only the word between the <> brackets
//Using the value $2? + ":" I tell the program to take the word between the <> brackets and add the ":"
//In this case </ Try > I tell the program to return a string "" , which I will then ensure not to print on the screen in main
//If the <> brackets are not present, simply return the string
//NOTE to do this I inserted all the letters and brackets < , > , </ as key elements into the calc.l file

Expr -> Result<StateConteiner, ()>:
      'OPEN' Sta { 
            let res = $2?;
            Ok(res) 
        }
      |'OPEN_CLOSE' Close  {
        let res =  $2?;
        Ok(res) 
      }
      |'OPEN_COMMENT' Comment {
        let res = $2?;
        Ok(res)
      }
    ;

Comment -> Result<StateConteiner, ()>:
    'TYPE' Btwop Atwo
    {
        //let v = $3.map_err(|_| ())?;
        //let result=comment_state($lexer.span_str(v),true);
        let result = comment_state(&$3?,true);
        Ok(result)
    }
    
    |'IDENTIFIER' Ntype
    {
        let v = $1.map_err(|_| ())?;
        let result=comment_state($lexer.span_str(v.span()),false);
        Ok(result)
    }
;

Btwop -> Result<String, ()>:
    'TWOPOINTS'
    {
        Ok("".to_string())
    }
    |'IDENTIFIER' Btwop
    {
        Ok("".to_string())
    }
    |'STATE' Btwop
    {
        Ok("".to_string())
    }
    |'ATTRIBUTE' Btwop
    {
        Ok("".to_string())
    }
    |'INITIAL' Btwop
    {
        Ok("".to_string())
    }
    |'DATAMODEL' Btwop
    {
        Ok("".to_string())
    }
    |'INT' Btwop
    {
        Ok("".to_string())
    }
    |'UNDERSCORE' Btwop
    {
        Ok("".to_string())
    }
    |'SEMICOLON' Btwop
    {
        Ok("".to_string())
    }
    |'ECOMMERCIAL' Btwop
    {
        Ok("".to_string())
    }
    |'BDOWN' Btwop
    {
        Ok("".to_string())
    }
    |'SPOT' Btwop
    {
        Ok("".to_string())
    }
    |'ASTERISK' Btwop
    {
        Ok("".to_string())
    }
    |'EQUAL' Btwop
    {
        Ok("".to_string())
    }
    |'CLOSE' Btwop
    {
        Ok("".to_string())
    }
    |'OPEN' Btwop
    {
        Ok("".to_string())
    }
    |'MINUS' Btwop
    {
        Ok("".to_string())
    }
    |'PSIGN' Btwop
    {
        Ok("".to_string())
    }
;

Atwo-> Result<String, ()>:
    'IDENTIFIER' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'IDENTIFIER' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'STATE' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'STATE' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'ATTRIBUTE' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'ATTRIBUTE' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'INITIAL' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'INITIAL' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'DATAMODEL' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'DATAMODEL' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'INT' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'INT' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'UNDERSCORE' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'UNDERSCORE' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'TWOPOINTS' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'TWOPOINTS' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'SEMICOLON' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'SEMICOLON' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'ECOMMERCIAL' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'ECOMMERCIAL' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'BDOWN' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'BDOWN' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'SPOT' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'SPOT' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'ASTERISK' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'ASTERISK' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'EQUAL' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'EQUAL' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'CLOSE' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'CLOSE' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'OPEN' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'OPEN' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'MINUS' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'MINUS' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    |'PSIGN' 'CLOSED_COMMENT'
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_string())
    }
    |'PSIGN' Atwo
    {
        let v = $1.map_err(|_| ())?;
        Ok($lexer.span_str(v.span()).to_owned() + &$2?)
    }
    ;

Ntype -> Result<String, ()>:
    'CLOSED_COMMENT'
    {
        Ok("".to_string())
    }
    |'IDENTIFIER' Ntype
    {
        Ok("".to_string())
    }
    |'STATE' Ntype
    {
        Ok("".to_string())
    }
    |'ATTRIBUTE' Ntype
    {
        Ok("".to_string())
    }
    |'INITIAL' Ntype
    {
        Ok("".to_string())
    }
    |'DATAMODEL' Ntype
    {
        Ok("".to_string())
    }
    |'INT' Ntype
    {
        Ok("".to_string())
    }
    |'UNDERSCORE' Ntype
    {
        Ok("".to_string())
    }
    |'TWOPOINTS' Ntype
    {
        Ok("".to_string())
    }
    |'SEMICOLON' Ntype
    {
        Ok("".to_string())
    }
    |'ECOMMERCIAL' Ntype
    {
        Ok("".to_string())
    }
    |'BDOWN' Ntype
    {
        Ok("".to_string())
    }
    |'SPOT' Ntype
    {
        Ok("".to_string())
    }
    |'ASTERISK' Ntype
    {
        Ok("".to_string())
    }
    |'EQUAL' Ntype
    {
        Ok("".to_string())
    }
    |'CLOSE' Ntype
    {
        Ok("".to_string())
    }
    |'OPEN' Ntype
    {
        Ok("".to_string())
    }
    |'MINUS' Ntype
    {
        Ok("".to_string())
    }
    |'PSIGN' Ntype
    {
        Ok("".to_string())
    }
;

Close -> Result<StateConteiner, ()>:
    'STATE' 'CLOSE'
    {
        let v = $1.map_err(|_| ())?;
        let result=close_state($lexer.span_str(v.span()));
        Ok(result)
    }
    |'INITIAL' 'CLOSE'
    {
        let v = $1.map_err(|_| ())?;
        let result=close_state($lexer.span_str(v.span()));
        Ok(result)
    }
    |'DATAMODEL' 'CLOSE'
    {
        let v = $1.map_err(|_| ())?;
        let result=close_state($lexer.span_str(v.span()));
        Ok(result)
    }
;
 
Sta -> Result<StateConteiner, ()>:
    'STATE' 'CLOSE'
    {
        //Set the default attribute of the state and chek if attribute controll is empty
        //If current_attributes_control isn't empty since in this case no attributes are initialized 
        //it means that some mandatory attributes have not been initialized so I print an error on the screen and block the program.
        //In this case the state is open but not is closed so i put the boolean to false
        
        let v = $1.map_err(|_| ())?;
        let current_attributes: Vec<AttributeValue> = set_current_attrubutes($lexer.span_str(v.span()));
        let current_attributes_control: Vec<AttributeControll> = set_current_attrubutes_control($lexer.span_str(v.span()));
        if !current_attributes_control.is_empty(){
            print!("ERROR One or more mandatory attributes have not been defined");
            std::process::exit(0);
        }
        Ok(set_state($lexer.span_str(v.span()),false,current_attributes))
    }
    |'STATE' 'RAPID_CLOSE'
    {
        //Set the default attribute of the state and chek if attribute controll is empty
        //If current_attributes_control isn't empty since in this case no attributes are initialized 
        //it means that some mandatory attributes have not been initialized so I print an error on the screen and block the program.
        //In this case the state is open e quickly closed so i put the boolean to true

        let v = $1.map_err(|_| ())?;
        let current_attributes: Vec<AttributeValue> = set_current_attrubutes($lexer.span_str(v.span()));
        let current_attributes_control: Vec<AttributeControll> = set_current_attrubutes_control($lexer.span_str(v.span()));
        if !current_attributes_control.is_empty(){
           print!("ERROR One or more mandatory attributes have not been defined");
           std::process::exit(0);
        }
        Ok(set_state($lexer.span_str(v.span()),true,current_attributes))
    }
    |'STATE' Atr
    {   
        //In this case I have an attribute with initialized states so I use the check attribute function 
        //to see if the attributes are correct
        
        let v = $1.map_err(|_| ())?;
        Ok(check_attribute($lexer.span_str(v.span())))
    }
    |'INITIAL' 'CLOSE'
    {
        //Set the default attribute of the state and chek if attribute controll is empty
        //If current_attributes_control isn't empty since in this case no attributes are initialized 
        //it means that some mandatory attributes have not been initialized so I print an error on the screen and block the program.
        //In this case the state is open but not is closed so i put the boolean to false
        
        let v = $1.map_err(|_| ())?;
        let current_attributes: Vec<AttributeValue> = set_current_attrubutes($lexer.span_str(v.span()));
        let current_attributes_control: Vec<AttributeControll> = set_current_attrubutes_control($lexer.span_str(v.span()));
        if !current_attributes_control.is_empty(){
            print!("ERROR One or more mandatory attributes have not been defined");
            std::process::exit(0);
        }
        Ok(set_state($lexer.span_str(v.span()),false,current_attributes))
    }
    |'INITIAL' 'RAPID_CLOSE'
    {
        //Set the default attribute of the state and chek if attribute controll is empty
        //If current_attributes_control isn't empty since in this case no attributes are initialized 
        //it means that some mandatory attributes have not been initialized so I print an error on the screen and block the program.
        //In this case the state is open e quickly closed so i put the boolean to true

        let v = $1.map_err(|_| ())?;
        let current_attributes: Vec<AttributeValue> = set_current_attrubutes($lexer.span_str(v.span()));
        let current_attributes_control: Vec<AttributeControll> = set_current_attrubutes_control($lexer.span_str(v.span()));
        if !current_attributes_control.is_empty(){
           print!("ERROR One or more mandatory attributes have not been defined");
           std::process::exit(0);
        }
        Ok(set_state($lexer.span_str(v.span()),true,current_attributes))
    }
    |'INITIAL' Atr
    {   
        //In this case I have an attribute with initialized states so I use the check attribute function 
        //to see if the attributes are correct
        
        let v = $1.map_err(|_| ())?;
        Ok(check_attribute($lexer.span_str(v.span())))
    }
    |'DATAMODEL' 'CLOSE'
    {
        //Set the default attribute of the state and chek if attribute controll is empty
        //If current_attributes_control isn't empty since in this case no attributes are initialized 
        //it means that some mandatory attributes have not been initialized so I print an error on the screen and block the program.
        //In this case the state is open but not is closed so i put the boolean to false
        
        let v = $1.map_err(|_| ())?;
        let current_attributes: Vec<AttributeValue> = set_current_attrubutes($lexer.span_str(v.span()));
        let current_attributes_control: Vec<AttributeControll> = set_current_attrubutes_control($lexer.span_str(v.span()));
        if !current_attributes_control.is_empty(){
            print!("ERROR One or more mandatory attributes have not been defined");
            std::process::exit(0);
        }
        Ok(set_state($lexer.span_str(v.span()),false,current_attributes))
    }
    |'DATAMODEL' 'RAPID_CLOSE'
    {
        //Set the default attribute of the state and chek if attribute controll is empty
        //If current_attributes_control isn't empty since in this case no attributes are initialized 
        //it means that some mandatory attributes have not been initialized so I print an error on the screen and block the program.
        //In this case the state is open e quickly closed so i put the boolean to true

        let v = $1.map_err(|_| ())?;
        let current_attributes: Vec<AttributeValue> = set_current_attrubutes($lexer.span_str(v.span()));
        let current_attributes_control: Vec<AttributeControll> = set_current_attrubutes_control($lexer.span_str(v.span()));
        if !current_attributes_control.is_empty(){
           print!("ERROR One or more mandatory attributes have not been defined");
           std::process::exit(0);
        }
        Ok(set_state($lexer.span_str(v.span()),true,current_attributes))
    }
    |'DATAMODEL' Atr
    {   
        //In this case I have an attribute with initialized states so I use the check attribute function 
        //to see if the attributes are correct
        
        let v = $1.map_err(|_| ())?;
        Ok(check_attribute($lexer.span_str(v.span())))
    }
    ;

Atr-> Result<String, ()>:
    'ATTRIBUTE' 'EQUAL' 'V' Text 'CLOSE'
    {
        let v = $1.map_err(|_| ())?;
        *CURRENT_S.write().unwrap() = "CLOSE";
        get_attribute(&($lexer.span_str(v.span()).to_owned() +"="+ &$4.clone()?));
        Ok("ok".to_string())
    }
    |'ATTRIBUTE' 'EQUAL' 'V' Text 'RAPID_CLOSE'
    {
        let v = $1.map_err(|_| ())?;
        *CURRENT_S.write().unwrap() = "RAPID_CLOSE";
        get_attribute(&($lexer.span_str(v.span()).to_owned() +"="+ &$4.clone()?));
        Ok("ok".to_string())
    }
    |'ATTRIBUTE' 'EQUAL' 'V' Text Atr
    {
       let v = $1.map_err(|_| ())?;
       get_attribute(&($lexer.span_str(v.span()).to_owned() +"="+ &$4.clone()?));
       Ok("ok".to_string())
    }
    |'INITIAL' 'EQUAL' 'V' Text 'CLOSE'
    {
        let v = $1.map_err(|_| ())?;
        *CURRENT_S.write().unwrap() = "CLOSE";
        get_attribute(&($lexer.span_str(v.span()).to_owned() +"="+ &$4.clone()?));
        Ok("ok".to_string())
    }
    |'INITIAL' 'EQUAL' 'V' Text 'RAPID_CLOSE'
    {
        let v = $1.map_err(|_| ())?;
       *CURRENT_S.write().unwrap() = "RAPID_CLOSE";
        get_attribute(&($lexer.span_str(v.span()).to_owned() +"="+ &$4.clone()?));
        Ok("ok".to_string())
    }
    |'INITIAL' 'EQUAL' 'V' Text Atr
    {
       let v = $1.map_err(|_| ())?;
       get_attribute(&($lexer.span_str(v.span()).to_owned() +"="+ &$4.clone()?));
       Ok("ok".to_string())
    }
    |'DATAMODEL' 'EQUAL' 'V' Text 'CLOSE'
    {
        let v = $1.map_err(|_| ())?;
        *CURRENT_S.write().unwrap() = "CLOSE";
        get_attribute(&($lexer.span_str(v.span()).to_owned() +"="+ &$4.clone()?));
        Ok("ok".to_string())
    }
    |'DATAMODEL' 'EQUAL' 'V' Text 'RAPID_CLOSE'
    {
        let v = $1.map_err(|_| ())?;
        *CURRENT_S.write().unwrap() = "RAPID_CLOSE";
        get_attribute(&($lexer.span_str(v.span()).to_owned() +"="+ &$4.clone()?));
        Ok("ok".to_string())
    }
    |'DATAMODEL' 'EQUAL' 'V' Text Atr
    {
       let v = $1.map_err(|_| ())?;
       get_attribute(&($lexer.span_str(v.span()).to_owned() +"="+ &$4.clone()?));
       Ok("ok".to_string())
    }
    ;
   
Text-> Result<String, ()>:
    'IDENTIFIER' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'IDENTIFIER' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'STATE' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'STATE' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'ATTRIBUTE' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'ATTRIBUTE' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'INITIAL' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'INITIAL' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'DATAMODEL' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'DATAMODEL' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'INT' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'INT' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'UNDERSCORE' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'UNDERSCORE' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'TWOPOINTS' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'TWOPOINTS' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'SEMICOLON' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'SEMICOLON' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'ECOMMERCIAL' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'ECOMMERCIAL' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'BDOWN' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'BDOWN' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'SPOT' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'SPOT' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'ASTERISK' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'ASTERISK' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'EQUAL' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'EQUAL' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'CLOSE' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'CLOSE' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'OPEN' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'OPEN' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'MINUS' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'MINUS' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'HBAR' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'HBAR' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'EXMARK' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'EXMARK' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    |'PSIGN' 'V'
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())))
    }
    |'PSIGN' Text
    {
        let v = $1.map_err(|_| ())?;
        Ok(get_text($lexer.span_str(v.span())) + &$2?)
    }
    ;
%%
// Any functions here are in scope for all the grammar actions above.
use crate::build_tree::attribute_value::AttributeValue;
use crate::build_tree::state::State;
use crate::build_tree::state_conteiner::StateConteiner;
use std::sync::RwLock;
static IS_PRIME: RwLock<bool> = RwLock::new(true);
static CHILDREN: RwLock<&[&str]> = RwLock::new(&[]);
static OPENED_STATE: RwLock<Vec<String>> = RwLock::new(vec![]);
static CURRENT_S: RwLock<&str> = RwLock::new("HELLO");
static CURRENT_A: RwLock<Vec<String>> = RwLock::new(vec![]);
static ID_USED: RwLock<Vec<String>> = RwLock::new(vec![]);
static CURRENT_TYPE: RwLock<Option<String>> = RwLock::new(None);


//Struct to handle attributes that need to be initialized
struct AttributeControll {
    name: String,
    value: bool,
}

//function for set the current state
//check if state is correct and return to a StateConteiner class
fn set_state(s: &str, b: bool,a:Vec<AttributeValue>) -> StateConteiner {
    let s_t = s.trim();                   //Remove whitespace before and after the string
    check_current_state(s,b);
    let new_state = State::new(s_t.to_string(),a);
    let conteiner_new_state = StateConteiner::new(new_state,b);
    conteiner_new_state
}

//function for set and controll the attribute of a state
fn check_attribute(s: &str) -> StateConteiner {
    let s_t = s.trim();                    //Remove whitespace before and after the string
    let mut current_attributes: Vec<AttributeValue> = set_current_attrubutes(s_t);
    let mut current_attributes_control: Vec<AttributeControll> = set_current_attrubutes_control(s_t);   
    for attribute in &mut CURRENT_A.read().unwrap().iter(){
        let eq_position = attribute.find("=");
        let (cur_atr,cur_v) = attribute.split_at(eq_position.expect("REASON"));
        let mut cur_val = cur_v.to_string();
        cur_val.remove(0);
        if current_attributes.len()>0{
            let mut is_valid_attribute = false;
            for cur in &mut current_attributes{
                if cur.get_name() == cur_atr{
                    if cur.get_name() == "id"{
                        check_single_id(&cur_val.clone());
                    }
                    if cur.get_name() == "type"{
                        if let Some(count) = CURRENT_TYPE.read().unwrap().as_ref() {
                            cur.set_value(count.to_string());
                            *CURRENT_TYPE.write().unwrap() = None;
                        } else {
                        }
                    }
                    cur.set_value(cur_val.clone());
                    is_valid_attribute = true;
                }
                let mut c_type = false;
                if cur.get_name() == "type" {
                    if let Some(r#count) = CURRENT_TYPE.read().unwrap().as_ref() {
                        cur.set_value(r#count.to_string());
                        c_type = true;
                    }
                }
                if c_type==true {
                    *CURRENT_TYPE.write().unwrap() = None;
                }
            }
            if is_valid_attribute == false{
                print!("ERROR the attribute {} does not exist or is not compatible with the current state {}",cur_atr,s_t);
                std::process::exit(0);
            }
        }
        if current_attributes_control.len()>0{
            for cur in &mut current_attributes_control{
                if cur.name == cur_atr{
                    cur.value = true;
                }
            }
        }  
    }
    if current_attributes_control.len()>0{
        let mut is_mandatory_confermed = true;
        for cur in &mut current_attributes_control{
            if cur.value == false{
                is_mandatory_confermed = false;
            }
        }
        if is_mandatory_confermed ==false{
            print!("ERROR One or more mandatory attributes of the state:{} have not been defined",s_t);
            std::process::exit(0);
        }
    }
    CURRENT_A.write().unwrap().clear();
    let new_state;
    if *CURRENT_S.read().unwrap() == "CLOSE"{
        new_state = set_state(s_t,false,current_attributes);
    }else{
        new_state = set_state(s_t,true,current_attributes);
    }
    new_state
    
}

fn check_single_id(s: &str){
    let s_t=s.trim();
    if ID_USED.read().unwrap().is_empty()==false{
        for val in ID_USED.read().unwrap().clone(){
            if val == s_t{
                print!("ERROR there isn't two id equal");
                std::process::exit(0);
            }
        }
    }
    ID_USED.write().unwrap().push(s_t.to_string()); 
}

//Function for obtein the attribute from a state
fn get_attribute(s: &str){
    let s_t = s.trim();//Remove whitespace before and after the string
    CURRENT_A.write().unwrap().push(s_t.to_string());
}

fn get_text(s: &str) -> String {
    let s_t = s.trim();//Remove whitespace before and after the string
    s_t.to_string()
}

//Function for close the state
//Check if the state that must be close is the correct state in another case put an error and block the program
fn close_state(s: &str) -> StateConteiner {
    let s_t = s.trim();
    //Check if the state that i closed is the state that i'm expected that must closed
    if s_t ==  OPENED_STATE.write().unwrap().pop().unwrap(){
        //If the state taht i closed is scxl say that is the end of file and i reset my parametres
        if s_t=="scxml"{
            *IS_PRIME.write().unwrap() = true;
            *CHILDREN.write().unwrap() = &[];
            OPENED_STATE.write().unwrap().clear();
            *CURRENT_S.write().unwrap() = "HELLO";
            CURRENT_A.write().unwrap().clear();
            ID_USED.write().unwrap().clear();
        }else{
                if let Some(val) = OPENED_STATE.write().unwrap().last() {
                    set_child(val);
            } else {
                println!("The vector is empty");
            }
        }
    }
    else{
        "ERROR there is a state that was not closed".to_string();
            std::process::exit(0);
    }
    let attributes_close_state: Vec<AttributeValue> = vec![];
    let close_state = State::new("CLOSE".to_string(),attributes_close_state);
    let conteiner_close_state = StateConteiner::new(close_state,false);
    conteiner_close_state
}

fn comment_state(s: &str,b: bool) -> StateConteiner {
    let s_t= s.trim();
    let attributes_close_state: Vec<AttributeValue> = vec![];
    let comment_state = State::new("COMMENT".to_string(),attributes_close_state);
    let conteiner_comment_state = StateConteiner::new(comment_state,false);
    if b==true{
        *CURRENT_TYPE.write().unwrap() = Some(s_t.to_string());
    }
    conteiner_comment_state
}

//Function that set the current_child when a state is closed
fn set_child(s:&str){
    let s_t = s.trim(); 
    match s_t{
        //Check if there are any special tags
        "scxml" => {
                    let current_child: &[&str] = &["parallel","state","final","datamodel","script"];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "state" => {
                    let current_child: &[&str] = &["onentry","onexit","transition","initial","parallel","final","history","datamodel","invoke"];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "parallel" => {
                    print!("ERROR SCAN not support this scxml state");
                    std::process::exit(0);
                },
        "transition" => {
                    let current_child: &[&str] = &["onentry","onexit","assign","send"];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "initial" => {
                    let current_child: &[&str] = &["transition"];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "final" => {
                    print!("ERROR SCAN not support this scxml state");
                    std::process::exit(0);
                },
        "onentry" => {
                    let current_child: &[&str] = &["if","elseif","else","raise","foreach","log","assign","script","send","cancel"];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "onexit" => {
                    let current_child: &[&str] = &["if","elseif","else","raise","foreach","log","assign","script","send","cancel"];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "history" => {
                    print!("ERROR SCAN not support this scxml state");
                    std::process::exit(0);
                },
        "raise" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "if" => {
                    let current_child: &[&str] = &["elseif","else","send"];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "elseif" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "else" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "foreach" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "log" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "datamodel" => {
                    let current_child: &[&str] = &["data"];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "data" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "assign" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child; 
                },
        "donedata" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "conten" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "param" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "script" => {
                    print!("ERROR SCAN not support this scxml state");
                    std::process::exit(0);
                },
        "send" => {
                    let current_child: &[&str] = &["param"];
                    *CHILDREN.write().unwrap() = current_child;
                },
        "cancel" => {
                    print!("ERROR SCAN not support this scxml state");
                    std::process::exit(0);
                },
        "invoke" => {
                    print!("ERROR SCAN not support this scxml state");
                    std::process::exit(0);
                },
        "finalize" => {
                    let current_child: &[&str] = &[];
                    *CHILDREN.write().unwrap() = current_child;
                },      
        &_ => todo!()
    }
}

//function that check if a state is correct
fn check_current_state(s:&str,b:bool){
    let s_t = s.trim();                   //Remove whitespace before and after the string
    if s_t == "scxml"{
        if *IS_PRIME.read().unwrap() == true{
            let current_child: &[&str] = &["parallel","state","final","datamodel","script"];
            *CHILDREN.write().unwrap() = current_child;
            *IS_PRIME.write().unwrap() = false;
            OPENED_STATE.write().unwrap().push("scxml".to_string());
        }else{
            print!("ERROR in a file there can only be one scxml");
            std::process::exit(0);
        }  
    }else{
        if *IS_PRIME.read().unwrap() == false{ 
            let mut is_child = false;
            for child in *CHILDREN.read().unwrap(){
                if *child == s_t{
                    is_child = true;
                }
            }
            if is_child == true{
                if b == false {
                    OPENED_STATE.write().unwrap().push(s_t.to_string());
                }
                match s_t{
                        //Check if there are any special tags
                        "state" => {
                                    if b == false{
                                        let current_child: &[&str] = &["onentry","onexit","transition","initial","parallel","final","history","datamodel","invoke"];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "parallel" => {
                                        print!("ERROR SCAN not support this scxml state");
                                        std::process::exit(0);
                                },
                        "transition" => {
                                    if b == false{
                                        let current_child: &[&str] = &["onentry","onexit","assign","send","raise","if"];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "initial" => {
                                    if b == false{
                                        let current_child: &[&str] = &["transition"];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "final" => {
                                    print!("ERROR SCAN not support this scxml state");
                                    std::process::exit(0);
                                },
                        "onentry" => {
                                    if b == false{
                                        let current_child: &[&str] = &["if","elseif","else","raise","foreach","log","assign","script","send","cancel"];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "onexit" => {
                                    if b == false{
                                        let current_child: &[&str] = &["if","elseif","else","raise","foreach","log","assign","script","send","cancel"];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "history" => {
                                    print!("ERROR SCAN not support this scxml state");
                                    std::process::exit(0);
                                },
                        "raise" => {
                                    if b == false{
                                        let current_child: &[&str] = &[];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "if" => {
                                    if b == false{
                                        let current_child: &[&str] = &["elseif","else","send","assign","raise","if"];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "elseif" => {
                                    if b == false{
                                        let current_child: &[&str] = &["send","assign","raise","if"];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "else" => {
                                    if b == false{
                                        let current_child: &[&str] = &["send","assign","raise","if"];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "foreach" => {
                                    if b == false{
                                        let current_child: &[&str] = &[];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "log" => {
                                    if b == false{
                                        let current_child: &[&str] = &[];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "datamodel" => {
                                    if b == false{
                                        let current_child: &[&str] = &["data"];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "data" => {
                                    if b == false{
                                        let current_child: &[&str] = &[];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "assign" => {
                                    if b == false{
                                        let current_child: &[&str] = &[];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "donedata" => {
                                    if b == false{
                                        let current_child: &[&str] = &[];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "conten" => {
                                    if b == false{
                                        let current_child: &[&str] = &[];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "param" => {
                                    if b == false{
                                        let current_child: &[&str] = &[];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "script" => {
                                    print!("ERROR SCAN not support this scxml state");
                                    std::process::exit(0);
                                },
                        "send" => {
                                    if b == false{
                                        let current_child: &[&str] = &["param"];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },
                        "cancel" => {
                                    print!("ERROR SCAN not support this scxml state");
                                    std::process::exit(0);
                                },
                        "invoke" => {
                                    print!("ERROR SCAN not support this scxml state");
                                    std::process::exit(0);
                                },
                        "finalize" => {
                                    if b == false{
                                        let current_child: &[&str] = &[];
                                        *CHILDREN.write().unwrap() = current_child;
                                    }
                                },      
                        &_ => todo!()
                }
            }else{
                print!("ERROR the state {} is not one of childern",s_t);
                std::process::exit(0);
            } 
        }else{
            print!("ERROR in a file must be only one scxml")
        }
    }
}

//function that initialized the attributes of the current state to the default value
fn set_current_attrubutes(s:&str)->Vec<AttributeValue>{
    let s_t = s.trim();
    let mut current_attributes: Vec<AttributeValue> = vec![];
    match s_t{
        "scxml" => {
                   current_attributes.push(AttributeValue::new("initial".to_string(),"none".to_string()));
                   current_attributes.push(AttributeValue::new("name".to_string(),"none".to_string()));
                   current_attributes.push(AttributeValue::new("xmlns".to_string(),"none".to_string()));
                   current_attributes.push(AttributeValue::new("version".to_string(),"none".to_string()));
                   current_attributes.push(AttributeValue::new("datamodel".to_string(),"null".to_string()));
                   current_attributes.push(AttributeValue::new("binding".to_string(),"early".to_string()));
                },
        "state" => {
                   current_attributes.push(AttributeValue::new("id".to_string(),"none".to_string()));
                   current_attributes.push(AttributeValue::new("initial".to_string(),"none".to_string())); 
                },
        "parallel" => {
                    print!("ERROR SCAN not support this scxml state");
                    std::process::exit(0);
                },
        "transition" => {
                   current_attributes.push(AttributeValue::new("event".to_string(),"none".to_string()));
                   current_attributes.push(AttributeValue::new("cond".to_string(),"none".to_string()));
                   current_attributes.push(AttributeValue::new("target".to_string(),"none".to_string()));
                   current_attributes.push(AttributeValue::new("type".to_string(),"external".to_string()));
                },
        "initial" => {
                   
                },
        "final" => {
                   print!("ERROR SCAN not support this scxml state");
                   std::process::exit(0);
                },
        "onentry" => {
                  
                },
        "onexit" => {
                 
                },
        "history" => {
                   print!("ERROR SCAN not support this scxml state");
                   std::process::exit(0);
                },
         "raise" => {
                   current_attributes.push(AttributeValue::new("event".to_string(),"none".to_string()));
                },
         "if" => {
                   current_attributes.push(AttributeValue::new("cond".to_string(),"none".to_string()));
                },
        "elseif" => {
                current_attributes.push(AttributeValue::new("cond".to_string(),"none".to_string()));
            },
        "else" => {
            
            },
        "foreach" => {
                current_attributes.push(AttributeValue::new("array".to_string(),"none".to_string()));
                current_attributes.push(AttributeValue::new("item".to_string(),"none".to_string()));
                current_attributes.push(AttributeValue::new("index".to_string(),"none".to_string()));
            },
        "log" => {
                current_attributes.push(AttributeValue::new("label".to_string()," ".to_string()));
                current_attributes.push(AttributeValue::new("expr".to_string(),"none".to_string()));
            },
        "datamodel" => {
               
            },
        "data" => {
                current_attributes.push(AttributeValue::new("name".to_string()," ".to_string()));
                current_attributes.push(AttributeValue::new("type".to_string(),"none".to_string()));
                current_attributes.push(AttributeValue::new("id".to_string(),"none".to_string()));
                current_attributes.push(AttributeValue::new("src".to_string(),"none".to_string()));
                current_attributes.push(AttributeValue::new("expr".to_string(),"none".to_string()));
            },
        "donedata" => {
               
            },
        "content" => {
               current_attributes.push(AttributeValue::new("expr".to_string(),"none".to_string()));
            },
        "param" => {
               current_attributes.push(AttributeValue::new("name".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("expr".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("location".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("type".to_string(),"none".to_string()));
            },
        "script" => {
               print!("ERROR SCAN not support this scxml state");
               std::process::exit(0);
            },
        "send" => {
               current_attributes.push(AttributeValue::new("event".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("eventexpr".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("target".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("targetexpr".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("type".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("typeexpr".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("id".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("idlocation".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("delay".to_string(),"None".to_string()));
               current_attributes.push(AttributeValue::new("delayexpr".to_string(),"None".to_string()));
               current_attributes.push(AttributeValue::new("namelist".to_string(),"none".to_string()));
            },
        "cancel" => {
               print!("ERROR SCAN not support this scxml state");
               std::process::exit(0);
            },
        "invoke" => {
               print!("ERROR SCAN not support this scxml state");
               std::process::exit(0);
            },
        "finalize" => {

        },
        "assign" => {
               current_attributes.push(AttributeValue::new("location".to_string(),"none".to_string()));
               current_attributes.push(AttributeValue::new("expr".to_string(),"none".to_string()));
        },
        &_ => todo!()
    }
    current_attributes
}

//function that initialized the attributes that must be inizialized of the current state 
fn set_current_attrubutes_control(s:&str)->Vec<AttributeControll>{
    let s_t = s.trim();
    let mut current_attributes_control: Vec<AttributeControll> = vec![];
    match s_t{
        "scxml" => {
                   current_attributes_control.push(AttributeControll{name : "xmlns".to_string(),value:false});
                   current_attributes_control.push(AttributeControll{name : "version".to_string(),value:false}); 
                },
        "state" => {
 
                },
        "parallel" => {
                    print!("ERROR SCAN not support this scxml state");
                    std::process::exit(0);
                },
        "transition" => {
                },
        "initial" => {
                   
                },
        "final" => {
                    print!("ERROR SCAN not support this scxml state");
                    std::process::exit(0);
                },
        "onentry" => {
                  
                },
        "onexit" => {
                 
                },
        "history" => {
                    print!("ERROR SCAN not support this scxml state");
                    std::process::exit(0);
                },
         "raise" => {
                   current_attributes_control.push(AttributeControll{name : "event".to_string(),value:false});
                },
         "if" => {                   
                   current_attributes_control.push(AttributeControll{name : "cond".to_string(),value:false});
                },
        "elseif" => {                
                current_attributes_control.push(AttributeControll{name : "cond".to_string(),value:false});
            },
        "else" => {
            
            },
        "foreach" => {
                current_attributes_control.push(AttributeControll{name : "array".to_string(),value:false});
                current_attributes_control.push(AttributeControll{name : "item".to_string(),value:false});
            },
        "log" => {
            },
        "datamodel" => {
               
            },
        "data" => {
                //current_attributes_control.push(AttributeControll{name : "id".to_string(),value:false});
            },
        "donedata" => {
               
            },
        "content" => {
               
            },
        "param" => {
               current_attributes_control.push(AttributeControll{name : "name".to_string(),value:false});
            },
        "script" => {
               print!("ERROR SCAN not support this scxml state");
               std::process::exit(0);
            },
        "send" => {
             
            },
        "cancel" => {
               print!("ERROR SCAN not support this scxml state");
               std::process::exit(0);
            },
        "invoke" => {
                print!("ERROR SCAN not support this scxml state");
                std::process::exit(0);
            },
        "finalize" => {

        },
        "assign" => {

        },
        &_ => todo!()
    }
    current_attributes_control
}