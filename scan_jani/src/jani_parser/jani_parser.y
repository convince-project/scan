%start Root
%expect-unused Unmatched "UNMATCHED"

%%
//TODO: implement JANI extensions!

Root -> Result<ASTNode, ParsingError>:
    '{' Model '}' 
    { 
        // The 'Model' rule ($2) returns a vector containing the fields (as ASTNodes, see jani_parser.rs) of the model ("jani-version", "name", etc... See JANI specification)
        let properties = $2?;

        // Check whether all the required fields and no duplicates are present in the vector 
        // If everything is okay, return a Model node containing the vector
        // Otherwise print an error message, indicating in which portion of the file (span of the rule) the error occurred
        match check_model(&properties) {
            Ok(()) => Ok(ASTNode::ASTModel{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: model is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

//Store the Nodes in a vector one at a time, and return the vector
Model -> Result<Vec<ASTNode>, ParsingError>:
    ModelEntry ',' Model
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ModelEntry { Ok(vec![$1?]) }
    ;

//For every possible field, return the corresponding Node
ModelEntry -> Result<ASTNode, ParsingError>:
    'jani-version' ':' 'NUMBER' 
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span());
        let float_number = f64::from_str(s).map_err(|_| ParsingError::InvalidSyntaxError)?;
        let number = float_number as u64;
        Ok(ASTNode::ASTModelVersion{ version: number })
    }
    | 'name' ':' Identifier 
    {
        Ok(ASTNode::ASTModelName{ name: Box::new($3?) })
    }
    | 'metadata' ':' '{' Metadata '}' 
    {   
        let properties = $4?;
        match check_metadata(&properties) {
            Ok(()) => Ok(ASTNode::ASTModelMetadata{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'metadata' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | 'type' ':' ModelType 
    { 
        Ok($3?) 
    }
    | 'features' ':' '[' ModelFeatures ']' 
    { 
        Ok(ASTNode::ASTModelFeatures{ features: $4? }) 
    }
    | 'features' ':' '[' ']' 
    { 
        Ok(ASTNode::ASTModelFeatures{ features: vec![ASTNode::ASTEmpty] }) 
    }
    | 'actions' ':' '[' Actions ']' 
    { 
        Ok(ASTNode::ASTModelActions{ actions: $4? }) 
    }
    | 'actions' ':' '[' ']' 
    { 
        Ok(ASTNode::ASTModelActions{ actions: vec![ASTNode::ASTEmpty] }) 
    }
    | 'constants' ':' '[' ConstantDeclarations ']' 
    { 
        Ok(ASTNode::ASTModelConstants{ constants: $4? }) 
    }
    | 'constants' ':' '[' ']' 
    { 
        Ok(ASTNode::ASTModelConstants{ constants: vec![ASTNode::ASTEmpty] }) 
    }
    | 'variables' ':' '[' VariableDeclarations ']' 
    { 
        Ok(ASTNode::ASTModelVariables{ variables: $4? }) 
    }
    | 'variables' ':' '[' ']' 
    { 
        Ok(ASTNode::ASTModelVariables{ variables: vec![ASTNode::ASTEmpty] }) 
    }
    | 'restrict-initial' ':' '{' RestrictInitial '}' 
    { 
        let properties = $4?;
        match check_restrict_initial(&properties) {
            Ok(()) => Ok(ASTNode::ASTModelRestrictInitial{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'restrict-initial' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    }
    | 'properties' ':' '[' Properties ']' 
    { 
        Ok(ASTNode::ASTModelProperties{ properties: $4? }) 
    }
    | 'properties' ':' '[' ']' 
    { 
        Ok(ASTNode::ASTModelProperties{ properties: vec![ASTNode::ASTEmpty] }) 
    }
    | 'automata' ':' '[' Automata ']' 
    { 
        Ok(ASTNode::ASTModelAutomata{ automata: $4? }) 
    }
    | 'system' ':' '{' Composition '}' 
    { 
        let properties = $4?;
        match check_model_system(&properties) {
            Ok(()) => Ok(ASTNode::ASTModelSystem{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'system' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    }
    ;

//Continue using the same pattern

Metadata -> Result<Vec<ASTNode>, ParsingError>:
    MetadataEntry ',' Metadata
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | MetadataEntry { Ok(vec![$1?]) }
    ;

MetadataEntry -> Result<ASTNode, ParsingError>:
    'version' ':' 'STRING' 
    { 
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTMetadataVersion{ version: s })
    }
    | 'author' ':' 'STRING' 
    { 
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTMetadataAuthor{ author: s })
    }
    | 'description' ':' 'STRING' 
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTMetadataDescription{ description: s })
    }
    | 'doi' ':' 'STRING' 
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTMetadataDoi{ doi: s })
    }
    | 'url' ':' 'STRING' 
    { 
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTMetadataUrl{ url: s })
    }
    ;


ModelType -> Result<ASTNode, ParsingError>:
    'MODELTYPE' 
    {
        let v = $1.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTModelType{ modeltype: s })
    }
    ;


ModelFeatures -> Result<Vec<ASTNode>, ParsingError>:
    ModelFeature ',' ModelFeatures
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ModelFeature { Ok(vec![$1?]) }
    ;

ModelFeature -> Result<ASTNode, ParsingError>:
    'MODELFEATURE' 
    {
        let v = $1.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTModelFeature{ modelfeature: s })
    }
    ;


Actions -> Result<Vec<ASTNode>, ParsingError>:
    '{' Action '}' ',' Actions
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_model_action(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTModelAction{ properties: properties });
                Ok(vec) 
            }
            Err(e) => {
                eprintln!("Parsing Error: an element in 'action' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' Action '}'
    {
        let properties = $2?;
        match check_model_action(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTModelAction{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'action' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

Action -> Result<Vec<ASTNode>, ParsingError>:
    ActionEntry ',' Action
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ActionEntry { Ok(vec![$1?]) }
    ;

ActionEntry -> Result<ASTNode, ParsingError>:
    'name' ':' Identifier 
    { 
        Ok(ASTNode::ASTModelActionName{ name: Box::new($3?) }) 
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;


ConstantDeclarations -> Result<Vec<ASTNode>, ParsingError>:
    '{' ConstantDeclaration '}' ',' ConstantDeclarations
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_constant_declaration(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTConstantDeclaration{ properties: properties });
                Ok(vec) 
            }
            Err(e) => {
                eprintln!("Parsing Error: an element in 'constants' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' ConstantDeclaration '}' 
    { 
        let properties = $2?;
        match check_constant_declaration(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTConstantDeclaration{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'constants' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

ConstantDeclaration -> Result<Vec<ASTNode>, ParsingError>:
    ConstantDeclarationEntry ',' ConstantDeclaration
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ConstantDeclarationEntry { Ok(vec![$1?]) }
    ;

ConstantDeclarationEntry -> Result<ASTNode, ParsingError>:
    'name' ':' Identifier
    {
        Ok(ASTNode::ASTConstantDeclarationName{ name: Box::new($3?) }) 
    }
    | 'type' ':' BasicType 
    { 
        Ok(ASTNode::ASTConstantDeclarationType{ type_: Box::new($3?) }) 
    }
    | 'type' ':' '{' BoundedType '}'
    { 
        let properties = $4?;
        match check_bounded_type(&properties) {
            Ok(()) => Ok(ASTNode::ASTConstantDeclarationType{ type_: Box::new(ASTNode::ASTBoundedType{ properties: properties }) }),
            Err(e) => {
                eprintln!("Parsing Error: 'type' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | 'value' ':' Expression 
    { 
        Ok(ASTNode::ASTConstantDeclarationValue{ value: Box::new($3?) }) 
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;


VariableDeclarations -> Result<Vec<ASTNode>, ParsingError>:
    '{' VariableDeclaration '}' ',' VariableDeclarations
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_variable_declaration(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTVariableDeclaration{ properties: properties });
                Ok(vec) 
            }
            Err(e) => {
                eprintln!("Parsing Error: an element in 'variables' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' VariableDeclaration '}' 
    { 
        let properties = $2?;
        match check_variable_declaration(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTVariableDeclaration{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'variables' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;
    
VariableDeclaration -> Result<Vec<ASTNode>, ParsingError>:
    VariableDeclarationEntry ',' VariableDeclaration
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | VariableDeclarationEntry { Ok(vec![$1?]) }
    ;

VariableDeclarationEntry -> Result<ASTNode, ParsingError>:
    'name' ':' Identifier
    {
        Ok(ASTNode::ASTVariableDeclarationName{ name: Box::new($3?) }) 
    }
    | 'type' ':' Type 
    { 
        Ok(ASTNode::ASTVariableDeclarationType{ type_: Box::new($3?) }) 
    }
    | 'transient' ':' 'true' 
    { 
        Ok(ASTNode::ASTVariableDeclarationTransient{ transient: true }) 
    }
    | 'transient' ':' 'false' 
    { 
        Ok(ASTNode::ASTVariableDeclarationTransient{ transient: false }) 
    }
    | 'initial-value' ':' Expression 
    { 
        Ok(ASTNode::ASTVariableDeclarationInitialValue{ initial_value: Box::new($3?) }) 
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;


RestrictInitial -> Result<Vec<ASTNode>, ParsingError>:
    RestrictInitialEntry ',' RestrictInitial
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | RestrictInitialEntry { Ok(vec![$1?]) }
    ;

RestrictInitialEntry -> Result<ASTNode, ParsingError>:
    'exp' ':' Expression 
    { 
        Ok(ASTNode::ASTRestrictInitialExp{ exp: Box::new($3?) }) 
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;


Properties -> Result<Vec<ASTNode>, ParsingError>:
    '{' Property '}' ',' Properties
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_property(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTProperty{ properties: properties });
                Ok(vec) 
            }
            Err(e) => {
                eprintln!("Parsing Error: an element in 'properties' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' Property '}' 
    { 
        let properties = $2?;
        match check_property(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTProperty{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'properties' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

Property -> Result<Vec<ASTNode>, ParsingError>:
    PropertyEntry ',' Property
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | PropertyEntry 
    { 
        Ok(vec![$1?]) 
    }
    ;

PropertyEntry -> Result<ASTNode, ParsingError>:
    'name' ':' Identifier 
    { 
        Ok(ASTNode::ASTPropertyName{ name: Box::new($3?) }) 
    }
    | 'expression' ':' PropertyExpression
    {
        Ok(ASTNode::ASTPropertyExpression{ expression: Box::new($3?) })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;


Automata -> Result<Vec<ASTNode>, ParsingError>:
    '{' Automaton '}' ',' Automata
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_automaton(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTAutomaton{ properties: properties });
                Ok(vec) 
            }
            Err(e) => {
                eprintln!("Parsing Error: an element in 'automata' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' Automaton '}' 
    { 
        let properties = $2?;
        match check_automaton(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTAutomaton{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'automata' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

Automaton -> Result<Vec<ASTNode>, ParsingError>:
    AutomatonEntry ',' Automaton
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | AutomatonEntry 
    { 
        Ok(vec![$1?]) 
    }
    ;

AutomatonEntry -> Result<ASTNode, ParsingError>:
    'name' ':' Identifier 
    { 
        Ok(ASTNode::ASTAutomatonName{ name: Box::new($3?) }) 
    }
    | 'variables' ':' '[' VariableDeclarations ']'
    {
        Ok(ASTNode::ASTAutomatonVariables{ variables: $4? }) 
    }
    | 'variables' ':' '[' ']'
    {
        Ok(ASTNode::ASTAutomatonVariables{ variables: vec![ASTNode::ASTEmpty] }) 
    }
    | 'restrict-initial' ':' '{' RestrictInitial '}' 
    { 
        let properties = $4?;
        match check_restrict_initial(&properties) {
            Ok(()) => Ok(ASTNode::ASTAutomatonRestrictInitial{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'restrict-initial' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    }
    | 'locations' ':' '[' AutomatonLocations ']'
    { 
        Ok(ASTNode::ASTAutomatonLocations{ locations: $4? }) 
    }
    | 'initial-locations' ':' '[' AutomatonInitialLocations ']'
    {
        Ok(ASTNode::ASTAutomatonInitialLocations{ initial_locations: $4? })
    }
    | 'edges' ':' '[' AutomatonEdges ']'
    {
        Ok(ASTNode::ASTAutomatonEdges{ edges: $4? })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

AutomatonLocations -> Result<Vec<ASTNode>, ParsingError>:
    '{' AutomatonLocation '}' ',' AutomatonLocations
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_automaton_location(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTAutomatonLocation{ properties: properties });
                Ok(vec) 
            },
            Err(e) => {
                eprintln!("Parsing Error: an element in 'locations' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' AutomatonLocation '}'
    {
        let properties = $2?;
        match check_automaton_location(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTAutomatonLocation{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'locations' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

AutomatonLocation -> Result<Vec<ASTNode>, ParsingError>:
    AutomatonLocationEntry ',' AutomatonLocation
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | AutomatonLocationEntry { Ok(vec![$1?]) }
    ;

AutomatonLocationEntry -> Result<ASTNode, ParsingError>:
    'name' ':' Identifier 
    { 
        Ok(ASTNode::ASTAutomatonLocationName{ name: Box::new($3?) }) 
    }
    | 'time-progress' ':' '{' AutomatonLocationTimeProgress '}'
    {   
        let properties = $4?;
        match check_time_progress(&properties) {
            Ok(()) => Ok(ASTNode::ASTAutomatonLocationTimeProgress{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'time-progress' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | 'transient-values' ':' '[' AutomatonLocationTransientValues ']'
    {   
        Ok(ASTNode::ASTAutomatonLocationTransientValues{ transient_values: $4? })
    }
    | 'transient-values' ':' '[' ']'
    {   
        Ok(ASTNode::ASTAutomatonLocationTransientValues{ transient_values: vec![ASTNode::ASTEmpty] })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

AutomatonLocationTimeProgress -> Result<Vec<ASTNode>, ParsingError>:
    AutomatonLocationTimeProgressEntry ',' AutomatonLocationTimeProgress
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | AutomatonLocationTimeProgressEntry { Ok(vec![$1?]) }
    ;

AutomatonLocationTimeProgressEntry -> Result<ASTNode, ParsingError>:
    'exp' ':' Expression 
    {
        Ok(ASTNode::ASTAutomatonLocationTimeProgressExp{ exp: Box::new($3?) })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

AutomatonLocationTransientValues -> Result<Vec<ASTNode>, ParsingError>:
    '{' AutomatonLocationTransientValue '}' ',' AutomatonLocationTransientValues
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_transient_value(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTAutomatonLocationTransientValue{ properties: properties });
                Ok(vec) 
            },
            Err(e) => {
                eprintln!("Parsing Error: an element in 'transient-values' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' AutomatonLocationTransientValue '}'
    {
        let properties = $2?;
        match check_transient_value(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTAutomatonLocationTransientValue{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'transient-values' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

AutomatonLocationTransientValue -> Result<Vec<ASTNode>, ParsingError>:
    AutomatonLocationTransientValueEntry ',' AutomatonLocationTransientValue
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | AutomatonLocationTransientValueEntry { Ok(vec![$1?]) }
    ;

AutomatonLocationTransientValueEntry -> Result<ASTNode, ParsingError>:
    'ref' ':' LValue
    {
        Ok(ASTNode::ASTTransientValueRef{ ref_: Box::new($3?) })
    }
    | 'value' ':' Expression
    {
        Ok(ASTNode::ASTTransientValueValue{ value: Box::new($3?) })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

AutomatonInitialLocations -> Result<Vec<ASTNode>, ParsingError>:
    Identifier ',' AutomatonInitialLocations
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | Identifier { Ok(vec![$1?]) }
    ;

AutomatonEdges -> Result<Vec<ASTNode>, ParsingError>:
    '{' AutomatonEdge '}' ',' AutomatonEdges
        {
            let mut vec = $5?;
            let properties = $2?;
            match check_automaton_edge(&properties) {
                Ok(()) => {
                    vec.insert(0, ASTNode::ASTAutomatonEdge{ properties: properties });
                    Ok(vec) 
                },
                Err(e) => {
                eprintln!("Parsing Error: an element in 'edges' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
            }
        }
        | '{' AutomatonEdge '}'
        {
            let properties = $2?;
            match check_automaton_edge(&properties) {
                Ok(()) => Ok(vec![ASTNode::ASTAutomatonEdge{ properties: properties }]),
                Err(e) => {
                eprintln!("Parsing Error: an element in 'edges' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
            }
        }
        ;

AutomatonEdge -> Result<Vec<ASTNode>, ParsingError>:
    AutomatonEdgeEntry ',' AutomatonEdge
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | AutomatonEdgeEntry { Ok(vec![$1?]) }
    ;

AutomatonEdgeEntry -> Result<ASTNode, ParsingError>:
    'location' ':' Identifier 
    { 
        Ok(ASTNode::ASTAutomatonEdgeLocation{ location: Box::new($3?) }) 
    }
    | 'action' ':' Identifier
    { 
        Ok(ASTNode::ASTAutomatonEdgeAction{ action: Box::new($3?) }) 
    }
    | 'rate' ':' '{' AutomatonEdgeRate '}'
    {
        let properties = $4?;
        match check_edge_rate(&properties) {
            Ok(()) => Ok(ASTNode::ASTAutomatonEdgeRate{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'rate' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | 'guard' ':' '{' AutomatonEdgeGuard '}'
    {
        let properties = $4?;
        match check_edge_guard(&properties) {
            Ok(()) => Ok(ASTNode::ASTAutomatonEdgeGuard{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'guard' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | 'destinations' ':' '[' AutomatonEdgeDestinations ']'
    {
        Ok(ASTNode::ASTAutomatonEdgeDestinations{ destinations: $4? })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

AutomatonEdgeRate -> Result<Vec<ASTNode>, ParsingError>:
    AutomatonEdgeRateEntry ',' AutomatonEdgeRate
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | AutomatonEdgeRateEntry { Ok(vec![$1?]) }
    ;

AutomatonEdgeRateEntry -> Result<ASTNode, ParsingError>:
    'exp' ':' Expression 
    {
        Ok(ASTNode::ASTAutomatonEdgeRateExp{ exp: Box::new($3?) })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

AutomatonEdgeGuard -> Result<Vec<ASTNode>, ParsingError>:
    AutomatonEdgeGuardEntry ',' AutomatonEdgeGuard
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | AutomatonEdgeGuardEntry { Ok(vec![$1?]) }
    ;

AutomatonEdgeGuardEntry -> Result<ASTNode, ParsingError>:
    'exp' ':' Expression 
    {
        Ok(ASTNode::ASTAutomatonEdgeGuardExp{ exp: Box::new($3?) })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

AutomatonEdgeDestinations -> Result<Vec<ASTNode>, ParsingError>:
    '{' AutomatonEdgeDestination '}' ',' AutomatonEdgeDestinations
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_edge_destination(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTAutomatonEdgeDestination{ properties: properties });
                Ok(vec) 
            },
            Err(e) => {
                eprintln!("Parsing Error: an element in 'destinations' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' AutomatonEdgeDestination '}'
    {
        let properties = $2?;
        match check_edge_destination(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTAutomatonEdgeDestination{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'destinations' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

AutomatonEdgeDestination -> Result<Vec<ASTNode>, ParsingError>:
    AutomatonEdgeDestinationEntry ',' AutomatonEdgeDestination
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | AutomatonEdgeDestinationEntry { Ok(vec![$1?]) }
    ;

AutomatonEdgeDestinationEntry -> Result<ASTNode, ParsingError>:
    'location' ':' Identifier 
    { 
        Ok(ASTNode::ASTAutomatonEdgeDestinationLocation{ location: Box::new($3?) }) 
    }
    | 'probability' ':' '{' AutomatonEdgeDestinationProbability '}'
    {
        let properties = $4?;
        match check_edge_destination_probability(&properties) {
            Ok(()) => Ok(ASTNode::ASTAutomatonEdgeDestinationProbability{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'probability' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | 'assignments' ':' '[' AutomatonEdgeDestinationAssignments ']'
    {
        Ok(ASTNode::ASTAutomatonEdgeDestinationAssignments{ assignments: $4? })
    }
    | 'assignments' ':' '[' ']'
    {
        Ok(ASTNode::ASTAutomatonEdgeDestinationAssignments{ assignments: vec![ASTNode::ASTEmpty] })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

AutomatonEdgeDestinationProbability -> Result<Vec<ASTNode>, ParsingError>:
    AutomatonEdgeDestinationProbabilityEntry ',' AutomatonEdgeDestinationProbability
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | AutomatonEdgeDestinationProbabilityEntry { Ok(vec![$1?]) }
    ;

AutomatonEdgeDestinationProbabilityEntry -> Result<ASTNode, ParsingError>:
    'exp' ':' Expression 
    {
        Ok(ASTNode::ASTAutomatonEdgeDestinationProbabilityExp{ exp: Box::new($3?) })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;
 
AutomatonEdgeDestinationAssignments -> Result<Vec<ASTNode>, ParsingError>:
    '{' AutomatonEdgeDestinationAssignment '}' ',' AutomatonEdgeDestinationAssignments
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_edge_destination_assignment(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTAutomatonEdgeDestinationAssignment{ properties: properties });
                Ok(vec) 
            },
            Err(e) => {
                eprintln!("Parsing Error: an element in 'assignments' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' AutomatonEdgeDestinationAssignment '}'
    {
        let properties = $2?;
        match check_edge_destination_assignment(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTAutomatonEdgeDestinationAssignment{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'assignments' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

AutomatonEdgeDestinationAssignment -> Result<Vec<ASTNode>, ParsingError>:
    AutomatonEdgeDestinationAssignmentEntry ',' AutomatonEdgeDestinationAssignment
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | AutomatonEdgeDestinationAssignmentEntry { Ok(vec![$1?]) }
    ;

AutomatonEdgeDestinationAssignmentEntry -> Result<ASTNode, ParsingError>:
    'ref' ':' LValue 
    { 
        Ok(ASTNode::ASTAutomatonEdgeDestinationAssignmentRef{ ref_: Box::new($3?) }) 
    }
    | 'value' ':' Expression
    {
        Ok(ASTNode::ASTAutomatonEdgeDestinationAssignmentValue{ value: Box::new($3?) }) 
    } 
    | 'index' ':' 'NUMBER'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span());
        let float_number = f64::from_str(s).map_err(|_| ParsingError::InvalidSyntaxError)?;
        let number = float_number as u64;
        Ok(ASTNode::ASTAutomatonEdgeDestinationAssignmentIndex{ index: number })
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;


Composition -> Result<Vec<ASTNode>, ParsingError>:
    CompositionEntry ',' Composition
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | CompositionEntry { Ok(vec![$1?]) }
    ;

CompositionEntry -> Result<ASTNode, ParsingError>:
    'elements' ':' '[' CompositionElements ']' 
    { 
        Ok(ASTNode::ASTCompositionElements{ elements: $4? }) 
    }
    | 'syncs' ':' '[' CompositionSyncs ']' 
    { 
        Ok(ASTNode::ASTCompositionSyncs{ syncs: $4? }) 
    }
    | 'syncs' ':' '[' ']' 
    { 
        Ok(ASTNode::ASTCompositionSyncs{ syncs: vec![ASTNode::ASTEmpty] }) 
    }
    | 'comment' ':' 'STRING' 
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

CompositionElements -> Result<Vec<ASTNode>, ParsingError>:
    '{' CompositionElement '}' ',' CompositionElements
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_composition_element(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTCompositionElement{ properties: properties });
                Ok(vec) 
            },
            Err(e) => {
                eprintln!("Parsing Error: an element in 'elements' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' CompositionElement '}'
    {
        let properties = $2?;
        match check_composition_element(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTCompositionElement{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'elements' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
        
    }
    ;

CompositionElement -> Result<Vec<ASTNode>, ParsingError>:
    CompositionElementEntry ',' CompositionElement
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | CompositionElementEntry { Ok(vec![$1?]) }
    ;

CompositionElementEntry -> Result<ASTNode, ParsingError>:
    'automaton' ':' Identifier 
    { 
        Ok(ASTNode::ASTCompositionElementAutomaton{ automaton: Box::new($3?) }) 
    }
    | 'input-enable' ':' '[' CompositionElementInputEnable ']' 
    { 
        Ok(ASTNode::ASTCompositionElementInputEnable{ input_enable: $4? }) 
    }
    | 'input-enable' ':' '[' ']' 
    { 
        Ok(ASTNode::ASTCompositionElementInputEnable{ input_enable: vec![ASTNode::ASTEmpty] }) 
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

CompositionElementInputEnable -> Result<Vec<ASTNode>, ParsingError>:
    Identifier ',' CompositionElementInputEnable
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | Identifier { Ok(vec![$1?]) }
    ;

CompositionSyncs -> Result<Vec<ASTNode>, ParsingError>:
    '{' CompositionSync '}' ',' CompositionSyncs
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_composition_sync(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTCompositionSync{ properties: properties });
                Ok(vec) 
            },
            Err(e) => {
                eprintln!("Parsing Error: an element in 'syncs' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' CompositionSync '}'
    {
        let properties = $2?;
        match check_composition_sync(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTCompositionSync{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'syncs' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
        
    }
    ;

CompositionSync -> Result<Vec<ASTNode>, ParsingError>:
    CompositionSyncEntry ',' CompositionSync
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | CompositionSyncEntry { Ok(vec![$1?]) }
    ;

CompositionSyncEntry -> Result<ASTNode, ParsingError>:
    'synchronise' ':' '[' CompositionSyncSynchronise ']' 
    { 
        Ok(ASTNode::ASTCompositionSyncSynchronise{ synchronise: $4? }) 
    }
    | 'result' ':' Identifier 
    { 
        Ok(ASTNode::ASTCompositionSyncResult{ result: Box::new($3?) }) 
    }
    | 'comment' ':' 'STRING'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTComment{ comment: s }) 
    }
    ;

CompositionSyncSynchronise -> Result<Vec<ASTNode>, ParsingError>:
    Identifier ',' CompositionSyncSynchronise
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | 'null' ',' CompositionSyncSynchronise
    {
        let mut vec = $3?;
        vec.insert(0, ASTNode::ASTIdentifier{ identifier: String::new() });
        Ok(vec)
    }
    | Identifier { Ok(vec![$1?]) }
    | 'null' { Ok(vec![ASTNode::ASTIdentifier{ identifier: String::new() }]) }
    ;



Identifier -> Result<ASTNode, ParsingError>:
    'STRING'
    {  
        let v = $1.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTIdentifier{ identifier: s })
    }
    ;


Type -> Result<ASTNode, ParsingError>:
    BasicType 
    { 
        Ok($1?)
    }
    | '{' BoundedType '}'
    { 
        let properties = $2?;
        match check_bounded_type(&properties) {
            Ok(()) => Ok(ASTNode::ASTBoundedType{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'type' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | 'clock' 
    { 
        Ok(ASTNode::ASTOtherType{ type_: String::from("clock") }) 
    }
    | 'continuous' 
    { 
        Ok(ASTNode::ASTOtherType{ type_: String::from("continuous") }) 
    }
    ;

BasicType -> Result<ASTNode, ParsingError>:
    'bool' { Ok(ASTNode::ASTBasicType{ type_: String::from("bool") }) }
    | 'int' { Ok(ASTNode::ASTBasicType{ type_: String::from("int") }) }
    | 'real' { Ok(ASTNode::ASTBasicType{ type_: String::from("real") }) }
    ;

BoundedType -> Result<Vec<ASTNode>, ParsingError>:
    BoundedTypeEntry ',' BoundedType
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | BoundedTypeEntry { Ok(vec![$1?]) }
    ;

BoundedTypeEntry -> Result<ASTNode, ParsingError>:
    'kind' ':' 'bounded' { Ok(ASTNode::ASTBoundedTypeKind{ kind: String::from("bounded") }) }
    | 'base' ':' 'int' { Ok(ASTNode::ASTBoundedTypeBase{ base: String::from("int") }) }
    | 'base' ':' 'real' { Ok(ASTNode::ASTBoundedTypeBase{ base: String::from("real") }) }
    | 'lower-bound' ':' Expression { Ok(ASTNode::ASTBoundedTypeLowerBound{ lower_bound: Box::new($3?) }) }
    | 'upper-bound' ':' Expression { Ok(ASTNode::ASTBoundedTypeUpperBound{ upper_bound: Box::new($3?) }) }
    ;


Expression -> Result<ASTNode, ParsingError>:
    ConstantValue { Ok($1?) }
    | Identifier { Ok($1?) }
    | '{' ExpressionIfThenElse '}' 
    {  
        let properties = $2?;
        match check_ite_op(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionIfThenElse{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    }
    | '{' ExpressionBinaryOperation '}' 
    {  
        let properties = $2?;
        match check_binary_op(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionBinaryOperation{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    } 
    | '{' ExpressionUnaryOperation '}' 
    {  
        let properties = $2?;
        match check_unary_op(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionUnaryOperation{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    } 
    | '{' ExpressionDerivativeOperation '}' 
    {  
        let properties = $2?;
        match check_derivative_op(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionDerivativeOperation{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    } 
    | '{' DistributionSampling '}'
    { 
        let properties = $2?;
        match check_distribution_sampling(&properties) {
            Ok(()) => Ok(ASTNode::ASTDistributionSampling{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    } 
    ;

ConstantValue -> Result<ASTNode, ParsingError>:
    'NUMBER' 
    { 
        let v = $1.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span());
        if let Ok(number) = s.parse::<i32>() {
            Ok(ASTNode::ASTConstantValueInteger { value: number })
        } else {
            let number = s.parse::<f64>().map_err(|_| ParsingError::InvalidSyntaxError)?;
            Ok(ASTNode::ASTConstantValueReal { value: number })
        }
    }
    | 'true' { Ok(ASTNode::ASTConstantValueBoolean{ value: true }) }
    | 'false' { Ok(ASTNode::ASTConstantValueBoolean{ value: false }) }
    | '{' 'constant' ':' 'e' '}' { Ok(ASTNode::ASTConstantValueReal{ value: E }) }
    | '{' 'constant' ':' 'π' '}' { Ok(ASTNode::ASTConstantValueReal{ value: PI }) }
    ;

ExpressionIfThenElse -> Result<Vec<ASTNode>, ParsingError>:
    ExpressionIfThenElseEntry ',' ExpressionIfThenElse
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ExpressionIfThenElseEntry { Ok(vec![$1?]) }
    ;

ExpressionIfThenElseEntry -> Result<ASTNode, ParsingError>:
    'op' ':' 'ite' { Ok(ASTNode::ASTExpressionOperation{ op: String::from("ite") }) }
    | 'if' ':' Expression { Ok(ASTNode::ASTExpressionIf{ if_: Box::new($3?) }) }
    | 'then' ':' Expression { Ok(ASTNode::ASTExpressionThen{ then: Box::new($3?) }) }
    | 'else' ':' Expression { Ok(ASTNode::ASTExpressionElse{ else_: Box::new($3?) }) }
    ; 

ExpressionBinaryOperation -> Result<Vec<ASTNode>, ParsingError>:
    ExpressionBinaryOperationEntry ',' ExpressionBinaryOperation
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ExpressionBinaryOperationEntry { Ok(vec![$1?]) }
    ;

ExpressionBinaryOperationEntry -> Result<ASTNode, ParsingError>:
    'op' ':' 'BINARYOPERATOR' 
    {   
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionOperation{ op: s }) 
    }
    | 'left' ':' Expression { Ok(ASTNode::ASTExpressionLeft{ left: Box::new($3?) }) }
    | 'right' ':' Expression { Ok(ASTNode::ASTExpressionRight{ right: Box::new($3?) }) }
    ; 

ExpressionUnaryOperation -> Result<Vec<ASTNode>, ParsingError>:
    ExpressionUnaryOperationEntry ',' ExpressionUnaryOperation
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ExpressionUnaryOperationEntry { Ok(vec![$1?]) }
    ;

ExpressionUnaryOperationEntry -> Result<ASTNode, ParsingError>:
    'op' ':' 'UNARYOPERATOR' 
    {   
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionOperation{ op: s }) 
    }
    | 'exp' ':' Expression { Ok(ASTNode::ASTExpressionOperand{ exp: Box::new($3?) }) }
    ; 

ExpressionDerivativeOperation -> Result<Vec<ASTNode>, ParsingError>:
    ExpressionDerivativeOperationEntry ',' ExpressionDerivativeOperation
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ExpressionDerivativeOperationEntry { Ok(vec![$1?]) }
    ;

ExpressionDerivativeOperationEntry -> Result<ASTNode, ParsingError>:
    'op' ':' 'der' { Ok(ASTNode::ASTExpressionOperation{ op: String::from("der") }) }
    | 'var' ':' Identifier { Ok(ASTNode::ASTExpressionVariable{ var: Box::new($3?) }) }
    ; 

DistributionSampling -> Result<Vec<ASTNode>, ParsingError>:
    DistributionSamplingEntry ',' DistributionSampling
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | DistributionSamplingEntry 
    { 
        Ok(vec![$1?]) 
    }
    ;

DistributionSamplingEntry -> Result<ASTNode, ParsingError>:
    'distribution' ':' 'DISTRIBUTIONTYPE'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTDistributionSamplingDistribution{ distribution: s })
    }
    | 'args' ':' '[' DistributionSamplingArgs ']'
    {
        Ok(ASTNode::ASTDistributionSamplingArgs{ args: $4? })
    }
    ;

DistributionSamplingArgs -> Result<Vec<ASTNode>, ParsingError>:
    Expression ',' DistributionSamplingArgs
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec) 
    }
    | Expression
    {
        Ok(vec![$1?])
    }
    ;


PropertyExpression -> Result<ASTNode, ParsingError>:
    ConstantValue { Ok($1?) }
    | Identifier { Ok($1?) }
    | '{' ExpressionIfThenElse '}' 
    {  
        let properties = $2?;
        match check_ite_op(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionIfThenElse{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    }
    | '{' PropertyExpressionBinaryOperation '}' 
    {  
        let properties = $2?;
        match check_property_binary_op(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionBinaryOperation{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    } 
    | '{' PropertyExpressionUnaryOperation '}' 
    {  
        let properties = $2?;
        match check_property_unary_op(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionUnaryOperation{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    } 
    | '{' ExpressionDerivativeOperation '}' 
    {  
        let properties = $2?;
        match check_derivative_op(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionDerivativeOperation{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    } 
    | '{' DistributionSampling '}'
    { 
        let properties = $2?;
        match check_distribution_sampling(&properties) {
            Ok(()) => Ok(ASTNode::ASTDistributionSampling{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    } 
    | '{' ExpressionFilter '}' 
    {  
        let properties = $2?;
        match check_filter_op(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionFilter{ properties: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'expression' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    }
    | '{' ExpressionStatePredicate '}' 
    {  
        Ok($2?)
    }
    ;

PropertyExpressionBinaryOperation -> Result<Vec<ASTNode>, ParsingError>:
    PropertyExpressionBinaryOperationEntry ',' PropertyExpressionBinaryOperation
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | PropertyExpressionBinaryOperationEntry { Ok(vec![$1?]) }
    ;

PropertyExpressionBinaryOperationEntry -> Result<ASTNode, ParsingError>:
    'op' ':' 'BINARYOPERATOR' 
    {   
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionOperation{ op: s }) 
    }
    | 'op' ':' 'UNTIL' 
    { 
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionOperation{ op: s }) 
    }
    | 'left' ':' PropertyExpression
    { 
        Ok(ASTNode::ASTExpressionLeft{ left: Box::new($3?) })
    }
    | 'right' ':' PropertyExpression
    { 
        Ok(ASTNode::ASTExpressionRight{ right: Box::new($3?) })
    }
    | 'step-bounds' ':' '{' PropertyInterval '}'
    { 
       let properties = $4?;
        match check_property_interval(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionStepBounds{ step_bounds: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'step-bounds' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    }
    | 'time-bounds' ':' '{' PropertyInterval '}'
    { 
       let properties = $4?;
        match check_property_interval(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionTimeBounds{ time_bounds: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'time-bounds' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    }
    | 'reward-bounds' ':' '[' ExpressionRewardBounds ']'
    {
        Ok(ASTNode::ASTExpressionRewardBounds{ reward_bounds: $4? })
    }
    | 'reward-bounds' ':' '[' ']'
    {
        Ok(ASTNode::ASTExpressionRewardBounds{ reward_bounds: vec![ASTNode::ASTEmpty] })
    }
    ; 

ExpressionRewardBounds -> Result<Vec<ASTNode>, ParsingError>:
    '{' ExpressionRewardBound '}' ',' ExpressionRewardBounds
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_reward_bounds(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTExpressionRewardBound{ properties: properties });
                Ok(vec) 
            },
            Err(e) => {
                eprintln!("Parsing Error: an element in 'reward-bounds' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' ExpressionRewardBound '}'
    {
        let properties = $2?;
        match check_reward_bounds(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTExpressionRewardBound{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'reward-bounds' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

ExpressionRewardBound -> Result<Vec<ASTNode>, ParsingError>:
    ExpressionRewardBoundEntry ',' ExpressionRewardBound
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ExpressionRewardBoundEntry { Ok(vec![$1?]) }
    ;

ExpressionRewardBoundEntry -> Result<ASTNode, ParsingError>:
    'exp' ':' Expression 
    { 
        Ok(ASTNode::ASTExpressionRewardBoundExp{ exp: Box::new($3?) }) 
    }
    | 'accumulate' ':' RewardAccumulation
    {
        Ok(ASTNode::ASTExpressionRewardBoundAccumulate{ accumulate: Box::new($3?) }) 
    } 
    | 'bounds' ':' '{' PropertyInterval '}'
    { 
       let properties = $4?;
        match check_property_interval(&properties) {
            Ok(()) => Ok(ASTNode::ASTExpressionRewardBoundBounds{ bounds: properties }),
            Err(e) => {
                eprintln!("Parsing Error: 'bounds' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        } 
    }
    ;

PropertyExpressionUnaryOperation -> Result<Vec<ASTNode>, ParsingError>:
    PropertyExpressionUnaryOperationEntry ',' PropertyExpressionUnaryOperation
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | PropertyExpressionUnaryOperationEntry { Ok(vec![$1?]) }
    ;

PropertyExpressionUnaryOperationEntry -> Result<ASTNode, ParsingError>:
    'op' ':' 'UNARYOPERATOR' 
    {   
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionOperation{ op: s }) 
    }
    | 'op' ':' 'PMINMAX' 
    { 
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionOperation{ op: s }) 
    }
    | 'op' ':' 'QUANTIFICATION' 
    { 
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionOperation{ op: s }) 
    }
    | 'op' ':' 'EMINMAX' 
    { 
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionOperation{ op: s }) 
    }
    | 'op' ':' 'SMINMAX' 
    { 
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionOperation{ op: s }) 
    }
    | 'exp' ':' PropertyExpression
    { 
        Ok(ASTNode::ASTExpressionOperand{ exp: Box::new($3?) })
    }
    | 'accumulate' ':' RewardAccumulation
    { 
        Ok(ASTNode::ASTExpressionAccumulate{ accumulate: Box::new($3?) })
    }
    | 'reach' ':' PropertyExpression
    {
        Ok(ASTNode::ASTExpressionReach{ reach: Box::new($3?) })
    }
    | 'step-instant' ':' Expression
    {
        Ok(ASTNode::ASTExpressionStepInstant{ step_instant: Box::new($3?) })
    }
    | 'time-instant' ':' Expression
    {
        Ok(ASTNode::ASTExpressionTimeInstant{ time_instant: Box::new($3?) })
    }
    | 'reward-instants' ':' '[' ExpressionRewardInstants ']'
    {
        Ok(ASTNode::ASTExpressionRewardInstants{ reward_instants: $4? })
    }
    | 'reward-instants' ':' '[' ']'
    {
        Ok(ASTNode::ASTExpressionRewardInstants{ reward_instants: vec![ASTNode::ASTEmpty] })
    }
    ; 

ExpressionRewardInstants -> Result<Vec<ASTNode>, ParsingError>:
    '{' ExpressionRewardInstant '}' ',' ExpressionRewardInstants
    {
        let mut vec = $5?;
        let properties = $2?;
        match check_reward_instants(&properties) {
            Ok(()) => {
                vec.insert(0, ASTNode::ASTExpressionRewardInstant{ properties: properties });
                Ok(vec) 
            },
            Err(e) => {
                eprintln!("Parsing Error: an element in 'reward-instants' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    | '{' ExpressionRewardInstant '}'
    {
        let properties = $2?;
        match check_reward_instants(&properties) {
            Ok(()) => Ok(vec![ASTNode::ASTExpressionRewardInstant{ properties: properties }]),
            Err(e) => {
                eprintln!("Parsing Error: an element in 'reward-instants' section is missing required fields or has duplicate fields.");
                let ((start_line, start_column), (end_line, end_column)) = $lexer.line_col($span);
                eprintln!("Span from (Line {:?} Column {:?}) to (Line {:?} Column {:?})", start_line, start_column, end_line, end_column);
                Err(e)
            },
        }
    }
    ;

ExpressionRewardInstant -> Result<Vec<ASTNode>, ParsingError>:
    ExpressionRewardInstantEntry ',' ExpressionRewardInstant
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ExpressionRewardInstantEntry { Ok(vec![$1?]) }
    ;

ExpressionRewardInstantEntry -> Result<ASTNode, ParsingError>:
    'exp' ':' Expression 
    { 
        Ok(ASTNode::ASTExpressionRewardInstantExp{ exp: Box::new($3?) }) 
    }
    | 'accumulate' ':' RewardAccumulation
    {
        Ok(ASTNode::ASTExpressionRewardInstantAccumulate{ accumulate: Box::new($3?) }) 
    } 
    | 'instant' ':' Expression
    {
        Ok(ASTNode::ASTExpressionRewardInstantInstant{ instant: Box::new($3?) }) 
    }
    ;

ExpressionFilter -> Result<Vec<ASTNode>, ParsingError>:
    ExpressionFilterEntry ',' ExpressionFilter
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | ExpressionFilterEntry { Ok(vec![$1?]) }
    ;

ExpressionFilterEntry -> Result<ASTNode, ParsingError>:
    'op' ':' 'filter' 
    { 
        Ok(ASTNode::ASTExpressionOperation{ op: String::from("filter") }) 
    }
    | 'fun' ':' 'FUNTYPE'
    { 
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionFilterFun{ fun: s }) 
    }
    | 'fun' ':' 'QUANTIFICATION'
    { 
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionFilterFun{ fun: s }) 
    }
    | 'fun' ':' 'values'
    { 
        Ok(ASTNode::ASTExpressionFilterFun{ fun: String::from("values") }) 
    }
    | 'values' ':' PropertyExpression
    {
        Ok(ASTNode::ASTExpressionFilterValues{ values: Box::new($3?) })
    }
    | 'states' ':' PropertyExpression
    {
        Ok(ASTNode::ASTExpressionFilterStates{ states: Box::new($3?) })
    }
    ; 

ExpressionStatePredicate -> Result<ASTNode, ParsingError>:
    'op' ':' 'STATEPREDICATE'
    {
        let v = $3.map_err(|_| ParsingError::InvalidSyntaxError)?;
        let s = $lexer.span_str(v.span()).to_string();
        Ok(ASTNode::ASTExpressionOperation{ op: s })
    }
    ;
 
PropertyInterval -> Result<Vec<ASTNode>, ParsingError>:
    PropertyIntervalEntry ',' PropertyInterval
    {
        let mut vec = $3?;
        vec.insert(0, $1?);
        Ok(vec)
    }
    | PropertyIntervalEntry 
    { 
        Ok(vec![$1?]) 
    }
    ;

PropertyIntervalEntry -> Result<ASTNode, ParsingError>:
    'lower' ':' Expression
    {
        Ok(ASTNode::ASTPropertyIntervalLower{ lower: Box::new($3?) })
    }
    | 'lower-exclusive' ':' 'true'
    {
        Ok(ASTNode::ASTPropertyIntervalLowerExclusive{ lower_exclusive: true })
    }
    | 'lower-exclusive' ':' 'false'
    {
        Ok(ASTNode::ASTPropertyIntervalLowerExclusive{ lower_exclusive: false })
    }
    | 'upper' ':' Expression
    {
        Ok(ASTNode::ASTPropertyIntervalUpper{ upper: Box::new($3?) })
    }
    | 'upper-exclusive' ':' 'true'
    {
        Ok(ASTNode::ASTPropertyIntervalUpperExclusive{ upper_exclusive: true })
    }
    | 'upper-exclusive' ':' 'false'
    {
        Ok(ASTNode::ASTPropertyIntervalUpperExclusive{ upper_exclusive: false })
    }
    ;

RewardAccumulation -> Result<ASTNode, ParsingError>:
    '[' 'steps' ',' 'time' ']'
    {
        Ok(ASTNode::ASTRewardAccumulation{ accumulate: vec![String::from("steps"), String::from("time")] })
    }
    | '[' 'time' ',' 'steps' ']'
    {
        Ok(ASTNode::ASTRewardAccumulation{ accumulate: vec![String::from("steps"), String::from("time")] })
    }
    | '[' 'steps' ']'
    {
        Ok(ASTNode::ASTRewardAccumulation{ accumulate: vec![String::from("steps")] })
    }
    | '[' 'time' ']'
    {
        Ok(ASTNode::ASTRewardAccumulation{ accumulate: vec![String::from("time")] })
    }
    ;


LValue -> Result<ASTNode, ParsingError>:
    Identifier { Ok($1?) }
    ;


Unmatched -> ():
      "UNMATCHED" { }
    ;


%%
use std::str::FromStr;
use std::f64::consts::{E, PI};
use crate::jani_parser::{ASTNode, ParsingError};

fn check_model(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut version_count = 0;
    let mut name_count = 0;
    let mut metadata_count = 0;
    let mut type_count = 0;
    let mut features_count = 0;
    let mut actions_count = 0;
    let mut constants_count = 0;
    let mut variablese_count = 0;
    let mut restrict_initial_count = 0;
    let mut properties_count = 0;
    let mut automata_count = 0;
    let mut system_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTModelVersion{..} => {
                if version_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                version_count += 1;
            }
            ASTNode::ASTModelName{..} => {
                if name_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                name_count += 1;
            }
            ASTNode::ASTModelMetadata{..} => {
                if metadata_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                metadata_count += 1;
            }
            ASTNode::ASTModelType{..} => {
                if type_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                type_count += 1;
            }
            ASTNode::ASTModelFeatures{..} => {
                if features_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                features_count += 1;
            }
            ASTNode::ASTModelActions{..} => {
                if actions_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                actions_count += 1;
            }
            ASTNode::ASTModelConstants{..} => {
                if constants_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                constants_count += 1;
            }
            ASTNode::ASTModelVariables{..} => {
                if variablese_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                variablese_count += 1;
            }
            ASTNode::ASTModelRestrictInitial{..} => {
                if restrict_initial_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                restrict_initial_count += 1;
            }
            ASTNode::ASTModelProperties{..} => {
                if properties_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                properties_count += 1;
            }
            ASTNode::ASTModelAutomata{..} => {
                if automata_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                automata_count += 1;
            }
            ASTNode::ASTModelSystem{..} => {
                if system_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                system_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if version_count != 1 || name_count != 1 || type_count != 1 || automata_count != 1 || system_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_metadata(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut version_count = 0;
    let mut author_count = 0;
    let mut description_count = 0;
    let mut doi_count = 0;
    let mut url_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTMetadataVersion{..} => {
                if version_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                version_count += 1;
            }
            ASTNode::ASTMetadataAuthor{..} => {
                if author_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                author_count += 1;
            }
            ASTNode::ASTMetadataDescription{..} => {
                if description_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                description_count += 1;
            }
            ASTNode::ASTMetadataDoi{..} => {
                if doi_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                doi_count += 1;
            }
            ASTNode::ASTMetadataUrl{..} => {
                if url_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                url_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    Ok(())
}

fn check_model_action(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut name_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTModelActionName{..} => {
                if name_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                name_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if name_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_constant_declaration(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut name_count = 0;
    let mut type_count = 0;
    let mut value_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTConstantDeclarationName{..} => {
                if name_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                name_count += 1;
            }
            ASTNode::ASTConstantDeclarationType{..} => {
                if type_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                type_count += 1;
            }
            ASTNode::ASTConstantDeclarationValue{..} => {
                if value_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                value_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if name_count != 1 || type_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_variable_declaration(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut name_count = 0;
    let mut type_count = 0;
    let mut transient_count = 0;
    let mut initial_value_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTVariableDeclarationName{..} => {
                if name_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                name_count += 1;
            }
            ASTNode::ASTVariableDeclarationType{..} => {
                if type_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                type_count += 1;
            }
            ASTNode::ASTVariableDeclarationTransient{..} => {
                if transient_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                transient_count += 1;
            }
            ASTNode::ASTVariableDeclarationInitialValue{..} => {
                if initial_value_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                initial_value_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if name_count != 1 || type_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_restrict_initial(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut exp_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTRestrictInitialExp{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if exp_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_property(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut name_count = 0;
    let mut expression_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTPropertyName{..} => {
                if name_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                name_count += 1;
            }
            ASTNode::ASTPropertyExpression{..} => {
                if expression_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                expression_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if name_count != 1 || expression_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_automaton(vec: &Vec<ASTNode>) -> Result<(), ParsingError> { 
    let mut name_count = 0;
    let mut variables_count = 0;
    let mut restrict_initial_count = 0;
    let mut locations_count = 0;
    let mut initial_locations_count = 0;
    let mut edges_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTAutomatonName{..} => {
                if name_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                name_count += 1;
            }
            ASTNode::ASTAutomatonVariables{..} => {
                if variables_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                variables_count += 1;
            }
            ASTNode::ASTAutomatonRestrictInitial{..} => {
                if restrict_initial_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                restrict_initial_count += 1;
            }
            ASTNode::ASTAutomatonLocations{..} => {
                if locations_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                locations_count += 1;
            }
            ASTNode::ASTAutomatonInitialLocations{..} => {
                if initial_locations_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                initial_locations_count += 1;
            }
            ASTNode::ASTAutomatonEdges{..} => {
                if edges_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                edges_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if name_count != 1 || locations_count != 1 || initial_locations_count != 1 || edges_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_automaton_location(vec: &Vec<ASTNode>) -> Result<(), ParsingError> { 
    let mut name_count = 0;
    let mut time_progress_count = 0;
    let mut transient_values_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTAutomatonLocationName{..} => {
                if name_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                name_count += 1;
            }
            ASTNode::ASTAutomatonLocationTimeProgress{..} => {
                if time_progress_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                time_progress_count += 1;
            }
            ASTNode::ASTAutomatonLocationTransientValues{..} => {
                if transient_values_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                transient_values_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if name_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_time_progress(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut exp_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTAutomatonLocationTimeProgressExp{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if exp_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_transient_value(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut ref_count = 0;
    let mut value_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTTransientValueRef{..} => {
                if ref_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                ref_count += 1;
            }
            ASTNode::ASTTransientValueValue{..} => {
                if value_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                value_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if ref_count != 1 || value_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_automaton_edge(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut location_count = 0;
    let mut action_count = 0;
    let mut rate_count = 0;
    let mut guard_count = 0;
    let mut destinations_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTAutomatonEdgeLocation{..} => {
                if location_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                location_count += 1;
            }
            ASTNode::ASTAutomatonEdgeAction{..} => {
                if action_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                action_count += 1;
            }
            ASTNode::ASTAutomatonEdgeRate{..} => {
                if rate_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                rate_count += 1;
            }
            ASTNode::ASTAutomatonEdgeGuard{..} => {
                if guard_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                guard_count += 1;
            }
            ASTNode::ASTAutomatonEdgeDestinations{..} => {
                if destinations_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                destinations_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if location_count != 1 || destinations_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_edge_rate(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut exp_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTAutomatonEdgeRateExp{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if exp_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_edge_guard(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut exp_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTAutomatonEdgeGuardExp{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if exp_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_edge_destination(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut location_count = 0;
    let mut probability_count = 0;
    let mut assignments_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTAutomatonEdgeDestinationLocation{..} => {
                if location_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                location_count += 1;
            }
            ASTNode::ASTAutomatonEdgeDestinationProbability{..} => {
                if probability_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                probability_count += 1;
            }
            ASTNode::ASTAutomatonEdgeDestinationAssignments{..} => {
                if assignments_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                assignments_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if location_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_edge_destination_probability(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut exp_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTAutomatonEdgeDestinationProbabilityExp{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if exp_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_edge_destination_assignment(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut ref_count = 0;
    let mut value_count = 0;
    let mut index_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTAutomatonEdgeDestinationAssignmentRef{..} => {
                if ref_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                ref_count += 1;
            }
            ASTNode::ASTAutomatonEdgeDestinationAssignmentValue{..} => {
                if value_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                value_count += 1;
            }
            ASTNode::ASTAutomatonEdgeDestinationAssignmentIndex{..} => {
                if index_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                index_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if ref_count != 1 || value_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_model_system(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut elements_count = 0;
    let mut syncs_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTCompositionElements{..} => {
                if elements_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                elements_count += 1;
            }
            ASTNode::ASTCompositionSyncs{..} => {
                if syncs_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                syncs_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if elements_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_composition_element(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut automaton_count = 0;
    let mut input_enable_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTCompositionElementAutomaton{..} => {
                if automaton_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                automaton_count += 1;
            }
            ASTNode::ASTCompositionElementInputEnable{..} => {
                if input_enable_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                input_enable_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if automaton_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_composition_sync(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut synchronise_count = 0;
    let mut result_count = 0;
    let mut comment_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTCompositionSyncSynchronise{..} => {
                if synchronise_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                synchronise_count += 1;
            }
            ASTNode::ASTCompositionSyncResult{..} => {
                if result_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                result_count += 1;
            }
            ASTNode::ASTComment{..} => {
                if comment_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                comment_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if synchronise_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_bounded_type(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut kind_count = 0;
    let mut base_count = 0;
    let mut lower_bound_count = 0;
    let mut upper_bound_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTBoundedTypeKind{..} => {
                if kind_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                kind_count += 1;
            }
            ASTNode::ASTBoundedTypeBase{..} => {
                if base_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                base_count += 1;
            }
            ASTNode::ASTBoundedTypeLowerBound{..} => {
                if lower_bound_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                lower_bound_count += 1;
            }
            ASTNode::ASTBoundedTypeUpperBound{..} => {
                if upper_bound_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                upper_bound_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if kind_count != 1 || base_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_ite_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_count = 0;
    let mut if_count = 0;
    let mut then_count = 0;
    let mut else_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionOperation{..} => {
                if op_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                op_count += 1;
            }
            ASTNode::ASTExpressionIf{..} => {
                if if_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                if_count += 1;
            }
            ASTNode::ASTExpressionThen{..} => {
                if then_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                then_count += 1;
            }
            ASTNode::ASTExpressionElse{..} => {
                if else_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                else_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if op_count != 1 || if_count != 1 || then_count != 1 || else_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_binary_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_count = 0;
    let mut left_count = 0;
    let mut right_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionOperation{..} => {
                if op_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                op_count += 1;
            }
            ASTNode::ASTExpressionLeft{..} => {
                if left_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                left_count += 1;
            }
            ASTNode::ASTExpressionRight{..} => {
                if right_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                right_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if op_count != 1 || left_count != 1 || right_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_unary_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_count = 0;
    let mut exp_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionOperation{..} => {
                if op_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                op_count += 1;
            }
            ASTNode::ASTExpressionOperand{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if op_count != 1 || exp_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_derivative_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_count = 0;
    let mut var_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionOperation{..} => {
                if op_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                op_count += 1;
            }
            ASTNode::ASTExpressionVariable{..} => {
                if var_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                var_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if op_count != 1 || var_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_distribution_sampling(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut distribution_count = 0;
    let mut args_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTDistributionSamplingDistribution{..} => {
                if distribution_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                distribution_count += 1;
            }
            ASTNode::ASTDistributionSamplingArgs{..} => {
                if args_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                args_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if distribution_count != 1 || args_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_filter_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_count = 0;
    let mut fun_count = 0;
    let mut values_count = 0;
    let mut states_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionOperation{..} => {
                if op_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                op_count += 1;
            }
            ASTNode::ASTExpressionFilterFun{..} => {
                if fun_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                fun_count += 1;
            }
            ASTNode::ASTExpressionFilterValues{..} => {
                if values_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                values_count += 1;
            }
            ASTNode::ASTExpressionFilterStates{..} => {
                if states_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                states_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if op_count != 1 || fun_count != 1 || values_count != 1 || states_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_property_binary_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_value = 0;
    let mut found_op = false;

    for node in vec {
        if let ASTNode::ASTExpressionOperation{ op } = node {
            if found_op {
                return Err(ParsingError::DuplicateFieldsError);
            }
            found_op = true;

            match op.as_str() {
                "\"∨\"" | "\"∧\"" | "\"=\"" | "\"≠\"" | "\"<\"" | "\"≤\"" | "\"+\"" | "\"-\"" | "\"*\"" | "\"%\"" | "\"\\\"" | "\"pow\"" | "\"log\"" | "\"⇒\"" | "\">\"" | "\"≥\"" => {
                    op_value = 1;
                }
                "\"U\"" | "\"W\"" => {
                    op_value = 2;
                }
                _ => {}
            }
        }
    }

    match op_value {
        1 => check_binary_op(vec),
        2 => check_until_op(vec),
        _ => Err(ParsingError::InvalidSyntaxError),
    }
}

fn check_until_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_count = 0;
    let mut left_count = 0;
    let mut right_count = 0;
    let mut step_bounds_count = 0;
    let mut time_bounds_count = 0;
    let mut reward_bounds_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionOperation{..} => {
                if op_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                op_count += 1;
            }
            ASTNode::ASTExpressionLeft{..} => {
                if left_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                left_count += 1;
            }
            ASTNode::ASTExpressionRight{..} => {
                if right_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                right_count += 1;
            }
            ASTNode::ASTExpressionStepBounds{..} => {
                if step_bounds_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                step_bounds_count += 1;
            }
            ASTNode::ASTExpressionTimeBounds{..} => {
                if time_bounds_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                time_bounds_count += 1;
            }
            ASTNode::ASTExpressionRewardBounds{..} => {
                if reward_bounds_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                reward_bounds_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if op_count != 1 || left_count != 1 || right_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_property_unary_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_value = 0;
    let mut found_op = false;

    for node in vec {
        if let ASTNode::ASTExpressionOperation{ op } = node {
            if found_op {
                return Err(ParsingError::DuplicateFieldsError);
            }
            found_op = true;

            match op.as_str() {
                "\"¬\"" | "\"floor\"" | "\"ceil\"" => {
                    op_value = 1;
                }
                "\"Pmin\"" | "\"Pmax\"" => {
                    op_value = 2;
                }
                "\"∀\"" | "\"∃\"" => {
                    op_value = 3;
                }
                "\"Emin\"" | "\"Emax\"" => {
                    op_value = 4;
                }
                "\"Smin\"" | "\"Smax\"" => {
                    op_value = 5;
                }
                _ => {}
            }
        }
    }

    match op_value {
        1 => check_unary_op(vec),
        2 => check_pminmax_op(vec),
        3 => check_aepath_op(vec),
        4 => check_eminmax_op(vec),
        5 => check_sminmax_op(vec),
        _ => Err(ParsingError::InvalidSyntaxError),
    }
}

fn check_pminmax_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_count = 0;
    let mut exp_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionOperation{..} => {
                if op_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                op_count += 1;
            }
            ASTNode::ASTExpressionOperand{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if op_count != 1 || exp_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_aepath_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_count = 0;
    let mut exp_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionOperation{..} => {
                if op_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                op_count += 1;
            }
            ASTNode::ASTExpressionOperand{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if op_count != 1 || exp_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_eminmax_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_count = 0;
    let mut exp_count = 0;
    let mut accumulate_count = 0;
    let mut reach_count = 0;
    let mut step_instant_count = 0;
    let mut time_instant_count = 0;
    let mut reward_instants_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionOperation{..} => {
                if op_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                op_count += 1;
            }
            ASTNode::ASTExpressionOperand{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            ASTNode::ASTExpressionAccumulate{..} => {
                if accumulate_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                accumulate_count += 1;
            }
            ASTNode::ASTExpressionReach{..} => {
                if reach_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                reach_count += 1;
            }
            ASTNode::ASTExpressionStepInstant{..} => {
                if step_instant_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                step_instant_count += 1;
            }
            ASTNode::ASTExpressionTimeInstant{..} => {
                if time_instant_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                time_instant_count += 1;
            }
            ASTNode::ASTExpressionRewardInstant{..} => {
                if reward_instants_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                reward_instants_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if op_count != 1 || exp_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_reward_instants(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut exp_count = 0;
    let mut accumulate_count = 0;
    let mut instant_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionRewardInstantExp{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            ASTNode::ASTExpressionRewardInstantAccumulate{..} => {
                if accumulate_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                accumulate_count += 1;
            }
            ASTNode::ASTExpressionRewardInstantInstant{..} => {
                if instant_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                instant_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if exp_count != 1 || accumulate_count != 1 || instant_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_sminmax_op(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut op_count = 0;
    let mut exp_count = 0;
    let mut accumulate_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionOperation{..} => {
                if op_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                op_count += 1;
            }
            ASTNode::ASTExpressionOperand{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            ASTNode::ASTExpressionAccumulate{..} => {
                if accumulate_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                accumulate_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if op_count != 1 || exp_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_reward_bounds(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut exp_count = 0;
    let mut accumulate_count = 0;
    let mut bounds_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTExpressionRewardBoundExp{..} => {
                if exp_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                exp_count += 1;
            }
            ASTNode::ASTExpressionRewardBoundAccumulate{..} => {
                if accumulate_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                accumulate_count += 1;
            }
            ASTNode::ASTExpressionRewardBoundBounds{..} => {
                if bounds_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                bounds_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    if exp_count != 1 || accumulate_count != 1 || bounds_count != 1 {
        return Err(ParsingError::MissingFieldsError);
    }

    Ok(())
}

fn check_property_interval(vec: &Vec<ASTNode>) -> Result<(), ParsingError> {
    let mut lower_count = 0;
    let mut lower_exclusive_count = 0;
    let mut upper_count = 0;
    let mut upper_exclusive_count = 0;

    for node in vec {
        match node {
            ASTNode::ASTPropertyIntervalLower{..} => {
                if lower_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                lower_count += 1;
            }
            ASTNode::ASTPropertyIntervalLowerExclusive{..} => {
                if lower_exclusive_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                lower_exclusive_count += 1;
            }
            ASTNode::ASTPropertyIntervalUpper{..} => {
                if upper_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                upper_count += 1;
            }
            ASTNode::ASTPropertyIntervalUpperExclusive{..} => {
                if upper_exclusive_count > 0 {
                    return Err(ParsingError::DuplicateFieldsError);
                }
                upper_exclusive_count += 1;
            }
            _ => return Err(ParsingError::InvalidSyntaxError),
        }
    }

    Ok(())
}