%start Spec 


%%

//Spec Rule
Spec -> Result<Vec<Module>, Box<dyn Error>>:
    Module { Ok(vec![$1?]) }
    | Spec Module { flatten($1, $2) } 
    ;

// Module Rule 
Module -> Result<Module, Box<dyn Error>>:
      Proctype { Ok(Module::Proctype($1?)) }
    | Init { Ok(Module::Init($1?)) }
    | Never { Ok(Module::Never($1?)) }
    | Trace { Ok(Module::Trace($1?)) }
    | Utype { Ok(Module::Utype($1?)) }
    //| Mtype { Ok(Module::Mtype($1?)) }
    | DeclList { Ok(Module::DeclList($1?)) }
    ;

Proctype -> Result<Proctype, Box<dyn Error>>:
    OptActive 'PROCTYPE' Name 'LPAREN' OptDecl_lst 'RPAREN' OptPriority OptEnabler 'LBRACE' Sequence 'RBRACE'
    {
        Ok(Proctype::new(
            $1?,$3?,$5?,$7?,$8?,$10?,false      
        ))
    }
    | OptActive 'D_PROCTYPE' Name 'LPAREN' OptDecl_lst 'RPAREN' OptPriority OptEnabler 'LBRACE' Sequence 'RBRACE'
    {
        Ok(Proctype::new(
            $1?,$3?,$5?,$7?,$8?,$10?,true      
        ))
    }
    ;

// Visible Rule (corresponding to 'visible')
Visible -> Result<Visible, Box<dyn Error>>:
    'HIDDEN' { Ok(Visible::new_hidden()) }
    | 'SHOW' { Ok(Visible::new_show()) }
    ;


// OptActive Rule
OptActive -> Result<Option<Active>, Box<dyn Error>>:
    Active { Ok(Some($1?)) }  
    | { Ok(None) }            
    ;

// OptDecl_lst Rule
OptDecl_lst -> Result<Option<DeclList>, Box<dyn Error>>:
    DeclList { Ok(Some($1?)) }
    | { Ok(None) } 
    ;

// OptPriority Rule
OptPriority -> Result<Option<Priority>, Box<dyn Error>>:
    Priority { Ok(Some($1?)) }
    | { Ok(None) } 
    ;


// OptEnabler Rule
OptEnabler -> Result<Option<Enabler>, Box<dyn Error>>:
    Enabler { Ok(Some($1?)) }
    |  { Ok(None) } 
    ; 

// Init Rule
Init -> Result<Init, Box<dyn Error>>:
    'INIT' OptPriority 'LBRACE' Sequence 'RBRACE'
    { Ok(Init::new($2?, $4?)) }
    ;

// Never Rule
Never -> Result<Never, Box<dyn Error>>:
    'NEVER' 'LBRACE' Sequence 'RBRACE'
    { Ok(Never::new($3?)) }
    ;

// Trace Rule
Trace -> Result<Trace, Box<dyn Error>>:
      'TRACE'   'LBRACE' Sequence 'RBRACE'
        { Ok(Trace::new($3?, TraceKind::Trace)) }
    | 'NOTRACE' 'LBRACE' Sequence 'RBRACE'
        { Ok(Trace::new($3?, TraceKind::Notrace)) }
    ;

// Utype Rule
Utype -> Result<Utype, Box<dyn Error>>:
    'TYPEDEF' Name 'LBRACE' DeclList 'RBRACE'
    { Ok(Utype::new($2?, $4?)) }
    ;

// Mtype Rule
Mtype -> Result<Mtype, Box<dyn Error>>:
    'MTYPE' OptSubtype 'LBRACE' NameList 'RBRACE' {
        Ok(Mtype::with_subtype($2?, OptEquals::from_option(None), $4?))
    }
    | 'MTYPE' OptSubtype 'ASSIGN' 'LBRACE' NameList 'RBRACE' {
        Ok(Mtype::with_subtype($2?, OptEquals::from_option(Some("=")), $5?))
    }
    | 'MTYPE' OptSubtype Name OptArrayDim OptInitScalar {
        let (opt_eq, maybe_name) = $5?;
        Ok(Mtype::var_decl_with_init(
            $3?,               /* name */
            $4?,               /* array dims */
            maybe_name,        
            $2?,               /* subtype */
            opt_eq
        ))
    };

// OptSubtype Rule
OptSubtype -> Result<Option<Name>, Box<dyn Error>>:
    'COLON' Name  {Ok(Some($2?))}
  | { Ok(None)};

OptInitScalar -> Result<(OptEquals, Option<Name>), Box<dyn Error>>:
      'ASSIGN' MtypeName { Ok((OptEquals::from_option(Some("=")), Some($2?))) }
    | { Ok((OptEquals::from_option(None), None)) }
;

MtypeName -> Result<Name, Box<dyn Error>>:
    Name { Ok($1?) }
;

// NameList Rule handles a list of names, with potential multiple names separated by commas
NameList -> Result<NameList, Box<dyn Error>>:
    Name { Ok(NameList::new(vec![$1?])) }
    | NameList 'COMMA' Name { Ok(flatten_namelist($1?, $3?)) }
    ;

// Typename Rule
Typename -> Result<Typename, Box<dyn Error>>:
    'BIT' { Ok(Typename::new_bit()) }
    | 'BOOL' { Ok(Typename::new_bool()) }
    | 'BYTE' { Ok(Typename::new_byte()) }
    | 'SHORT' { Ok(Typename::new_short()) }
    | 'INT' { Ok(Typename::new_int()) }
    | 'SMTYPE' {Ok(Typename::new_mtype())}
    | 'SMTYPE' 'COLON' Name { Ok(Typename::MtypeSubtype($3?)) }
    | 'CHAN' { Ok(Typename::new_chan()) }
    | Name { Ok(Typename::new_custom($1?)) }
    ;

// Const Rule
Const -> Result<Const, Box<dyn Error>>:
    'NUMBERS' { 
        let lexeme = $1.expect("Error: Expected a number token."); 
        let num_str = $lexer.span_str(lexeme.span()).to_string();
        if num_str.is_empty() {
            Err(Box::from("Error: Number cannot be empty")) 
        } else {
            match num_str.parse::<i32>() {
                Ok(value) => {
                    // debug_message(&format!("Parsed number: {}", value)); 
                    Ok(Const::new_number(value)) 
                }
                Err(err) => {
                    Err(Box::from(format!("Error parsing number '{}': {}", num_str, err))) 
                }
            }
        }
    }
    | 'FALSE' { 
        // debug_message("Received FALSE token");
        Ok(Const::new_false()) 
    }
    | 'SKIP' { 
        // debug_message("Received SKIP token");
        Ok(Const::new_skip()) 
    }
    | 'TRUE' { 
        // debug_message("Received TRUE token");
        Ok(Const::new_true()) 
    }
    ;

// Unarop Rule 
Unarop -> Result<Unarop, Box<dyn Error>>:
    'BITNOT' { Ok(Unarop::BitNot) }
    | 'NEGATIVE' { Ok(Unarop::Neg) }
    | 'NOT_OR_SEND' { Ok(Unarop::Not) }
    ;

// Andor Rule 
Andor -> Result<Andor, Box<dyn Error>>:
    'AND' { Ok(Andor::And) }
    | 'OR' { Ok(Andor::Or) }
    ;

// Any_expr Rule
Any_expr -> Result<AnyExpr, Box<dyn Error>>:
    LogicalExpr { Ok($1?) } 
    ;

// LogicalExpr Rule (OR, AND)
LogicalExpr -> Result<AnyExpr, Box<dyn Error>>:
    LogicalExpr Andor EqualityExpr { Ok(AnyExpr::new_andor($1?, $3?)) }
    | EqualityExpr { Ok($1?) }
    ;

// EqualityExpr Rule (==, !=)
EqualityExpr -> Result<AnyExpr, Box<dyn Error>>:
    EqualityExpr 'EQ' RelationalExpr { Ok(AnyExpr::new_equal($1?, $3?)) }
    | EqualityExpr 'NE' RelationalExpr { Ok(AnyExpr::new_not_equal($1?, $3?)) }
    | RelationalExpr { Ok($1?) }
    ;

// RelationalExpr Rule (<, >, <=, >=)
RelationalExpr -> Result<AnyExpr, Box<dyn Error>>:
    RelationalExpr 'LT' AdditiveExpr { Ok(AnyExpr::new_less_than($1?, $3?)) }
    | RelationalExpr 'LE' AdditiveExpr { Ok(AnyExpr::new_less_equal($1?, $3?)) }
    | RelationalExpr 'GT' AdditiveExpr { Ok(AnyExpr::new_greater_than($1?, $3?)) }
    | RelationalExpr 'GE' AdditiveExpr { Ok(AnyExpr::new_greater_equal($1?, $3?)) }
    | AdditiveExpr { Ok($1?) }
    ;

// AdditiveExpr Rule (+, -)
AdditiveExpr -> Result<AnyExpr, Box<dyn Error>>:
    AdditiveExpr 'PLUS' MultiplicativeExpr { Ok(AnyExpr::new_add($1?, $3?)) }
    | AdditiveExpr 'NEGATIVE' MultiplicativeExpr { Ok(AnyExpr::new_subtract($1?, $3?)) }
    | MultiplicativeExpr { Ok($1?) }
    ;

// MultiplicativeExpr Rule (*, /, %)
MultiplicativeExpr -> Result<AnyExpr, Box<dyn Error>>:
    MultiplicativeExpr 'MULTIPLY' UnaryExpr { Ok(AnyExpr::new_multiply($1?, $3?)) }
    | MultiplicativeExpr 'DIVIDE' UnaryExpr { Ok(AnyExpr::new_divide($1?, $3?)) }
    | MultiplicativeExpr 'MODULO' UnaryExpr { Ok(AnyExpr::new_modulo($1?, $3?)) }
    | UnaryExpr { Ok($1?) }
    ;

// UnaryExpr Rule
UnaryExpr -> Result<AnyExpr, Box<dyn Error>>:
    Unarop UnaryExpr {
        let op = $1?;
        let expr = $2?;
        Ok(AnyExpr::new_unary(op, expr)) 
    }
    | PrimaryExpr { Ok($1?) }
    ;

// PrimaryExpr Rule
PrimaryExpr -> Result<AnyExpr, Box<dyn Error>>:
    'LPAREN' Any_expr 'RPAREN' { Ok(AnyExpr::new_paren($2?)) }
    |'LPAREN' Any_expr 'ARROW' Any_expr 'COLON' Any_expr 'RPAREN' { Ok(AnyExpr::new_ternary($2?, $4?, $6?)) }
    | Const { Ok(AnyExpr::new_const($1?)) } 
    | Varref { Ok(AnyExpr::new_varref($1?)) }
    | 'TIMEOUT' { Ok(AnyExpr::Timeout) }
    | 'NP_' { Ok(AnyExpr::Np) }
    | 'ENABLED' 'LPAREN' Any_expr 'RPAREN' { Ok(AnyExpr::new_enabled($3?)) }
    | 'PC_VALUE' 'LPAREN' Any_expr 'RPAREN' { Ok(AnyExpr::new_pc_value($3?)) }
    | 'LEN' 'LPAREN' Varref 'RPAREN' { Ok(AnyExpr::new_len($3?)) }
    | 'RUN' Name 'LPAREN' OptArgLst 'RPAREN' OptPriority { Ok(AnyExpr::new_run($2?, $4?, $6?)) }
    | 'get_priority' 'LPAREN' Any_expr 'RPAREN' { Ok(AnyExpr::new_get_priority($3?)) }
    | 'set_priority' 'LPAREN' Any_expr 'COMMA' Any_expr 'RPAREN' { Ok(AnyExpr::new_set_priority($3?, $5?)) }
    | Chanpoll 'LPAREN' Any_expr 'RPAREN' { Ok(AnyExpr::new_chanpoll($1?, $3?)) }
    | Poll { Ok(AnyExpr::new_poll($1?)) } 
    | Name 'LBRACKET' Any_expr 'RBRACKET' '@' Name { Ok(AnyExpr::new_array_access($1?, $3?, $6?)) } 
    ;


// OptArgLst Rule
OptArgLst -> Result<Option<ArgList>, Box<dyn Error>>:
    { Ok(None) }
    | Arg_lst {
        let expr_vec = $1?;
        Ok(Some(ArgList::from_expr_vec(expr_vec)))
    }
    ;

// Chanpoll Rule
Chanpoll -> Result<Chanpoll, Box<dyn Error>>:
    'FULL' { Ok(Chanpoll::full()) }
    | 'EMPTY' { Ok(Chanpoll::empty()) }
    | 'NFULL' { Ok(Chanpoll::nfull()) }
    | 'NEMPTY' { Ok(Chanpoll::nempty()) }
    ;

// Name Rule
Name -> Result<Name, Box<dyn Error>>:
    'IDENTIFIER' { 
        let lexeme = $1.expect("Error: Expected an identifier token."); 
        let span = lexeme.span();
        let name_str = $lexer.span_str(span).to_string();
        debug!("In Name rule, token span: {:?}, testo: '{}'", span, name_str);
        if name_str.is_empty() {
            Err(Box::from("Error: Name cannot be empty"))
        } else {
            Ok(Name::new(name_str))
        }
    }
    ;

// IvarList Rule
IvarList -> Result<Vec<Ivar>, Box<dyn Error>>:
    Ivar { Ok(vec![$1?]) }
    | IvarList 'COMMA' Ivar {
        let ivars1 = $1?; 
        let ivar2 = $3?; 
        let ivars2 = vec![ivar2];
        flatten_ivar(ivars1, ivars2)
    }
    ;

//Ivar Rule
Ivar -> Result<Ivar, Box<dyn Error>>:
    Name OptArrayDim OptInit
    {
        Ok(Ivar::new($1?, $2?, $3?))
    }
    ;

// OptArrayDim Rule
OptArrayDim -> Result<Option<Const>, Box<dyn Error>>:
    'LBRACKET' Const 'RBRACKET' { Ok(Some($2?)) }
    | { Ok(None) }
    ;

// OptInit Rule
OptInit -> Result<Option<OptInit>, Box<dyn Error>>:
      'ASSIGN' Any_expr { Ok(Some(OptInit::new_expr($2?))) }
    | 'ASSIGN' ChInit { Ok(Some(OptInit::new_chinit($2?))) }
    | { Ok(None) }
    ;
    
// UnsignedDecl Rule
UnsignedDecl -> Result<UnsignedDecl, Box<dyn Error>>:
    'UNSIGNED' Name 'COLON' Const OptUInit
    {
        let (eq, maybe_expr) = $5?;       
        Ok(UnsignedDecl::new($2?, $4?, eq, maybe_expr))
    }
;

// = Any_expr   |  ε
OptUInit -> Result<(OptEquals, Option<AnyExpr>), Box<dyn Error>>:
      'ASSIGN' Any_expr  { Ok((OptEquals::from_option(Some("=")), Some($2?))) } 
    | { Ok((OptEquals::None,  None)) }
;


//  Active Rule
Active -> Result<Active, Box<dyn Error>>:
    'ACTIVE' OptCount
    { Ok(Active::new($2?)) }
    ;

// OptCount Rule
OptCount -> Result<Option<Const>, Box<dyn Error>>:
    'LBRACKET' Const 'RBRACKET' { Ok(Some($2?)) }
    | { Ok(None) }
    ;

// Priority Rule
Priority -> Result<Priority, Box<dyn Error>>:
    'PRIORITY' Const
    {
        Ok(Priority::new($2?))
    }
    ;

// enabler rule  
Enabler -> Result<Enabler, Box<dyn Error>>:
    'PROVIDED' 'LPAREN' Any_expr 'RPAREN'
    {
        Ok(Enabler::new($3?))
    }
    ;

// Sequence Rule
Sequence -> Result<Sequence, Box<dyn Error>>:
    Step { Ok(Sequence::new(vec![$1?])) }  
    | Sequence 'SEMICOLON' Step
        { Ok(flatten_sequence($1?, $3?)) }
    | Sequence 'ARROW' Step 
        { Ok(flatten_sequence($1?, $3?)) }
    ;

// Assign Rule
Assign -> Result<Assign, Box<dyn Error>>:
    Varref 'ASSIGN' Any_expr
    { Ok(Assign::new($1?, AssignOp::Eq, Some($3?))) }
    | Varref 'INCREMENT' { Ok(Assign::new($1?, AssignOp::Inc, None)) }
    | Varref 'DECREMENT' { Ok(Assign::new($1?, AssignOp::Dec, None)) }
    ;


// Stmnt Rule
Stmnt -> Result<Stmnt, Box<dyn Error>>:
    'IF' Options 'FI'
    { Ok(Stmnt::new_if($2?)) }
    | 'DO' Options 'OD' { Ok(Stmnt::new_do($2?)) }
    | 'FOR' 'LPAREN' Range 'RPAREN' 'LBRACE' Sequence 'RBRACE'
    { Ok(Stmnt::new_for($3?, $6?)) }
    | 'ATOMIC' 'LBRACE' Sequence 'RBRACE'
    { Ok(Stmnt::new_atomic($3?)) }
    | 'D_STEP' 'LBRACE' Sequence 'RBRACE'
    { Ok(Stmnt::new_dstep($3?)) }
    | 'SELECT' 'LPAREN' Range 'RPAREN'
    { Ok(Stmnt::new_select($3?)) }
    | 'LBRACE' Sequence 'RBRACE'
    { Ok(Stmnt::new_block($2?)) }
    | SendMsg
    { Ok(Stmnt::new_send_msg($1?)) }
    | Receive
    { Ok(Stmnt::new_receive($1?)) }
    | Assign { Ok(Stmnt::new_assign($1?)) }
    | 'ELSE' { Ok(Stmnt::new_else()) }
    | 'BREAK'
    { Ok(Stmnt::new_break()) }
    | 'GOTO' Name
    { Ok(Stmnt::new_goto($2?)) }
    | Name 'COLON' Stmnt
    { Ok(Stmnt::new_labeled($1?, $3?)) }
    | 'ASSERT' Any_expr
    { Ok(Stmnt::new_assert($2?)) }
    | Any_expr
    { Ok(Stmnt::new_expr($1?)) }
    | 'c_code' CBlock
    { Ok(Stmnt::new_c_code($2)) }
    | 'c_expr' CBlock
    { Ok(Stmnt::new_c_expr($2)) }
    | 'c_decl' CBlock
    { Ok(Stmnt::new_c_decl($2)) }
    | 'c_track' CBlock
    { Ok(Stmnt::new_c_track($2)) }
    | 'c_state' CBlock
    { Ok(Stmnt::new_c_state($2)) }
    ;

// Step Rule
Step -> Result<Step, Box<dyn Error>>:
    Stmnt UnlessClause { Ok(Step::new_statement(Box::new($1?), Some($2?))) }
    | 'XR' Varref VarrefList {
        let varref = $2?;  
        let varrefs_list = $3?;  
        let varrefs = flatten_varreflist(varrefs_list, varref)?;  
        Ok(Step::new_xr(varrefs))
    }
    | 'XS' Varref VarrefList {
        let varref = $2?;  
        let varrefs_list = $3?;
        let varrefs = flatten_varreflist(varrefs_list, varref)?; 
        Ok(Step::new_xs(varrefs))
    }
    | OneDecl { Ok(Step::new_declaration(Box::new($1?))) }
    ;

// DeclList Rule 
DeclList -> Result<DeclList, Box<dyn Error>>:
    OneDecl { Ok(DeclList::new(vec![$1?])) }
    | DeclList 'SEMICOLON' OneDecl { flatten_decl_list($1?, $3?) }    
    | DeclList 'ARROW' OneDecl { flatten_decl_list($1?, $3?) }
    ;

// OneDecl Rule
OneDecl -> Result<OneDecl, Box<dyn Error>>:
    Visible Typename IvarList { Ok(OneDecl::var_decl($2?, Some($1?),$3?)) }
    | Typename IvarList { Ok(OneDecl::var_decl_no_visible($1?, $2?)) }
    | Visible UnsignedDecl { Ok(OneDecl::unsigned_decl($2?, Some($1?))) }
    | UnsignedDecl { Ok(OneDecl::unsigned_decl($1?, None)) }
    | Mtype { Ok(OneDecl::new_mtype($1?)) }
    ;

// UnlessClause Rule
UnlessClause -> Result<UnlessClause, Box<dyn Error>>:
      { Ok(UnlessClause::none()) }  
    | 'UNLESS' Stmnt { Ok(UnlessClause::with_statement($2?)) }
    ;  

// VarrefList Rule
VarrefList -> Result<VarrefList, Box<dyn Error>>:
    { Ok(VarrefList::new()) } 
    | VarrefList 'COMMA' Varref { flatten_varreflist($1?, $3?) } 
    ;

// Varref Rule
Varref -> Result<Varref, Box<dyn Error>>:
    Name VarrefDetails
    {
        Ok(Varref::new($1?, Some($2?)))
    }
    ;

// VarrefDetails Rule
VarrefDetails -> Result<VarrefDetails, Box<dyn Error>>:
      { Ok(VarrefDetails::new(None, None)) } 
    | 'LBRACKET' Any_expr 'RBRACKET' 'DOT' Varref { 
        Ok(VarrefDetails::with_both($2?, $5?)) 
    }
    | 'LBRACKET' Any_expr 'RBRACKET' { 
        Ok(VarrefDetails::with_array_expr($2?)) 
    }
    | 'DOT' Varref { 
        Ok(VarrefDetails::with_nested_varref($2?)) 
    }
    ;


// ChInit Rule
ChInit -> Result<ChInit, Box<dyn Error>>:
    'LBRACKET' Const 'RBRACKET' 'OF' 'LBRACE' TypenameList 'RBRACE'
    { Ok(ChInit::new($2?, $6?)) }
    ;

// TypenameList Rule
TypenameList -> Result<Vec<Typename>, Box<dyn Error>>:
    Typename { Ok(vec![$1?]) }
    | TypenameList 'COMMA' Typename {
        let mut result = $1?;
        result.push($3?);
        Ok(result)
    }
    ;

// SendMsg Rule
SendMsg -> Result<SendMsg, Box<dyn Error>>:
    Varref 'NOT_OR_SEND' Send_args
    { Ok(SendMsg::new($1?, $3?, false)) }
    | Varref 'SORTED_SEND' Send_args
    { Ok(SendMsg::new($1?, $3?, true)) }
    ;

// Receive Rule
Receive -> Result<Receive, Box<dyn Error>>:
    Varref 'RECEIVE' Recv_args
    {
        Ok(Receive::new($1?, $3?, false, false, false)) // normal
    }
    | Varref 'RANDOM_RECEIVE' Recv_args
    {
        Ok(Receive::new($1?, $3?, true, false, false)) // random
    }
    | Varref 'POOL_WITH_SIDE-EFFECT' Recv_args 'GT'
    {
        Ok(Receive::new($1?, $3?, false, true, false)) // with side effect
    }
    | Varref 'DITTO_RECEIVE' Recv_args 'GT'
    {
        Ok(Receive::new($1?, $3?, true, true, true)) // random + side effect + ditto 
    }
    ;

// Poll Rule
Poll -> Result<Poll, Box<dyn Error>>:
    Varref 'POOL_WITHOUT_SIDE-EFFECT' Recv_args 'RBRACKET' // 'CLOSE_POOL_WITHOUT_SIDE-EFFECT' 
    {
        Ok(Poll::new($1?, $3?, false, false, false)) // normal
    }
    | Varref 'DITTO_POLL' Recv_args 'RBRACKET' // 'CLOSE_DITTO_POLL'
    {
        Ok(Poll::new($1?, $3?, true, false, true)) // random + ditto
    }
    ;

// Send_args Rules
Send_args -> Result<SendArgs, Box<dyn Error>>:
    Arg_lst {
            let args: Vec<Arg> = $1?.into_iter().map(|expr| Arg::new(expr)).collect();
            Ok(SendArgs::new_simple(args))
        }
    | Any_expr 'LPAREN' Arg_lst 'RPAREN' {
            let expr = $1?;
            let args: Vec<Arg> = $3?.into_iter().map(|expr| Arg::new(expr)).collect();
            Ok(SendArgs::new_with_expr(expr, args))
        }
    ;

// Arg_lst Rule
Arg_lst -> Result<Vec<AnyExpr>, Box<dyn Error>>:
    Any_expr { Ok(vec![$1?]) }
    | Arg_lst 'COMMA' Any_expr {
        let mut combined = $1?;
        combined.push($3?);
        Ok(combined)
    }
    ;

// Recv_args Rule
Recv_args -> Result<RecvArgs, Box<dyn Error>>:
    Recv_arg { Ok(RecvArgs::new_simple(vec![$1?])) }
    | Recv_args 'COMMA' Recv_arg { Ok(RecvArgs::new_simple(flatten_recv_args($1?, $3?)?)) }
    | Recv_arg 'LPAREN' Recv_args 'RPAREN' {
        Ok(RecvArgs::new_nested($1?, $3?))
    }
    ;

// Recv_arg Rule
Recv_arg -> Result<RecvArg, Box<dyn Error>>:
    Varref { Ok(RecvArg::new_varref($1?)) }
    | 'EVAL' 'LPAREN' Varref 'RPAREN' { Ok(RecvArg::new_eval($3?)) }
    | SignOpt Const
    {
        let sign = match $1 {
            Ok(Some(_)) => true,  
            Ok(None) => false,    
            Err(_) => return Err(Box::new(std::io::Error::other("Error in the SignOpt rule"))),

        };
        Ok(RecvArg::new_const(sign, $2?))
    }
    ;

// SignOpt Rule
SignOpt -> Result<Option<bool>, Box<dyn Error>>:
    'NEGATIVE' { Ok(Some(true)) }
    | { Ok(None) }
    ;

// Options Rule
Options -> Result<Options, Box<dyn Error>>:
    'COLONCOLON' Sequence MoreOptions
    {
        let seq2 = __gt_arg_2?;
        let more_options = __gt_arg_3?;
        let combined_sequences = combine_sequences(vec![seq2], Ok(more_options))?;  
        Ok(Options::new(combined_sequences))
    }
    ;

// MoreOptions Rule
MoreOptions -> Result<Vec<Sequence>, Box<dyn Error>>:
    { Ok(Vec::new()) }
    | MoreOptions 'COLONCOLON' Sequence
    {
        let mut sequences = $1?;
        sequences.push($3?);
        Ok(sequences)
    }
    ;

// Range Rule
Range -> Result<Range, Box<dyn Error>>:
    Name 'COLON' Any_expr 'DOTDOT' Any_expr
    {
        Ok(Range::FromTo($1?, $3?, $5?))
    }
    | Name 'IN' Name
    {
        Ok(Range::In($1?, $3?))
    }
    ;

    CBlock -> String:
        'LBRACE' 'RBRACE'
        {
            let code: String = $lexer.span_str($span).into();  
            code
        }
    ;

%%
use std::error::Error;
use std::fmt; // Import fmt for formatting and displaying
use log::debug;

// Flatten function for Spec Rule
fn flatten(lhs: Result<Vec<Module>, Box<dyn Error>>, 
            rhs: Result<Module, Box<dyn Error>>) 
            -> Result<Vec<Module>, Box<dyn Error>> {
    let mut flt = lhs?;
    flt.push(rhs?);
    Ok(flt)
}

// module Structs
#[derive(Debug)]
pub enum Module {
    Proctype(Proctype),
    Init(Init),
    Never(Never),
    Trace(Trace),
    Utype(Utype),
    Mtype(Mtype),
    DeclList(DeclList),
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Module::Proctype(p) => writeln!(f, "Proctype_Struct: {p}"),
            Module::Init(i) => writeln!(f, "Init_Struct:\n {i}"),
            Module::Never(n) => writeln!(f, "Never_Struct:\n {n}"),
            Module::Trace(t) => writeln!(f, "Trace_Struct:\n {t}"),
            Module::Utype(u) => writeln!(f, "Utype_Struct:\n {u}"),
            Module::Mtype(m) => writeln!(f, "Mtype_Struct: {m}"),
            Module::DeclList(d) => writeln!(f, "DeclList_Struct:\n {d}"),
        }
    }
}

// Proctype Struct
#[derive(Debug)]
pub struct Proctype {
    pub active: Option<Active>,
    pub name: Name,
    pub decl_list: Option<DeclList>,
    pub priority: Option<Priority>,
    pub enabler: Option<Enabler>,
    pub sequence: Sequence,
    pub deterministic: bool,
}

impl Proctype {
    pub fn new(
        active: Option<Active>,
        name: Name,
        decl_list: Option<DeclList>,
        priority: Option<Priority>,
        enabler: Option<Enabler>,
        sequence: Sequence,
        deterministic: bool,
    ) -> Self {
        Self {
            active,
            name,
            decl_list,
            priority,
            enabler,
            sequence,
            deterministic,
        }
    }
}

impl fmt::Display for Proctype {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Proctype {{\n   active: {},\n   name: {},\n   decl_list:\n  {},\n   priority: {},\n   enabler: {},\n   sequence:\n  {},\n   deterministic: {}\n}}",
            self.active.as_ref().map_or("None".to_string(), |a| format!("   {}", a)),
            self.name,
            self.decl_list.as_ref().map_or("None".to_string(), |d| format!("   {}", d)),
            self.priority.as_ref().map_or("None".to_string(), |p| format!("   {}", p)),
            self.enabler.as_ref().map_or("None".to_string(), |e| format!("   {}", e)),
            self.sequence,
            self.deterministic,
        )
    }
}

// Active Struct
#[derive(Debug)]
pub struct Active {
    pub count: Option<Const>,
}

impl Active {
    // Costruttore per la struttura Active
    pub fn new(count: Option<Const>) -> Self {
        Active { count }
    }
}

impl fmt::Display for Active {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Active {{\n  count: {}\n}}",
            self.count.as_ref().map_or("None".to_string(), |c| format!("{}", c))
        )
    }
}

// Const Struct
#[derive(Debug)]
pub enum Const {
    True,
    False,
    Skip,
    Number(i32),
}

impl Const {
    pub fn new_number(value: i32) -> Self {
        Const::Number(value)
    }

    pub fn new_true() -> Self {
        Const::True
    }

    pub fn new_false() -> Self {
        Const::False
    }

    pub fn new_skip() -> Self {
        Const::Skip
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Const::True => write!(f, "True"),
            Const::False => write!(f, "False"),
            Const::Skip => write!(f, "Skip"),
            Const::Number(n) => write!(f, "{n}"),
        }
    }
}

// Name Struct
#[derive(Debug)]
pub struct Name {
    pub identifier: String,
}

impl Name {
    // Costruttore per la struttura Name
    pub fn new(identifier: String) -> Self {
        Name { identifier }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.identifier)  
    }
}


// DeclList Struct
#[derive(Debug)]
pub struct DeclList {
    pub decls: Vec<OneDecl>,  // Encapsulates a list of OneDecl
}

impl DeclList {
    pub fn new(decls: Vec<OneDecl>) -> Self {
        DeclList { decls }
    }

    pub fn add_decl(&mut self, decl: OneDecl) {
        self.decls.push(decl);
    }
}

// Function to join two OneDecls into a DeclList
fn flatten_decl_list(mut decl_list: DeclList, decl: OneDecl) -> Result<DeclList, Box<dyn Error>> {
    decl_list.add_decl(decl);  // Adds the new declaration to the list
    Ok(decl_list)  // Returns the updated list
}

impl fmt::Display for DeclList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Declarations:")?;
        for (i, decl) in self.decls.iter().enumerate() {
            writeln!(f, "      {}:", i + 1)?;
            writeln!(f, "        {}", decl)?;
        }
        Ok(())
    }
}

// VarDecl Struct
#[derive(Debug)]
pub struct VarDecl {
    pub visible: Option<Visible>,  
    pub typename: Typename,        
    pub ivars: Vec<Ivar>,         
}

impl VarDecl {
    pub fn new(typename: Typename, visible: Option<Visible>, ivars: Vec<Ivar>) -> Self {
        VarDecl { visible, typename, ivars }
    }
}

impl fmt::Display for VarDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "   Type: {}", self.typename)?;
        if let Some(visible) = &self.visible {
            write!(f, " (Visible: {})", visible)?;
        }
        write!(f, "\n         Variables:")?;
        for ivar in &self.ivars {
            writeln!(f, "\n          {}", ivar)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct UnsignedDecl {
        pub name: Name,
        pub colon: Const,
        pub equals: OptEquals, 
        pub any_expr: Option<AnyExpr>, 
    }

    impl UnsignedDecl {
        pub fn new(
            name: Name,
            colon: Const,
            equals: OptEquals,
            any_expr: Option<AnyExpr>
        ) -> Self {
                assert!(matches!(equals, OptEquals::None) == any_expr.is_none(),
                "If OptEquals is None, AnyExpr must also be None.");

            UnsignedDecl {
                name,
                colon,
                equals,
                any_expr,
            }
        }
    }

impl fmt::Display for UnsignedDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {}", self.name, self.colon)?;
        if let Some(expr) = &self.any_expr {
            write!(f, " = {}", expr)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum OneDecl {
    Var(VarDecl),                              
    Unsigned(UnsignedDecl, Option<Visible>),
    Mtype(Mtype),
}

impl OneDecl {
    pub fn var_decl(
        typename: Typename,
        visible: Option<Visible>,
        ivars: Vec<Ivar>,
    ) -> Self {
        OneDecl::Var(VarDecl::new(typename, visible, ivars))
    }

    pub fn var_decl_no_visible(
        typename: Typename,
        ivars: Vec<Ivar>,
    ) -> Self {
        OneDecl::Var(VarDecl::new(typename, None, ivars)) 
    }

    pub fn unsigned_decl(
        unsigned_decl: UnsignedDecl,
        visible: Option<Visible>,
    ) -> Self {
        let UnsignedDecl {
            name,
            colon,
            equals,
            any_expr,
        } = unsigned_decl;

        OneDecl::Unsigned(UnsignedDecl::new(name, colon, equals, any_expr), visible)
    }
    pub fn new_mtype(mtype: Mtype) -> Self {
        OneDecl::Mtype(mtype)
    }
}

impl fmt::Display for OneDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OneDecl::Var(var_decl) => write!(f, "VarDecl:\n      {}", var_decl),
            OneDecl::Unsigned(unsigned_decl, visible) => {
                write!(f, "UnsignedDecl: \n {}", unsigned_decl)?;
                if let Some(visible) = visible {
                    write!(f, "\nVisibility: {}", visible)?;
                }
                Ok(())
            }
            OneDecl::Mtype(mtype) => write!(f, "Mtype:\n{}", mtype),
        }
    }
}

// OptEquals Struct
#[derive(Debug)]
pub enum OptEquals {
    Equals(String), 
    None,         
}

impl OptEquals {
    pub fn from_option(input: Option<&str>) -> Self {
        match input {
            Some("=") => OptEquals::Equals("=".to_string()),
            _ => OptEquals::None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            OptEquals::Equals(s) => Some(s),
            OptEquals::None => None,
        }
    }
}

impl fmt::Display for OptEquals {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OptEquals::Equals( s) => write!(f, "Equals: {}", s),
            OptEquals::None => write!(f, "No Equals"),
        }
    }
}

// Visible Struct
#[derive(Debug)]
pub enum Visible {
    Hidden,
    Show,
}

impl Visible {
    pub fn new_hidden() -> Self {
        Visible::Hidden
    }

    pub fn new_show() -> Self {
        Visible::Show
    }
}

impl fmt::Display for Visible {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Visible::Hidden => write!(f, "Hidden"),
            Visible::Show => write!(f, "Show"),
        }
    }
}

// Typename Struct
#[derive(Debug)]
pub enum Typename {
    Bit,
    Bool,
    Byte,
    Short,
    Int,
    Chan,
    Custom(Name),
    MtypeSubtype(Name),
    Mtype,
}

impl Typename {
    pub fn new_bit() -> Self {
        Typename::Bit
    }

    pub fn new_bool() -> Self {
        Typename::Bool
    }

    pub fn new_byte() -> Self {
        Typename::Byte
    }

    pub fn new_short() -> Self {
        Typename::Short
    }

    pub fn new_int() -> Self {
        Typename::Int
    }

    pub fn new_chan() -> Self {
        Typename::Chan
    }

    pub fn new_mtype() -> Self {
        Typename::Mtype
    }

    pub fn new_mtype_subtype(name: Name) -> Self {
        Typename::MtypeSubtype(name)
    }

    pub fn new_custom(name: Name) -> Self {
        Typename::Custom(name)
    }
}

impl fmt::Display for Typename {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Typename::Bit => write!(f, "Bit"),
            Typename::Bool => write!(f, "Bool"),
            Typename::Byte => write!(f, "Byte"),
            Typename::Short => write!(f, "Short"),
            Typename::Int => write!(f, "Int"),
            Typename::Chan => write!(f, "Chan"),
            Typename::Mtype => write!(f, "Mtype"),
            Typename::Custom( name) => write!(f, " Custom Type with name: {}", name),
            Typename::MtypeSubtype( name) => write!(f, "Mtype Subtype with name: {}", name),
        }
    }
}

// Init Struct
#[derive(Debug)]
pub struct Init {
    pub priority: Option<Priority>,
    pub sequence: Sequence,
}

impl Init {
    pub fn new(priority: Option<Priority>, sequence: Sequence) -> Self {
        Init { priority, sequence }
    }
}

impl fmt::Display for Init {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, " \n{}", self.sequence)?;
        if let Some(priority) = &self.priority {
            write!(f, "  Priority: {}", priority.value)?;
        }
        Ok(())
    }
}

// Priority Struct
#[derive(Debug)]
pub struct Priority {
    pub value: Const,
}

impl Priority {
    pub fn new(value: Const) -> Self {
        Priority { value }
    }
}

impl fmt::Display for Priority {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Priority: {}", self.value)
    }
}

// Sequence Struct
#[derive(Debug)]
pub struct Sequence {
    pub steps: Vec<Step>,
}

impl Sequence {
    pub fn new(steps: Vec<Step>) -> Self {
        Sequence { steps }
    }
}

pub fn flatten_sequence(seq1: Sequence, step: Step) -> Sequence {
    let mut combined = seq1.steps;
    combined.push(step);
    Sequence::new(combined)
}

impl fmt::Display for Sequence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "    Sequence:")?;
        for (i, step) in self.steps.iter().enumerate() {
            writeln!(f, "            Step {}:\n             {}\n", i + 1, step)?;
        }
        Ok(())
    }
}

// Never Struct
#[derive(Debug)]
pub struct Never {
    sequence: Sequence,
}

impl Never {
    pub fn new(sequence: Sequence) -> Self {
        Never { sequence }
    }
}

impl fmt::Display for Never {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n Never sequence:\n{}", self.sequence)
    }
}

// Trace Struct
#[derive(Debug)]
pub enum TraceKind { Trace, Notrace }

#[derive(Debug)]
pub struct Trace {
    kind: TraceKind,   
    sequence: Sequence,
}

impl Trace {
    pub fn new(sequence: Sequence, kind: TraceKind) -> Self {
        Self { kind, sequence }
    }
}

impl fmt::Display for Trace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let label = match self.kind {
            TraceKind::Trace   => "Trace",
            TraceKind::Notrace => "Notrace",
        };
        write!(f, "\n {label} sequence:\n{}", self.sequence)
    }
}

// Utype Struct
#[derive(Debug)]
pub struct Utype {
    pub name: Name,
    pub decl_list: DeclList,
}

impl Utype {
    pub fn new(name: Name, decl_list: DeclList) -> Self {
        Utype { name, decl_list }
    }
}

impl fmt::Display for Utype {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Utype_name: {} \n   {}", self.name, self.decl_list)
    }
}

// NameList Struct
#[derive(Debug)]
pub struct NameList {
    pub names: Vec<Name>,
}

impl NameList {
    pub fn new(names: Vec<Name>) -> Self {
        NameList { names }
    }
}

pub fn flatten_namelist(mut list1: NameList, name: Name) -> NameList {
    list1.names.push(name);
    list1
}

impl fmt::Display for NameList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NameList: ")?;
        for name in &self.names {
            write!(f, "{} ", name)?;
        }
        Ok(())
    }
}

// Mtype Struct
#[derive(Debug)]
pub enum MtypeInit {
    Enum(NameList),   
    Scalar(Name),     
    None,            
}

#[derive(Debug)]
pub struct Mtype {
    pub subtype:     Option<Name>,   
    pub array_dims:  Option<Const>,  
    pub name:        Name,           
    pub opt_equals:  OptEquals,      
    pub init:        MtypeInit,    
}

impl Mtype {
    pub fn with_subtype(
        subtype:    Option<Name>,
        opt_eq:     OptEquals,     
        name_list:  NameList
    ) -> Self {
        Self {
            subtype,
            array_dims: None,
            name: Name { identifier: String::new() },
            opt_equals: opt_eq,
            init: MtypeInit::Enum(name_list),
        }
    }

    pub fn var_decl_with_init(
        name:       Name,
        array_dims: Option<Const>,  
        maybe_val:  Option<Name>,    
        subtype:    Option<Name>,
        opt_eq:     OptEquals,     
    ) -> Self {
        let init = match maybe_val {
            Some(v) => MtypeInit::Scalar(v),
            None    => MtypeInit::None,
        };

        Self { subtype, array_dims, name, opt_equals: opt_eq, init }
    }
}

impl fmt::Display for Mtype {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "mtype")?;
        if let Some(sub) = &self.subtype {
            write!(f, ":{}", sub)?;
        }

        let is_enum = self.name.identifier.is_empty();

        if !is_enum {
            write!(f, " {}", self.name)?;
            if let Some(dim) = &self.array_dims {
                write!(f, "[{}]", dim)?;
            }
        }

        match &self.init {
            MtypeInit::Enum(list) => {
                if matches!(self.opt_equals, OptEquals::Equals(_)) {
                    write!(f, " =")?;
                }
                write!(f, " {{ ")?;
                for (i, n) in list.names.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", n)?;
                }
                write!(f, " }}")?;
            }

            MtypeInit::Scalar(val) => {
                if matches!(self.opt_equals, OptEquals::Equals(_)) {
                    write!(f, " = {}", val)?;
                }
            }

            MtypeInit::None => {
            }
        }

        Ok(())
    }
}

// IvarList Struct
#[derive(Debug)]
pub struct IvarList {
    pub ivars: Vec<Ivar>,
}

impl IvarList {
    pub fn new(ivars: Vec<Ivar>) -> Self {
        IvarList { ivars }
    }
}

// Helper function for "flattening" the vector of ivars
fn flatten_ivar(v1: Vec<Ivar>, v2: Vec<Ivar>) -> Result<Vec<Ivar>, Box<dyn Error>> {
    let mut result = v1;
    result.extend(v2);
    Ok(result)
}

impl fmt::Display for IvarList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "IvarList: ")?;
        for ivar in &self.ivars {
            write!(f, "  {}\n", ivar)?;
        }
        Ok(())
    }
}

// Ivar Struct
#[derive(Debug)]
pub struct Ivar {
    pub name: Name,               
    pub array_dims: Option<Const>, 
    pub init: Option<OptInit>,   
}

impl Ivar {
    pub fn new(name: Name, array_dims: Option<Const>, init: Option<OptInit>) -> Self {
        Ivar {
            name,
            array_dims,
            init,
        }
    }
}

impl fmt::Display for Ivar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let array_dims_str = match &self.array_dims {
            Some(dim) => format!("[{}]", dim),
            None => String::from(""),
        };

        let init_str = match &self.init {
            Some(init) => format!("({})", init),
            None => String::from("None"),
        };

        write!(f, "Ivar: {}{} \n           (Init : {})", self.name, array_dims_str, init_str)
    }
}

// OptArrayDim Struct
#[derive(Debug)]
pub enum OptArrayDim {
    Some(Vec<Const>), 
    None,            
}

impl OptArrayDim {
    pub fn from_option(input: Option<Vec<Const>>) -> Self {
        match input {
            Some(dimensions) => OptArrayDim::Some(dimensions),
            None => OptArrayDim::None,
        }
    }

    pub fn as_option(&self) -> Option<&Vec<Const>> {
        match self {
            OptArrayDim::Some(dimensions) => Some(dimensions),
            OptArrayDim::None => None,
        }
    }
}

impl fmt::Display for OptArrayDim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OptArrayDim::Some( dims) => {
                write!(f, "ArrayDims: [")?;
                for (i, dim) in dims.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", dim)?;
                }
                write!(f, "]")
            }
            OptArrayDim::None => write!(f, "No ArrayDims"),
        }
    }
}

#[derive(Debug)]
pub enum OptInit {
    Expr(AnyExpr),   
    ChInit(ChInit), 
}

impl OptInit {
    pub fn new_expr(expr: AnyExpr) -> Self {
        OptInit::Expr(expr)
    }

    pub fn new_chinit(chinit: ChInit) -> Self {
        OptInit::ChInit(chinit)
    }
}

impl fmt::Display for OptInit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OptInit::Expr( expr) => write!(f, "Expression -> {}", expr),
            OptInit::ChInit( chinit) => write!(f, "ChInit -> {}", chinit),
        }
    }
}

// ChInit Struct
#[derive(Debug)]
pub struct ChInit {
    pub const_value: Const,     
    pub typename_list: Vec<Typename>,
}

impl ChInit {
    pub fn new(const_value: Const, typename_list: Vec<Typename>) -> Self {
        ChInit {
            const_value,
            typename_list,
        }
    }
}

impl fmt::Display for ChInit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, " {{ const_value: {}, typename_list: {:?} }}", self.const_value, self.typename_list)
    }
}

// Struct VarrefList
#[derive(Debug)]
pub struct VarrefList {
    pub varrefs: Vec<Varref>, 
}

impl VarrefList {
    pub fn new() -> Self {
        VarrefList {
            varrefs: Vec::new(),
        }
    }

    pub fn add_varref(&mut self, varref: Varref) {
        self.varrefs.push(varref);
    }
}

// Function to flatten VarrefList
fn flatten_varreflist(varref_list: VarrefList, varref: Varref) -> Result<VarrefList, Box<dyn Error>> {
    let mut updated_list = varref_list;  
    updated_list.add_varref(varref);  
    Ok(updated_list)  
}

impl fmt::Display for VarrefList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.varrefs.is_empty() {
            write!(f, "VarrefList: Empty\n")
        } else {
            write!(f, "VarrefList:\n")?;
            for (i, varref) in self.varrefs.iter().enumerate() {
                write!(f, "  Varref {}: {}\n", i + 1, varref)?;
            }
            Ok(())
        }
    }
}

// Varref Struct
#[derive(Debug)]
pub struct Varref {
    pub name: Name,
    pub details: Option<VarrefDetails>,
}

impl Varref {
    pub fn new(name: Name, details: Option<VarrefDetails>) -> Self {
        Varref { name, details }
    }
}

impl fmt::Display for Varref {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, " Name: {}", self.name.identifier)?;
        if let Some( details) = &self.details {
            write!(f, "  Details:\n   {}", details)
        } else {
            write!(f, "  Details:\n   None")
        }
    }
}

// VarrefDetails Struct 
#[derive(Debug)]
pub struct VarrefDetails {
    pub array_expr: Option<AnyExpr>,    
    pub nested_varref: Option<Box<Varref>> 
}

impl VarrefDetails {
    pub fn new(array_expr: Option<AnyExpr>, nested_varref: Option<Box<Varref>>) -> Self {
        VarrefDetails {
            array_expr,
            nested_varref,
        }
    }

    pub fn with_array_expr(expr: AnyExpr) -> Self {
        VarrefDetails {
            array_expr: Some(expr),
            nested_varref: None,
        }
    }

    pub fn with_nested_varref(varref: Varref) -> Self {
        VarrefDetails {
            array_expr: None,
            nested_varref: Some(Box::new(varref)),
        }
    }

    pub fn with_both(expr: AnyExpr, varref: Varref) -> Self {
        VarrefDetails {
            array_expr: Some(expr),
            nested_varref: Some(Box::new(varref)),
        }
    }

    pub fn format_option<T: fmt::Display>(&self, option: &Option<T>) -> String {
        match option {
            Some(value) => format!("{}", value),
            None => "None".to_string(),
        }
    }
}

impl fmt::Display for VarrefDetails {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "VarrefDetails:")?;
        writeln!(f, "  ArrayExpr: {}", self.format_option(&self.array_expr))?;
        writeln!(f, "  NestedVarref: {}", self.format_option(&self.nested_varref))?;
        Ok(())
    }
}

// SendMsg Struct
#[derive(Debug)]
pub struct SendMsg {
    pub varref: Varref,
    pub args: SendArgs,
    pub is_sorted: bool,
}

impl SendMsg {
    pub fn new(varref: Varref, args: SendArgs, is_sorted: bool) -> Self {
        SendMsg { varref, args, is_sorted }
    }
}

impl fmt::Display for SendMsg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SendMsg(Varref: {}, Args: {}, IsSorted: {})", 
                self.varref, 
                self.args, 
                self.is_sorted)
    }
}

// SendArgs Struct
#[derive(Debug)]
pub enum SendArgs {
    Simple(Vec<Arg>),
    WithExpr(AnyExpr, Vec<Arg>),
}

impl SendArgs {
    pub fn new_simple(args: Vec<Arg>) -> Self {
        SendArgs::Simple(args)
    }

    pub fn new_with_expr(expr: AnyExpr, args: Vec<Arg>) -> Self {
        SendArgs::WithExpr(expr, args)
    }
}

impl fmt::Display for SendArgs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SendArgs::Simple( args) => write!(f, "Simple Args: {:?}", args),
            SendArgs::WithExpr( expr,  args) => write!(f, "WithExpr: {}, Args: {:?}", expr, args),
        }
    }
}

// ArgList Struct
#[derive(Debug)]
pub struct ArgList {
    pub args: Vec<Arg>,
}

impl ArgList {
    pub fn new_single(arg: Arg) -> Self {
        ArgList {
            args: vec![arg],
        }
    }

    pub fn add_arg(mut self, arg: Arg) -> Self {
        self.args.push(arg);
        self
    }

    pub fn from_expr_vec(expr_vec: Vec<AnyExpr>) -> Self {
        let args = expr_vec.into_iter()
                            .map(Arg::new) 
                            .collect();    
        ArgList { args } 
    }
}

impl fmt::Display for ArgList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;  
            }
            write!(f, "{}", arg)?; 
        }
        Ok(())
    }
}


// Arg Struct
#[derive(Debug)]
pub struct Arg {
    pub expr: AnyExpr,  
}

impl Arg {
    pub fn new(expr: AnyExpr) -> Self {
        Arg { expr }
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr) 
    }
}

// RecvArgs Struct
#[derive(Debug)]
pub enum RecvArgs {
    Simple(Vec<RecvArg>),
    Nested(RecvArg, Box<RecvArgs>), 
}

impl RecvArgs {
    pub fn new_simple(args: Vec<RecvArg>) -> Self {
        RecvArgs::Simple(args)
    }

    pub fn new_nested(arg: RecvArg, args: RecvArgs) -> Self {
        RecvArgs::Nested(arg, Box::new(args))
    }
}

fn flatten_recv_args(args: RecvArgs, arg: RecvArg) -> Result<Vec<RecvArg>, Box<dyn Error>> {
    let mut flattened = Vec::new();

    match args {
        RecvArgs::Simple(mut vec) => flattened.append(&mut vec),
        RecvArgs::Nested(arg, boxed_args) => {
            flattened.push(arg);
            match *boxed_args {
                RecvArgs::Simple(mut vec) => flattened.append(&mut vec),
                RecvArgs::Nested(_, _) => return Err(Box::new(std::io::Error::other("Annidamento complesso non supportato"))),
            }
        }
    }
    flattened.push(arg);
    Ok(flattened)
}

impl fmt::Display for RecvArgs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RecvArgs::Simple( args) => write!(f, "Simple RecvArgs: [{}]", 
                args.iter().map(|arg| format!("{}", arg)).collect::<Vec<String>>().join(", ")),
            RecvArgs::Nested( arg,  nested_args) => write!(f, "Nested RecvArgs: ({} -> {})", 
                arg, nested_args),
        }
    }
}

// Enum to handle different types of arguments in Recv
#[derive(Debug)]
pub enum RecvArg {
    Varref(Varref),       
    Eval(Varref),            
    Const(bool, Const),     
}

impl RecvArg {
    pub fn new_varref(varref: Varref) -> Self {
        RecvArg::Varref(varref)
    }
    pub fn new_eval(varref: Varref) -> Self {
        RecvArg::Eval(varref)
    }
    pub fn new_const(sign: bool, constant: Const) -> Self {
        RecvArg::Const(sign, constant)
    }
}

impl fmt::Display for RecvArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RecvArg::Varref( varref) => write!(f, "Varref({})", varref),
            RecvArg::Eval( varref) => write!(f, "Eval({})", varref),
            RecvArg::Const(sign,  constant) => {
                if *sign {
                    write!(f, "+{}", constant) 
                } else {
                    write!(f, "-{}", constant) 
                }
            }
        }
    }
}

// Custom Flatten Options Rule function
fn combine_sequences(
    seq1: Vec<Sequence>,
    seq2_result: Result<Vec<Sequence>, Box<dyn Error>>
) -> Result<Vec<Sequence>, Box<dyn Error>> {
    let seq2 = seq2_result?;
    let mut combined = seq1;
    combined.extend(seq2);
    
    Ok(combined)
}

// Assign Struct
#[derive(Debug)]
pub struct Assign {
    pub varref: Varref,       
    pub op: AssignOp,            
    pub expr: Option<AnyExpr>,   
}

impl Assign {
    pub fn new(varref: Varref, op: AssignOp, expr: Option<AnyExpr>) -> Self {
        Assign { varref, op, expr }
    }
}

impl fmt::Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "  \n")?;
        write!(f, "       Variable :\n        {}\n", self.varref)?;
        write!(f, "        Operator: {} \n", self.op)?;
        match &self.expr {
            Some(expr) => write!(f, "         Expression:\n    {}\n", expr),
            None => write!(f, "         Expression: None\n"),
        }
    }
}

// AssignOp Struct
#[derive(Debug)]
pub enum AssignOp {
    Eq,    // '=' 
    Inc,   // '++' 
    Dec,   // '--' 
}

impl fmt::Display for AssignOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignOp::Eq => write!(f, "="),  
            AssignOp::Inc => write!(f, "++"), 
            AssignOp::Dec => write!(f, "--"), 
        }
    }
}

// Step Struct
#[derive(Debug)]
pub enum Step {
    Statement(Box<Stmnt>, Option<UnlessClause>),  
    XR(VarrefList),                           
    XS(VarrefList),                             
    Declaration(Box<OneDecl>),              
}

impl Step {
    pub fn new_statement(stmt: Box<Stmnt>, unless: Option<UnlessClause>) -> Self {
        Step::Statement(stmt, unless)
    }

    pub fn new_xr(varrefs: VarrefList) -> Self {
        Step::XR(varrefs)
    }

    pub fn new_xs(varrefs: VarrefList) -> Self {
        Step::XS(varrefs)
    }

    pub fn new_declaration(decl: Box<OneDecl>) -> Self {
        Step::Declaration(decl)
    }
}

impl fmt::Display for Step {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Step::Statement(stmt, unless) => {
                writeln!(f, "    Statement:")?; 
                writeln!(f, "                    {}", stmt)?; 
                
                // FIX UNLESS
                if let Some(unless_clause) = unless {
                    writeln!(f, "      Unless: {}", unless_clause)?; 
                }
                Ok(())
            }
            Step::XR(varref_list) => {
                writeln!(f, "    XR:")?;
                writeln!(f, "      {}", varref_list) 
            }
            Step::XS(varref_list) => {
                writeln!(f, "    XS:")?;
                writeln!(f, "      {}", varref_list)
            }
            Step::Declaration(decl) => {
                writeln!(f, "    Declaration:")?;
                writeln!(f, "      {}", decl)
            }
        }
    }
}

#[derive(Debug)]
pub enum Chanpoll {
    Full,
    Empty,
    NFull,
    NEmpty,
}

impl Chanpoll {
    pub fn full() -> Self {
        Chanpoll::Full
    }
    pub fn empty() -> Self {
        Chanpoll::Empty
    }
    pub fn nfull() -> Self {
        Chanpoll::NFull
    }
    pub fn nempty() -> Self {
        Chanpoll::NEmpty
    }
}

impl fmt::Display for Chanpoll {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Chanpoll::Full => write!(f, "Full"),
            Chanpoll::Empty => write!(f, "Empty"),
            Chanpoll::NFull => write!(f, "NFull"),
            Chanpoll::NEmpty => write!(f, "NEmpty"),
        }
    }
}

// Unarop Struct
#[derive(Debug)]
pub enum Unarop {
    BitNot, // ~
    Neg,    // -
    Not,    // !
}

impl Unarop {
    pub fn apply(&self, expr: AnyExpr) -> AnyExpr {
        match self {
            Unarop::BitNot => AnyExpr::Unary(Unarop::BitNot, Box::new(expr)),
            Unarop::Neg => AnyExpr::Unary(Unarop::Neg, Box::new(expr)),
            Unarop::Not => AnyExpr::Unary(Unarop::Not, Box::new(expr)),
        }
    }
}

impl fmt::Display for Unarop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Unarop::BitNot => write!(f, "~"),
            Unarop::Neg => write!(f, "-"),
            Unarop::Not => write!(f, "!"),
        }
    }
}

// Andor Struct
#[derive(Debug)]
pub enum Andor {
    And, // &&
    Or,  // ||
}

impl Andor {
    pub fn apply(&self, left: AnyExpr, right: AnyExpr) -> AnyExpr {
        match self {
            Andor::And => AnyExpr::Logical(Box::new(left), LogicalOp::And, Box::new(right)),
            Andor::Or => AnyExpr::Logical(Box::new(left), LogicalOp::Or, Box::new(right)),
        }
    }
}

impl fmt::Display for Andor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Andor::And => write!(f, "&&"),
            Andor::Or => write!(f, "||"),
        }
    }
}

// AnyExpr Struct
#[derive(Debug)]
pub enum AnyExpr {
    Default,                                                    // Default variant
    Paren(Box<AnyExpr>),                                        // Parenthesized expression: (expr)
    Unary(Unarop, Box<AnyExpr>),                                // Unary expression: -expr, !expr, ~expr    
    Logical(Box<AnyExpr>, LogicalOp, Box<AnyExpr>),             // Logical expression: logical operations (OR, AND, etc.)
    Ternary(Box<AnyExpr>, Box<AnyExpr>, Box<AnyExpr>),          // Ternary: (cond ? expr1 : expr2)
    Len(Box<Varref>),                                           // Length of a variable reference: LEN(varref)
    Poll(Box<Poll>),                                            // Polling mechanism: Poll(expr)
    Varref(Box<Varref>),                                        // Variable reference
    Const(Box<Const>),                                          // Constant value
    Timeout,                                                    // TIMEOUT constant
    Np,                                                         // NP constant
    Enabled(Box<AnyExpr>),                                      // ENABLED(expression)
    PcValue(Box<AnyExpr>),                                      // PC_VALUE(expression)
    ArrayAccess(Name, Box<AnyExpr>, Name),                      // Array access with index: Name[expr] @ Name
    Run(Name, Option<ArgList>, Option<Priority>),               // Run a process with optional args and priority
    GetPriority(Box<AnyExpr>),                                  // Get priority of an expression
    SetPriority(Box<AnyExpr>, Box<AnyExpr>),                    // Set priority: set_priority(expr1, expr2)
    Chanpoll(Box<Chanpoll>, Box<AnyExpr>),                      // Chanpoll operation: Chanpoll(expr, varref)
    Andor(Box<AnyExpr>, Box<AnyExpr>),                          // Logical AND/OR
    Equal(Box<AnyExpr>, Box<AnyExpr>),                          // Equality: ==
    NotEqual(Box<AnyExpr>, Box<AnyExpr>),                       // Inequality: !=
    GreaterThan(Box<AnyExpr>, Box<AnyExpr>),                    // Greater than: >
    LessThan(Box<AnyExpr>, Box<AnyExpr>),                       // Less than: <
    GreaterEqual(Box<AnyExpr>, Box<AnyExpr>),                   // Greater or equal: >=
    LessEqual(Box<AnyExpr>, Box<AnyExpr>),                      // Less or equal: <=
    Add(Box<AnyExpr>, Box<AnyExpr>),                            // Addition: +
    Subtract(Box<AnyExpr>, Box<AnyExpr>),                       // Subtraction: -
    Multiply(Box<AnyExpr>, Box<AnyExpr>),                       // Multiplication: *
    Divide(Box<AnyExpr>, Box<AnyExpr>),                         // Division: /
    Modulo(Box<AnyExpr>, Box<AnyExpr>),                         // Modulo: %
    Negate(Box<AnyExpr>),                                       // Unary negation: -
}

impl AnyExpr {

    pub fn new_default() -> Self {
        AnyExpr::Default
    }

    pub fn new_paren(expr: AnyExpr) -> Self {
        AnyExpr::Paren(Box::new(expr))
    }
    
    pub fn new_const(value: Const) -> Self {
    // debug!("Converting Const to AnyExpr: {:?}", value); 
    match value {
        Const::True => AnyExpr::Const(Box::new(Const::new_true())),
        Const::False => AnyExpr::Const(Box::new(Const::new_false())),
        Const::Skip => AnyExpr::Const(Box::new(Const::new_skip())),
        Const::Number(val) => {
            // debug!("Const is a number: {}", val);
            AnyExpr::Const(Box::new(Const::new_number(val)))
        }
    }
    }

    pub fn new_varref(varref: Varref) -> Self {
        AnyExpr::Varref(Box::new(varref))
    }

    pub fn new_len(varref: Varref) -> Self {
        AnyExpr::Len(Box::new(varref))
    }

    pub fn new_poll(poll : Poll) -> Self {
        AnyExpr::Poll(Box::new(poll))
    }

    pub fn new_unary(op: Unarop, expr: AnyExpr) -> Self {
        AnyExpr::Unary(op, Box::new(expr))
    }

    pub fn new_logical(left: AnyExpr, op: LogicalOp, right: AnyExpr) -> Self {
        AnyExpr::Logical(Box::new(left), op, Box::new(right))
    }

    pub fn new_ternary(condition: AnyExpr, true_expr: AnyExpr, false_expr: AnyExpr) -> Self {
        AnyExpr::Ternary(Box::new(condition), Box::new(true_expr), Box::new(false_expr))
    }

    pub fn new_array_access(name: Name, index: AnyExpr, field: Name) -> Self {
        AnyExpr::ArrayAccess(name, Box::new(index), field)
    }

    pub fn new_run(name: Name, args: Option<ArgList>, priority: Option<Priority>) -> Self {
        AnyExpr::Run(name, args, priority)
    }

    pub fn new_get_priority(expr: AnyExpr) -> Self {
        AnyExpr::GetPriority(Box::new(expr))
    }

    pub fn new_set_priority(expr1: AnyExpr, expr2: AnyExpr) -> Self {
        AnyExpr::SetPriority(Box::new(expr1), Box::new(expr2))
    }

    pub fn new_chanpoll(chanpoll: Chanpoll, expr: AnyExpr) -> Self {
        AnyExpr::Chanpoll(Box::new(chanpoll), Box::new(expr))
    }

    pub fn new_enabled(expr: AnyExpr) -> Self {
        AnyExpr::Enabled(Box::new(expr))
    }

    pub fn new_pc_value(expr: AnyExpr) -> Self {
        AnyExpr::PcValue(Box::new(expr))
    }

    pub fn new_andor(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::Andor(Box::new(left), Box::new(right))
    }

    pub fn new_equal(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::Equal(Box::new(left), Box::new(right))
    }

    pub fn new_not_equal(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::NotEqual(Box::new(left), Box::new(right))
    }

    pub fn new_greater_than(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::GreaterThan(Box::new(left), Box::new(right))
    }

    pub fn new_less_than(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::LessThan(Box::new(left), Box::new(right))
    }

    pub fn new_greater_equal(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::GreaterEqual(Box::new(left), Box::new(right))
    }

    pub fn new_less_equal(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::LessEqual(Box::new(left), Box::new(right))
    }

    pub fn new_add(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::Add(Box::new(left), Box::new(right))
    }

    pub fn new_subtract(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::Subtract(Box::new(left), Box::new(right))
    }

    pub fn new_multiply(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::Multiply(Box::new(left), Box::new(right))
    }

    pub fn new_divide(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::Divide(Box::new(left), Box::new(right))
    }

    pub fn new_modulo(left: AnyExpr, right: AnyExpr) -> Self {
        AnyExpr::Modulo(Box::new(left), Box::new(right))
    }

    pub fn new_negate(expr: AnyExpr) -> Self {
        AnyExpr::Negate(Box::new(expr))
    }
}

impl fmt::Display for AnyExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AnyExpr::Default => write!(f, "Default"),
            AnyExpr::Paren(expr) => write!(f, "( {} )", expr),
            AnyExpr::Unary(op, expr) => write!(f, "{}({})", op, expr),
            AnyExpr::Logical(left, op, right) => write!(f, "({} {} {})", left, op, right),
            AnyExpr::Ternary(cond, true_expr, false_expr) => {
                write!(f, "({} ? {} : {})", cond, true_expr, false_expr)
            }
            AnyExpr::Len(varref) => write!(f, "LEN({})", varref),
            AnyExpr::Poll(poll) => write!(f, "Poll({})", poll),
            AnyExpr::Varref(varref) => write!(f, " Varref_struct [ \n {} ] ", varref),
            AnyExpr::Const(const_val) => write!(f, "Const({})", const_val),
            AnyExpr::Timeout => write!(f, "TIMEOUT"),
            AnyExpr::Np => write!(f, "NP"),
            AnyExpr::Enabled(expr) => write!(f, "ENABLED({})", expr),
            AnyExpr::PcValue(expr) => write!(f, "PC_VALUE({})", expr),
            AnyExpr::ArrayAccess(name, index, field) => {
                write!(f, "{}[{}] @ {}", name, index, field)
            }
            AnyExpr::Run(name, args, priority) => {
            write!(
                f,
                "Run(\n  Name: {},\n  Args: {},\n  Priority: {:?}\n)",
                name,
                args.as_ref().map_or("None".to_string(), |a| a.to_string()),
                priority.as_ref().map_or("None".to_string(), |p| p.to_string())
            )}
            AnyExpr::GetPriority(expr) => write!(f, "GetPriority({})", expr),
            AnyExpr::SetPriority(expr1, expr2) => write!(f, "SetPriority({}, {})", expr1, expr2),
            AnyExpr::Chanpoll(chanpoll, expr) => write!(f, "Chanpoll({}, {})", chanpoll, expr),

            // Varianti provenienti da Expr
            AnyExpr::Andor(left, right) => write!(f, "({} && {})", left, right),
            AnyExpr::Equal(left, right) => write!(f, "({} == {})", left, right),
            AnyExpr::NotEqual(left, right) => write!(f, "({} != {})", left, right),
            AnyExpr::GreaterThan(left, right) => write!(f, "({} > {})", left, right),
            AnyExpr::LessThan(left, right) => write!(f, "({} < {})", left, right),
            AnyExpr::GreaterEqual(left, right) => write!(f, "({} >= {})", left, right),
            AnyExpr::LessEqual(left, right) => write!(f, "({} <= {})", left, right),
            AnyExpr::Add(left, right) => write!(f, "({} + {})", left, right),
            AnyExpr::Subtract(left, right) => write!(f, "({} - {})", left, right),
            AnyExpr::Multiply(left, right) => write!(f, "({} * {})", left, right),
            AnyExpr::Divide(left, right) => write!(f, "({} / {})", left, right),
            AnyExpr::Modulo(left, right) => write!(f, "({} % {})", left, right),
            AnyExpr::Negate(expr) => write!(f, "-{}", expr),
        }
    }
}

// LogicalOp Struct
#[derive(Debug)]
pub enum LogicalOp {
    And, // &&
    Or,  // ||
}

impl fmt::Display for LogicalOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogicalOp::And => write!(f, "&&"),
            LogicalOp::Or => write!(f, "||"),
        }
    }
}

// Enabler Struct 
#[derive(Debug)]
pub struct Enabler {
    pub condition: AnyExpr, 
}

impl Enabler {
    pub fn new(condition: AnyExpr) -> Self {
        Enabler { condition }
    }
}

impl fmt::Display for Enabler {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Enabler({})", self.condition)
    }
}

// Range Struct
#[derive(Debug)]
pub enum Range {
    FromTo(Name, AnyExpr, AnyExpr),  
    In(Name, Name),                 
}

impl Range {
    pub fn from_to(name: Name, expr1: AnyExpr, expr2: AnyExpr) -> Self {
        Range::FromTo(name, expr1, expr2)
    }

    pub fn in_range(name1: Name, name2: Name) -> Self {
        Range::In(name1, name2)
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Range::FromTo(name, expr1, expr2) => write!(f, "{}[{} .. {}]", name, expr1, expr2),
            Range::In(name1, name2) => write!(f, "{} in {}", name1, name2),
        }
    }
}

// Receive Struct
#[derive(Debug)]
pub struct Receive {
    pub varref: Varref,       
    pub recv_args: RecvArgs, 
    pub random: bool,       
    pub side_effect: bool,   
    pub ditto: bool,
}

impl Receive {
    pub fn new(varref: Varref, recv_args: RecvArgs, random: bool, side_effect: bool, ditto: bool) -> Self {
        Receive {
            varref,
            recv_args,
            random,
            side_effect,
            ditto,
        }
    }
}

impl fmt::Display for Receive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Receive({}: {:?}, Random: {}, SideEffect: {}, Ditto: {})",
            self.varref, self.recv_args, self.random, self.side_effect, self.ditto
        )
    }
}

// Poll Struct
#[derive(Debug)]
pub struct Poll {
    pub varref: Varref,      
    pub recv_args: RecvArgs,  
    pub random: bool,      
    pub side_effect: bool, 
    pub ditto: bool,         
}

impl Poll {
    pub fn new(varref: Varref, recv_args: RecvArgs, random: bool, side_effect: bool, ditto: bool) -> Self {
        Poll {
            varref,
            recv_args,
            random,
            side_effect,
            ditto,
        }
    }
}

impl fmt::Display for Poll {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Poll({}: {:?}, Random: {}, SideEffect: {}, Ditto: {})",
            self.varref, self.recv_args, self.random, self.side_effect, self.ditto
        )
    }
}

// Options Struct
#[derive(Debug)]
pub struct Options {
    pub sequences: Vec<Sequence>,
}

impl Options {
    pub fn new(sequences: Vec<Sequence>) -> Self {
        Options { sequences }
    }
}

impl fmt::Display for Options {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Options:")?; 
        for (i, sequence) in self.sequences.iter().enumerate() {
            writeln!(f, "  Option {}:", i + 1)?; 
            writeln!(f, "{}", sequence)?; 
        }
        Ok(())
    }
}

// UnlessClause Struct
#[derive(Debug)]
pub struct UnlessClause {
    pub statement: Option<Box<Stmnt>>,  
}

impl UnlessClause {
    pub fn none() -> Self {
        UnlessClause { statement: None }
    }

    pub fn with_statement(stmnt: Stmnt) -> Self {
        UnlessClause {
            statement: Some(Box::new(stmnt)),
        }
    }
}

impl fmt::Display for UnlessClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.statement {
            Some(stmnt) => write!(f, "UnlessClause:\n  Statement: {}\n", stmnt),
            None => write!(f, " "),
        }
    }
}

// Enumeration for different types of statements
// Stmnt Struct
#[derive(Debug)]
pub enum Stmnt {
    If(Box<Options>),
    Do(Box<Options>),
    For(Box<Range>, Box<Sequence>),
    Atomic(Box<Sequence>),
    DStep(Box<Sequence>),
    Select(Box<Range>),
    Block(Box<Sequence>),
    SendMsg(Box<SendMsg>),
    Receive(Box<Receive>),
    Assign(Box<Assign>),
    Else,
    Break,
    Goto(Name),
    Labeled(Name, Box<Stmnt>),
    Assert(Box<AnyExpr>),
    Expr(Box<AnyExpr>),
    CCode(String),
    CExpr(String),
    CDecl(String),
    CTrack(String),
    CState(String)
}

impl Stmnt {
    // Costruttori per ogni variante

    pub fn new_if(options: Options) -> Self {
        Stmnt::If(Box::new(options))
    }

    pub fn new_do(options: Options) -> Self {
        Stmnt::Do(Box::new(options))
    }

    pub fn new_for(range: Range, sequence: Sequence) -> Self {
        Stmnt::For(Box::new(range), Box::new(sequence))
    }

    pub fn new_atomic(sequence: Sequence) -> Self {
        Stmnt::Atomic(Box::new(sequence))
    }

    pub fn new_dstep(sequence: Sequence) -> Self {
        Stmnt::DStep(Box::new(sequence))
    }

    pub fn new_select(range: Range) -> Self {
        Stmnt::Select(Box::new(range))
    }

    pub fn new_block(sequence: Sequence) -> Self {
        Stmnt::Block(Box::new(sequence))
    }

    pub fn new_send_msg(send_msg: SendMsg) -> Self {
        Stmnt::SendMsg(Box::new(send_msg))
    }

    pub fn new_receive(receive: Receive) -> Self {
        Stmnt::Receive(Box::new(receive))
    }

    pub fn new_assign(assign: Assign) -> Self {
        Stmnt::Assign(Box::new(assign))
    }

    pub fn new_else() -> Self {
        Stmnt::Else
    }

    pub fn new_break() -> Self {
        Stmnt::Break
    }

    pub fn new_goto(name: Name) -> Self {
        Stmnt::Goto(name)
    }

    pub fn new_labeled(name: Name, stmnt: Stmnt) -> Self {
        Stmnt::Labeled(name, Box::new(stmnt))
    }

    pub fn new_assert(expr: AnyExpr) -> Self {
        Stmnt::Assert(Box::new(expr))
    }

    pub fn new_expr(expr: AnyExpr) -> Self {
        Stmnt::Expr(Box::new(expr))
    }

    pub fn new_c_code(code: String) -> Self {
        Stmnt::CCode(code)
    }

    pub fn new_c_expr(expr: String) -> Self {
        Stmnt::CExpr(expr)
    }

    pub fn new_c_decl(decl: String) -> Self {
        Stmnt::CDecl(decl)
    }

    pub fn new_c_track(track: String) -> Self {
        Stmnt::CTrack(track)
    }

    pub fn new_c_state(state: String) -> Self {
        Stmnt::CState(state)
    }
}


impl fmt::Display for Stmnt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmnt::If(options) => {
                writeln!(f, "If Statement:")?; // Usa writeln per maggiore leggibilità
                writeln!(f, "{}", options)?;  // Chiama il Display di Options
                writeln!(f, "End_If")
            }
            Stmnt::Do(options) => {
                            writeln!(f, "Do Statement:")?;
                            writeln!(f, "{}", options)?; // Usa il `Display` di Options
                            writeln!(f, "End_Do")
                        }            
            Stmnt::For(range, sequence) => {
                writeln!(f, "For Loop: Range {:?}, Sequence:", range)?;
                writeln!(f, "{}", sequence)?;
                writeln!(f, "End_For")
            }
            Stmnt::Atomic(sequence) => {
                writeln!(f, "Atomic Block:")?;
                writeln!(f, "{}", sequence)
            }
            Stmnt::DStep(sequence) => {
                writeln!(f, "DStep Block:")?;
                writeln!(f, "{}", sequence)
            }
            Stmnt::Select(range) => {
                writeln!(f, "Select with Range:")?;
                writeln!(f, "  {:?}", range)
            }
            Stmnt::Block(sequence) => {
                writeln!(f, "Block:")?;
                writeln!(f, "{}", sequence)
            }
            Stmnt::SendMsg(msg) => {
                writeln!(f, "Send Message:")?;
                writeln!(f, "  {}", msg)
            }
            Stmnt::Receive(receive) => {
                writeln!(f, "Receive:")?;
                writeln!(f, "  {}", receive)
            }
            Stmnt::Assign(assign) => {
                writeln!(f, "Assignment:")?;
                writeln!(f, " {}", assign)
            }
            Stmnt::Else => write!(f, "Else"),
            Stmnt::Break => write!(f, "Break"),
            Stmnt::Goto(name) => write!(f, "Goto {}", name),
            Stmnt::Labeled(name, stmnt) => {
                writeln!(f, "Label: {}", name)?;
                writeln!(f, "{}", stmnt)
            }
            Stmnt::Expr(expr) => {
                writeln!(f, "Expression:")?;
                writeln!(f, "  {}", expr)
            }
            Stmnt::CCode(code) => {
                writeln!(f, "C Code:")?;
                writeln!(f, "  {}", code)
            }
            Stmnt::CExpr(expr) => {
                writeln!(f, "C Expression:")?;
                writeln!(f, "  {}", expr)
            }
            Stmnt::CDecl(decl) => {
                writeln!(f, "C Declaration:")?;
                writeln!(f, "  {}", decl)
            }
            Stmnt::CTrack(track) => {
                writeln!(f, "C Track:")?;
                writeln!(f, "  {}", track)
            }
            Stmnt::CState(state) => {
                writeln!(f, "C State:")?;
                writeln!(f, "  {}", state)
            }
            other => write!(f, "Unhandled Stmnt variant: {:?}", other),
        }
    }
}

