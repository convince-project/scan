//! Model builder for SCAN's JANI specification format.

// TODO list:
// * add support (if later needed) to the following features:
//      * Model -> "restrict-initial"
//      * BoundedType
//      * Type -> "clock" and "continuous"
//      * VariableDeclaration -> "transient"
//      * Automaton -> "restrict-initial"
//      * Automaton -> "location" -> "transient-values" and "time-progress"
//      * Automaton -> "edges" -> "rate"
//      * Automaton -> "edges" -> "destinations" -> "probability"
//      * Automaton -> "edges" -> "destinations" -> "assignment" -> "index"
//      * Expression -> if-then-else, modulus, division, power, logarithm, floor and ceil operations

use crate::jani_parser::ASTNode;
use ordered_float::OrderedFloat;
use rand::rngs::SmallRng;
use rand::Rng;
use scan_core::channel_system::*;
use scan_core::{Expression, Type, Val};
use std::collections::{BTreeMap, HashMap};
use thiserror::Error;

/// Error type for building errors.
#[derive(Error, Clone, Debug)]
pub enum BuildingError {
    #[error("{0}")]
    CsError(#[from] CsError),
    #[error("Location {0} not found")]
    LocationNotFound(String),
    #[error("Variable {0} not found")]
    VariableNotFound(String),
    #[error("{0} name {1} already taken")]
    NameAlreadyTaken(String, String),
    #[error("Cannot update constant {0}")]
    UpdateConstant(String),
    #[error("Costant {0} has no value, model parameters not supported")]
    NoValueConstant(String),
    #[error("{0} not supported yet")]
    FeatureNotSupported(String),
    #[error("Unknown error")]
    UnknownError,
}

/// Model builder for JANI specification format.
pub struct ModelBuilder;
impl ModelBuilder {
    /// Build the [`ChannelSystem`] corresponding to a JANI model.
    ///
    /// # Arguments
    ///
    /// * `ast` - The root [`ASTNode`] of the abstract syntax tree representing the JANI model.
    ///
    /// # Returns
    ///
    /// * Ok(`ChannelSystem`): The [`ChannelSystem`] corresponding to the JANI model, if building is successful.
    /// * Err(`BuildingError`): A [`BuildingError`] indicating the type of error that occurred during building, if building fails.
    pub fn build(ast: ASTNode) -> Result<ChannelSystem<SmallRng>, BuildingError> {
        let mut builder = ChannelSystemBuilder::new();
        Self::visit_model(ast, &mut builder)?;
        Ok(builder.build())
    }

    fn visit_model(
        ast: ASTNode,
        builder: &mut ChannelSystemBuilder<SmallRng>,
    ) -> Result<(), BuildingError> {
        let mut constants_node: Vec<ASTNode> = Vec::new();
        let mut variables_node: Vec<ASTNode> = Vec::new();
        let mut automata_node: Vec<ASTNode> = Vec::new();
        match ast {
            ASTNode::ASTModel { properties } => {
                for node in properties {
                    match node {
                        ASTNode::ASTModelConstants { constants } => {
                            constants_node = constants;
                        }
                        ASTNode::ASTModelVariables { variables } => {
                            variables_node = variables;
                        }
                        ASTNode::ASTModelAutomata { automata } => {
                            automata_node = automata;
                        }
                        ASTNode::ASTModelRestrictInitial { properties: _ } => {
                            return Err(BuildingError::FeatureNotSupported(
                                "\"restrict-initial\"".to_string(),
                            ));
                        }
                        _ => continue,
                    }
                }
            }
            _ => {
                return Err(BuildingError::UnknownError);
            }
        }

        // Maps the global constants names to their value
        let mut constants_hm: HashMap<String, (CsExpression, Type)> = HashMap::new();
        // Populate the constants_hm
        for c in constants_node {
            Self::visit_constant(c, &mut constants_hm)?;
        }

        // Create a program graph to hold the global variables
        let vars_pg = builder.new_program_graph();
        // Maps the global variables names to their Var in the vars_pg, initial value and type. BTreeMap because order matters
        let mut variables_hm: BTreeMap<String, (Var, CsExpression, Type)> = BTreeMap::new();
        // Populate the variables_hm
        for v in variables_node {
            Self::visit_variable(v, builder, vars_pg, &constants_hm, &mut variables_hm)?;
        }

        // Create the program graphs
        for a in automata_node {
            Self::visit_automaton(a, builder, vars_pg, &constants_hm, &variables_hm)?;
        }

        Ok(())
    }

    fn visit_constant(
        ast: ASTNode,
        constants_hm: &mut HashMap<String, (CsExpression, Type)>,
    ) -> Result<(), BuildingError> {
        let mut constant_name = String::new();
        let mut constant_value: Option<CsExpression> = None;
        let mut constant_type = String::new();

        match ast {
            ASTNode::ASTConstantDeclaration { properties } => {
                for p in properties {
                    match p {
                        ASTNode::ASTConstantDeclarationName { name } => match *name {
                            ASTNode::ASTIdentifier { identifier } => {
                                constant_name = identifier.trim_matches('\"').to_string();
                            }
                            _ => {
                                return Err(BuildingError::UnknownError);
                            }
                        },
                        ASTNode::ASTConstantDeclarationValue { value } => {
                            constant_value = Self::visit_expression(
                                &value,
                                constants_hm,
                                None,
                                None,
                                &mut None,
                            )?;
                        }
                        ASTNode::ASTConstantDeclarationType { type_ } => match *type_ {
                            ASTNode::ASTBasicType { type_ } => {
                                constant_type = type_.trim_matches('\"').to_string();
                            }
                            ASTNode::ASTBoundedType { properties: _ } => {
                                return Err(BuildingError::FeatureNotSupported(
                                    "BoundedType".to_string(),
                                ));
                            }
                            _ => return Err(BuildingError::UnknownError),
                        },
                        _ => continue,
                    }
                }
            }
            ASTNode::ASTEmpty => {
                return Ok(());
            }
            _ => {
                return Err(BuildingError::UnknownError);
            }
        }

        if constant_value.is_none() {
            return Err(BuildingError::NoValueConstant(constant_name));
        }

        let type_ = match constant_type.as_str() {
            "int" => Type::Integer,
            "bool" => Type::Boolean,
            "real" => Type::Float,
            _ => return Err(BuildingError::UnknownError),
        };

        match constants_hm.insert(
            constant_name.clone(),
            (constant_value.clone().unwrap(), type_),
        ) {
            None => {}
            Some(_) => {
                return Err(BuildingError::NameAlreadyTaken(
                    "Constant".to_string(),
                    constant_name,
                ));
            }
        }

        Ok(())
    }

    fn visit_variable(
        ast: ASTNode,
        builder: &mut ChannelSystemBuilder<SmallRng>,
        vars_pg: PgId,
        constants_hm: &HashMap<String, (CsExpression, Type)>,
        variables_hm: &mut BTreeMap<String, (Var, CsExpression, Type)>,
    ) -> Result<(), BuildingError> {
        let mut variable_name = String::new();
        let mut variable_initial_value: Option<CsExpression> = None;
        let mut variable_type = String::new();

        match ast {
            ASTNode::ASTVariableDeclaration { properties } => {
                for p in properties {
                    match p {
                        ASTNode::ASTVariableDeclarationName { name } => match *name {
                            ASTNode::ASTIdentifier { identifier } => {
                                variable_name = identifier.trim_matches('\"').to_string();
                                if constants_hm.contains_key(&variable_name) {
                                    return Err(BuildingError::NameAlreadyTaken(
                                        "Global variable".to_string(),
                                        variable_name,
                                    ));
                                }
                            }
                            _ => {
                                return Err(BuildingError::UnknownError);
                            }
                        },
                        ASTNode::ASTVariableDeclarationType { type_ } => match *type_ {
                            ASTNode::ASTBasicType { type_ } => {
                                variable_type = type_.trim_matches('\"').to_string();
                            }
                            _ => {
                                return Err(BuildingError::FeatureNotSupported(
                                    "BoundedType, \"clock\" and \"continuous\"".to_string(),
                                ));
                            }
                        },
                        ASTNode::ASTVariableDeclarationInitialValue { initial_value } => {
                            variable_initial_value = Self::visit_expression(
                                &initial_value,
                                constants_hm,
                                Some(variables_hm),
                                None,
                                &mut None,
                            )?;
                        }
                        ASTNode::ASTVariableDeclarationTransient { transient } => {
                            if transient {
                                return Err(BuildingError::FeatureNotSupported(
                                    "\"transient\"".to_string(),
                                ));
                            }
                        }
                        _ => continue,
                    }
                }
            }
            ASTNode::ASTEmpty => {
                return Ok(());
            }
            _ => {
                return Err(BuildingError::UnknownError);
            }
        }

        if variable_initial_value.is_none() {
            match variable_type.as_str() {
                "int" => {
                    variable_initial_value = Some(Expression::Const(Val::Integer(0_i32)));
                }
                "bool" => {
                    variable_initial_value = Some(Expression::Const(Val::Boolean(false)));
                }
                "real" => {
                    variable_initial_value =
                        Some(Expression::Const(Val::Float(OrderedFloat(0.0_f64))));
                }
                _ => {
                    return Err(BuildingError::UnknownError);
                }
            }
        }

        let variable_initial_value = variable_initial_value.unwrap();

        let type_ = match variable_type.as_str() {
            "int" => Type::Integer,
            "bool" => Type::Boolean,
            "real" => Type::Float,
            _ => return Err(BuildingError::UnknownError),
        };

        let var = builder.new_var(vars_pg, variable_initial_value.clone())?;

        match variables_hm.insert(variable_name.clone(), (var, variable_initial_value, type_)) {
            None => {}
            Some(_) => {
                return Err(BuildingError::NameAlreadyTaken(
                    "Global variable".to_string(),
                    variable_name,
                ));
            }
        }

        Ok(())
    }

    fn visit_automaton(
        ast: ASTNode,
        builder: &mut ChannelSystemBuilder<SmallRng>,
        vars_pg: PgId,
        constants_hm: &HashMap<String, (CsExpression, Type)>,
        variables_hm: &BTreeMap<String, (Var, CsExpression, Type)>,
    ) -> Result<(), BuildingError> {
        let mut initial_locations_node: Vec<ASTNode> = Vec::new();
        let mut variables_node: Vec<ASTNode> = Vec::new();
        let mut locations_node: Vec<ASTNode> = Vec::new();
        let mut edges_node: Vec<ASTNode> = Vec::new();
        match ast {
            ASTNode::ASTAutomaton { properties } => {
                for node in properties {
                    match node {
                        ASTNode::ASTAutomatonInitialLocations { initial_locations } => {
                            initial_locations_node = initial_locations;
                        }
                        ASTNode::ASTAutomatonVariables { variables } => {
                            variables_node = variables;
                        }
                        ASTNode::ASTAutomatonLocations { locations } => {
                            locations_node = locations;
                        }
                        ASTNode::ASTAutomatonEdges { edges } => {
                            edges_node = edges;
                        }
                        ASTNode::ASTAutomatonRestrictInitial { properties: _ } => {
                            return Err(BuildingError::FeatureNotSupported(
                                "\"restrict-initial\"".to_string(),
                            ));
                        }
                        _ => continue,
                    }
                }
            }
            _ => {
                return Err(BuildingError::UnknownError);
            }
        }

        let pg = builder.new_program_graph();
        // Map the locations and variable names to their respective Location and Var
        let mut locations_hm: HashMap<String, Location> = HashMap::new();
        let mut local_variables_hm: HashMap<String, (Var, Type)> = HashMap::new();

        // Add global constants to the pg
        for (k, v) in constants_hm.iter() {
            match builder.new_var(pg, v.0.clone()) {
                Ok(var) => {
                    local_variables_hm.insert(k.clone(), (var, v.1.clone()));
                }
                Err(e) => {
                    return Err(BuildingError::CsError(e));
                }
            }
        }

        // Add global variables to the pg
        for (k, v) in variables_hm.iter() {
            match builder.new_var(pg, v.1.clone()) {
                Ok(var) => {
                    local_variables_hm.insert(k.clone(), (var, v.2.clone()));
                }
                Err(e) => {
                    return Err(BuildingError::CsError(e));
                }
            }
        }

        // Add locations to the pg
        Self::add_locations(
            initial_locations_node,
            locations_node,
            builder,
            pg,
            &mut locations_hm,
        )?;

        // Add local variables to the pg
        Self::add_variables(
            variables_node,
            builder,
            pg,
            constants_hm,
            variables_hm,
            &mut local_variables_hm,
        )?;

        // Add transitions to the pg
        Self::add_transitions(
            edges_node,
            builder,
            pg,
            vars_pg,
            constants_hm,
            variables_hm,
            &local_variables_hm,
            &locations_hm,
        )?;

        Ok(())
    }

    fn add_locations(
        initial_locations_node: Vec<ASTNode>,
        locations_node: Vec<ASTNode>,
        builder: &mut ChannelSystemBuilder<SmallRng>,
        pg: PgId,
        locations_hm: &mut HashMap<String, Location>,
    ) -> Result<(), BuildingError> {
        // Choose a random initial location if there are multiple
        let rng = rand::rng().random_range(0..initial_locations_node.len());
        let initial_location: String = match &initial_locations_node[rng] {
            ASTNode::ASTIdentifier { identifier } => identifier.trim_matches('\"').to_string(),
            _ => {
                return Err(BuildingError::UnknownError);
            }
        };
        for l in locations_node {
            match l {
                ASTNode::ASTAutomatonLocation { properties } => {
                    for p in properties {
                        match p {
                            ASTNode::ASTAutomatonLocationName { name } => match *name {
                                ASTNode::ASTIdentifier { identifier } => {
                                    let identifier = identifier.trim_matches('\"').to_string();
                                    if identifier == initial_location {
                                        let l = builder.initial_location(pg)?;
                                        match locations_hm.insert(initial_location.clone(), l) {
                                            None => {}
                                            Some(_) => {
                                                return Err(BuildingError::NameAlreadyTaken(
                                                    "Location".to_string(),
                                                    initial_location,
                                                ));
                                            }
                                        }
                                    } else {
                                        let l = builder.new_location(pg)?;
                                        match locations_hm.insert(identifier.clone(), l) {
                                            None => {}
                                            Some(_) => {
                                                return Err(BuildingError::NameAlreadyTaken(
                                                    "Location".to_string(),
                                                    identifier,
                                                ));
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    return Err(BuildingError::UnknownError);
                                }
                            },
                            ASTNode::ASTAutomatonLocationTimeProgress { properties: _ } => {
                                return Err(BuildingError::FeatureNotSupported(
                                    "\"time-progress\"".to_string(),
                                ));
                            }
                            ASTNode::ASTAutomatonLocationTransientValues { transient_values } => {
                                for v in transient_values {
                                    match v {
                                        ASTNode::ASTEmpty => {}
                                        _ => {
                                            return Err(BuildingError::FeatureNotSupported(
                                                "\"transient-values\"".to_string(),
                                            ));
                                        }
                                    }
                                }
                            }
                            _ => continue,
                        }
                    }
                }
                _ => {
                    return Err(BuildingError::UnknownError);
                }
            }
        }

        Ok(())
    }

    fn add_variables(
        variables_node: Vec<ASTNode>,
        builder: &mut ChannelSystemBuilder<SmallRng>,
        pg: PgId,
        constants_hm: &HashMap<String, (CsExpression, Type)>,
        variables_hm: &BTreeMap<String, (Var, CsExpression, Type)>,
        local_variables_hm: &mut HashMap<String, (Var, Type)>,
    ) -> Result<(), BuildingError> {
        for v in variables_node {
            match v {
                ASTNode::ASTVariableDeclaration { properties } => {
                    let mut variable_name = String::new();
                    let mut variable_initial_value: Option<CsExpression> = None;
                    let mut variable_type = String::new();

                    for p in properties {
                        match p {
                            ASTNode::ASTVariableDeclarationName { name } => match *name {
                                ASTNode::ASTIdentifier { identifier } => {
                                    variable_name = identifier.trim_matches('\"').to_string();
                                    if constants_hm.contains_key(&variable_name)
                                        || variables_hm.contains_key(&variable_name)
                                        || local_variables_hm.contains_key(&variable_name)
                                    {
                                        return Err(BuildingError::NameAlreadyTaken(
                                            "Variable".to_string(),
                                            variable_name,
                                        ));
                                    }
                                }
                                _ => {
                                    return Err(BuildingError::UnknownError);
                                }
                            },
                            ASTNode::ASTVariableDeclarationType { type_ } => match *type_ {
                                ASTNode::ASTBasicType { type_ } => {
                                    variable_type = type_.trim_matches('\"').to_string();
                                }
                                _ => {
                                    return Err(BuildingError::FeatureNotSupported(
                                        "BoundedType, \"clock\" and \"continuous\"".to_string(),
                                    ));
                                }
                            },
                            ASTNode::ASTVariableDeclarationInitialValue { initial_value } => {
                                variable_initial_value = Self::visit_expression(
                                    &initial_value,
                                    constants_hm,
                                    Some(variables_hm),
                                    Some(local_variables_hm),
                                    &mut None,
                                )?;
                            }
                            ASTNode::ASTVariableDeclarationTransient { transient } => {
                                if transient {
                                    return Err(BuildingError::FeatureNotSupported(
                                        "\"transient\"".to_string(),
                                    ));
                                }
                            }
                            _ => continue,
                        }
                    }

                    if variable_initial_value.is_none() {
                        match variable_type.as_str() {
                            "int" => {
                                variable_initial_value =
                                    Some(Expression::Const(Val::Integer(0_i32)));
                            }
                            "bool" => {
                                variable_initial_value =
                                    Some(Expression::Const(Val::Boolean(false)));
                            }
                            "real" => {
                                variable_initial_value =
                                    Some(Expression::Const(Val::Float(OrderedFloat(0.0_f64))));
                            }
                            _ => {
                                return Err(BuildingError::UnknownError);
                            }
                        }
                    }

                    let type_ = match variable_type.as_str() {
                        "int" => Type::Integer,
                        "bool" => Type::Boolean,
                        "real" => Type::Float,
                        _ => return Err(BuildingError::UnknownError),
                    };

                    match builder.new_var(pg, variable_initial_value.unwrap()) {
                        Ok(v) => {
                            local_variables_hm.insert(variable_name, (v, type_));
                        }
                        Err(e) => {
                            return Err(BuildingError::CsError(e));
                        }
                    }
                }
                ASTNode::ASTEmpty => {}
                _ => {
                    return Err(BuildingError::UnknownError);
                }
            }
        }

        Ok(())
    }

    fn add_transitions(
        edges_node: Vec<ASTNode>,
        builder: &mut ChannelSystemBuilder<SmallRng>,
        pg: PgId,
        vars_pg: PgId,
        constants_hm: &HashMap<String, (CsExpression, Type)>,
        variables_hm: &BTreeMap<String, (Var, CsExpression, Type)>,
        local_variables_hm: &HashMap<String, (Var, Type)>,
        locations_hm: &HashMap<String, Location>,
    ) -> Result<(), BuildingError> {
        // PG:
        // * To start a transition, pg sends a message to vars_pg through ch_init, moving to init_loc.
        // * When the message is received (i.e. vars_pg is not aiding another transition), pg moves to req_{var0}, then to read_{var0}, ..., req_{varn}, read_{varn},
        //   where {var0}, ..., {varn} are the global variables involved in the guard of the transition. If no global variables is involved, pg moves directly to act_loc.
        //   In req_{var} pg sends the id of {var} through ch_req, and in read_{var} pg receives the value of the variable through the channel read_{var}.
        // * After all global variables are read, pg moves to act_loc, where the transition action is executed if the guard is true, otherwise pg moves to not_ok_loc.
        // * If the transition action is executed, pg moves to write_{var0}, ..., write_{varn}, wait_{var0}, ..., wait_{varn},
        //   where {var0}, ..., {varn} are the global variables updated in the transition. If no global variables is updated, pg moves directly to ok_loc.
        //   In write_{var} pg sends the new value of {var} through the channel write_{var}, and in wait_{var} pg waits for vars_pg to receive the value.
        // * After all global variables are updated, pg moves to ok_loc, where it sends -1 through ch_req (terminating the transition) and moves to the destination location.
        // * If the guard was false and pg moved to not_ok_loc, it sends -1 through ch_req (terminating the transition) and moves back to the initial location.
        // These locations/transitions are created for each edge of the automaton in the jani model.
        //
        // Vars_PG:
        // * if vars_pg is in init_loc (i.e. is not aiding another transition), it can receive the init messages through ch_init, moving the wait_loc.
        // * in wait_loc, vars_pg can receive the ids of the global variables through ch_req, moving to send_loc. If -1 is received, vars_pg moves back to init_loc.
        // * in send_loc, vars_pg can send the value of the global variables through the channels read_{var0}, ..., read_{varn}, moving back to wait_loc.
        // * in wait_loc, vars_pg can receive the write messages through the channels write_{var0}, ..., write_{varn}, staying in wait_loc.
        // These locations/transitions are created for each automaton in the jani model (except for the initial location).
        //
        // Channels:
        // * ch_init: channel where pg sends a message to vars_pg to start a transition.
        // * ch_req: channel where pg sends the id of the global variables to vars_pg.
        // * read_{var}: channel where vars_pg sends the value of the global variables to pg. One channel for each global variable.
        // * write_{var}: channel where pg sends the value of the updated global variables to vars_pg. One channel for each global variable.

        // Create channels between pg and vars_pg
        let ch_init = builder.new_channel(Type::Integer, Some(1));
        let ch_req = builder.new_channel(Type::Integer, Some(1));
        let mut channels_hm: HashMap<String, Channel> = HashMap::new();
        for (k, v) in variables_hm.iter() {
            let ch = builder.new_channel(v.2.clone(), Some(1));
            channels_hm.insert(format!("read_{}", k), ch);
            let ch = builder.new_channel(v.2.clone(), Some(1));
            channels_hm.insert(format!("write_{}", k), ch);
        }

        // Add locations to vars_pg
        let vars_init_loc = builder.initial_location(vars_pg)?;
        let vars_wait_loc = builder.new_location(vars_pg)?;
        let vars_send_loc = builder.new_location(vars_pg)?;

        // Add variables to vars_pg
        let vars_init_var = builder.new_var(vars_pg, Expression::Const(Val::Integer(0)))?;
        let vars_req_var = builder.new_var(vars_pg, Expression::Const(Val::Integer(-1)))?;

        // Add transitions to vars_pg
        let a = builder.new_receive(vars_pg, ch_init, vars_init_var)?;
        builder.add_transition(vars_pg, vars_init_loc, a, vars_wait_loc, None)?;
        let a = builder.new_receive(vars_pg, ch_req, vars_req_var)?;
        builder.add_transition(vars_pg, vars_wait_loc, a, vars_send_loc, None)?;
        for (id, (k, v)) in variables_hm.iter().enumerate() {
            let ch = channels_hm
                .get(&format!("read_{}", k))
                .ok_or(BuildingError::UnknownError)?;
            let a = builder.new_send(vars_pg, *ch, Expression::Var(v.0, v.2.clone()))?;
            let guard = Some(Expression::Equal(Box::new((
                Expression::Var(vars_req_var, Type::Integer),
                Expression::Const(Val::Integer(id as i32)),
            ))));
            builder.add_transition(vars_pg, vars_send_loc, a, vars_wait_loc, guard)?;
        }
        for (k, v) in variables_hm.iter() {
            let ch = channels_hm
                .get(&format!("write_{}", k))
                .ok_or(BuildingError::UnknownError)?;
            let a = builder.new_receive(vars_pg, *ch, v.0)?;
            builder.add_transition(vars_pg, vars_wait_loc, a, vars_wait_loc, None)?;
        }
        let a = builder.new_action(vars_pg)?;
        let guard = Some(Expression::Equal(Box::new((
            Expression::Var(vars_req_var, Type::Integer),
            Expression::Const(Val::Integer(-1)),
        ))));
        builder.add_transition(vars_pg, vars_send_loc, a, vars_init_loc, guard)?;

        // visit edges
        for e in edges_node {
            let mut start_location: Option<Location> = None;
            let transition_action = builder.new_action(pg)?;
            let mut guard: Option<Expression<Var>> = None;
            let mut guard_global_variables: Vec<String> = Vec::new();
            let mut destination_locations: Vec<Location> = Vec::new();
            let mut destination_variables: Vec<Var> = Vec::new();
            let mut destination_global_variables: Vec<String> = Vec::new();
            let mut destination_values: Vec<Expression<Var>> = Vec::new();

            // Collet the properties of the transition
            match e {
                ASTNode::ASTAutomatonEdge { properties } => {
                    for p in properties {
                        match p {
                            ASTNode::ASTAutomatonEdgeLocation { location } => match *location {
                                ASTNode::ASTIdentifier { identifier } => {
                                    let identifier = identifier.trim_matches('\"').to_string();
                                    match locations_hm.get(&identifier) {
                                        Some(l) => {
                                            start_location = Some(*l);
                                        }
                                        None => {
                                            return Err(BuildingError::LocationNotFound(
                                                identifier,
                                            ));
                                        }
                                    }
                                }
                                _ => {
                                    return Err(BuildingError::UnknownError);
                                }
                            },
                            ASTNode::ASTAutomatonEdgeGuard { properties } => {
                                for p in properties {
                                    match p {
                                        ASTNode::ASTAutomatonEdgeGuardExp { exp } => {
                                            guard = Self::visit_expression(
                                                &exp,
                                                constants_hm,
                                                Some(variables_hm),
                                                Some(local_variables_hm),
                                                &mut Some(&mut guard_global_variables),
                                            )?;
                                        }
                                        _ => continue,
                                    }
                                }
                            }
                            ASTNode::ASTAutomatonEdgeDestinations { destinations } => {
                                for d in destinations {
                                    match d {
                                        ASTNode::ASTAutomatonEdgeDestination { properties } => {
                                            for p in properties {
                                                match p {
                                                    ASTNode::ASTAutomatonEdgeDestinationLocation { location } => {
                                                        match *location {
                                                            ASTNode::ASTIdentifier { identifier } => {
                                                                let identifier = identifier.trim_matches('\"').to_string();
                                                                match locations_hm.get(&identifier) {
                                                                    Some(l) => {
                                                                        destination_locations.push(*l);
                                                                    },
                                                                    None => {
                                                                        return Err(BuildingError::LocationNotFound(identifier));
                                                                    }
                                                                }
                                                            },
                                                            _ => {
                                                                return Err(BuildingError::UnknownError);
                                                            }
                                                        }
                                                    },
                                                    ASTNode::ASTAutomatonEdgeDestinationAssignments { assignments } => {
                                                        for a in assignments {
                                                            match a {
                                                                ASTNode::ASTAutomatonEdgeDestinationAssignment { properties } => {
                                                                    for p in properties {
                                                                        match p {
                                                                            ASTNode::ASTAutomatonEdgeDestinationAssignmentRef { ref_ } => {
                                                                                match *ref_ {
                                                                                    ASTNode::ASTIdentifier { identifier } => {
                                                                                        let identifier = identifier.trim_matches('\"').to_string();
                                                                                        if constants_hm.contains_key(&identifier) {
                                                                                            return Err(BuildingError::UpdateConstant(identifier));
                                                                                        }
                                                                                        match local_variables_hm.get(&identifier) {
                                                                                            Some((v, _)) => {
                                                                                                destination_variables.push(*v);
                                                                                                if variables_hm.contains_key(&identifier) {
                                                                                                    destination_global_variables.push(identifier.to_string());
                                                                                                }
                                                                                            },
                                                                                            None => {
                                                                                                return Err(BuildingError::VariableNotFound(identifier));
                                                                                            }
                                                                                        }
                                                                                    },
                                                                                    _ => {
                                                                                        return Err(BuildingError::UnknownError);
                                                                                    }
                                                                                }
                                                                            },
                                                                            ASTNode::ASTAutomatonEdgeDestinationAssignmentValue { value } => {
                                                                                if let Some(exp) = Self::visit_expression(&value, constants_hm, Some(variables_hm), Some(local_variables_hm), &mut None)? {
                                                                                    destination_values.push(exp);
                                                                                }
                                                                            },
                                                                            ASTNode::ASTAutomatonEdgeDestinationAssignmentIndex { index: _ } => {
                                                                                return Err(BuildingError::FeatureNotSupported("\"index\"".to_string()));
                                                                            },
                                                                            _ => continue,
                                                                        }
                                                                    }
                                                                },
                                                                ASTNode::ASTEmpty => { },
                                                                _ => {
                                                                    return Err(BuildingError::UnknownError);
                                                                }
                                                            }
                                                        }
                                                    },
                                                    ASTNode::ASTAutomatonEdgeDestinationProbability { properties: _ } => {
                                                        return Err(BuildingError::FeatureNotSupported("\"probability\"".to_string()));
                                                    },
                                                    _ => continue,
                                                }
                                            }
                                        }
                                        _ => {
                                            return Err(BuildingError::UnknownError);
                                        }
                                    }
                                }
                            }
                            ASTNode::ASTAutomatonEdgeRate { properties: _ } => {
                                return Err(BuildingError::FeatureNotSupported(
                                    "\"rate\"".to_string(),
                                ));
                            }
                            _ => continue,
                        }
                    }
                }
                _ => {
                    return Err(BuildingError::UnknownError);
                }
            }

            // Add effects to transition_action
            for i in 0..destination_variables.len() {
                builder.add_effect(
                    pg,
                    transition_action,
                    destination_variables[i],
                    destination_values[i].clone(),
                )?;
            }

            // Add locations to pg
            let init_loc = builder.new_location(pg)?;
            let mut transition_locations: HashMap<String, Location> = HashMap::new();
            for v in guard_global_variables.iter() {
                let req = builder.new_location(pg)?;
                let read = builder.new_location(pg)?;
                transition_locations.insert(format!("req_{}", v), req);
                transition_locations.insert(format!("read_{}", v), read);
            }
            let act_loc = builder.new_location(pg)?;
            for v in destination_global_variables.iter() {
                let write = builder.new_location(pg)?;
                let wait = builder.new_location(pg)?;
                transition_locations.insert(format!("write_{}", v), write);
                transition_locations.insert(format!("wait_{}", v), wait);
            }
            let ok_loc = builder.new_location(pg)?;
            let not_ok_loc = builder.new_location(pg)?;

            // Add transitions to pg
            if guard_global_variables.is_empty() && destination_global_variables.is_empty() {
                for post in &destination_locations {
                    builder.add_transition(
                        pg,
                        start_location.unwrap(),
                        transition_action,
                        *post,
                        guard.clone(),
                    )?;
                }
                continue;
            }
            let a = builder.new_send(pg, ch_init, Expression::Const(Val::Integer(0)))?;
            builder.add_transition(pg, start_location.unwrap(), a, init_loc, None)?;
            if guard_global_variables.is_empty() {
                let a = builder.new_probe_empty_queue(pg, ch_init)?;
                builder.add_transition(pg, init_loc, a, act_loc, None)?;
            } else {
                let mut pre = init_loc;
                let mut var: Option<Var> = None;
                let mut ch_read: Option<Channel> = None;
                for (i, v) in guard_global_variables.iter().enumerate() {
                    let id = variables_hm
                        .keys()
                        .position(|k| k == v)
                        .ok_or(BuildingError::UnknownError)?;
                    let req = transition_locations
                        .get(&format!("req_{}", v))
                        .ok_or(BuildingError::UnknownError)?;
                    let read = transition_locations
                        .get(&format!("read_{}", v))
                        .ok_or(BuildingError::UnknownError)?;
                    if i == 0 {
                        let a = builder.new_probe_empty_queue(pg, ch_init)?;
                        builder.add_transition(pg, pre, a, *req, None)?;
                    } else {
                        let a = builder.new_receive(pg, ch_read.unwrap(), var.unwrap())?;
                        builder.add_transition(pg, pre, a, *req, None)?;
                    }
                    let a =
                        builder.new_send(pg, ch_req, Expression::Const(Val::Integer(id as i32)))?;
                    builder.add_transition(pg, *req, a, *read, None)?;
                    pre = *read;
                    var = Some(
                        local_variables_hm
                            .get(v)
                            .ok_or(BuildingError::UnknownError)?
                            .0,
                    );
                    ch_read = Some(
                        *channels_hm
                            .get(&format!("read_{}", v))
                            .ok_or(BuildingError::UnknownError)?,
                    );
                }
                let a = builder.new_receive(pg, ch_read.unwrap(), var.unwrap())?;
                builder.add_transition(pg, pre, a, act_loc, None)?;
            }
            if destination_global_variables.is_empty() {
                builder.add_transition(pg, act_loc, transition_action, ok_loc, guard.clone())?;
                if let Some(ref guard) = guard {
                    let a = builder.new_action(pg)?;
                    builder.add_transition(
                        pg,
                        act_loc,
                        a,
                        not_ok_loc,
                        Some(Expression::Not(Box::new(guard.clone()))),
                    )?;
                }
            } else {
                let mut pre = act_loc;
                let mut ch_write: Option<Channel> = None;
                for (i, v) in destination_global_variables.iter().enumerate() {
                    let (var, type_) = local_variables_hm
                        .get(v)
                        .ok_or(BuildingError::UnknownError)?;
                    let write = transition_locations
                        .get(&format!("write_{}", v))
                        .ok_or(BuildingError::UnknownError)?;
                    let wait = transition_locations
                        .get(&format!("wait_{}", v))
                        .ok_or(BuildingError::UnknownError)?;
                    if i == 0 {
                        builder.add_transition(
                            pg,
                            act_loc,
                            transition_action,
                            *write,
                            guard.clone(),
                        )?;
                        if let Some(ref guard) = guard {
                            let a = builder.new_action(pg)?;
                            builder.add_transition(
                                pg,
                                act_loc,
                                a,
                                not_ok_loc,
                                Some(Expression::Not(Box::new(guard.clone()))),
                            )?;
                        }
                    } else {
                        let a = builder.new_probe_empty_queue(pg, ch_write.unwrap())?;
                        builder.add_transition(pg, pre, a, *write, None)?;
                    }
                    ch_write = Some(
                        *channels_hm
                            .get(&format!("write_{}", v))
                            .ok_or(BuildingError::UnknownError)?,
                    );
                    let a = builder.new_send(
                        pg,
                        ch_write.unwrap(),
                        Expression::Var(*var, type_.clone()),
                    )?;
                    builder.add_transition(pg, *write, a, *wait, None)?;
                    pre = *wait;
                }
                let a = builder.new_probe_empty_queue(pg, ch_write.unwrap())?;
                builder.add_transition(pg, pre, a, ok_loc, None)?;
            }
            let end = builder.new_send(pg, ch_req, Expression::Const(Val::Integer(-1)))?;
            for location in &destination_locations {
                builder.add_transition(pg, ok_loc, end, *location, None)?;
            }
            builder.add_transition(pg, not_ok_loc, end, start_location.unwrap(), None)?;
        }

        Ok(())
    }

    fn visit_expression(
        expression: &ASTNode,
        constants_hm: &HashMap<String, (CsExpression, Type)>,
        variables_hm: Option<&BTreeMap<String, (Var, CsExpression, Type)>>,
        local_variables_hm: Option<&HashMap<String, (Var, Type)>>,
        guard_global_variables: &mut Option<&mut Vec<String>>,
    ) -> Result<Option<CsExpression>, BuildingError> {
        let exp: Option<CsExpression>;

        match expression {
            ASTNode::ASTConstantValueInteger { value } => {
                exp = Some(Expression::Const(Val::Integer(*value)));
            }
            ASTNode::ASTConstantValueBoolean { value } => {
                exp = Some(Expression::Const(Val::Boolean(*value)));
            }
            ASTNode::ASTConstantValueReal { value } => {
                exp = Some(Expression::Const(Val::Float(OrderedFloat(*value))));
            }
            ASTNode::ASTIdentifier { identifier } => {
                let identifier = identifier.trim_matches('\"').to_string();
                if let Some(local_variables_hm) = local_variables_hm {
                    match local_variables_hm.get(&identifier) {
                        Some((var, type_)) => {
                            exp = Some(Expression::Var(*var, type_.clone()));
                            if let Some(guard_global_variables) = guard_global_variables {
                                if let Some(variables_hm) = variables_hm {
                                    if variables_hm.contains_key(&identifier) {
                                        guard_global_variables.push(identifier.to_string());
                                    }
                                }
                            }
                        }
                        None => {
                            return Err(BuildingError::VariableNotFound(identifier.to_string()));
                        }
                    }
                } else if let Some(variables_hm) = variables_hm {
                    match variables_hm.get(&identifier) {
                        Some((_, val, _)) => {
                            exp = Some(val.clone());
                        }
                        None => match constants_hm.get(&identifier) {
                            Some((val, _)) => {
                                exp = Some(val.clone());
                            }
                            None => {
                                return Err(BuildingError::VariableNotFound(identifier));
                            }
                        },
                    }
                } else {
                    match constants_hm.get(&identifier) {
                        Some((val, _)) => {
                            exp = Some(val.clone());
                        }
                        None => {
                            return Err(BuildingError::VariableNotFound(identifier));
                        }
                    }
                }
            }
            ASTNode::ASTExpressionIfThenElse { properties: _ } => {
                return Err(BuildingError::FeatureNotSupported(
                    "\"if-then-else\" expression".to_string(),
                ));
            }
            ASTNode::ASTExpressionBinaryOperation { properties } => {
                let mut op_exp: Option<String> = None;
                let mut left_exp: Option<CsExpression> = None;
                let mut right_exp: Option<CsExpression> = None;

                for p in properties {
                    match p {
                        ASTNode::ASTExpressionOperation { op } => {
                            op_exp = Some(op.clone());
                        }
                        ASTNode::ASTExpressionLeft { left } => {
                            left_exp = Self::visit_expression(
                                left,
                                constants_hm,
                                variables_hm,
                                local_variables_hm,
                                guard_global_variables,
                            )?;
                        }
                        ASTNode::ASTExpressionRight { right } => {
                            right_exp = Self::visit_expression(
                                right,
                                constants_hm,
                                variables_hm,
                                local_variables_hm,
                                guard_global_variables,
                            )?;
                        }
                        _ => {
                            return Err(BuildingError::UnknownError);
                        }
                    }
                }

                if let (Some(op_exp), Some(left_exp), Some(right_exp)) =
                    (op_exp, left_exp, right_exp)
                {
                    match op_exp.trim_matches('\"') {
                        "∨" => {
                            exp = Some(Expression::Or(vec![left_exp, right_exp]));
                        }
                        "∧" => {
                            exp = Some(Expression::And(vec![left_exp, right_exp]));
                        }
                        "=" => {
                            exp = Some(Expression::Equal(Box::new((left_exp, right_exp))));
                        }
                        "≠" => {
                            exp = Some(Expression::Not(Box::new(Expression::Equal(Box::new((
                                left_exp, right_exp,
                            ))))));
                        }
                        "<" => {
                            exp = Some(Expression::Less(Box::new((left_exp, right_exp))));
                        }
                        ">" => {
                            exp = Some(Expression::Greater(Box::new((left_exp, right_exp))));
                        }
                        "≤" => {
                            exp = Some(Expression::LessEq(Box::new((left_exp, right_exp))));
                        }
                        "≥" => {
                            exp = Some(Expression::GreaterEq(Box::new((left_exp, right_exp))));
                        }
                        "+" => {
                            exp = Some(Expression::Sum(vec![left_exp, right_exp]));
                        }
                        "-" => {
                            exp = Some(Expression::Sum(vec![
                                left_exp,
                                Expression::Opposite(Box::new(right_exp)),
                            ]));
                        }
                        "*" => {
                            exp = Some(Expression::Mult(vec![left_exp, right_exp]));
                        }
                        "%" => {
                            return Err(BuildingError::FeatureNotSupported(
                                "Modulus operation".to_string(),
                            ));
                        }
                        "/" => {
                            return Err(BuildingError::FeatureNotSupported(
                                "Division operation".to_string(),
                            ));
                        }
                        "pow" => {
                            return Err(BuildingError::FeatureNotSupported(
                                "Power operation".to_string(),
                            ));
                        }
                        "log" => {
                            return Err(BuildingError::FeatureNotSupported(
                                "Logarithm operation".to_string(),
                            ));
                        }
                        _ => {
                            return Err(BuildingError::UnknownError);
                        }
                    }
                } else {
                    return Err(BuildingError::UnknownError);
                }
            }
            ASTNode::ASTExpressionUnaryOperation { properties } => {
                let mut op_exp: Option<String> = None;
                let mut operand_exp: Option<CsExpression> = None;

                for p in properties {
                    match p {
                        ASTNode::ASTExpressionOperation { op } => {
                            op_exp = Some(op.clone());
                        }
                        ASTNode::ASTExpressionOperand { exp } => {
                            operand_exp = Self::visit_expression(
                                exp,
                                constants_hm,
                                variables_hm,
                                local_variables_hm,
                                guard_global_variables,
                            )?;
                        }
                        _ => {
                            return Err(BuildingError::UnknownError);
                        }
                    }
                }

                if let (Some(op_exp), Some(operand_exp)) = (op_exp, operand_exp) {
                    match op_exp.trim_matches('\"') {
                        "¬" => {
                            exp = Some(Expression::Not(Box::new(operand_exp)));
                        }
                        "floor" => {
                            return Err(BuildingError::FeatureNotSupported(
                                "Floor operation".to_string(),
                            ));
                        }
                        "ceil" => {
                            return Err(BuildingError::FeatureNotSupported(
                                "Ceil operation".to_string(),
                            ));
                        }
                        _ => {
                            return Err(BuildingError::UnknownError);
                        }
                    }
                } else {
                    return Err(BuildingError::UnknownError);
                }
            }
            _ => {
                return Err(BuildingError::UnknownError);
            }
        }

        Ok(exp)
    }
}
