//! Parser for SCAN's JANI specification format.

// TODO list: 
// * add support for JANI extensions

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;
use std::path::Path;
use thiserror::Error;

lrlex_mod!("jani_parser/jani_parser.l");
lrpar_mod!("jani_parser/jani_parser.y");

/// Error type for parsing errors.
#[derive(Error, Clone, Debug)]
pub enum ParsingError {
    #[error("Invalid path")]
    InvalidPathError,
    #[error("Invalid syntax")]
    InvalidSyntaxError,
    #[error("Duplicate fields")]
    DuplicateFieldsError,
    #[error("Missing fields")]
    MissingFieldsError
}

/// Parser for JANI specification format.
pub struct Parser;
impl Parser {
    /// Parse a file containing a JANI model.
    ///
    /// # Arguments
    ///
    /// * `input` - The path to the file containing the JANI model.
    ///
    /// # Returns
    ///
    /// * Ok(`ASTNode`): The root [`ASTNode`] of the abstract syntax tree representing the JANI model, if parsing is successful.
    /// * Err(`ParsingError`): A [`ParsingError`] indicating the type of error that occurred during parsing, if parsing fails.
    /// Information on the error, such as the location in the file and possible suggestions for fixing it, are printed to stderr.
    pub fn parse(input: &Path) -> Result<ASTNode, ParsingError> {
        // Parse the input string
        let input = std::fs::read_to_string(input).map_err(|_| ParsingError::InvalidPathError)?;
        let lexerdef = jani_parser_l::lexerdef();
        let lexer = lexerdef.lexer(&input);
        let (res, errs) = jani_parser_y::parse(&lexer);
    
        // Print any errors that occurred during parsing
        if !errs.is_empty() {
            for e in &errs {
                eprintln!("{}", e.pp(&lexer, &jani_parser_y::token_epp));
            }
            return Err(ParsingError::InvalidSyntaxError);
        }   
        // Return the AST node if parsing is successful
        match res {
            Some(res) => res,
            None => Err(ParsingError::InvalidSyntaxError)
        }
    }
}

/// Abstract Syntax Tree (AST) node representing an element of a JANI model.
///
/// Each variant corresponds to a different element of the JANI model. 
/// Check the [JANI specification](https://docs.google.com/document/d/1BDQIzPBtscxJFFlDUEPIo8ivKHgXT8_X6hz5quq7jK0) for details on the model structure.
#[derive(Debug)]
pub enum ASTNode {
    ASTModel {
        properties: Vec<ASTNode>
    },

    ASTModelVersion{
        version: u64
    },

    ASTModelName{
        name: Box<ASTNode>
    },

    ASTModelMetadata{
        properties: Vec<ASTNode>
    },
    ASTMetadataVersion{
        version: String
    },
    ASTMetadataAuthor{
        author: String
    },
    ASTMetadataDescription{
        description: String
    },
    ASTMetadataDoi{
        doi: String
    },
    ASTMetadataUrl{
        url: String
    },

    ASTModelType{
        modeltype: String
    },

    ASTModelFeatures{
        features: Vec<ASTNode>
    },
    ASTModelFeature{
        modelfeature: String
    },

    ASTModelActions{
        actions: Vec<ASTNode>
    },
    ASTModelAction{
        properties: Vec<ASTNode>
    },
    ASTModelActionName{
        name: Box<ASTNode>
    },

    ASTModelConstants{
        constants: Vec<ASTNode>
    },
    ASTConstantDeclaration{
        properties: Vec<ASTNode>
    },
    ASTConstantDeclarationName{
        name: Box<ASTNode>
    },
    ASTConstantDeclarationType{
        type_: Box<ASTNode>
    },
    ASTConstantDeclarationValue{
        value: Box<ASTNode>
    },

    ASTModelVariables{
        variables: Vec<ASTNode>
    },
    ASTVariableDeclaration{
        properties: Vec<ASTNode>
    },
    ASTVariableDeclarationName{
        name: Box<ASTNode>
    },
    ASTVariableDeclarationType{
        type_: Box<ASTNode>
    },
    ASTVariableDeclarationTransient{
        transient: bool
    },
    ASTVariableDeclarationInitialValue{
        initial_value: Box<ASTNode>
    },
    
    ASTModelRestrictInitial{
        properties: Vec<ASTNode>
    },
    ASTRestrictInitialExp{
        exp: Box<ASTNode>
    },

    ASTModelProperties{
        properties: Vec<ASTNode>
    },
    ASTProperty{
        properties: Vec<ASTNode>
    },
    ASTPropertyName{
        name: Box<ASTNode>
    },
    ASTPropertyExpression{
        expression: Box<ASTNode>
    },

    ASTModelAutomata{
        automata: Vec<ASTNode>
    },
    ASTAutomaton{
        properties: Vec<ASTNode>
    },
    ASTAutomatonName{
        name: Box<ASTNode>
    },
    ASTAutomatonVariables{
        variables: Vec<ASTNode>
    },
    ASTAutomatonRestrictInitial{
        properties: Vec<ASTNode>
    },
    ASTAutomatonLocations{
        locations: Vec<ASTNode>
    },
    ASTAutomatonLocation{
        properties: Vec<ASTNode>
    },
    ASTAutomatonLocationName{
        name: Box<ASTNode>
    },
    ASTAutomatonLocationTimeProgress{
        properties: Vec<ASTNode>
    },
    ASTAutomatonLocationTimeProgressExp{
        exp: Box<ASTNode>
    },
    ASTAutomatonLocationTransientValues{
        transient_values: Vec<ASTNode>
    },
    ASTAutomatonLocationTransientValue{
        properties: Vec<ASTNode>
    },
    ASTTransientValueRef{
        ref_: Box<ASTNode>
    },
    ASTTransientValueValue{
        value: Box<ASTNode>
    },
    ASTAutomatonInitialLocations{
        initial_locations: Vec<ASTNode>
    },
    ASTAutomatonEdges{
        edges: Vec<ASTNode>
    },
    ASTAutomatonEdge{
        properties: Vec<ASTNode>
    },
    ASTAutomatonEdgeLocation{
        location: Box<ASTNode>
    },
    ASTAutomatonEdgeAction{
        action: Box<ASTNode>
    },
    ASTAutomatonEdgeRate{
        properties: Vec<ASTNode>
    },
    ASTAutomatonEdgeRateExp{
        exp: Box<ASTNode>
    },
    ASTAutomatonEdgeGuard{
        properties: Vec<ASTNode>
    },
    ASTAutomatonEdgeGuardExp{
        exp: Box<ASTNode>
    },
    ASTAutomatonEdgeDestinations{
        destinations: Vec<ASTNode>
    },
    ASTAutomatonEdgeDestination{
        properties: Vec<ASTNode>
    },
    ASTAutomatonEdgeDestinationLocation{
        location: Box<ASTNode>
    },
    ASTAutomatonEdgeDestinationProbability{
        properties: Vec<ASTNode>
    },
    ASTAutomatonEdgeDestinationProbabilityExp{
        exp: Box<ASTNode>
    },
    ASTAutomatonEdgeDestinationAssignments{
        assignments: Vec<ASTNode>
    },
    ASTAutomatonEdgeDestinationAssignment{
        properties: Vec<ASTNode>
    },
    ASTAutomatonEdgeDestinationAssignmentRef{
        ref_: Box<ASTNode>
    },
    ASTAutomatonEdgeDestinationAssignmentValue{
        value: Box<ASTNode>
    },
    ASTAutomatonEdgeDestinationAssignmentIndex{
        index: u64
    },

    ASTModelSystem{
        properties: Vec<ASTNode>
    },
    ASTCompositionElements{
        elements: Vec<ASTNode>
    },
    ASTCompositionElement{
        properties: Vec<ASTNode>
    },
    ASTCompositionElementAutomaton{
        automaton: Box<ASTNode>
    },
    ASTCompositionElementInputEnable{
        input_enable: Vec<ASTNode>
    },
    ASTCompositionSyncs{
        syncs: Vec<ASTNode>
    },
    ASTCompositionSync{
        properties: Vec<ASTNode>
    },
    ASTCompositionSyncSynchronise{
        synchronise: Vec<ASTNode>
    },
    ASTCompositionSyncResult{
        result: Box<ASTNode>
    },


    ASTIdentifier {
        identifier: String
    },

    ASTBasicType {
        type_: String
    },
    ASTBoundedType{
        properties: Vec<ASTNode>
    },
    ASTBoundedTypeKind{
        kind: String
    },
    ASTBoundedTypeBase{
        base: String
    },
    ASTBoundedTypeLowerBound{
        lower_bound: Box<ASTNode>
    },
    ASTBoundedTypeUpperBound{ 
        upper_bound: Box<ASTNode>
    },
    ASTOtherType {
        type_: String
    },

    ASTConstantValueInteger{
        value: i32
    },
    ASTConstantValueReal{
        value: f64
    },
    ASTConstantValueBoolean{
        value: bool
    },
    ASTExpressionOperation{
        op: String
    },
    ASTExpressionIfThenElse{
        properties: Vec<ASTNode>
    },
    ASTExpressionIf{
        if_: Box<ASTNode>
    },
    ASTExpressionThen{
        then: Box<ASTNode>
    },
    ASTExpressionElse{
        else_: Box<ASTNode>
    },
    ASTExpressionBinaryOperation{
        properties: Vec<ASTNode>
    },
    ASTExpressionLeft{
        left: Box<ASTNode>
    },
    ASTExpressionRight{
        right: Box<ASTNode>
    },
    ASTExpressionUnaryOperation{
        properties: Vec<ASTNode>
    },
    ASTExpressionOperand{
        exp: Box<ASTNode>
    },
    ASTExpressionDerivativeOperation{
        properties: Vec<ASTNode>
    },
    ASTExpressionVariable{
        var: Box<ASTNode>
    },
    ASTDistributionSampling{
        properties: Vec<ASTNode>
    },
    ASTDistributionSamplingDistribution{
        distribution: String
    },
    ASTDistributionSamplingArgs{
        args: Vec<ASTNode>
    },

    ASTExpressionFilter{
        properties: Vec<ASTNode>
    },
    ASTExpressionFilterFun{
        fun: String
    },
    ASTExpressionFilterValues{
        values: Box<ASTNode>
    },
    ASTExpressionFilterStates{
        states: Box<ASTNode>
    },
    ASTExpressionReach{
        reach: Box<ASTNode>
    },
    ASTExpressionStepInstant{
        step_instant: Box<ASTNode>
    },
    ASTExpressionTimeInstant{
        time_instant: Box<ASTNode>
    },
    ASTExpressionRewardInstants{
        reward_instants: Vec<ASTNode>
    },
    ASTExpressionRewardInstant{
        properties: Vec<ASTNode>
    },
    ASTExpressionRewardInstantExp{
        exp: Box<ASTNode>
    },
    ASTExpressionRewardInstantAccumulate{
        accumulate: Box<ASTNode>
    },
    ASTExpressionRewardInstantInstant{
        instant: Box<ASTNode>
    },
    ASTExpressionAccumulate{
        accumulate: Box<ASTNode>
    },
    ASTExpressionStepBounds{
        step_bounds: Vec<ASTNode>
    },
    ASTExpressionTimeBounds{
        time_bounds: Vec<ASTNode>
    },
    ASTExpressionRewardBounds{
        reward_bounds: Vec<ASTNode>
    },
    ASTExpressionRewardBound{
        properties: Vec<ASTNode>
    },
    ASTExpressionRewardBoundExp{
        exp: Box<ASTNode>
    },
    ASTExpressionRewardBoundAccumulate{
        accumulate: Box<ASTNode>
    },
    ASTExpressionRewardBoundBounds{
        bounds: Vec<ASTNode>
    },
    ASTPropertyIntervalLower{
        lower: Box<ASTNode>
    },
    ASTPropertyIntervalLowerExclusive{
        lower_exclusive: bool
    },
    ASTPropertyIntervalUpper{
        upper: Box<ASTNode>
    },
    ASTPropertyIntervalUpperExclusive{
        upper_exclusive: bool
    },
    ASTRewardAccumulation{
        accumulate: Vec<String>
    },


    ASTComment{
        comment: String
    }
}
