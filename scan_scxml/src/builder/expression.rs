use std::{collections::HashMap, ops::Not};

use anyhow::{Context, anyhow, bail};
use boa_ast::expression::{
    access::{PropertyAccess, PropertyAccessField},
    operator::binary::BinaryOp,
};
use boa_interner::{Interner, ToInternedString};
use scan_core::{Expression, Integer, Type};

use crate::parser::{OmgBaseType, OmgType, OmgTypeDef, OmgTypes};

pub(super) fn infer_type(
    expr: &boa_ast::Expression,
    vars: &HashMap<String, OmgType>,
    interner: &Interner,
    omg_types: &OmgTypes,
) -> anyhow::Result<OmgType> {
    match expr {
        boa_ast::Expression::Identifier(ident) => {
            let ident = ident.to_interned_string(interner);
            vars.get(&ident)
                .cloned()
                .or_else(|| {
                    if omg_types.type_defs.iter().any(|(_, omg_type)| {
                        if let OmgTypeDef::Enumeration(labels) = omg_type {
                            labels.contains(&ident)
                        } else {
                            false
                        }
                    }) {
                        Some(OmgBaseType::Int32.into())
                    } else {
                        None
                    }
                })
                .ok_or(anyhow!("type cannot be inferred"))
        }
        boa_ast::Expression::ArrayLiteral(lit) => {
            let omg_type = infer_type(
                lit.as_ref()
                    .first()
                    .ok_or(anyhow!("array literal missing expression"))?
                    .as_ref()
                    .ok_or(anyhow!("cannot infer type of this array"))?,
                vars,
                interner,
                omg_types,
            )?;
            let len = lit.as_ref().len();
            if let OmgType::Base(omg_base_type) = omg_type {
                Ok(OmgType::Array(omg_base_type, Some(len)))
            } else {
                Err(anyhow!("only arrays of base types are currently supported"))
            }
        }
        boa_ast::Expression::Literal(lit) => {
            use boa_ast::expression::literal::LiteralKind;
            match lit.kind() {
                LiteralKind::String(_) => Ok(OmgBaseType::String.into()),
                LiteralKind::Num(_) => Ok(OmgBaseType::F64.into()),
                LiteralKind::Int(_) => Ok(OmgBaseType::Int32.into()),
                // Literal::BigInt(_) => todo!(),
                LiteralKind::Bool(_) => Ok(OmgBaseType::Boolean.into()),
                _ => Err(anyhow!(
                    "unable to infer type for literal expression '{lit:?}'"
                )),
            }
        }
        boa_ast::Expression::Unary(unary) => {
            let type_name = infer_type(unary.target(), vars, interner, omg_types)?;
            match unary.op() {
                boa_ast::expression::operator::unary::UnaryOp::Minus
                | boa_ast::expression::operator::unary::UnaryOp::Plus => Ok(type_name),
                boa_ast::expression::operator::unary::UnaryOp::Not => {
                    Ok(OmgBaseType::Boolean.into())
                }
                _ => Err(anyhow!(
                    "unable to infer type for operator '{:?}'",
                    unary.op()
                )),
            }
        }
        boa_ast::Expression::Binary(bin) => {
            let lhs = infer_type(bin.lhs(), vars, interner, omg_types)?;
            let rhs = infer_type(bin.rhs(), vars, interner, omg_types)?;
            match bin.op() {
                BinaryOp::Arithmetic(op) => {
                    if let (OmgType::Base(lhs_type), OmgType::Base(rhs_type)) = (lhs, rhs) {
                        if lhs_type == rhs_type {
                            Ok(OmgType::Base(lhs_type))
                        } else {
                            Err(anyhow!(
                                "unable to infer type for operator '{op:?}' arithmetic expression"
                            ))
                        }
                    } else {
                        Err(anyhow!(
                            "unable to infer type for operator '{op:?}' arithmetic expression"
                        ))
                    }
                }
                BinaryOp::Bitwise(_) => Err(anyhow!("bitwise operations not supported")),
                BinaryOp::Relational(_) | BinaryOp::Logical(_) => Ok(OmgBaseType::Boolean.into()),
                BinaryOp::Comma => Err(anyhow!(
                    "unknown binary operator 'comma', unable to infer type"
                )),
            }
        }
        boa_ast::Expression::Call(call) => {
            // TODO FIXME: fix name of args
            let mut vars_args = vars.clone();
            let args = call
                .args()
                .iter()
                .map(|arg| {
                    infer_type(arg, vars, interner, omg_types)
                        .map(|omg_type| (String::from("name of arg???"), omg_type))
                })
                .collect::<anyhow::Result<Vec<_>>>()?;
            vars_args.extend(args);
            infer_type(call.function(), &vars_args, interner, omg_types)
        }
        boa_ast::Expression::PropertyAccess(property_access) => match property_access {
            PropertyAccess::Simple(simple_property_access) => {
                if let &boa_ast::Expression::Identifier(ident) = simple_property_access.target()
                    && ident.to_interned_string(interner) == "Math"
                {
                    match simple_property_access.field() {
                        PropertyAccessField::Const(identifier) => {
                            if identifier.sym() == interner.get("floor").unwrap() {
                                Ok(OmgBaseType::Int32.into())
                            } else if identifier.sym() == interner.get("random").unwrap() {
                                Ok(OmgBaseType::F64.into())
                            } else {
                                Err(anyhow!(
                                    "unknown expression '{expr:?}', unable to infer type"
                                ))
                            }
                        }
                        PropertyAccessField::Expr(expression) => Err(anyhow!(
                            "unknown expression '{expression:?}', unable to infer type"
                        )),
                    }
                } else {
                    match infer_type(simple_property_access.target(), vars, interner, omg_types)? {
                        OmgType::Base(omg_base_type) => {
                            bail!("trying to access property of base type {omg_base_type:?}")
                        }
                        OmgType::Array(omg_base_type, _) => {
                            bail!("trying to access property of array [{omg_base_type:?}]")
                        }
                        OmgType::Custom(omg_type_name) => {
                            match simple_property_access.field() {
                                PropertyAccessField::Const(identifier) => {
                                    let field = identifier.to_interned_string(interner);
                                    match omg_types.type_defs.get(&omg_type_name).ok_or_else(
                                        || anyhow!("type {omg_type_name} undefined"),
                                    )? {
                                        OmgTypeDef::Enumeration(_items) => bail!(
                                            "trying to access property of enumeration {omg_type_name}"
                                        ),
                                        OmgTypeDef::Structure(btree_map) => {
                                            btree_map.get(&field).cloned().ok_or_else(|| anyhow!("field {field} of type {omg_type_name} undefined"))
                                        }
                                    }
                                }
                                PropertyAccessField::Expr(expression) => Err(anyhow!(
                                    "unknown field expression '{expression:?}', unable to infer type"
                                )),
                            }
                        }
                    }
                }
            }
            PropertyAccess::Private(_private_property_access) => {
                todo!()
            }
            PropertyAccess::Super(_super_property_access) => {
                todo!()
            }
        },
        _ => Err(anyhow!(
            "unknown expression '{expr:?}', unable to infer type"
        )),
    }
}

// WARN: vars and params have the same type so they could be easily swapped by mistake when calling the function.
pub(super) fn expression<V: Clone>(
    expr: &boa_ast::Expression,
    interner: &Interner,
    vars: &HashMap<String, (OmgType, Vec<(V, Type)>)>,
    strings: &mut HashMap<String, Integer>,
    expr_type: Option<&OmgType>,
    omg_types: &OmgTypes,
) -> anyhow::Result<Vec<Expression<V>>> {
    let expr = match expr {
        boa_ast::Expression::This(_this) => todo!(),
        boa_ast::Expression::Identifier(ident) => {
            let ident = ident.to_interned_string(interner);
            vars.get(&ident)
                .map(|(_, vars)| {
                    vars.iter()
                        .map(|(var, t)| Expression::Var(var.clone(), t.to_owned()))
                        .collect::<Vec<Expression<V>>>()
                })
                .or_else(|| {
                    omg_types
                        .type_defs
                        .iter()
                        .find_map(|(_, omg_type)| {
                            if let OmgTypeDef::Enumeration(labels) = omg_type {
                                labels.iter().position(|label| label == &ident)
                            } else {
                                None
                            }
                        })
                        .map(|val| vec![Expression::from(val as Integer)])
                })
                .ok_or(anyhow!("unknown identifier: {ident}"))?
        }
        boa_ast::Expression::Literal(lit) => {
            use boa_ast::expression::literal::LiteralKind;
            vec![match lit.kind() {
                LiteralKind::String(s) => {
                    let len = strings.len() as Integer;
                    Expression::from(
                        *strings
                            .entry(interner.resolve_expect(*s).to_string())
                            .or_insert(len),
                    )
                }
                LiteralKind::Num(f) => Expression::from(*f),
                LiteralKind::Int(i)
                    if expr_type.is_some_and(|t| matches!(t, OmgType::Base(OmgBaseType::F64))) =>
                {
                    Expression::from(*i as f64)
                }
                LiteralKind::Int(i) => Expression::from(*i),
                LiteralKind::BigInt(_) => todo!(),
                LiteralKind::Bool(b) => Expression::from(*b),
                LiteralKind::Null => todo!(),
                LiteralKind::Undefined => todo!(),
            }]
        }
        boa_ast::Expression::ArrayLiteral(arr) => {
            let expr_type = expr_type.ok_or(anyhow!("unknown array type"))?;
            if let OmgType::Array(omg_base_type, len) = expr_type {
                let default = Expression::Const(Type::from(*omg_base_type).default_value());
                if len.is_some_and(|len| len != arr.as_ref().len()) {
                    bail!("array literal of length incompatible with declared type len");
                }
                arr.as_ref()
                    .iter()
                    .map(|entry| {
                        entry.as_ref().map_or(Ok(vec![default.clone()]), |entry| {
                            expression(
                                entry,
                                interner,
                                vars,
                                strings,
                                Some((*omg_base_type).into()).as_ref(),
                                omg_types,
                            )
                        })
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?
                    .into_iter()
                    .flatten()
                    .collect()
            } else {
                bail!("expression not matching its type {expr_type:?}");
            }
        }
        boa_ast::Expression::PropertyAccess(prop_acc) => match prop_acc {
            PropertyAccess::Simple(simple_property_access) => {
                let target = simple_property_access.target();
                let target_type = infer_type(
                    target,
                    &vars
                        .iter()
                        .map(|(var, (omg_type, _))| (var.clone(), omg_type.clone()))
                        .collect(),
                    interner,
                    omg_types,
                )?;
                let target = expression(
                    target,
                    interner,
                    vars,
                    strings,
                    Some(&target_type),
                    omg_types,
                )?;
                let target_type_def = match target_type {
                    OmgType::Base(omg_base_type) => {
                        bail!("property access on base type {omg_base_type:?}")
                    }
                    OmgType::Array(omg_base_type, _) => {
                        bail!("property access on array [{omg_base_type:?}]")
                    }
                    OmgType::Custom(omg_name) => omg_types
                        .type_defs
                        .get(&omg_name)
                        .ok_or(anyhow!("type '{omg_name}' undefined"))?,
                };
                match simple_property_access.field() {
                    PropertyAccessField::Const(identifier) => {
                        let field_name = identifier.to_interned_string(interner);
                        match target_type_def {
                            OmgTypeDef::Enumeration(_items) => {
                                bail!("property access on enumeration")
                            }
                            OmgTypeDef::Structure(fields) => {
                                let mut target = target.as_slice();
                                for (next_field_name, next_field_type) in fields {
                                    let field_size = next_field_type.size(omg_types).with_context(|| {
                                        format!(
                                            "failed computing field '{next_field_name}' type '{next_field_type:?}' size"
                                        )
                                    })?;
                                    if *next_field_name == field_name {
                                        return Ok(target[..field_size].to_vec());
                                    } else {
                                        // Skip field-size-many expressions
                                        target = &target[field_size..];
                                    }
                                }
                                bail!("unknown field {field_name}");
                            }
                        }
                    }
                    PropertyAccessField::Expr(_expression) => todo!(),
                }
            }
            PropertyAccess::Private(_private_property_access) => todo!(),
            PropertyAccess::Super(_super_property_access) => todo!(),
        },
        boa_ast::Expression::Unary(unary) => {
            use boa_ast::expression::operator::unary::UnaryOp;
            let expr = expression(
                unary.target(),
                interner,
                vars,
                strings,
                expr_type,
                omg_types,
            )?;
            if expr.len() != 1 {
                bail!("expression does not support unary operator");
            }
            let expr = expr[0].clone();
            let new_expr = match unary.op() {
                UnaryOp::Minus => -expr,
                UnaryOp::Plus => expr,
                UnaryOp::Not => Expression::not(expr)?,
                _ => return Err(anyhow!("unimplemented operator")),
            };
            vec![new_expr]
        }
        boa_ast::Expression::Binary(bin) => {
            use boa_ast::expression::operator::binary::{
                ArithmeticOp, BinaryOp, LogicalOp, RelationalOp,
            };
            match bin.op() {
                BinaryOp::Arithmetic(ar_bin) => {
                    let lhs_hint;
                    let rhs_hint;
                    match ar_bin {
                        ArithmeticOp::Add
                        | ArithmeticOp::Sub
                        | ArithmeticOp::Mul
                        | ArithmeticOp::Exp => {
                            lhs_hint = expr_type;
                            rhs_hint = expr_type;
                        }
                        ArithmeticOp::Div => {
                            // WARN: Type inference is tricky: integer division could produce a float
                            lhs_hint = None;
                            rhs_hint = None;
                        }
                        ArithmeticOp::Mod => {
                            lhs_hint = Some(&OmgType::Base(OmgBaseType::Int32));
                            rhs_hint = Some(&OmgType::Base(OmgBaseType::Int32));
                        }
                    }
                    let lhs = expression(bin.lhs(), interner, vars, strings, lhs_hint, omg_types)?;
                    if lhs.len() != 1 {
                        bail!("expression lhs does not support arithmetic binary operator");
                    }
                    let lhs = lhs[0].clone();
                    let rhs = expression(bin.rhs(), interner, vars, strings, rhs_hint, omg_types)?;
                    if rhs.len() != 1 {
                        bail!("expression rhs does not support arithmetic binary operator");
                    }
                    let rhs = rhs[0].clone();
                    let new_expr = match ar_bin {
                        ArithmeticOp::Add => lhs + rhs,
                        ArithmeticOp::Sub => lhs + (-rhs),
                        ArithmeticOp::Div => Expression::Div(Box::new((lhs, rhs))),
                        ArithmeticOp::Mul => lhs * rhs,
                        ArithmeticOp::Exp => todo!(),
                        ArithmeticOp::Mod => Expression::Mod(Box::new((lhs, rhs))),
                    };
                    vec![new_expr]
                }
                BinaryOp::Relational(rel_bin) => {
                    // Type inference is not possible as multiple types are possible
                    let lhs = expression(bin.lhs(), interner, vars, strings, None, omg_types)?;
                    if lhs.len() != 1 {
                        bail!("expression lhs does not support binary relational operator");
                    }
                    let lhs = lhs[0].clone();
                    let rhs = expression(bin.rhs(), interner, vars, strings, None, omg_types)?;
                    if rhs.len() != 1 {
                        bail!("expression rhs does not support binary relational operator");
                    }
                    let rhs = rhs[0].clone();
                    let new_expr = match rel_bin {
                        RelationalOp::Equal => Expression::Equal(Box::new((lhs, rhs))),
                        RelationalOp::NotEqual => Expression::Equal(Box::new((lhs, rhs))).not()?,
                        RelationalOp::GreaterThan => Expression::Greater(Box::new((lhs, rhs))),
                        RelationalOp::GreaterThanOrEqual => {
                            Expression::GreaterEq(Box::new((lhs, rhs)))
                        }
                        RelationalOp::LessThan => Expression::Less(Box::new((lhs, rhs))),
                        RelationalOp::LessThanOrEqual => Expression::LessEq(Box::new((lhs, rhs))),
                        _ => return Err(anyhow!("unimplemented operator")),
                    };
                    vec![new_expr]
                }
                BinaryOp::Logical(op) => {
                    let lhs = expression(
                        bin.lhs(),
                        interner,
                        vars,
                        strings,
                        Some(&OmgType::Base(OmgBaseType::Boolean)),
                        omg_types,
                    )?;
                    if lhs.len() != 1 {
                        bail!("expression lhs does not support binary logical operator");
                    }
                    let lhs = lhs[0].clone();
                    let rhs = expression(
                        bin.rhs(),
                        interner,
                        vars,
                        strings,
                        Some(&OmgType::Base(OmgBaseType::Boolean)),
                        omg_types,
                    )?;
                    if rhs.len() != 1 {
                        bail!("expression rhs does not support binary logical operator");
                    }
                    let rhs = rhs[0].clone();
                    let new_expr = match op {
                        LogicalOp::And => Expression::and(vec![lhs, rhs])?,
                        LogicalOp::Or => Expression::or(vec![lhs, rhs])?,
                        _ => return Err(anyhow!("unimplemented operator")),
                    };
                    vec![new_expr]
                }
                BinaryOp::Comma => todo!(),
                _ => return Err(anyhow!("unimplemented operator")),
            }
        }
        boa_ast::Expression::Conditional(_) => todo!(),
        boa_ast::Expression::Parenthesized(par) => expression(
            par.expression(),
            interner,
            vars,
            strings,
            expr_type,
            omg_types,
        )?,
        boa_ast::Expression::Call(call) => {
            let fun = call.function();
            let args = call.args();
            if let boa_ast::Expression::PropertyAccess(
                boa_ast::expression::access::PropertyAccess::Simple(property_access),
            ) = fun
            {
                if let boa_ast::Expression::Identifier(target_id) = property_access.target() {
                    let target = target_id.to_interned_string(interner);
                    match property_access.field() {
                        boa_ast::expression::access::PropertyAccessField::Const(field_id) => {
                            let field = field_id.to_interned_string(interner);
                            if target == "Math" {
                                match field.as_str() {
                                    "random" => vec![Expression::RandFloat(0., 1.)],
                                    "floor" => {
                                        if let [arg] = args {
                                            let arg = expression(
                                                arg,
                                                interner,
                                                vars,
                                                strings,
                                                Some(&OmgType::Base(OmgBaseType::F64)),
                                                omg_types,
                                            )?;
                                            if arg.len() != 1 {
                                                bail!("expression does not support floor operator");
                                            }
                                            let arg = arg[0].clone();
                                            vec![Expression::Floor(Box::new(arg))]
                                        } else {
                                            bail!(
                                                "Math.floor() called with wrong number of arguments"
                                            );
                                        }
                                    }
                                    _ => bail!("unknown call"),
                                }
                            } else {
                                bail!("unknown call");
                            }
                        }
                        boa_ast::expression::access::PropertyAccessField::Expr(expression) => {
                            return Err(anyhow!("unimplemented expression {expression:?}"));
                        }
                    }
                } else {
                    return Err(anyhow!("unknown target"));
                }
            } else {
                return Err(anyhow!("unknown call"));
            }
        }
        _ => return Err(anyhow!("unimplemented expression")),
    };
    Ok(expr)
}
