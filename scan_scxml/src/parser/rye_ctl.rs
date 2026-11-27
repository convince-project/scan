use std::ops::Bound;

use anyhow::{anyhow, bail};
use chumsky::{IterParser, Parser, prelude::*, select};
use logos::Logos;
use scan_core::{Time, TimeRange};
use scan_ctl::{Ctlp, PathCtl};

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"[ \t\n]+")]
#[logos(error = String)]
enum Token {
    #[token("true")]
    #[token("True")]
    True,

    #[token("false")]
    #[token("False")]
    False,

    #[token("A")]
    #[token("all")]
    All,

    #[token("E")]
    #[token("exist")]
    Exist,

    #[token("P")]
    #[token("once")]
    Once,

    #[token("H")]
    #[token("historically")]
    Historically,

    #[token("S")]
    #[token("since")]
    Since,

    #[token("&&")]
    #[token("and")]
    And,

    #[token("||")]
    #[token("or")]
    Or,

    #[token("!")]
    #[token("not")]
    Not,

    #[token("->")]
    #[token("implies")]
    Implies,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[regex(r#"\[[0-9]*:[0-9]*\]"#, |lex| {
        let (lhs, rhs) = lex.slice().strip_circumfix("[", "]").unwrap().split_once(":").unwrap();
        let lb = lhs.parse::<Time>().map_or(Bound::Included(0), Bound::Included);
        let ub = rhs.parse::<Time>().map_or(Bound::Unbounded, Bound::Included);
        TimeRange::new((lb, ub))
    })]
    TimeBound(TimeRange),

    #[regex(r#"\{[^{}]*\}"#, |lex| lex.slice().strip_circumfix("{", "}").unwrap().trim().to_owned())]
    Predicate(String),
}

fn parser<'src>() -> impl Parser<'src, &'src [Token], Ctlp<String>, extra::Err<Simple<'src, Token>>>
{
    let mut ctl_state = Recursive::declare();

    let range = select! {
        Token::TimeBound(range) => range,
    };
    let once = just(Token::Once)
        .ignore_then(range.or_not())
        .then(ctl_state.clone())
        .map(|(bounds, ctl)| PathCtl::Once(Box::new(ctl), bounds.unwrap_or_default()))
        .labelled("once");
    let historically = just(Token::Historically)
        .ignore_then(range.or_not())
        .then(ctl_state.clone())
        .map(|(bounds, ctl)| PathCtl::Historically(Box::new(ctl), bounds.unwrap_or_default()))
        .labelled("historically");
    let since = ctl_state
        .clone()
        .then_ignore(just(Token::Since))
        .then(range.or_not())
        .then(ctl_state.clone())
        .map(|((lhs, range), rhs)| PathCtl::Since(Box::new((lhs, rhs)), range.unwrap_or_default()))
        .labelled("since");
    let ctl_path = chumsky::recursive::recursive(|ctl_path| {
        choice((once, historically, since))
            .or(ctl_path.delimited_by(just(Token::LParen), just(Token::RParen)))
    })
    .labelled("path formula");

    let all = just(Token::All)
        .ignore_then(ctl_path.clone())
        .map(Ctlp::All)
        .labelled("universal quantifier");
    let exists = just(Token::Exist)
        .ignore_then(ctl_path)
        .map(Ctlp::Exist)
        .labelled("existential quantifier");
    let atom = select! {
        Token::Predicate(pred) => Ctlp::Atom(pred),
        Token::True => Ctlp::True,
        Token::False => Ctlp::False,
    }
    .labelled("atomic predicate")
    .or(ctl_state
        .clone()
        .delimited_by(just(Token::LParen), just(Token::RParen)))
    .or(choice((all, exists)));
    let not = just(Token::Not)
        .ignored()
        .repeated()
        .at_least(1)
        .foldr(atom.clone(), |_, ctl| Ctlp::Not(Box::new(ctl)))
        .labelled("negation")
        .or(atom);
    let unary = not;
    let conjunction = unary
        .clone()
        .separated_by(just(Token::And))
        .at_least(2)
        .collect::<Box<[Ctlp<_>]>>()
        .map(Ctlp::And)
        .labelled("conjunction")
        .or(unary);
    let disjunction = conjunction
        .clone()
        .separated_by(just(Token::Or))
        .at_least(2)
        .collect::<Box<[Ctlp<_>]>>()
        .map(Ctlp::Or)
        .labelled("disjunction")
        .or(conjunction);
    let implication = disjunction
        .clone()
        .foldl(
            just(Token::Implies)
                .ignore_then(disjunction.clone())
                .repeated(),
            |lhs, rhs| Ctlp::Implies(Box::new((lhs, rhs))),
        )
        .labelled("implication")
        .or(disjunction);
    ctl_state.define({ implication.labelled("state formula") });

    ctl_state.then_ignore(end())
}

pub fn parse(input: &str) -> anyhow::Result<Ctlp<String>> {
    //creates a lexer instance from the input
    let lexer = Token::lexer(input);

    //splits the input into tokens, using the lexer
    let mut tokens = vec![];
    for (token, _span) in lexer.spanned() {
        match token {
            Ok(token) => tokens.push(token),
            Err(e) => {
                bail!(e);
            }
        }
    }

    //parses the tokens to construct an AST
    parser()
        .parse(&tokens)
        .into_result()
        .map_err(|_err| anyhow!("failed parsing Rye expression"))
}

// #[cfg(test)]
// mod test {
//     use scan_core::TimeRange;
//     use scan_ctl::*;

//     use super::parse;

//     #[test]
//     fn not() {
//         let not = parse("not { var > 10 }").expect("parse formula");
//         assert!(matches!(not, Ctl::Not(_)));
//         if let Ctl::Not(atom) = not {
//             assert!(matches!(*atom, Ctl::Atom(_)));
//             if let Ctl::Atom(pred) = *atom {
//                 assert_eq!(pred, "var > 10".to_string());
//             } else {
//                 unreachable!();
//             }
//         } else {
//             unreachable!();
//         }
//     }

//     #[test]
//     fn bounded_once() {
//         let once = parse("P[0:1] { var > 10 }").expect("parse formula");
//         assert!(matches!(once, Ctl::Once(..)));
//         if let Ctl::Once(atom, range) = once {
//             assert_eq!(range, TimeRange::new(0u32..=1));
//             assert!(matches!(*atom, Ctl::Atom(_)));
//             if let Ctl::Atom(pred) = *atom {
//                 assert_eq!(pred, "var > 10".to_string());
//             } else {
//                 unreachable!();
//             }
//         }
//     }

//     #[test]
//     fn low_bounded_once() {
//         let once = parse("P[1:] { var > 10 }").expect("parse formula");
//         assert!(matches!(once, Ctl::Once(..)));
//         if let Ctl::Once(atom, range) = once {
//             assert_eq!(range, TimeRange::new(1..));
//             assert!(matches!(*atom, Ctl::Atom(_)));
//             if let Ctl::Atom(pred) = *atom {
//                 assert_eq!(pred, "var > 10".to_string());
//             } else {
//                 unreachable!();
//             }
//         }
//     }

//     #[test]
//     fn bounded_historically() {
//         let once = parse("H[0:1] { var > 10 }").expect("parse formula");
//         assert!(matches!(once, Ctl::Historically(..)));
//         if let Ctl::Historically(atom, range) = once {
//             assert_eq!(range, TimeRange::new(0..=1));
//             assert!(matches!(*atom, Ctl::Atom(_)));
//             if let Ctl::Atom(pred) = *atom {
//                 assert_eq!(pred, "var > 10".to_string());
//             } else {
//                 unreachable!();
//             }
//         }
//     }

//     #[test]
//     fn up_bounded_historically() {
//         let once = parse("H[:1] { var > 10 }").expect("parse formula");
//         assert!(matches!(once, Ctl::Historically(..)));
//         if let Ctl::Historically(atom, range) = once {
//             assert_eq!(range, TimeRange::new(..=1));
//             assert!(matches!(*atom, Ctl::Atom(_)));
//             if let Ctl::Atom(pred) = *atom {
//                 assert_eq!(pred, "var > 10".to_string());
//             } else {
//                 unreachable!();
//             }
//         }
//     }

//     #[test]
//     fn unbounded_once() {
//         let once = parse("P { var > 10 }").expect("parse formula");
//         assert!(matches!(once, Ctl::Once(..)));
//         if let Ctl::Once(atom, range) = once {
//             assert_eq!(range, TimeRange::new(..));
//             assert!(matches!(*atom, Ctl::Atom(_)));
//             if let Ctl::Atom(pred) = *atom {
//                 assert_eq!(pred, "var > 10".to_string());
//             } else {
//                 unreachable!();
//             }
//         }
//     }

//     #[test]
//     fn unbounded_historically() {
//         let once = parse("H { var > 10 }").expect("parse formula");
//         assert!(matches!(once, Ctl::Historically(..)));
//         if let Ctl::Historically(atom, range) = once {
//             assert_eq!(range, TimeRange::new(..));
//             assert!(matches!(*atom, Ctl::Atom(_)));
//             if let Ctl::Atom(pred) = *atom {
//                 assert_eq!(pred, "var > 10".to_string());
//             } else {
//                 unreachable!();
//             }
//         }
//     }

//     #[test]
//     fn historically_once() {
//         let historically = parse("H[0:1] P[2:3] { var > 10 }").expect("parse formula");
//         assert!(matches!(historically, Ctl::Historically(..)));
//         if let Ctl::Historically(once, range) = historically {
//             assert_eq!(range, TimeRange::new(0..=1));
//             assert!(matches!(*once, Ctl::Once(..)));
//             if let Ctl::Once(_, range) = *once {
//                 assert_eq!(range, TimeRange::new(2..=3));
//             }
//         } else {
//             unreachable!();
//         }
//     }

//     #[test]
//     fn once_historically() {
//         let once = parse("P[2:3] H[:1] { var > 10 }").expect("parse formula");
//         assert!(matches!(once, Ctl::Once(..)));
//         if let Ctl::Once(historically, range) = once {
//             assert_eq!(range, TimeRange::new(2..=3));
//             assert!(matches!(*historically, Ctl::Historically(..)));
//             if let Ctl::Historically(_, range) = *historically {
//                 assert_eq!(range, TimeRange::new(..=1));
//             }
//         } else {
//             unreachable!();
//         }
//     }

//     #[test]
//     fn since() {
//         let since =
//             parse("not { var > 10 } since[2:10] { other_var == 1 }").expect("parse formula");
//         assert!(matches!(since, Ctl::Since(..)));
//         if let Ctl::Since(args, range) = since {
//             assert_eq!(range, TimeRange::new(2..=10));
//             let (lhs, rhs) = *args;
//             assert!(matches!(lhs, Ctl::Not(_)));
//             assert!(matches!(rhs, Ctl::Atom(_)));
//         }
//     }
// }
