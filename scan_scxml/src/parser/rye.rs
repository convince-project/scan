use anyhow::{anyhow, bail};
use chumsky::{IterParser, Parser, prelude::*, select};
use logos::Logos;
use scan_core::{Pmtl, Time};

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"[ \t\n]+")]
#[logos(error = String)]
pub enum Token {
    #[token("true")]
    #[token("True")]
    True,

    #[token("false")]
    #[token("False")]
    False,

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

    #[token("[")]
    BracketOpen,

    #[token("]")]
    BracketClose,

    // #[token(",")]
    // Comma,
    #[token(":")]
    Colon,

    #[regex("[0-9]+", |lex| lex.slice().parse::<usize>().unwrap())]
    Integer(usize),

    #[regex(r#"\{[^{}]*\}"#, |lex| lex.slice().strip_prefix("{").unwrap().strip_suffix("}").unwrap().trim().to_owned())]
    Predicate(String),
}

fn parser<'src>() -> impl Parser<'src, &'src [Token], Pmtl<String>, extra::Err<Simple<'src, Token>>>
{
    let integer = select! {
        Token::Integer(n) => n as u32,
    };

    let bounds = just(Token::BracketOpen)
        .ignore_then(integer.or_not().map(|p| p.unwrap_or(Time::MIN)))
        .then_ignore(just(Token::Colon))
        .then(integer.or_not().map(|p| p.unwrap_or(Time::MAX)))
        .then_ignore(just(Token::BracketClose));

    recursive(|p| {
        let atom = {
            let parenthesized = p
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen));

            let predicate = select! {
                Token::Predicate(pred) => Pmtl::Atom(pred),
                Token::True => Pmtl::True,
                Token::False => Pmtl::False,
            };

            parenthesized.or(predicate)
        };

        let unary = just(Token::Not)
            .or(just(Token::Once))
            .or(just(Token::Historically))
            .repeated()
            .foldr(atom, |op, rhs| match op {
                Token::Not => Pmtl::Not(Box::new(rhs)),
                Token::Once => Pmtl::Once(Box::new(rhs), Time::MIN, Time::MAX),
                Token::Historically => Pmtl::Historically(Box::new(rhs), Time::MIN, Time::MAX),
                _ => unreachable!(),
            });

        let temp_unary = just(Token::Once)
            .or(just(Token::Historically))
            .then(bounds.clone())
            .repeated()
            .foldr(unary, |(op, (l, u)), rhs| match op {
                Token::Once => Pmtl::Once(Box::new(rhs), l, u),
                Token::Historically => Pmtl::Historically(Box::new(rhs), l, u),
                _ => unreachable!(),
            });

        let binary = temp_unary.clone().foldl(
            just(Token::And)
                .or(just(Token::Or))
                .or(just(Token::Implies))
                .or(just(Token::Since))
                .then(temp_unary)
                .repeated(),
            |lhs, (op, rhs)| match op {
                Token::And => Pmtl::And(vec![lhs, rhs]),
                Token::Or => Pmtl::Or(vec![lhs, rhs]),
                Token::Implies => Pmtl::Implies(Box::new((lhs, rhs))),
                Token::Since => Pmtl::Since(Box::new((lhs, rhs)), Time::MIN, Time::MAX),
                _ => unreachable!(),
            },
        );

        binary.clone().foldl(
            just(Token::Since).then(bounds).then(binary).repeated(),
            |lhs, ((_op, (l, u)), rhs)| Pmtl::Since(Box::new((lhs, rhs)), l, u),
        )
    })
    .then_ignore(end())
}

pub fn parse(input: &str) -> anyhow::Result<Pmtl<String>> {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn not() {
        let not = parse("not { var > 10 }").expect("parse formula");
        assert!(matches!(not, Pmtl::Not(_)));
        if let Pmtl::Not(atom) = not {
            assert!(matches!(*atom, Pmtl::Atom(_)));
            if let Pmtl::Atom(pred) = *atom {
                assert_eq!(pred, "var > 10".to_string());
            } else {
                unreachable!();
            }
        } else {
            unreachable!();
        }
    }

    #[test]
    fn bounded_once() {
        let once = parse("P[0:1] { var > 10 }").expect("parse formula");
        assert!(matches!(once, Pmtl::Once(_, 0, 1)));
        if let Pmtl::Once(atom, 0, 1) = once {
            assert!(matches!(*atom, Pmtl::Atom(_)));
            if let Pmtl::Atom(pred) = *atom {
                assert_eq!(pred, "var > 10".to_string());
            } else {
                unreachable!();
            }
        }
    }

    #[test]
    fn low_bounded_once() {
        let once = parse("P[1:] { var > 10 }").expect("parse formula");
        assert!(matches!(once, Pmtl::Once(_, 1, Time::MAX)));
        if let Pmtl::Once(atom, 1, Time::MAX) = once {
            assert!(matches!(*atom, Pmtl::Atom(_)));
            if let Pmtl::Atom(pred) = *atom {
                assert_eq!(pred, "var > 10".to_string());
            } else {
                unreachable!();
            }
        }
    }

    #[test]
    fn bounded_historically() {
        let once = parse("H[0:1] { var > 10 }").expect("parse formula");
        assert!(matches!(once, Pmtl::Historically(_, 0, 1)));
        if let Pmtl::Historically(atom, 0, 1) = once {
            assert!(matches!(*atom, Pmtl::Atom(_)));
            if let Pmtl::Atom(pred) = *atom {
                assert_eq!(pred, "var > 10".to_string());
            } else {
                unreachable!();
            }
        }
    }

    #[test]
    fn up_bounded_historically() {
        let once = parse("H[:1] { var > 10 }").expect("parse formula");
        assert!(matches!(once, Pmtl::Historically(_, Time::MIN, 1)));
        if let Pmtl::Historically(atom, Time::MIN, 1) = once {
            assert!(matches!(*atom, Pmtl::Atom(_)));
            if let Pmtl::Atom(pred) = *atom {
                assert_eq!(pred, "var > 10".to_string());
            } else {
                unreachable!();
            }
        }
    }

    #[test]
    fn unbounded_once() {
        let once = parse("P { var > 10 }").expect("parse formula");
        assert!(matches!(once, Pmtl::Once(_, Time::MIN, Time::MAX)));
        if let Pmtl::Once(atom, Time::MIN, Time::MAX) = once {
            assert!(matches!(*atom, Pmtl::Atom(_)));
            if let Pmtl::Atom(pred) = *atom {
                assert_eq!(pred, "var > 10".to_string());
            } else {
                unreachable!();
            }
        }
    }

    #[test]
    fn unbounded_historically() {
        let once = parse("H { var > 10 }").expect("parse formula");
        assert!(matches!(once, Pmtl::Historically(_, Time::MIN, Time::MAX)));
        if let Pmtl::Historically(atom, Time::MIN, Time::MAX) = once {
            assert!(matches!(*atom, Pmtl::Atom(_)));
            if let Pmtl::Atom(pred) = *atom {
                assert_eq!(pred, "var > 10".to_string());
            } else {
                unreachable!();
            }
        }
    }

    #[test]
    fn historically_once() {
        let historically = parse("H[0:1] P[2:3] { var > 10 }").expect("parse formula");
        assert!(matches!(historically, Pmtl::Historically(_, 0, 1)));
        if let Pmtl::Historically(once, 0, 1) = historically {
            assert!(matches!(*once, Pmtl::Once(_, 2, 3)));
        } else {
            unreachable!();
        }
    }

    #[test]
    fn once_historically() {
        let once = parse("P[2:3] H[0:1] { var > 10 }").expect("parse formula");
        assert!(matches!(once, Pmtl::Once(_, 2, 3)));
        if let Pmtl::Once(historically, 2, 3) = once {
            assert!(matches!(*historically, Pmtl::Historically(_, 0, 1)));
        } else {
            unreachable!();
        }
    }

    #[test]
    fn since() {
        let since =
            parse("not { var > 10 } since[2:10] { other_var == 1 }").expect("parse formula");
        assert!(matches!(since, Pmtl::Since(_, 2, 10)));
        if let Pmtl::Since(args, _, _) = since {
            let (lhs, rhs) = *args;
            assert!(matches!(lhs, Pmtl::Not(_)));
            assert!(matches!(rhs, Pmtl::Atom(_)));
        }
    }
}
