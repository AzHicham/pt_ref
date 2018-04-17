use combine::combinator::recognize;
use combine::parser::char::*;
use combine::stream::state::State;
use combine::*;

pub type Error<'a> = easy::Errors<char, &'a str, stream::state::SourcePosition>;

pub fn parse<'a>(s: &'a str) -> Result<ToObject, Error<'a>> {
    spaces()
        .with(to_object().skip(eof()))
        .easy_parse(State::new(s))
        .map(|res| res.0)
}

#[derive(Debug, PartialEq)]
pub struct ToObject {
    pub object: String,
    pub expr: Expr,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Pred(Pred),
    ToObject(Box<ToObject>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Diff(Box<Expr>, Box<Expr>),
}
impl Expr {
    pub fn and<L: Into<Self>, R: Into<Self>>(lhs: L, rhs: R) -> Self {
        Expr::And(Box::new(lhs.into()), Box::new(rhs.into()))
    }
    pub fn or<L: Into<Self>, R: Into<Self>>(lhs: L, rhs: R) -> Self {
        Expr::Or(Box::new(lhs.into()), Box::new(rhs.into()))
    }
    pub fn diff<L: Into<Self>, R: Into<Self>>(lhs: L, rhs: R) -> Self {
        Expr::Diff(Box::new(lhs.into()), Box::new(rhs.into()))
    }
}

#[derive(Debug, PartialEq)]
pub enum Pred {
    All,
    Empty,
    Fun(Fun),
}
impl From<Pred> for Expr {
    fn from(other: Pred) -> Self {
        Expr::Pred(other)
    }
}

#[derive(Debug, PartialEq)]
pub struct Fun {
    pub obj: String,
    pub method: String,
    pub args: Vec<String>,
}
impl Fun {
    pub fn new(obj: &str, method: &str, args: &[&str]) -> Self {
        Fun {
            obj: obj.into(),
            method: method.into(),
            args: args.iter().map(|s| s.to_string()).collect(),
        }
    }
}
impl ::std::fmt::Display for Fun {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        let args = self.args
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}.{}({})", self.obj, self.method, args)
    }
}

fn lex<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    p.skip(spaces())
}

fn to_object<I>() -> impl Parser<Input = I, Output = ToObject>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (lex(string("get")), ident(), lex(string("<-")), expr()).map(|t| ToObject {
        object: t.1,
        expr: t.3,
    })
}

fn expr_leaf<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        between(lex(char('(')), lex(char(')')), expr()),
        pred().map(Expr::Pred),
    ))
}
parser!{
    fn expr_diff[I]()(I) -> Expr where [I: Stream<Item=char>]
    {
        (
            expr_leaf(),
            optional(lex(string("-")).with(expr_diff()))
        ).map(|e| match e {
            (e, None) => e,
            (lhs, Some(rhs)) => Expr::diff(lhs, rhs),
        })
    }
}
parser!{
    fn expr_and[I]()(I) -> Expr where [I: Stream<Item=char>]
    {
        (
            expr_diff(),
            optional(lex(string("AND").or(string("and"))).with(expr_and()))
        ).map(|e| match e {
            (e, None) => e,
            (lhs, Some(rhs)) => Expr::and(lhs, rhs),
        })
    }
}
parser!{
    fn expr_or[I]()(I) -> Expr where [I: Stream<Item=char>]
    {
        (
            expr_and(),
            optional(lex(string("OR").or(string("or"))).with(expr_or()))
        ).map(|e| match e {
            (e, None) => e,
            (lhs, Some(rhs)) => Expr::or(lhs, rhs),
        })
    }
}
parser!{
    fn expr[I]()(I) -> Expr where [I: Stream<Item=char>]
    {
        choice((
            to_object().map(|t| Expr::ToObject(Box::new(t))),
            expr_or(),
        ))
    }
}

fn pred<I>() -> impl Parser<Input = I, Output = Pred>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        lex(string("all")).map(|_| Pred::All),
        lex(string("empty")).map(|_| Pred::Empty),
        fun().map(Pred::Fun),
    ))
}
fn fun<I>() -> impl Parser<Input = I, Output = Fun>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        ident(),
        lex(char('.')),
        ident(),
        choice((
            between(
                lex(char('(')),
                lex(char(')')),
                sep_by(my_str(), lex(char(','))),
            ),
            lex(char('=')).with(my_str()).map(|s| vec![s]),
        )),
    ).map(|t| Fun {
            obj: t.0,
            method: t.2,
            args: t.3,
        })
        .skip(spaces())
}
parser!{
    fn my_str[I]()(I) -> String where [I: Stream<Item = char>] {
        quoted_str().or(ident())
    }
}
parser!{
    fn ident[I]()(I) -> String where [I: Stream<Item = char>]
    {
        lex(recognize((
            letter(),
            skip_many(choice((
                letter(),
                digit(),
                one_of("_:".chars()),
            ))),
        )))
    }
}
parser!{
    fn quoted_str[I]()(I) -> String where [I: Stream<Item = char>]
    {
        between(
            lex(char('"')),
            lex(char('"')),
            many(choice((
                none_of("\\\"".chars()),
                char('\\').with(one_of("\\\"".chars())),
            )))
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_quoted_str() {
        assert_eq!(quoted_str().easy_parse(r#""""#), Ok(("".to_string(), "")));
        assert_eq!(
            quoted_str().easy_parse(r#""foo"  "#),
            Ok(("foo".to_string(), ""))
        );
        assert_eq!(
            quoted_str().easy_parse(r#""\""   "#),
            Ok(("\"".to_string(), ""))
        );
        assert_eq!(
            quoted_str().easy_parse(r#""\\"   "#),
            Ok(("\\".to_string(), ""))
        );
        assert!(quoted_str().easy_parse(r#""\a""#).is_err());
        assert!(quoted_str().easy_parse(r#"""#).is_err());
        assert!(quoted_str().easy_parse(r#" "#).is_err());
        assert!(quoted_str().easy_parse(r#""#).is_err());
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident().easy_parse("foo "), Ok(("foo".to_string(), "")));
        assert_eq!(
            ident().easy_parse("stop_area "),
            Ok(("stop_area".to_string(), ""))
        );
        assert_eq!(ident().easy_parse("e1337 "), Ok(("e1337".to_string(), "")));
        assert_eq!(
            ident().easy_parse("OIF:42_42 "),
            Ok(("OIF:42_42".to_string(), ""))
        );
        assert!(ident().easy_parse("").is_err());
        assert!(ident().easy_parse("1").is_err());
        assert!(ident().easy_parse("=").is_err());
    }

    #[test]
    fn test_fun() {
        assert_eq!(
            fun().easy_parse("f . a ( ) "),
            Ok((Fun::new("f", "a", &[]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#"vehicle_journey . has_code ( external_code , "OIF:42" ) "#),
            Ok((
                Fun::new("vehicle_journey", "has_code", &["external_code", "OIF:42"]),
                ""
            ))
        );
        assert_eq!(
            fun().easy_parse(r#"stop_area . uri ( "OIF:42" ) "#),
            Ok((Fun::new("stop_area", "uri", &["OIF:42"]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#"stop_area . uri = "OIF:42""#),
            Ok((Fun::new("stop_area", "uri", &["OIF:42"]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#"stop_area . uri = foo"#),
            Ok((Fun::new("stop_area", "uri", &["foo"]), ""))
        );
    }

    #[test]
    fn test_pred() {
        assert_eq!(pred().easy_parse("all "), Ok((Pred::All, "")));
        assert_eq!(pred().easy_parse("empty "), Ok((Pred::Empty, "")));
        assert_eq!(
            pred().easy_parse("f . a ( ) "),
            Ok((Pred::Fun(Fun::new("f", "a", &[])), ""))
        );
    }

    #[test]
    fn test_basic_expr() {
        assert_eq!(expr().easy_parse("all "), Ok((Expr::Pred(Pred::All), "")));
        assert_eq!(
            expr().easy_parse("empty "),
            Ok((Expr::Pred(Pred::Empty), ""))
        );
        assert_eq!(
            expr().easy_parse("f . a ( ) "),
            Ok((Expr::Pred(Pred::Fun(Fun::new("f", "a", &[]))), ""))
        );
    }

    #[test]
    fn test_and_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse("all and empty "),
            Ok((Expr::and(All, Empty), ""))
        );
        assert_eq!(
            expr().easy_parse("all and empty and all and empty "),
            Ok((Expr::and(All, Expr::and(Empty, Expr::and(All, Empty))), ""))
        );
    }

    #[test]
    fn test_or_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse("all or empty "),
            Ok((Expr::or(All, Empty), ""))
        );
        assert_eq!(
            expr().easy_parse("all or empty or all or empty "),
            Ok((Expr::or(All, Expr::or(Empty, Expr::or(All, Empty))), ""))
        );
    }

    #[test]
    fn test_diff_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse("all - empty "),
            Ok((Expr::diff(All, Empty), ""))
        );
        assert_eq!(
            expr().easy_parse("all - empty - all - empty "),
            Ok((
                Expr::diff(All, Expr::diff(Empty, Expr::diff(All, Empty))),
                ""
            ))
        );
    }

    #[test]
    fn test_and_or_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse("all and all or empty and all "),
            Ok((Expr::or(Expr::and(All, All), Expr::and(Empty, All)), ""))
        );
        assert_eq!(
            expr().easy_parse("all and all or empty and all or empty and all "),
            Ok((
                Expr::or(
                    Expr::and(All, All),
                    Expr::or(Expr::and(Empty, All), Expr::and(Empty, All))
                ),
                ""
            ))
        );
    }

    #[test]
    fn test_and_diff_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse("all - all and empty - all "),
            Ok((Expr::and(Expr::diff(All, All), Expr::diff(Empty, All)), ""))
        );
    }

    #[test]
    fn test_or_diff_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse("all - all or empty - all "),
            Ok((Expr::or(Expr::diff(All, All), Expr::diff(Empty, All)), ""))
        );
    }

    #[test]
    fn test_and_or_diff_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse("all - all and empty - empty or empty - all "),
            Ok((
                Expr::or(
                    Expr::and(Expr::diff(All, All), Expr::diff(Empty, Empty)),
                    Expr::diff(Empty, All)
                ),
                ""
            ))
        );
    }

    #[test]
    fn test_parenthesis_expr() {
        use self::Pred::*;

        assert_eq!(expr().easy_parse("( all ) "), Ok((All.into(), "")));
        assert_eq!(
            expr().easy_parse("( all and all ) - ( empty and all ) "),
            Ok((Expr::diff(Expr::and(All, All), Expr::and(Empty, All)), ""))
        );
        assert_eq!(
            expr().easy_parse("( all and all ) - ( empty or all ) "),
            Ok((Expr::diff(Expr::and(All, All), Expr::or(Empty, All)), ""))
        );
    }
}
