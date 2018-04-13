use combine::combinator::recognize;
use combine::parser::char::*;
use combine::stream::state::State;
use combine::*;

pub type Error<'a> = easy::Errors<char, &'a str, stream::state::SourcePosition>;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Pred(Pred),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Diff(Box<Expr>, Box<Expr>),
}
impl Expr {
    pub fn parse(s: &str) -> Result<Self, Error> {
        expr()
            .skip(eof())
            .easy_parse(State::new(s))
            .map(|res| res.0)
    }
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
    None,
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

fn expr_leaf<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces()
        .with(choice((
            between(char('('), char(')'), expr()),
            pred().map(Expr::Pred),
        )))
        .skip(spaces())
}
parser!{
    fn expr_diff[I]()(I) -> Expr where [I: Stream<Item=char>]
    {
        (
            expr_leaf(),
            optional(string("-").with(expr_diff()))
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
            optional(string("AND").or(string("and")).with(expr_and()))
        ).map(|e| match e {
            (e, None) => e,
            (lhs, Some(rhs)) => Expr::and(lhs, rhs),
        })
    }
}
parser!{
    fn expr[I]()(I) -> Expr where [I: Stream<Item=char>]
    {
        (
            expr_and(),
            optional(string("OR").or(string("or")).with(expr()))
        ).map(|e| match e {
            (e, None) => e,
            (lhs, Some(rhs)) => Expr::or(lhs, rhs),
        })
    }
}

fn pred<I>() -> impl Parser<Input = I, Output = Pred>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces()
        .with(choice((
            string("all").map(|_| Pred::All),
            string("none").map(|_| Pred::None),
            fun().map(Pred::Fun),
        )))
        .skip(spaces())
}
fn fun<I>() -> impl Parser<Input = I, Output = Fun>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        ident(),
        char('.'),
        ident(),
        choice((
            between(
                char('('),
                char(')'),
                spaces().with(sep_by(spaces().with(str()), char(','))),
            ),
            token('=').skip(spaces()).with(str()).map(|s| vec![s]),
        )),
    ).map(|t| Fun {
            obj: t.0,
            method: t.2,
            args: t.3,
        })
        .skip(spaces())
}
fn str<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    quoted_str().or(ident())
}
fn ident<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces()
        .with(recognize((
            letter(),
            skip_many(letter().or(digit()).or(char('_'))),
        )))
        .skip(spaces())
}
parser!{
    fn quoted_str[I]()(I) -> String where [I: Stream<Item = char>]
    {
        spaces()
            .skip(char('"'))
            .with(
                many(choice((
                    none_of("\\\"".chars()),
                    char('\\').with(one_of("\\\"".chars())),
                )))
            )
            .skip(char('"'))
            .skip(spaces())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_quoted_str() {
        assert_eq!(quoted_str().easy_parse(r#""""#), Ok(("".to_string(), "")));
        assert_eq!(
            quoted_str().easy_parse(r#"  "foo"  "#),
            Ok(("foo".to_string(), ""))
        );
        assert_eq!(
            quoted_str().easy_parse(r#"  "\""   "#),
            Ok(("\"".to_string(), ""))
        );
        assert_eq!(
            quoted_str().easy_parse(r#"  "\\"   "#),
            Ok(("\\".to_string(), ""))
        );
        assert!(quoted_str().easy_parse(r#""\a""#).is_err());
        assert!(quoted_str().easy_parse(r#"""#).is_err());
        assert!(quoted_str().easy_parse(r#" "#).is_err());
        assert!(quoted_str().easy_parse(r#""#).is_err());
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident().easy_parse(" foo "), Ok(("foo".to_string(), "")));
        assert_eq!(
            ident().easy_parse(" stop_area "),
            Ok(("stop_area".to_string(), ""))
        );
        assert_eq!(ident().easy_parse(" e1337 "), Ok(("e1337".to_string(), "")));
        assert!(ident().easy_parse("").is_err());
        assert!(ident().easy_parse("1").is_err());
        assert!(ident().easy_parse("=").is_err());
    }

    #[test]
    fn test_fun() {
        assert_eq!(
            fun().easy_parse(" f . a ( ) "),
            Ok((Fun::new("f", "a", &[]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#" vehicle_journey . has_code ( external_code , "OIF:42" ) "#),
            Ok((
                Fun::new("vehicle_journey", "has_code", &["external_code", "OIF:42"]),
                ""
            ))
        );
        assert_eq!(
            fun().easy_parse(r#" stop_area . uri ( "OIF:42" ) "#),
            Ok((Fun::new("stop_area", "uri", &["OIF:42"]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#" stop_area . uri = "OIF:42""#),
            Ok((Fun::new("stop_area", "uri", &["OIF:42"]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#" stop_area . uri = foo"#),
            Ok((Fun::new("stop_area", "uri", &["foo"]), ""))
        );
    }

    #[test]
    fn test_pred() {
        assert_eq!(pred().easy_parse(" all "), Ok((Pred::All, "")));
        assert_eq!(pred().easy_parse(" none "), Ok((Pred::None, "")));
        assert_eq!(
            pred().easy_parse(" f . a ( ) "),
            Ok((Pred::Fun(Fun::new("f", "a", &[])), ""))
        );
    }

    #[test]
    fn test_basic_expr() {
        assert_eq!(expr().easy_parse(" all "), Ok((Expr::Pred(Pred::All), "")));
        assert_eq!(
            expr().easy_parse(" none "),
            Ok((Expr::Pred(Pred::None), ""))
        );
        assert_eq!(
            expr().easy_parse(" f . a ( ) "),
            Ok((Expr::Pred(Pred::Fun(Fun::new("f", "a", &[]))), ""))
        );
    }

    #[test]
    fn test_and_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse(" all and none "),
            Ok((Expr::and(All, None), ""))
        );
        assert_eq!(
            expr().easy_parse(" all and none and all and none "),
            Ok((Expr::and(All, Expr::and(None, Expr::and(All, None))), ""))
        );
    }

    #[test]
    fn test_or_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse(" all or none "),
            Ok((Expr::or(All, None), ""))
        );
        assert_eq!(
            expr().easy_parse(" all or none or all or none "),
            Ok((Expr::or(All, Expr::or(None, Expr::or(All, None))), ""))
        );
    }

    #[test]
    fn test_diff_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse(" all - none "),
            Ok((Expr::diff(All, None), ""))
        );
        assert_eq!(
            expr().easy_parse(" all - none - all - none "),
            Ok((Expr::diff(All, Expr::diff(None, Expr::diff(All, None))), ""))
        );
    }

    #[test]
    fn test_and_or_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse(" all and all or none and all "),
            Ok((Expr::or(Expr::and(All, All), Expr::and(None, All)), ""))
        );
        assert_eq!(
            expr().easy_parse(" all and all or none and all or none and all "),
            Ok((
                Expr::or(
                    Expr::and(All, All),
                    Expr::or(Expr::and(None, All), Expr::and(None, All))
                ),
                ""
            ))
        );
    }

    #[test]
    fn test_and_diff_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse(" all - all and none - all "),
            Ok((Expr::and(Expr::diff(All, All), Expr::diff(None, All)), ""))
        );
    }

    #[test]
    fn test_or_diff_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse(" all - all or none - all "),
            Ok((Expr::or(Expr::diff(All, All), Expr::diff(None, All)), ""))
        );
    }

    #[test]
    fn test_and_or_diff_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse(" all - all and none - none or none - all "),
            Ok((
                Expr::or(
                    Expr::and(Expr::diff(All, All), Expr::diff(None, None)),
                    Expr::diff(None, All)
                ),
                ""
            ))
        );
    }

    #[test]
    fn test_parenthesis_expr() {
        use self::Pred::*;

        assert_eq!(expr().easy_parse(" ( all ) "), Ok((All.into(), "")));
        assert_eq!(
            expr().easy_parse(" ( all and all ) - ( none and all ) "),
            Ok((Expr::diff(Expr::and(All, All), Expr::and(None, All)), ""))
        );
        assert_eq!(
            expr().easy_parse(" ( all and all ) - ( none or all ) "),
            Ok((Expr::diff(Expr::and(All, All), Expr::or(None, All)), ""))
        );
    }
}
