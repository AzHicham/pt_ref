use combine::parser::char::{char, digit, letter, spaces, string};
use combine::parser::combinator::recognize;
use combine::{
    between, choice, easy, eof, many, many1, none_of, one_of, sep_by, skip_many, stream,
    EasyParser, ParseError, Parser, Stream, StreamOnce,
};
use expr::stream::position::SourcePosition;
use serde_json::ser::State;

pub type Error<'a> = easy::Errors<char, &'a str, SourcePosition>;

pub fn parse(s: &str) -> Result<ToObject, Error> {
    spaces()
        .with(to_object().skip(eof()))
        .easy_parse(State::new(s))
        .map(|res| res.0)
}

#[derive(EnumString, Debug, PartialEq, Display, Copy, Clone)]
pub enum Object {
    #[strum(serialize = "contributor")]
    Contributor,
    #[strum(serialize = "dataset")]
    Dataset,
    #[strum(serialize = "network")]
    Network,
    #[strum(serialize = "commercial_mode")]
    CommercialMode,
    #[strum(serialize = "line")]
    Line,
    #[strum(serialize = "route")]
    Route,
    #[strum(serialize = "vehicle_journey")]
    VehicleJourney,
    #[strum(serialize = "physical_mode")]
    PhysicalMode,
    #[strum(serialize = "stop_area")]
    StopArea,
    #[strum(serialize = "stop_point")]
    StopPoint,
    #[strum(serialize = "company")]
    Company,
    #[strum(serialize = "connection")]
    Connection,
}

#[derive(Debug, PartialEq)]
pub struct ToObject {
    pub object: Object,
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
    pub fn to_object<T: Into<Self>>(object: Object, expr: T) -> Self {
        Expr::ToObject(Box::new(ToObject {
            object: object,
            expr: expr.into(),
        }))
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
    pub obj: Object,
    pub method: String,
    pub args: Vec<String>,
}
impl Fun {
    pub fn new(obj: Object, method: &str, args: &[&str]) -> Self {
        Fun {
            obj: obj,
            method: method.into(),
            args: args.iter().map(|s| s.to_string()).collect(),
        }
    }
}
impl ::std::fmt::Display for Fun {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        let args = self
            .args
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{:?}.{}({})", self.obj, self.method, args)
    }
}

fn lex<I>(p: I) -> impl Parser<I, Output>
where
    I: Stream<Token = char>,
{
    p.skip(spaces())
}

fn to_object<I>() -> impl Parser<I, Output = ToObject>
where
    I: Stream<Token = char>,
    I::Error: ParseError<I, I::Range, I::Position>,
{
    (lex(string("get")), object(), lex(string("<-")), expr()).map(|t| ToObject {
        object: t.1,
        expr: t.3,
    })
}

fn expr_leaf<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
    I::Error: ParseError<I, I::Range, I::Position>,
{
    choice((
        between(lex(char('(')), lex(char(')')), expr()),
        pred().map(Expr::Pred),
        to_object().map(|o| Expr::ToObject(Box::new(o))),
    ))
}
parser! {
    fn expr_diff[I]()(I) -> Expr where [I: Stream]
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
parser! {
    fn expr_and[I]()(I) -> Expr where [I: Stream]
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
parser! {
    fn expr_or[I]()(I) -> Expr where [I: Stream]
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
parser! {
    fn expr[I]()(I) -> Expr where [I: Stream]
    {
        choice((
            to_object().map(|t| Expr::ToObject(Box::new(t))),
            expr_or(),
        ))
    }
}

fn pred<I>() -> impl Parser<I, Output = Pred>
where
    I: Stream,
    I::Error: ParseError<I, I::Range, I::Position>,
{
    choice((
        lex(string("all")).map(|_| Pred::All),
        lex(string("empty")).map(|_| Pred::Empty),
        fun().map(Pred::Fun),
    ))
}

fn fun<I>() -> impl Parser<I, Output = Fun>
where
    I: Stream,
    I::Error: ParseError<I, I::Range, I::Position>,
{
    (
        object(),
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
    )
        .map(|t| Fun {
            obj: t.0,
            method: t.2,
            args: t.3,
        })
}

fn my_str<I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = char>,
    I::Error: ParseError<I, I::Range, I::Position>,
{
    quoted_str().or(lazy_str())
}

fn lazy_str<I>() -> impl Parser<I, Output = String>
where
    I: Stream,
    I::Error: ParseError<I, I::Range, I::Position>,
{
    lex(many1(choice((
        letter(),
        digit(),
        one_of("_-.:;<>=|".chars()),
    ))))
}

fn object<I>() -> impl Parser<I, Output = Object>
where
    I: Stream,
    I::Error: ParseError<I, I::Range, I::Position>,
{
    lex(choice((
        string("contributor"),
        string("dataset"),
        string("network"),
        string("commercial_mode"),
        string("line"),
        string("route"),
        string("vehicle_journey"),
        string("physical_mode"),
        string("stop_area"),
        string("stop_point"),
        string("company"),
        string("connection"),
    )))
    .map(|s| s.parse().unwrap())
}

fn ident<I>() -> impl Parser<I, Output = String>
where
    I: Stream,
    I::Error: ParseError<I, I::Range, I::Position>,
{
    lex(recognize((
        letter(),
        skip_many(choice((letter(), digit(), one_of("_:".chars())))),
    )))
}

fn quoted_str<I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = char>,
    I::Error: ParseError<I, I::Range, I::Position>,
{
    between(
        char('"'),
        lex(char('"')),
        many(choice((
            none_of("\\\"".chars()),
            char('\\').with(one_of("\\\"".chars())),
        ))),
    )
}

#[cfg(test)]
mod test {
    use super::Object::*;
    use super::*;

    #[test]
    fn test_object() {
        assert_eq!(object().easy_parse("contributor "), Ok((Contributor, "")));
        assert_eq!(object().easy_parse("dataset "), Ok((Dataset, "")));
        assert_eq!(object().easy_parse("network "), Ok((Network, "")));
        assert_eq!(
            object().easy_parse("commercial_mode "),
            Ok((CommercialMode, ""))
        );
        assert_eq!(object().easy_parse("line "), Ok((Line, "")));
        assert_eq!(object().easy_parse("route "), Ok((Route, "")));
        assert_eq!(
            object().easy_parse("vehicle_journey "),
            Ok((VehicleJourney, ""))
        );
        assert_eq!(
            object().easy_parse("physical_mode "),
            Ok((PhysicalMode, ""))
        );
        assert_eq!(object().easy_parse("stop_area "), Ok((StopArea, "")));
        assert_eq!(object().easy_parse("stop_point "), Ok((StopPoint, "")));
        assert_eq!(object().easy_parse("company "), Ok((Company, "")));
        assert_eq!(object().easy_parse("connection "), Ok((Connection, "")));
        assert!(object().easy_parse("commerical_mode ").is_err());
    }

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
        assert_eq!(
            quoted_str().easy_parse(r#""  ""#),
            Ok(("  ".to_string(), ""))
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
            fun().easy_parse("line . a ( ) "),
            Ok((Fun::new(Line, "a", &[]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#"vehicle_journey . has_code ( external_code , "OIF:42" ) "#),
            Ok((
                Fun::new(VehicleJourney, "has_code", &["external_code", "OIF:42"]),
                ""
            ))
        );
        assert_eq!(
            fun().easy_parse(r#"stop_area . uri ( "OIF:42" ) "#),
            Ok((Fun::new(StopArea, "uri", &["OIF:42"]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#"stop_area . uri = "OIF:42" "#),
            Ok((Fun::new(StopArea, "uri", &["OIF:42"]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#"stop_area . uri = OIF:42 "#),
            Ok((Fun::new(StopArea, "uri", &["OIF:42"]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#"stop_area . uri = foo "#),
            Ok((Fun::new(StopArea, "uri", &["foo"]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#"stop_area . uri = 42 "#),
            Ok((Fun::new(StopArea, "uri", &["42"]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#"stop_area . uri = 1ee7_: "#),
            Ok((Fun::new(StopArea, "uri", &["1ee7_:"]), ""))
        );
        assert_eq!(
            fun().easy_parse(r#"stop_point.within(42, -2.2;4.9e-2)"#),
            Ok((Fun::new(StopPoint, "within", &["42", "-2.2;4.9e-2"]), ""))
        );
    }

    #[test]
    fn test_pred() {
        assert_eq!(pred().easy_parse("all "), Ok((Pred::All, "")));
        assert_eq!(pred().easy_parse("empty "), Ok((Pred::Empty, "")));
        assert_eq!(
            pred().easy_parse("line . a ( ) "),
            Ok((Pred::Fun(Fun::new(Line, "a", &[])), ""))
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
            expr().easy_parse("line . a ( ) "),
            Ok((Expr::Pred(Pred::Fun(Fun::new(Line, "a", &[]))), ""))
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

    #[test]
    fn test_to_object_expr() {
        use self::Pred::*;

        assert_eq!(
            expr().easy_parse("get line <- all "),
            Ok((Expr::to_object(Line, All), "")),
        );
        assert_eq!(
            expr().easy_parse("get line <- get stop_area <- all "),
            Ok((Expr::to_object(Line, Expr::to_object(StopArea, All)), "")),
        );
        assert_eq!(
            expr().easy_parse("get line <- empty and get stop_area <- all "),
            Ok((
                Expr::to_object(Line, Expr::and(Empty, Expr::to_object(StopArea, All))),
                ""
            )),
        );
        assert!(expr().easy_parse("get line <- foo").is_err());
    }
}
