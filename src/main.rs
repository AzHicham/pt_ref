#[macro_use]
extern crate combine;

use combine::parser::char::{string, letter, char, digit, spaces};
use combine::{Parser, Stream, skip_many, sep_by, none_of, one_of, many};
use combine::combinator::recognize;

#[derive(Debug)]
enum Expr {
    Pred(Pred),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Diff(Box<Expr>, Box<Expr>),
}
#[derive(Debug)]
enum Pred {
    All,
    None,
    Fun(Fun)
}
#[derive(Debug)]
struct Fun {
    obj: String,
    method: String,
    args: Vec<String>,
}

parser!{
    fn pred[I]()(I) -> Pred where [I: Stream<Item=char>]
    {
        spaces()
            .with(
                string("all").map(|_| Pred::All)
                    .or(string("none").map(|_| Pred::None))
                    .or(fun().map(Pred::Fun))
            )
            .skip(spaces())
    }
}
parser!{
    fn fun[I]()(I) -> Fun where [I: Stream<Item=char>]
    {
        (
            ident(),
            char('.'),
            ident(),
            char('('),
            sep_by(quoted_str(), char(',')),
            char(')')
        ).map(|t| Fun {
            obj: t.0,
            method: t.2,
            args: t.4,
        })
    }
}
parser!{
    fn ident[I]()(I) -> String where [I: Stream<Item=char>]
    {
        spaces()
            .with(
                recognize((
                    letter(),
                    skip_many(letter().or(digit()).or(char('_')))
                ))
            )
            .skip(spaces())
    }
}
parser!{
    fn quoted_str[I]()(I) -> String where [I: Stream<Item=char>]
    {
        spaces()
            .skip(char('"'))
            .with(
                many(
                    none_of("\\\"".chars())
                        .or(char('\\').with(one_of("\\\"".chars())))
                )
            )
            .skip(char('"'))
            .skip(spaces())
    }
}

fn main() {
    let s = std::env::args().nth(1).unwrap();
    println!("input: {}", s);
    match pred().easy_parse(s.as_str()) {
        Ok(expr) => println!("{:?}", expr),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
