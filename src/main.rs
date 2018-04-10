#[macro_use]
extern crate combine;

pub mod expr;

fn main() {
    let s = std::env::args().nth(1).unwrap();
    println!("input: {}", s);
    match expr::Expr::parse(s.as_str()) {
        Ok(expr) => println!("{:?}", expr),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
