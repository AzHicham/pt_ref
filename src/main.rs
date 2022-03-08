#![feature(specialization)]

#[macro_use]
extern crate combine;
#[macro_use]
extern crate failure;
extern crate humantime;
extern crate serde;
extern crate serde_json;
extern crate structopt;
extern crate strum;
extern crate transit_model;
extern crate typed_index_collection;
#[macro_use]
extern crate strum_macros;
extern crate relational_types;

use relational_types::IdxSet;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use structopt::StructOpt;
use typed_index_collection::Collection;

pub type Result<T> = ::std::result::Result<T, failure::Error>;

#[macro_use]
pub mod eval;
pub mod expr;

fn main() {
    if let Err(err) = run(Opt::from_args()) {
        for cause in err.iter_chain() {
            eprintln!("{}", cause);
        }
        std::process::exit(1);
    }
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    ntfs: PathBuf,
}

fn run(opt: Opt) -> Result<()> {
    write!(io::stderr(), "Reading NTFS...")?;
    io::stderr().flush()?;
    let model = timed(" done", || transit_model::ntfs::read(&opt.ntfs)).unwrap();
    let stdin = io::BufReader::new(io::stdin());
    prompt()?;
    for cmd in stdin.lines() {
        let cmd = cmd?;
        match expr::parse(cmd.as_str()) {
            Ok(e) => dispatch!(model, e.object, |c| run_eval(&e.expr, c, &model))?,
            Err(e) => writeln!(io::stderr(), "{}", e)?,
        }
        prompt()?;
    }
    Ok(())
}

fn run_eval<T>(
    expr: &expr::Expr,
    collection: &Collection<T>,
    model: &transit_model::Model,
) -> Result<()>
where
    T: serde::Serialize,
{
    let run = || {
        let idx_set = match eval::Eval::new(collection, model).run(expr) {
            Ok(set) => set,
            Err(e) => {
                for cause in e.iter_chain() {
                    writeln!(io::stderr(), "{}", cause)?;
                }
                write!(io::stderr(), "Expression not evaluated")?;
                return Ok(());
            }
        };
        print(&idx_set, collection)
    };
    timed("", run)
}

fn print<T>(set: &IdxSet<T>, objects: &Collection<T>) -> Result<()>
where
    T: serde::Serialize,
{
    let objs: Vec<_> = objects.iter_from(set).collect();
    let mut stdout = io::stdout();
    serde_json::to_writer_pretty(&mut stdout, &objs)?;
    writeln!(stdout)?;
    write!(io::stderr(), "{} objects", objs.len())?;
    Ok(())
}

fn timed<T, F: FnOnce() -> T>(s: &str, f: F) -> T {
    use humantime::format_duration;
    use std::time::Instant;
    let begin = Instant::now();
    let res = f();
    let duration = Instant::now().duration_since(begin);
    eprintln!("{} in {}.", s, format_duration(duration));
    res
}

fn prompt() -> Result<()> {
    let mut stderr = io::stderr();
    write!(stderr, "> ")?;
    Ok(stderr.flush()?)
}
