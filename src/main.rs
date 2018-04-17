#![feature(specialization)]

#[macro_use]
extern crate combine;
#[macro_use]
extern crate structopt;
extern crate failure;
extern crate humantime;
extern crate navitia_model as ntm;
extern crate serde;
extern crate serde_json;

use ntm::collection::Collection;
use ntm::relations::IdxSet;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use structopt::StructOpt;

#[macro_use]
pub mod eval;
pub mod expr;

fn main() {
    if let Err(err) = run(Opt::from_args()) {
        for cause in err.causes() {
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

fn run(opt: Opt) -> Result<(), failure::Error> {
    eprint!("Reading NTFS...");
    io::stderr().flush()?;
    let model = timed(" done", || ntm::ntfs::read(&opt.ntfs))?;
    let stdin = io::BufReader::new(io::stdin());
    prompt()?;
    for cmd in stdin.lines() {
        let cmd = cmd?;
        match expr::parse(cmd.as_str()) {
            Ok(expr) => dispatch!(
                model,
                expr.object.as_str(),
                |c| timed("", || print(&eval::Eval::new(c, &model).run(&expr.expr), c)),
                {
                    eprintln!("unknown object {}", expr.object);
                    Ok(())
                }
            )?,
            Err(e) => eprintln!("{}", e),
        }
        prompt()?;
    }
    Ok(())
}

fn print<T>(set: &IdxSet<T>, objects: &Collection<T>) -> Result<(), failure::Error>
where
    T: serde::Serialize,
{
    let objs: Vec<_> = objects.iter_from(set).collect();
    serde_json::to_writer_pretty(io::stdout(), &objs)?;
    println!();
    eprint!("{} objects", objs.len());
    Ok(())
}

fn timed<T, F: FnOnce() -> T>(s: &str, f: F) -> T {
    let begin = std::time::Instant::now();
    let res = f();
    let end = std::time::Instant::now();
    eprintln!(
        "{} in {}.",
        s,
        humantime::format_duration(end.duration_since(begin))
    );
    res
}

fn prompt() -> Result<(), io::Error> {
    let mut stderr = io::stderr();
    write!(stderr, "> ")?;
    io::stderr().flush()
}
