#![feature(specialization)]

#[macro_use]
extern crate combine;
#[macro_use]
extern crate structopt;
#[macro_use]
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

pub type Result<T> = ::std::result::Result<T, failure::Error>;

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

fn run(opt: Opt) -> Result<()> {
    write!(io::stderr(), "Reading NTFS...")?;
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
                |c| run_eval(&expr.expr, c, &model),
                Ok(writeln!(io::stderr(), "unknown object {}", expr.object)?)
            )?,
            Err(e) => writeln!(io::stderr(), "{}", e)?,
        }
        prompt()?;
    }
    Ok(())
}

fn run_eval<T>(expr: &expr::Expr, collection: &Collection<T>, model: &ntm::Model) -> Result<()>
where
    T: serde::Serialize,
{
    let run = || {
        let idx_set = match eval::Eval::new(collection, model).run(expr) {
            Ok(set) => set,
            Err(e) => {
                write!(io::stderr(), "Error: {}", e)?;
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
