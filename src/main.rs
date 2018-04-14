#[macro_use]
extern crate combine;
#[macro_use]
extern crate structopt;
extern crate failure;
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
    print!("Reading NTFS...");
    io::stdout().flush()?;
    let model = ntm::ntfs::read(&opt.ntfs)?;
    println!(" done.");
    let stdin = io::BufReader::new(io::stdin());
    for cmd in stdin.lines() {
        let cmd = cmd?;
        match expr::parse(cmd.as_str()) {
            Ok(expr) => dispatch!(
                model,
                expr.object.as_str(),
                |c| print(
                    &eval::Eval::new(c, &model).run(&expr.expr),
                    c,
                ),
                { eprintln!("unknown object {}", expr.object); Ok(()) }
            )?,
            Err(e) => eprintln!("{}", e),
        }
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
    Ok(())
}
