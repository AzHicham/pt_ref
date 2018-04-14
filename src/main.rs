#[macro_use]
extern crate combine;
#[macro_use]
extern crate structopt;
extern crate failure;
extern crate navitia_model as ntm;

use ntm::collection::{Collection, Id};
use ntm::relations::IdxSet;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use structopt::StructOpt;

pub mod expr;
pub mod eval;

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
        match expr::Expr::parse(cmd.as_str()) {
            Ok(expr) => print_ids(
                &eval::Eval::new(&model.stop_areas, &model).run(&expr),
                &model.stop_areas,
            ),
            Err(e) => eprintln!("{}", e),
        }
    }
    Ok(())
}

fn print_ids<T: Id<T>>(set: &IdxSet<T>, objects: &Collection<T>) {
    print!("ids: ");
    for &idx in set {
        print!("{},", objects[idx].id());
    }
    println!();
}
