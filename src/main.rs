#[macro_use]
extern crate combine;
#[macro_use]
extern crate structopt;
extern crate failure;
extern crate navitia_model;

use std::io::{self, BufRead};
use std::path::PathBuf;
use structopt::StructOpt;
use navitia_model as ntm;
use ntm::collection::{Collection, CollectionWithId, Id};
use ntm::relations::{IdxSet};

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
    let model = ntm::ntfs::read(&opt.ntfs)?;
    let stdin = io::BufReader::new(io::stdin());
    for cmd in stdin.lines() {
        let cmd = cmd?;
        match expr::Expr::parse(cmd.as_str()) {
            Ok(expr) => print_ids(
                &Eval::new(&model.stop_areas, &model).expr(&expr),
                &model.stop_areas
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

struct Eval<'a, T: 'a> {
    model: &'a ntm::Model,
    target: &'a Collection<T>,
}
impl<'a, T> Eval<'a, T>
where
    IdxSet<ntm::objects::Line>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::StopArea>: ntm::model::GetCorresponding<T>,
{
    fn new(target: &'a Collection<T>, model: &'a ntm::Model) -> Self {
        Eval { target, model }
    }
    fn all(&self) -> IdxSet<T> {
        self.target.iter().map(|o| o.0).collect()
    }
    fn expr(&self, e: &expr::Expr) -> IdxSet<T> {
        use expr::Expr::*;
        match e {
            Pred(p) => self.pred(p),
            And(l, r) => &self.expr(l) & &self.expr(r),
            Or(l, r) => &self.expr(l) | &self.expr(r),
            Diff(l, r) => &self.expr(l) - &self.expr(r),
        }
    }
    fn pred(&self, p: &expr::Pred) -> IdxSet<T> {
        use expr::Pred::*;
        match p {
            All => self.all(),
            Empty => IdxSet::default(),
            Fun(f) => self.fun(f),
        }
    }
    fn fun(&self, f: &expr::Fun) -> IdxSet<T> {
        match (f.method.as_str(), f.args.as_slice()) {
            ("id", [arg]) | ("uri", [arg]) => self.id(&f.obj, arg),
            _ => unimplemented!()
        }
    }
    fn id(&self, obj: &str, id: &str) -> IdxSet<T> {
        match obj {
            "line" => self.id_impl(&self.model.lines, id),
            "stop_area" => self.id_impl(&self.model.stop_areas, id),
            _ => unimplemented!()
        }
    }
    fn id_impl<U: Id<U>>(&self, objs: &CollectionWithId<U>, id: &str) -> IdxSet<T>
    where
        IdxSet<U>: ntm::model::GetCorresponding<T>,
    {
        let from = objs.get_idx(id).into_iter().collect();
        self.model.get_corresponding(&from)
    }
}
