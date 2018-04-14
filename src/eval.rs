use expr;
use ntm;
use ntm::collection::{Collection, CollectionWithId, Id};
use ntm::relations::IdxSet;

macro_rules! dispatch {
    ($model:expr, $obj:expr, $expr:expr) => {
        match $obj {
            "contributor" => $expr(&$model.contributors),
            "dataset" => $expr(&$model.datasets),
            "network" => $expr(&$model.networks),
            "commercial_mode" => $expr(&$model.commercial_modes),
            "line" => $expr(&$model.lines),
            "route" => $expr(&$model.routes),
            "vehicle_journey" => $expr(&$model.vehicle_journeys),
            "physical_mode" => $expr(&$model.physical_modes),
            "stop_area" => $expr(&$model.stop_areas),
            "company" => $expr(&$model.companies),
            _ => Default::default(),
        }
    };
}

pub struct Eval<'a, T: 'a> {
    model: &'a ntm::Model,
    target: &'a Collection<T>,
}
impl<'a, T> Eval<'a, T>
where
    IdxSet<ntm::objects::Contributor>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::Dataset>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::Network>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::CommercialMode>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::Line>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::Route>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::VehicleJourney>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::PhysicalMode>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::StopArea>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::StopPoint>: ntm::model::GetCorresponding<T>,
    IdxSet<ntm::objects::Company>: ntm::model::GetCorresponding<T>,
{
    pub fn new(target: &'a Collection<T>, model: &'a ntm::Model) -> Self {
        Eval { target, model }
    }
    pub fn run(&self, e: &expr::Expr) -> IdxSet<T> {
        self.expr(e)
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
            _ => unimplemented!(),
        }
    }
    fn id(&self, obj: &str, id: &str) -> IdxSet<T> {
        dispatch!(self.model, obj, |c| self.id_impl(c, id))
    }
    fn id_impl<U: Id<U>>(&self, objs: &CollectionWithId<U>, id: &str) -> IdxSet<T>
    where
        IdxSet<U>: ntm::model::GetCorresponding<T>,
    {
        let from = objs.get_idx(id).into_iter().collect();
        self.model.get_corresponding(&from)
    }
}
