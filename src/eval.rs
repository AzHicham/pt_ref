use expr;
use ntm;
use ntm::collection::{Collection, CollectionWithId, Id};
use ntm::relations::IdxSet;

macro_rules! dispatch {
    ($model:expr, $obj:expr, $expr:expr) => {
        dispatch!($model, $obj, $expr, Default::default())
    };
    ($model:expr, $obj:expr, $expr:expr, $default:expr) => {
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
            "connection" => $expr(&$model.transfers),
            _ => $default,
        }
    };
}

pub struct Eval<'a, T: 'a> {
    model: &'a ntm::Model,
    target: &'a Collection<T>,
}
impl<'a, T> Eval<'a, T> {
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
            ToObject(o) => self.to_object(&o),
            And(l, r) => &self.expr(l) & &self.expr(r),
            Or(l, r) => &self.expr(l) | &self.expr(r),
            Diff(l, r) => &self.expr(l) - &self.expr(r),
        }
    }
    fn to_object(&self, o: &expr::ToObject) -> IdxSet<T> {
        dispatch!(self.model, o.object.as_str(), |c| {
            self.get_corresponding(&Eval::new(c, &self.model).expr(&o.expr))
        })
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
        dispatch!(self.model, obj, |c| self.get_from_id(c, id))
    }
}

trait GetCorresponding<T, U> {
    fn get_corresponding(&self, &IdxSet<T>) -> IdxSet<U>;
}
impl<'a, T, U> GetCorresponding<T, U> for Eval<'a, U> {
    default fn get_corresponding(&self, _: &IdxSet<T>) -> IdxSet<U> {
        Default::default()
    }
}
impl<'a, T, U> GetCorresponding<T, U> for Eval<'a, U>
where
    IdxSet<T>: ntm::model::GetCorresponding<U>,
{
    fn get_corresponding(&self, from: &IdxSet<T>) -> IdxSet<U> {
        (from as &ntm::model::GetCorresponding<U>).get_corresponding(self.model)
    }
}

trait GetFromId<T, U> {
    fn get_from_id(&self, objs: &T, id: &str) -> IdxSet<U>;
}
impl<'a, T: Id<T>, U> GetFromId<CollectionWithId<T>, U> for Eval<'a, U> {
    fn get_from_id(&self, objs: &CollectionWithId<T>, id: &str) -> IdxSet<U> {
        self.get_corresponding(&objs.get_idx(id).into_iter().collect())
    }
}
impl<'a, T, U> GetFromId<Collection<T>, U> for Eval<'a, U> {
    fn get_from_id(&self, _: &Collection<T>, _: &str) -> IdxSet<U> {
        Default::default()
    }
}
