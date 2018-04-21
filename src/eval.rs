use expr;
use ntm;
use ntm::collection::{Collection, CollectionWithId, Id};
use ntm::relations::IdxSet;

use Result;

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
            "stop_point" => $expr(&$model.stop_points),
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
    pub fn run(&self, e: &expr::Expr) -> Result<IdxSet<T>> {
        self.expr(e)
    }
    fn all(&self) -> IdxSet<T> {
        self.target.iter().map(|o| o.0).collect()
    }
    fn expr(&self, e: &expr::Expr) -> Result<IdxSet<T>> {
        use expr::Expr::*;
        let res = match e {
            Pred(p) => self.pred(p)?,
            ToObject(o) => self.to_object(&o)?,
            And(l, r) => &self.expr(l)? & &self.expr(r)?,
            Or(l, r) => &self.expr(l)? | &self.expr(r)?,
            Diff(l, r) => &self.expr(l)? - &self.expr(r)?,
        };
        Ok(res)
    }
    fn to_object(&self, o: &expr::ToObject) -> Result<IdxSet<T>> {
        dispatch!(
            self.model,
            o.object,
            |c| Ok(self.get_corresponding(&Eval::new(c, &self.model).expr(&o.expr)?)),
            bail!("unknown object {}", o.object)
        )
    }
    fn pred(&self, p: &expr::Pred) -> Result<IdxSet<T>> {
        use expr::Pred::*;
        match p {
            All => Ok(self.all()),
            Empty => Ok(IdxSet::default()),
            Fun(f) => self.fun(f),
        }
    }
    fn fun(&self, f: &expr::Fun) -> Result<IdxSet<T>> {
        match (f.obj, f.method.as_str(), f.args.as_slice()) {
            (_, "id", [arg]) | (_, "uri", [arg]) => self.id(&f.obj, arg),
            (_, "has_code", [key, value]) => self.has_code(&f.obj, key, value),
            ("line", "code", [arg]) => Ok(self.line_code(arg)),
            ("stop_point", "within_distance", [dist, coord]) => self.stop_point_within_distance(dist, coord),
            _ => bail!("function {} is not supported, returning empty result", f),
        }
    }
    fn id(&self, obj: &str, id: &str) -> Result<IdxSet<T>> {
        dispatch!(
            self.model,
            obj,
            |c| self.get_from_id(c, id),
            bail!("unknown object {}", obj)
        )
    }
    fn has_code(&self, obj: &str, key: &str, value: &str) -> Result<IdxSet<T>> {
        dispatch!(
            self.model,
            obj,
            |c| self.get_from_code(c, key, value),
            bail!("unknown object {}", obj)
        )
    }
    fn line_code(&self, code: &str) -> IdxSet<T> {
        let code = Some(code.to_string());
        let lines = self.model
            .lines
            .iter()
            .filter_map(|(idx, l)| if l.code == code { Some(idx) } else { None })
            .collect();
        self.get_corresponding(&lines)
    }
    fn stop_point_within_distance(&self, distance: &str, coord: &str) -> Result<IdxSet<T>> {
        let distance = distance.parse()?;
        let split = coord.find(';').ok_or_else(|| format_err!("invalid coord: no `;`"))?;
        let coord = ::ntm::objects::Coord {
            lon: coord[..split].parse()?,
            lat: coord[split + 1..].parse()?
        };
        let from = self.model.stop_points.iter()
            .filter(|(_, sp)| sp.coord.distance_to(&coord) <= distance)
            .map(|(idx, _)| idx)
            .collect();
        Ok(self.get_corresponding(&from))
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
        self.model.get_corresponding(from)
    }
}

trait GetFromId<T, U> {
    fn get_from_id(&self, objs: &T, id: &str) -> Result<IdxSet<U>>;
}
impl<'a, T: Id<T>, U> GetFromId<CollectionWithId<T>, U> for Eval<'a, U> {
    fn get_from_id(&self, objs: &CollectionWithId<T>, id: &str) -> Result<IdxSet<U>> {
        Ok(self.get_corresponding(&objs.get_idx(id).into_iter().collect()))
    }
}
impl<'a, T, U> GetFromId<Collection<T>, U> for Eval<'a, U> {
    fn get_from_id(&self, _: &Collection<T>, _: &str) -> Result<IdxSet<U>> {
        bail!("This object does not have id")
    }
}

trait GetFromCode<T, U> {
    fn get_from_code(&self, &Collection<T>, &str, &str) -> Result<IdxSet<U>>;
}
impl<'a, T, U> GetFromCode<T, U> for Eval<'a, U>
where
    T: ntm::objects::Codes,
{
    fn get_from_code(&self, objs: &Collection<T>, key: &str, value: &str) -> Result<IdxSet<U>> {
        let code = (key.to_string(), value.to_string());
        let from = objs.iter()
            .filter(|&(_, obj)| obj.codes().contains(&code))
            .map(|(idx, _)| idx)
            .collect();
        Ok(self.get_corresponding(&from))
    }
}
impl<'a, T, U> GetFromCode<T, U> for Eval<'a, U> {
    default fn get_from_code(&self, _: &Collection<T>, _: &str, _: &str) -> Result<IdxSet<U>> {
        bail!("This object does not support has_code")
    }
}
