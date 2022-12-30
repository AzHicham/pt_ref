use expr;
use failure::ResultExt;
use relational_types::IdxSet;
use transit_model::Model;
use typed_index_collection::{Collection, CollectionWithId, Id};
use Result;

macro_rules! dispatch {
    ($model:expr, $obj:expr, $expr:expr) => {{
        use $crate::expr::Object::*;
        match $obj {
            Contributor => $expr(&$model.contributors),
            Dataset => $expr(&$model.datasets),
            Network => $expr(&$model.networks),
            CommercialMode => $expr(&$model.commercial_modes),
            Line => $expr(&$model.lines),
            Route => $expr(&$model.routes),
            VehicleJourney => $expr(&$model.vehicle_journeys),
            PhysicalMode => $expr(&$model.physical_modes),
            StopArea => $expr(&$model.stop_areas),
            StopPoint => $expr(&$model.stop_points),
            Company => $expr(&$model.companies),
            Connection => $expr(&$model.transfers),
        }
    }};
}

pub struct Eval<'a, T: 'a> {
    model: &'a Model,
    target: &'a Collection<T>,
}
impl<'a, T> Eval<'a, T> {
    pub fn new(target: &'a Collection<T>, model: &'a Model) -> Self {
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
            ToObject(o) => self.to_object(o)?,
            And(l, r) => &self.expr(l)? & &self.expr(r)?,
            Or(l, r) => &self.expr(l)? | &self.expr(r)?,
            Diff(l, r) => &self.expr(l)? - &self.expr(r)?,
        };
        Ok(res)
    }
    fn to_object(&self, o: &expr::ToObject) -> Result<IdxSet<T>> {
        dispatch!(self.model, o.object, |c| Ok(
            self.get_corresponding(&Eval::new(c, self.model).expr(&o.expr)?)
        ))
    }
    fn pred(&self, p: &expr::Pred) -> Result<IdxSet<T>> {
        use expr::Pred::*;
        match p {
            All => Ok(self.all()),
            Empty => Ok(IdxSet::default()),
            Fun(f) => self
                .fun(f)
                .with_context(|_| format!("Error while evaluating {f}"))
                .map_err(|e| e.into()),
        }
    }
    fn fun(&self, f: &expr::Fun) -> Result<IdxSet<T>> {
        use expr::Object::*;
        match (f.obj, f.method.as_str(), f.args.as_slice()) {
            (_, "id", [arg]) | (_, "uri", [arg]) => self.id(f.obj, arg),
            (_, "has_code", [key, value]) => self.has_code(f.obj, key, value),
            (Line, "code", [arg]) => Ok(self.line_code(arg)),
            (StopPoint, "within", [dist, coord]) => {
                self.within(&self.model.stop_points, dist, coord)
            }
            (StopArea, "within", [dist, coord]) => self.within(&self.model.stop_areas, dist, coord),
            _ => bail!("function {} is not supported", f),
        }
    }
    fn id(&self, obj: expr::Object, id: &str) -> Result<IdxSet<T>> {
        dispatch!(self.model, obj, |c| self.get_from_id(c, id))
    }
    fn has_code(&self, obj: expr::Object, key: &str, value: &str) -> Result<IdxSet<T>> {
        dispatch!(self.model, obj, |c| self.get_from_code(c, key, value))
    }
    fn line_code(&self, code: &str) -> IdxSet<T> {
        let code = Some(code.to_string());
        let lines = self
            .model
            .lines
            .iter()
            .filter_map(|(idx, l)| if l.code == code { Some(idx) } else { None })
            .collect();
        self.get_corresponding(&lines)
    }
    fn within<U>(
        &self,
        collection: &Collection<U>,
        distance: &str,
        coord: &str,
    ) -> Result<IdxSet<T>>
    where
        U: Coord,
    {
        let distance: f64 = distance.parse()?;
        let sq_distance = distance * distance;
        let split = coord
            .find(';')
            .ok_or_else(|| format_err!("invalid coord: no `;`"))?;
        let coord = transit_model::objects::Coord {
            lon: coord[..split].parse()?,
            lat: coord[split + 1..].parse()?,
        };
        let approx = coord.approx();
        let from = collection
            .iter()
            .filter(|(_, sp)| approx.sq_distance_to(sp.coord()) <= sq_distance)
            .map(|(idx, _)| idx)
            .collect();
        Ok(self.get_corresponding(&from))
    }
}

trait GetCorresponding<T, U> {
    fn get_corresponding(&self, from: &IdxSet<T>) -> IdxSet<U>;
}
impl<'a, T, U> GetCorresponding<T, U> for Eval<'a, U> {
    default fn get_corresponding(&self, _from: &IdxSet<T>) -> IdxSet<U> {
        Default::default()
    }
}

impl<'a, T, U> GetCorresponding<T, U> for Eval<'a, U>
where
    IdxSet<T>: transit_model::model::GetCorresponding<U>,
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
    fn get_from_code(&self, objs: &Collection<T>, key: &str, value: &str) -> Result<IdxSet<U>>;
}
impl<'a, T, U> GetFromCode<T, U> for Eval<'a, U>
where
    T: transit_model::objects::Codes,
{
    fn get_from_code(&self, objs: &Collection<T>, key: &str, value: &str) -> Result<IdxSet<U>> {
        let code = (key.to_string(), value.to_string());
        let from = objs
            .iter()
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

pub trait Coord {
    fn coord(&self) -> &transit_model::objects::Coord;
}
impl Coord for transit_model::objects::StopPoint {
    fn coord(&self) -> &transit_model::objects::Coord {
        &self.coord
    }
}
impl Coord for transit_model::objects::StopArea {
    fn coord(&self) -> &transit_model::objects::Coord {
        &self.coord
    }
}
