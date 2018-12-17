use std::fmt;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Ty {
    Field,
    Boolean,
    Array(usize, Box<Ty>),
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Field => write!(f, "field"),
            Ty::Boolean => write!(f, "bool"),
            Ty::Array(size, ty) => write!(f, "{}[{}]", ty, size),
        }
    }
}
impl Ty {
    pub fn field() -> Self {
        Ty::Field
    }

    pub fn array(s: usize, t: Ty) -> Self {
        Ty::Array(s, Box::new(t))
    }
}
