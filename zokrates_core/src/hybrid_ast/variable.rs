use hybrid_ast::ty::Ty;
use std::fmt;

#[derive(Clone, PartialEq, Hash, Eq)]
pub struct Variable {
    pub id: String,
    pub _type: Ty,
}

impl Variable {
    pub fn new<S: Into<String>>(id: S, t: Ty) -> Variable {
        Variable {
            id: id.into(),
            _type: t,
        }
    }

    pub fn field_element<S: Into<String>>(id: S) -> Variable {
        Variable {
            id: id.into(),
            _type: Ty::Field,
        }
    }

    pub fn boolean<S: Into<String>>(id: S) -> Variable {
        Variable {
            id: id.into(),
            _type: Ty::Boolean,
        }
    }

    pub fn field_array<S: Into<String>>(id: S, size: usize) -> Variable {
        Variable {
            id: id.into(),
            _type: Ty::Array(size, box Ty::Field),
        }
    }

    pub fn get_type(&self) -> Ty {
        self._type.clone()
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self._type, self.id,)
    }
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Variable(type: {:?}, id: {:?})", self._type, self.id,)
    }
}
