use hybrid_ast::variable::Variable;
use std::fmt;

#[derive(Clone, PartialEq)]
pub struct Parameter {
    pub id: Variable,
    pub private: bool,
}

impl Parameter {
    pub fn public(v: Variable) -> Self {
        Parameter {
            id: v,
            private: true,
        }
    }

    pub fn private(v: Variable) -> Self {
        Parameter {
            id: v,
            private: false,
        }
    }
}

impl fmt::Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let visibility = if self.private { "private " } else { "" };
        write!(f, "{}{} {}", visibility, self.id.get_type(), self.id.id)
    }
}

impl fmt::Debug for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parameter(variable: {:?})", self.id)
    }
}
