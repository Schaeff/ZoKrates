use field::Field as FieldTrait;
use hybrid_ast::types::field::Field;
use hybrid_ast::Expression;
use std::fmt;

#[derive(Clone, PartialEq, Hash, Eq)]
pub enum Boolean<T: FieldTrait> {
    Identifier(String),
    Value(bool),
    Lt(Box<Field<T>>, Box<Field<T>>),
    Le(Box<Field<T>>, Box<Field<T>>),
    Eq(Box<Field<T>>, Box<Field<T>>),
    Ge(Box<Field<T>>, Box<Field<T>>),
    Gt(Box<Field<T>>, Box<Field<T>>),
    Or(Box<Boolean<T>>, Box<Boolean<T>>),
    And(Box<Boolean<T>>, Box<Boolean<T>>),
    Not(Box<Boolean<T>>),
}
impl<T: FieldTrait> fmt::Display for Boolean<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Boolean::Identifier(ref var) => write!(f, "{}", var),
            Boolean::Lt(ref lhs, ref rhs) => write!(f, "{} < {}", lhs, rhs),
            Boolean::Le(ref lhs, ref rhs) => write!(f, "{} <= {}", lhs, rhs),
            Boolean::Eq(ref lhs, ref rhs) => write!(f, "{} == {}", lhs, rhs),
            Boolean::Ge(ref lhs, ref rhs) => write!(f, "{} >= {}", lhs, rhs),
            Boolean::Gt(ref lhs, ref rhs) => write!(f, "{} > {}", lhs, rhs),
            Boolean::Or(ref lhs, ref rhs) => write!(f, "{} || {}", lhs, rhs),
            Boolean::And(ref lhs, ref rhs) => write!(f, "{} && {}", lhs, rhs),
            Boolean::Not(ref exp) => write!(f, "!{}", exp),
            Boolean::Value(b) => write!(f, "{}", b),
        }
    }
}
impl<T: FieldTrait> fmt::Debug for Boolean<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl<T: FieldTrait> From<Boolean<T>> for Expression<T> {
    fn from(e: Boolean<T>) -> Expression<T> {
        Expression::Boolean(e)
    }
}
