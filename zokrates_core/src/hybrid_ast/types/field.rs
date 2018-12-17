use field::Field as FieldTrait;
use hybrid_ast::folder::Folder;
use hybrid_ast::ty::Ty;
use hybrid_ast::types::array::Array;
use hybrid_ast::types::boolean::Boolean;
use hybrid_ast::Expression;
use hybrid_ast::Type;
use std::fmt;

#[derive(Clone, PartialEq, Hash, Eq)]
pub enum Field<T: FieldTrait> {
    Number(T),
    Identifier(String),
    Add(Box<Field<T>>, Box<Field<T>>),
    Sub(Box<Field<T>>, Box<Field<T>>),
    Mult(Box<Field<T>>, Box<Field<T>>),
    Div(Box<Field<T>>, Box<Field<T>>),
    Pow(Box<Field<T>>, Box<Field<T>>),
    IfElse(Box<Boolean<T>>, Box<Field<T>>, Box<Field<T>>),
    FunctionCall(String, Vec<Expression<T>>),
    Select(Box<Array<T>>, Box<Field<T>>),
}
impl<FT: FieldTrait> Type<FT> for Field<FT> {
    fn ty(&self) -> Ty {
        Ty::Field
    }
    fn fold<F: Folder<FT>>(self, f: &mut F) -> Field<FT> {
        self
    }
}
impl<T: FieldTrait> fmt::Display for Field<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Field::Number(ref i) => write!(f, "{}", i),
            Field::Identifier(ref var) => write!(f, "{}", var),
            Field::Add(ref lhs, ref rhs) => write!(f, "({} + {})", lhs, rhs),
            Field::Sub(ref lhs, ref rhs) => write!(f, "({} - {})", lhs, rhs),
            Field::Mult(ref lhs, ref rhs) => write!(f, "({} * {})", lhs, rhs),
            Field::Div(ref lhs, ref rhs) => write!(f, "({} / {})", lhs, rhs),
            Field::Pow(ref lhs, ref rhs) => write!(f, "{}**{}", lhs, rhs),
            Field::IfElse(ref condition, ref consequent, ref alternative) => write!(
                f,
                "if {} then {} else {} fi",
                condition, consequent, alternative
            ),
            Field::FunctionCall(ref i, ref p) => {
                try!(write!(f, "{}(", i,));
                for (i, param) in p.iter().enumerate() {
                    try!(write!(f, "{}", param));
                    if i < p.len() - 1 {
                        try!(write!(f, ", "));
                    }
                }
                write!(f, ")")
            }
            Field::Select(ref id, ref index) => write!(f, "{}[{}]", id, index),
        }
    }
}
impl<T: FieldTrait> fmt::Debug for Field<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Field::Number(ref i) => write!(f, "Num({})", i),
            Field::Identifier(ref var) => write!(f, "Ide({})", var),
            Field::Add(ref lhs, ref rhs) => write!(f, "Add({:?}, {:?})", lhs, rhs),
            Field::Sub(ref lhs, ref rhs) => write!(f, "Sub({:?}, {:?})", lhs, rhs),
            Field::Mult(ref lhs, ref rhs) => write!(f, "Mult({:?}, {:?})", lhs, rhs),
            Field::Div(ref lhs, ref rhs) => write!(f, "Div({:?}, {:?})", lhs, rhs),
            Field::Pow(ref lhs, ref rhs) => write!(f, "Pow({:?}, {:?})", lhs, rhs),
            Field::IfElse(ref condition, ref consequent, ref alternative) => write!(
                f,
                "IfElse({:?}, {:?}, {:?})",
                condition, consequent, alternative
            ),
            Field::FunctionCall(ref i, ref p) => {
                try!(write!(f, "FunctionCall({:?}, (", i));
                try!(f.debug_list().entries(p.iter()).finish());
                write!(f, ")")
            }
            Field::Select(ref id, ref index) => write!(f, "Select({:?}, {:?})", id, index),
        }
    }
}
