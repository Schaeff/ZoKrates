//! Module containing structs and enums to represent a program.
//!
//! @file absy.rs
//! @author Dennis Kuhnert <dennis.kuhnert@campus.tu-berlin.de>
//! @author Jacob Eberhardt <jacob.eberhardt@tu-berlin.de>
//! @date 2017

pub mod folder;
mod parameter;
mod ty;
mod types;
mod variable;

use types::Signature;

use self::ty::*;
use field::Field as FieldTrait;
use flat_absy::*;
use imports::Import;
use std::fmt;

pub use self::folder::Folder;

use self::parameter::Parameter;
use self::types::array::Array;
use self::types::boolean::Boolean;
use self::types::field::Field;
use self::variable::Variable;

#[derive(Clone, PartialEq)]
pub struct Prog<T: FieldTrait> {
    /// Functions of the program
    pub functions: Vec<Function<T>>,
    pub imports: Vec<Import>,
    pub imported_functions: Vec<FlatFunction<T>>,
}

impl<T: FieldTrait> fmt::Display for Prog<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut res = vec![];
        res.extend(
            self.imports
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>(),
        );
        res.extend(
            self.imported_functions
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>(),
        );
        res.extend(
            self.functions
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>(),
        );
        write!(f, "{}", res.join("\n"))
    }
}

impl<T: FieldTrait> fmt::Debug for Prog<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "program(\n\timports:\n\t\t{}\n\tfunctions:\n\t\t{}{}\n)",
            self.imports
                .iter()
                .map(|x| format!("{:?}", x))
                .collect::<Vec<_>>()
                .join("\n\t\t"),
            self.imported_functions
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>()
                .join("\n\t\t"),
            self.functions
                .iter()
                .map(|x| format!("{:?}", x))
                .collect::<Vec<_>>()
                .join("\n\t\t")
        )
    }
}

#[derive(Clone, PartialEq)]
pub struct Function<T: FieldTrait> {
    /// Name of the program
    pub id: String,
    /// Arguments of the function
    pub arguments: Vec<Parameter>,
    /// Vector of statements that are executed when running the function
    pub statements: Vec<Statement<T>>,
    /// function signature
    pub signature: Signature,
}

impl<T: FieldTrait> fmt::Display for Function<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "def {}({}) -> ({}):\n{}",
            self.id,
            self.arguments
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>()
                .join(", "),
            self.signature
                .outputs
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>()
                .join(", "),
            self.statements
                .iter()
                .map(|x| format!("\t{}", x))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl<T: FieldTrait> fmt::Debug for Function<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Function(id: {:?}, arguments: {:?}, ...):\n{}",
            self.id,
            self.arguments,
            self.statements
                .iter()
                .map(|x| format!("\t{:?}", x))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

pub trait Type<T: FieldTrait>: fmt::Debug + fmt::Display {
    fn ty(&self) -> Ty;
    fn fold<F: Folder<T>>(self, f: &mut F) -> Self;
}

#[derive(Clone, PartialEq, Hash, Eq)]
pub enum Assignee<T: FieldTrait> {
    Identifier(Variable),
    ArrayElement(Box<Assignee<T>>, Box<Field<T>>),
}

impl<T: FieldTrait> fmt::Debug for Assignee<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Assignee::Identifier(ref s) => write!(f, "{}", s.id),
            Assignee::ArrayElement(ref a, ref e) => write!(f, "{}[{}]", a, e),
        }
    }
}

impl<T: FieldTrait> fmt::Display for Assignee<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, PartialEq)]
pub enum Statement<T: FieldTrait> {
    Return(Vec<Expression<T>>),
    Definition(Assignee<T>, Expression<T>),
    Declaration(Variable),
    Condition(Expression<T>, Expression<T>),
    For(Variable, T, T, Vec<Statement<T>>),
    MultipleDefinition(Vec<Variable>, ExpressionList<T>),
}

impl<T: FieldTrait> fmt::Debug for Statement<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Return(ref exprs) => {
                try!(write!(f, "Return("));
                for (i, expr) in exprs.iter().enumerate() {
                    try!(write!(f, "{}", expr));
                    if i < exprs.len() - 1 {
                        try!(write!(f, ", "));
                    }
                }
                write!(f, ")")
            }
            Statement::Declaration(ref var) => write!(f, "Declaration({:?})", var),
            Statement::Definition(ref lhs, ref rhs) => {
                write!(f, "Definition({:?}, {:?})", lhs, rhs)
            }
            Statement::Condition(ref lhs, ref rhs) => write!(f, "Condition({:?}, {:?})", lhs, rhs),
            Statement::For(ref var, ref start, ref stop, ref list) => {
                try!(write!(f, "for {:?} in {:?}..{:?} do\n", var, start, stop));
                for l in list {
                    try!(write!(f, "\t\t{:?}\n", l));
                }
                write!(f, "\tendfor")
            }
            Statement::MultipleDefinition(ref lhs, ref rhs) => {
                write!(f, "MultipleDefinition({:?}, {:?})", lhs, rhs)
            }
        }
    }
}

impl<T: FieldTrait> fmt::Display for Statement<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Return(ref exprs) => {
                try!(write!(f, "return "));
                for (i, expr) in exprs.iter().enumerate() {
                    try!(write!(f, "{}", expr));
                    if i < exprs.len() - 1 {
                        try!(write!(f, ", "));
                    }
                }
                write!(f, "")
            }
            Statement::Declaration(ref var) => write!(f, "{}", var),
            Statement::Definition(ref lhs, ref rhs) => write!(f, "{} = {}", lhs, rhs),
            Statement::Condition(ref lhs, ref rhs) => write!(f, "{} == {}", lhs, rhs),
            Statement::For(ref var, ref start, ref stop, ref list) => {
                try!(write!(f, "for {} in {}..{} do\n", var, start, stop));
                for l in list {
                    try!(write!(f, "\t\t{}\n", l));
                }
                write!(f, "\tendfor")
            }
            Statement::MultipleDefinition(ref ids, ref rhs) => {
                for (i, id) in ids.iter().enumerate() {
                    try!(write!(f, "{}", id));
                    if i < ids.len() - 1 {
                        try!(write!(f, ", "));
                    }
                }
                write!(f, " = {}", rhs)
            }
        }
    }
}

#[derive(Clone, PartialEq, Hash, Eq)]
pub enum Expression<T: FieldTrait> {
    Boolean(Boolean<T>),
    FieldElement(Field<T>),
    Array(Array<T>),
}

impl<T: FieldTrait> From<Field<T>> for Expression<T> {
    fn from(e: Field<T>) -> Expression<T> {
        Expression::FieldElement(e)
    }
}

impl<T: FieldTrait> From<Array<T>> for Expression<T> {
    fn from(e: Array<T>) -> Expression<T> {
        Expression::Array(e)
    }
}

impl<T: FieldTrait> fmt::Display for Expression<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Boolean(ref e) => write!(f, "{}", e),
            Expression::FieldElement(ref e) => write!(f, "{}", e),
            Expression::Array(ref e) => write!(f, "{}", e),
        }
    }
}

impl<T: FieldTrait> fmt::Debug for Expression<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Boolean(ref e) => write!(f, "{:?}", e),
            Expression::FieldElement(ref e) => write!(f, "{:?}", e),
            Expression::Array(ref e) => write!(f, "{:?}", e),
        }
    }
}

pub trait MultiTyped {
    fn get_types(&self) -> &Vec<Ty>;
}

#[derive(Clone, PartialEq)]
pub enum ExpressionList<T: FieldTrait> {
    FunctionCall(String, Vec<Expression<T>>, Vec<Ty>),
}

impl<T: FieldTrait> MultiTyped for ExpressionList<T> {
    fn get_types(&self) -> &Vec<Ty> {
        match *self {
            ExpressionList::FunctionCall(_, _, ref types) => types,
        }
    }
}

impl<FT: FieldTrait> Type<FT> for Boolean<FT> {
    fn ty(&self) -> Ty {
        Ty::Boolean
    }

    fn fold<F: Folder<FT>>(self, f: &mut F) -> Boolean<FT> {
        f.fold_boolean_expression(self)
    }
}

impl<T: FieldTrait> fmt::Display for ExpressionList<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ExpressionList::FunctionCall(ref i, ref p, _) => {
                try!(write!(f, "{}(", i,));
                for (i, param) in p.iter().enumerate() {
                    try!(write!(f, "{}", param));
                    if i < p.len() - 1 {
                        try!(write!(f, ", "));
                    }
                }
                write!(f, ")")
            }
        }
    }
}

impl<T: FieldTrait> fmt::Debug for ExpressionList<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ExpressionList::FunctionCall(ref i, ref p, _) => {
                try!(write!(f, "FunctionCall({:?}, (", i));
                try!(f.debug_list().entries(p.iter()).finish());
                write!(f, ")")
            }
        }
    }
}

impl<T: FieldTrait> Function<T> {
    pub fn to_slug(&self) -> String {
        format!("{}_{}", self.id, self.signature.to_slug())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use field::FieldPrime;

    #[test]
    fn hybrid() {
        let e: Expression<FieldPrime> = Field::Number(FieldPrime::from(5)).into();
        let f: Expression<FieldPrime> =
            Array::Field(vec![Field::Number(FieldPrime::from(5))].into()).into();
        println!("{}", f);
    }

    #[test]
    fn complex_definition() {
        let e: Expression<FieldPrime> =
            Array::Array(vec![vec![Field::Number(FieldPrime::from(5))].into()].into()).into();
        // here it's possible for the assignee and the expression to have different types, has to be checked at run time
        let s = Statement::Definition(
            Assignee::Identifier(Variable::new(
                "foo",
                Ty::Array(1, box Ty::Array(1, box Ty::Field)),
            )),
            e,
        );
    }
}
