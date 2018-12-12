mod field_element;

use field::Field;
use std::fmt::{Debug, Display};
use types::Type;

pub trait TypedExpressionTrait<T: Field>: Typed + Debug + Display {}

pub trait Typed {
    fn get_type(&self) -> Type;
}
