use absy::variable::Variable;
use field::Field;
use recursive_absy::types::TypedExpressionTrait;
use std::fmt;

#[derive(Debug)]
pub enum FieldElement<T: Field> {
    Identifier(Variable),
    Value(usize),
    Add(Box<FieldElement<T>>, Box<FieldElement<T>>),
    FunctionCall(String, Vec<Box<TypedExpressionTrait<T>>>),
}

impl<T: Field> fmt::Display for FieldElement<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FieldElement::Identifier(ref id) => write!(f, "{:?}", id),
            FieldElement::Value(ref v) => write!(f, "{}", v),
            FieldElement::Add(ref f1, ref f2) => write!(f, "{} + {}", f1, f2),
            FieldElement::FunctionCall(ref id, ref args) => write!(
                f,
                "{}({})",
                id,
                args.iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}
