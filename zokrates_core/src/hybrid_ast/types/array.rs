use field::Field as FieldTrait;
use hybrid_ast::ty::Ty;

use hybrid_ast::Type;
use hybrid_ast::*;

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub enum Array<F: FieldTrait> {
    Field(ArrayInner<F, Field<F>>),
    Boolean(ArrayInner<F, Boolean<F>>),
    Array(ArrayInner<F, Array<F>>),
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct ArrayInner<F: FieldTrait, T: Type<F>> {
    pub size: usize,
    pub content_ty: Ty,
    pub value: ArrayValue<F, T>,
}

impl<F: FieldTrait> Array<F> {
    pub fn id<S: Into<String>>(size: usize, content_ty: Ty, name: S) -> Self {
        match content_ty {
            Ty::Field => ArrayInner::<F, Field<F>>::id(size, content_ty, name).into(),
            Ty::Boolean => ArrayInner::<F, Boolean<F>>::id(size, content_ty, name).into(),
            Ty::Array(..) => ArrayInner::<F, Array<F>>::id(size, content_ty, name).into(),
        }
    }

    pub fn function_call<S: Into<String>>(
        function_name: S,
        arguments: Vec<Expression<F>>,
        content_ty: Ty,
        size: usize,
    ) -> Self {
        match content_ty {
            Ty::Field => {
                ArrayInner::<F, Field<F>>::function_call(function_name, arguments, content_ty, size)
                    .into()
            }
            Ty::Boolean => ArrayInner::<F, Boolean<F>>::function_call(
                function_name,
                arguments,
                content_ty,
                size,
            )
            .into(),
            Ty::Array(..) => {
                ArrayInner::<F, Array<F>>::function_call(function_name, arguments, content_ty, size)
                    .into()
            }
        }
    }
}

impl<F: FieldTrait, T: Type<F>> ArrayInner<F, T> {
    fn ty(&self) -> Ty {
        Ty::Array(self.size, Box::new(self.content_ty.clone()))
    }

    fn id<S: Into<String>>(size: usize, content_ty: Ty, name: S) -> Self {
        ArrayInner {
            size,
            content_ty,
            value: ArrayValue::Identifier(name.into()),
        }
    }

    pub fn value(&self) -> &ArrayValue<F, T> {
        &self.value
    }

    fn function_call<S: Into<String>>(
        function_name: S,
        arguments: Vec<Expression<F>>,
        content_ty: Ty,
        size: usize,
    ) -> Self {
        ArrayInner {
            size,
            content_ty,
            value: ArrayValue::FunctionCall(function_name.into(), arguments),
        }
    }
}

impl<FT: FieldTrait> Type<FT> for Array<FT> {
    fn ty(&self) -> Ty {
        match self {
            Array::Field(a) => a.ty(),
            Array::Array(a) => a.ty(),
            Array::Boolean(a) => a.ty(),
        }
    }
    fn fold<F: Folder<FT>>(self, f: &mut F) -> Array<FT> {
        match self {
            Array::Field(a) => Array::Field(f.fold_array_inner(a)),
            Array::Boolean(a) => Array::Boolean(f.fold_array_inner(a)),
            Array::Array(a) => Array::Array(f.fold_array_inner(a)),
        }
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub enum ArrayValue<F: FieldTrait, T: Type<F>> {
    Value(Vec<T>),
    Identifier(String),
    Select(Box<ArrayInner<F, Array<F>>>, Box<Field<F>>),
    FunctionCall(String, Vec<Expression<F>>),
}

impl<F: FieldTrait, T: Type<F>> From<Vec<T>> for ArrayValue<F, T> {
    fn from(s: Vec<T>) -> ArrayValue<F, T> {
        ArrayValue::Value(s)
    }
}

impl<F: FieldTrait> From<ArrayInner<F, Field<F>>> for Array<F> {
    fn from(a: ArrayInner<F, Field<F>>) -> Array<F> {
        Array::Field(a)
    }
}

impl<F: FieldTrait> From<ArrayInner<F, Boolean<F>>> for Array<F> {
    fn from(a: ArrayInner<F, Boolean<F>>) -> Array<F> {
        Array::Boolean(a)
    }
}

impl<F: FieldTrait> From<ArrayInner<F, Array<F>>> for Array<F> {
    fn from(a: ArrayInner<F, Array<F>>) -> Array<F> {
        Array::Array(a)
    }
}
impl<F: FieldTrait> fmt::Display for Array<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Array::Field(ref e) => write!(f, "{}", e),
            Array::Array(ref e) => write!(f, "{}", e),
            Array::Boolean(ref e) => write!(f, "{}", e),
        }
    }
}
impl<F: FieldTrait, T: Type<F>> fmt::Display for ArrayInner<F, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl<F: FieldTrait, T: Type<F>> fmt::Display for ArrayValue<F, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArrayValue::Value(ref e) => write!(
                f,
                "[{}]",
                e.iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ArrayValue::Identifier(e) => write!(f, "{}", e),
            ArrayValue::Select(a, e) => write!(f, "{}[{}]", a, e),
            ArrayValue::FunctionCall(a, args) => write!(
                f,
                "{}({})",
                a,
                args.iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}
impl<F: FieldTrait, T: Type<F>> From<Vec<T>> for ArrayInner<F, T> {
    fn from(v: Vec<T>) -> ArrayInner<F, T> {
        let t = v.iter().fold(None, |acc, e| match acc {
                None => Some(e.ty()),
                Some(acc) => {
                    assert_eq!(e.ty(), acc, "Tried creating an array, found inconsistent types in [{}]: expected {} to be of type {}, found {}", v.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join(", "), e, acc, e.ty());
                    Some(acc)
                }
            });
        ArrayInner {
            size: v.len(),
            content_ty: t.unwrap(),
            value: ArrayValue::Value(v),
        }
    }
}

impl<F: FieldTrait> From<Vec<Field<F>>> for Array<F> {
    fn from(v: Vec<Field<F>>) -> Array<F> {
        Array::Field(ArrayInner::from(v))
    }
}

impl<F: FieldTrait> From<Vec<Array<F>>> for Array<F> {
    fn from(v: Vec<Array<F>>) -> Array<F> {
        Array::Array(ArrayInner::from(v))
    }
}

mod tests {
    use super::*;
    use field::FieldPrime;

    #[test]
    #[should_panic]
    fn wrong_size() {
        let _a: Array<FieldPrime> = ArrayInner::from(vec![
            Array::Field(vec![Field::Number(FieldPrime::from(5))].into()),
            Array::Field(
                vec![
                    Field::Number(FieldPrime::from(5)),
                    Field::Number(FieldPrime::from(5)),
                ]
                .into(),
            ),
        ])
        .into();
    }

    #[test]
    fn array_of_field() {
        let a: ArrayInner<FieldPrime, _> = ArrayInner::from(vec![
            Field::Number(FieldPrime::from(1)),
            Field::Number(FieldPrime::from(2)),
        ]);
        assert_eq!(a.ty(), Ty::Array(2, box Ty::Field));
        assert_eq!(
            a.value(),
            &ArrayValue::Value(vec![
                Field::Number(FieldPrime::from(1)),
                Field::Number(FieldPrime::from(2))
            ])
        );

        let a: Array<FieldPrime> = a.into();
        assert_eq!(a.ty(), Ty::Array(2, box Ty::Field));
    }

    #[test]
    fn array_of_array_of_booleans() {
        // [[true, false], [true, false]]

        let a: ArrayInner<FieldPrime, _> =
            ArrayInner::from(vec![Boolean::Value(true), Boolean::Value(false)]);
        let b: ArrayInner<FieldPrime, _> =
            ArrayInner::from(vec![Boolean::Value(true), Boolean::Value(false)]);

        let c: ArrayInner<FieldPrime, _> = ArrayInner::from(vec![a.into(), b.into()]);
        assert_eq!(c.ty(), Ty::Array(2, box Ty::Array(2, box Ty::Boolean)));
        assert_eq!(
            c.value(),
            &ArrayValue::Value(vec![
                Array::Boolean(ArrayInner {
                    value: ArrayValue::Value(vec![Boolean::Value(true), Boolean::Value(false)]),
                    content_ty: Ty::Boolean,
                    size: 2
                }),
                Array::Boolean(ArrayInner {
                    value: ArrayValue::Value(vec![Boolean::Value(true), Boolean::Value(false)]),
                    content_ty: Ty::Boolean,
                    size: 2
                })
            ])
        );

        let c: Array<FieldPrime> = c.into();
        assert_eq!(c.ty(), Ty::Array(2, box Ty::Array(2, box Ty::Boolean)));
    }
}
