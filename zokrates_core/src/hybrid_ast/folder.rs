// Generic walk through a typed AST. Not mutating in place

use field::Field as FieldTrait;
use hybrid_ast::types::array::{ArrayInner, ArrayValue};
use hybrid_ast::*;

pub trait Folder<FT: FieldTrait>: Sized {
    fn fold_program(&mut self, p: Prog<FT>) -> Prog<FT> {
        fold_program(self, p)
    }

    fn fold_function(&mut self, f: Function<FT>) -> Function<FT> {
        fold_function(self, f)
    }

    fn fold_parameter(&mut self, p: Parameter) -> Parameter {
        Parameter {
            id: self.fold_variable(p.id),
            ..p
        }
    }

    fn fold_name(&mut self, n: String) -> String {
        n
    }

    fn fold_variable(&mut self, v: Variable) -> Variable {
        Variable {
            id: self.fold_name(v.id),
            ..v
        }
    }

    fn fold_assignee(&mut self, a: Assignee<FT>) -> Assignee<FT> {
        match a {
            Assignee::Identifier(v) => Assignee::Identifier(self.fold_variable(v)),
            Assignee::ArrayElement(box a, box index) => Assignee::ArrayElement(
                box self.fold_assignee(a),
                box self.fold_field_expression(index),
            ),
        }
    }

    fn fold_statement(&mut self, s: Statement<FT>) -> Vec<Statement<FT>> {
        fold_statement(self, s)
    }

    fn fold_expression(&mut self, e: Expression<FT>) -> Expression<FT> {
        match e {
            Expression::FieldElement(e) => self.fold_field_expression(e).into(),
            Expression::Boolean(e) => self.fold_boolean_expression(e).into(),
            Expression::Array(e) => self.fold_array_expression(e).into(),
        }
    }

    fn fold_expression_list(&mut self, es: ExpressionList<FT>) -> ExpressionList<FT> {
        match es {
            ExpressionList::FunctionCall(id, arguments, types) => ExpressionList::FunctionCall(
                id,
                arguments
                    .into_iter()
                    .map(|a| self.fold_expression(a))
                    .collect(),
                types,
            ),
        }
    }

    fn fold_field_expression(&mut self, e: Field<FT>) -> Field<FT> {
        fold_field_expression(self, e)
    }
    fn fold_boolean_expression(&mut self, e: Boolean<FT>) -> Boolean<FT> {
        fold_boolean_expression(self, e)
    }
    fn fold_array_expression(&mut self, e: Array<FT>) -> Array<FT> {
        fold_array_expression(self, e)
    }
    fn fold_array_inner<T: Type<FT>>(&mut self, e: ArrayInner<FT, T>) -> ArrayInner<FT, T> {
        fold_array_inner(self, e)
    }
    fn fold_array_value<T: Type<FT>>(&mut self, e: ArrayValue<FT, T>) -> ArrayValue<FT, T> {
        fold_array_value(self, e)
    }
}

pub fn fold_program<FT: FieldTrait, F: Folder<FT>>(f: &mut F, p: Prog<FT>) -> Prog<FT> {
    Prog {
        functions: p
            .functions
            .into_iter()
            .map(|fun| f.fold_function(fun))
            .collect(),
        ..p
    }
}

pub fn fold_statement<FT: FieldTrait, F: Folder<FT>>(
    f: &mut F,
    s: Statement<FT>,
) -> Vec<Statement<FT>> {
    let res = match s {
        Statement::Return(expressions) => Statement::Return(
            expressions
                .into_iter()
                .map(|e| f.fold_expression(e))
                .collect(),
        ),
        Statement::Definition(a, e) => {
            Statement::Definition(f.fold_assignee(a), f.fold_expression(e))
        }
        Statement::Declaration(v) => Statement::Declaration(f.fold_variable(v)),
        Statement::Condition(left, right) => {
            Statement::Condition(f.fold_expression(left), f.fold_expression(right))
        }
        Statement::For(v, from, to, statements) => Statement::For(
            f.fold_variable(v),
            from,
            to,
            statements
                .into_iter()
                .flat_map(|s| f.fold_statement(s))
                .collect(),
        ),
        Statement::MultipleDefinition(variables, elist) => Statement::MultipleDefinition(
            variables.into_iter().map(|v| f.fold_variable(v)).collect(),
            f.fold_expression_list(elist),
        ),
    };
    vec![res]
}

pub fn fold_array_expression<FT: FieldTrait, F: Folder<FT>, T: Type<FT>>(f: &mut F, e: T) -> T {
    e.fold(f)
}

pub fn fold_array_inner<FT: FieldTrait, F: Folder<FT>, T: Type<FT>>(
    f: &mut F,
    e: ArrayInner<FT, T>,
) -> ArrayInner<FT, T> {
    ArrayInner {
        value: f.fold_array_value(e.value),
        ..e
    }
}

pub fn fold_array_value<FT: FieldTrait, F: Folder<FT>, T: Type<FT>>(
    f: &mut F,
    e: ArrayValue<FT, T>,
) -> ArrayValue<FT, T> {
    match e {
        ArrayValue::Value(v) => ArrayValue::Value(v.into_iter().map(|e| e.fold(f)).collect()),
        ArrayValue::Identifier(name) => ArrayValue::Identifier(f.fold_name(name)),
        ArrayValue::FunctionCall(id, arguments) => ArrayValue::FunctionCall(
            id,
            arguments
                .into_iter()
                .map(|a| f.fold_expression(a))
                .collect(),
        ),
        ArrayValue::Select(box array, box index) => ArrayValue::Select(
            box f.fold_array_inner(array),
            box f.fold_field_expression(index),
        ),
    }
}

pub fn fold_field_expression<FT: FieldTrait, F: Folder<FT>>(f: &mut F, e: Field<FT>) -> Field<FT> {
    match e {
        Field::Number(n) => Field::Number(n),
        Field::Identifier(id) => Field::Identifier(f.fold_name(id)),
        Field::Add(box e1, box e2) => {
            let e1 = f.fold_field_expression(e1);
            let e2 = f.fold_field_expression(e2);
            Field::Add(box e1, box e2)
        }
        Field::Sub(box e1, box e2) => {
            let e1 = f.fold_field_expression(e1);
            let e2 = f.fold_field_expression(e2);
            Field::Sub(box e1, box e2)
        }
        Field::Mult(box e1, box e2) => {
            let e1 = f.fold_field_expression(e1);
            let e2 = f.fold_field_expression(e2);
            Field::Mult(box e1, box e2)
        }
        Field::Div(box e1, box e2) => {
            let e1 = f.fold_field_expression(e1);
            let e2 = f.fold_field_expression(e2);
            Field::Div(box e1, box e2)
        }
        Field::Pow(box e1, box e2) => {
            let e1 = f.fold_field_expression(e1);
            let e2 = f.fold_field_expression(e2);
            Field::Pow(box e1, box e2)
        }
        Field::IfElse(box cond, box cons, box alt) => {
            let cond = f.fold_boolean_expression(cond);
            let cons = f.fold_field_expression(cons);
            let alt = f.fold_field_expression(alt);
            Field::IfElse(box cond, box cons, box alt)
        }
        Field::FunctionCall(id, exps) => {
            let exps = exps.into_iter().map(|e| f.fold_expression(e)).collect();
            Field::FunctionCall(id, exps)
        }
        Field::Select(box array, box index) => {
            let array = f.fold_array_expression(array);
            let index = f.fold_field_expression(index);
            Field::Select(box array, box index)
        }
    }
}

pub fn fold_boolean_expression<FT: FieldTrait, F: Folder<FT>>(
    f: &mut F,
    e: Boolean<FT>,
) -> Boolean<FT> {
    match e {
        Boolean::Value(v) => Boolean::Value(v),
        Boolean::Identifier(id) => Boolean::Identifier(f.fold_name(id)),
        Boolean::Eq(box e1, box e2) => {
            let e1 = f.fold_field_expression(e1);
            let e2 = f.fold_field_expression(e2);
            Boolean::Eq(box e1, box e2)
        }
        Boolean::Lt(box e1, box e2) => {
            let e1 = f.fold_field_expression(e1);
            let e2 = f.fold_field_expression(e2);
            Boolean::Lt(box e1, box e2)
        }
        Boolean::Le(box e1, box e2) => {
            let e1 = f.fold_field_expression(e1);
            let e2 = f.fold_field_expression(e2);
            Boolean::Le(box e1, box e2)
        }
        Boolean::Gt(box e1, box e2) => {
            let e1 = f.fold_field_expression(e1);
            let e2 = f.fold_field_expression(e2);
            Boolean::Gt(box e1, box e2)
        }
        Boolean::Ge(box e1, box e2) => {
            let e1 = f.fold_field_expression(e1);
            let e2 = f.fold_field_expression(e2);
            Boolean::Ge(box e1, box e2)
        }
        Boolean::Or(box e1, box e2) => {
            let e1 = f.fold_boolean_expression(e1);
            let e2 = f.fold_boolean_expression(e2);
            Boolean::Or(box e1, box e2)
        }
        Boolean::And(box e1, box e2) => {
            let e1 = f.fold_boolean_expression(e1);
            let e2 = f.fold_boolean_expression(e2);
            Boolean::And(box e1, box e2)
        }
        Boolean::Not(box e) => {
            let e = f.fold_boolean_expression(e);
            Boolean::Not(box e)
        }
    }
}

pub fn fold_function<FT: FieldTrait, F: Folder<FT>>(f: &mut F, fun: Function<FT>) -> Function<FT> {
    Function {
        arguments: fun
            .arguments
            .into_iter()
            .map(|a| f.fold_parameter(a))
            .collect(),
        statements: fun
            .statements
            .into_iter()
            .flat_map(|s| f.fold_statement(s))
            .collect(),
        ..fun
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use field::FieldPrime;

    struct DefaultFolder {}
    impl<FT: FieldTrait> Folder<FT> for DefaultFolder {}

    #[test]
    fn default_visit() {
        let a: Expression<_> = Array::Array(
            vec![
                vec![Field::Number(FieldPrime::from(5))].into(),
                vec![Field::Number(FieldPrime::from(5))].into(),
            ]
            .into(),
        )
        .into();

        let mut folder = DefaultFolder {};
        let b = folder.fold_expression(a.clone());
        assert_eq!(a, b);
    }
}
