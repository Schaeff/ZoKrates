// Generic walk through a typed AST. Not mutating in place

use self::types::TypedExpressionTrait;
use absy::variable::Variable;
use field::Field;
use recursive_absy::*;

pub trait Folder<T: Field>: Sized {
    fn fold_program(&mut self, p: TypedProg<T>) -> TypedProg<T> {
        fold_program(self, p)
    }

    fn fold_function(&mut self, f: TypedFunction<T>) -> TypedFunction<T> {
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

    fn fold_assignee(&mut self, a: TypedAssignee) -> TypedAssignee {
        match a {
            TypedAssignee::Identifier(v) => TypedAssignee::Identifier(self.fold_variable(v)),
        }
    }

    fn fold_statement(&mut self, s: TypedStatement<T>) -> Vec<TypedStatement<T>> {
        fold_statement(self, s)
    }

    fn fold_expression<U: Box<TypedExpressionTrait<T>>>(&mut self, e: U) -> U {
        e
    }

    fn fold_expression_list(&mut self, es: TypedExpressionList<T>) -> TypedExpressionList<T> {
        match es {
            TypedExpressionList::FunctionCall(id, arguments, types) => {
                TypedExpressionList::FunctionCall(
                    id,
                    arguments
                        .into_iter()
                        .map(|a| self.fold_expression(a))
                        .collect(),
                    types,
                )
            }
        }
    }
}

pub fn fold_program<T: Field, F: Folder<T>>(f: &mut F, p: TypedProg<T>) -> TypedProg<T> {
    TypedProg {
        functions: p
            .functions
            .into_iter()
            .map(|fun| f.fold_function(fun))
            .collect(),
        ..p
    }
}

pub fn fold_statement<T: Field, F: Folder<T>>(
    f: &mut F,
    s: TypedStatement<T>,
) -> Vec<TypedStatement<T>> {
    let res = match s {
        TypedStatement::Return(expressions) => TypedStatement::Return(
            expressions
                .into_iter()
                .map(|e| f.fold_expression(e))
                .collect(),
        ),
        TypedStatement::Definition(a, e) => {
            TypedStatement::Definition(f.fold_assignee(a), f.fold_expression(e))
        }
        TypedStatement::Declaration(v) => TypedStatement::Declaration(f.fold_variable(v)),
        TypedStatement::Condition(left, right) => {
            TypedStatement::Condition(f.fold_expression(left), f.fold_expression(right))
        }
        TypedStatement::For(v, from, to, statements) => TypedStatement::For(
            f.fold_variable(v),
            from,
            to,
            statements
                .into_iter()
                .flat_map(|s| f.fold_statement(s))
                .collect(),
        ),
        TypedStatement::MultipleDefinition(variables, elist) => TypedStatement::MultipleDefinition(
            variables.into_iter().map(|v| f.fold_variable(v)).collect(),
            f.fold_expression_list(elist),
        ),
    };
    vec![res]
}

pub fn fold_function<T: Field, F: Folder<T>>(f: &mut F, fun: TypedFunction<T>) -> TypedFunction<T> {
    TypedFunction {
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
