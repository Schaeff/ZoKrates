//! Module containing semantic analysis tools to run at compile time
//! The goal is to detect semantic errors such as undefined variables
//! A variable is undefined if it isn't present in the static scope
//!
//! @file semantics.rs
//! @author Thibaut Schaeffer <thibaut@schaeff.fr>
//! @date 2017

use std::collections::HashSet;
use absy::*;
use field::Field;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Symbol {
	id: String,
	level: usize
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FunctionDeclaration {
	id: String,
	return_count: usize,
	arg_count: usize,
}

// Checker, checks the semantics of a program.
pub struct Checker {
	scope: HashSet<Symbol>,
	functions: HashSet<FunctionDeclaration>,
	level: usize
}

impl Checker {
	pub fn new() -> Checker {
		Checker {
			scope: HashSet::new(),
			functions: HashSet::new(),
			level: 0
		}
	}

	#[test]
	pub fn new_with_args(scope: HashSet<Symbol>, level: usize, functions: HashSet<FunctionDeclaration>) -> Checker {
		Checker {
			scope: scope,
			functions: functions,
			level: level,
		}
	}

	pub fn check_program<T: Field>(&mut self, prog: &Prog<T>) -> Result<(), String> {
		for func in prog.functions.iter() {
			self.functions.insert(FunctionDeclaration {
				id: func.clone().id,
				return_count: func.clone().return_count,
				arg_count: func.clone().arguments.len()
			});
			self.check_function(&func)?;
		}
		Ok(())
	}

	fn check_function<T: Field>(&mut self, funct: &Function<T>) -> Result<(), String> {
		self.level += 1;
		for arg in funct.arguments.iter() {
			self.scope.insert(Symbol {
				id: arg.id.to_string(),
				level: self.level
			});
		}
		for stat in funct.statements.iter() {
			self.check_statement(&stat)?;
		}
		let current_level = self.level.clone();
		let current_scope = self.scope.clone();
		let to_remove = current_scope.iter().filter(|symbol| symbol.level == current_level);
		for symbol in to_remove {
			self.scope.remove(symbol);
		}
		self.level -= 1;
		Ok(())
	}

	fn check_statement<T: Field>(&mut self, stat: &Statement<T>) -> Result<(), String> {
		match *stat {
			Statement::Return(ref list) => {
				self.check_expression_list(list)?;
				Ok(())
			}
			Statement::Definition(ref id, ref expr) | Statement::Compiler(ref id, ref expr) => {
				self.check_expression(expr)?;
				self.scope.insert(Symbol {
					id: id.to_string(),
					level: self.level
				});
				Ok(())

			}
			Statement::Condition(ref lhs, ref rhs) => {
				self.check_expression(lhs)?;
				self.check_expression(rhs)?;
				Ok(())
			}
			Statement::For(ref id, _, _, ref statements) => {
				self.level += 1;
				let index = Symbol {
					id: id.to_string(),
					level: self.level
				};
				self.scope.insert(index.clone());
				for stat in statements {
					self.check_statement(stat)?;
				}
				self.scope.remove(&index);
				self.level -= 1;
				Ok(())
			},
            Statement::MultipleDefinition(ref ids, ref rhs) => {
        		// All elements of the left side have to be identifiers
                match *rhs {
                	// Right side has to be a function call
                    Expression::FunctionCall(ref fun_id, ref arguments) => {
                    	match self.find_function(fun_id, arguments) {
                    		// the function has to be defined
                    		Some(f) => {
                    			if f.return_count == ids.len() {
                    				// the return count has to match the left side
                    				for id in ids {
                        				self.scope.insert(Symbol {
											id: id.to_string(),
											level: self.level
										});
                    				}
                    				return Ok(())
                    			}
                    			Err(format!("{} returns {} values but left side is of size {}", f.id, f.return_count, ids.len()))
                    		},
                    		None => Err(format!("Function definition for function {} with {} argument(s) not found.", fun_id, arguments.len()))
                    	}
                    },
                    _ => Err(format!("{} should be a FunctionCall", rhs))
                }
            },
		}
	}

	fn check_expression<T: Field>(&mut self, expr: &Expression<T>) -> Result<(), String> {
		match *expr {
			Expression::Identifier(ref id) => {
				// check that `id` is defined in the scope
				match self.scope.iter().find(|symbol| symbol.id == id.to_string()) {
					Some(_) => Ok(()),
					None => Err(format!("{} is undefined", id.to_string())),
				}
			}
			Expression::Add(ref e1, ref e2) | Expression::Sub(ref e1, ref e2) | Expression::Mult(ref e1, ref e2) |
			Expression::Div(ref e1, ref e2) | Expression::Pow(ref e1, ref e2) => {
				self.check_expression(&e1)?;
				self.check_expression(&e2)?;
				Ok(())
			}
			Expression::IfElse(ref condition, ref consequence, ref alternative) => {
				self.check_condition(&condition)?; 
				self.check_expression(&consequence)?;
				self.check_expression(&alternative)?;
				Ok(())
			}
			Expression::FunctionCall(ref fun_id, ref arguments) => {
				match self.find_function(fun_id, arguments) {
					// the function has to be defined
					Some(f) => {
						if f.return_count == 1 { // Functions must return a single value when not in a MultipleDefinition
							for expr in arguments {
								self.check_expression(&expr)?;
							}
							return Ok(())
						}
						Err(format!("{} returns {} values but is called outside of a definition", fun_id, f.return_count))
					},
                   	None => Err(format!("Function definition for function {} with {} argument(s) not found.", fun_id, arguments.len()))
				}
			}
			Expression::Number(_) => Ok(())
		}
	}

	fn check_expression_list<T: Field>(&mut self, list: &ExpressionList<T>) -> Result<(), String> {
		for expr in list.expressions.iter() { // implement Iterator trait?
			self.check_expression(&expr)?
		}
		Ok(())
	}

	fn check_condition<T: Field>(&mut self, cond: &Condition<T>) -> Result<(), String> {
		match *cond {
			Condition::Lt(ref e1, ref e2) |
			Condition::Le(ref e1, ref e2) |
			Condition::Eq(ref e1, ref e2) |
			Condition::Ge(ref e1, ref e2) |
			Condition::Gt(ref e1, ref e2) => {
				self.check_expression(e1)?;
				self.check_expression(e2)?;
				Ok(())
			}
		}
	}

	fn find_function<T: Field>(&mut self, id: &str, args: &Vec<Expression<T>>) -> Option<FunctionDeclaration> {
		self.functions.clone().into_iter().find(|fun| fun.id == id && fun.arg_count == args.len())
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use field::FieldPrime;

	#[test]
	fn undefined_variable_in_statement() {
		// a = b
		// b undefined
		let statement: Statement<FieldPrime> = Statement::Definition(
			String::from("a"),
			Expression::Identifier(String::from("b"))
		);
		let mut checker = Checker::new();
		assert_eq!(checker.check_statement(&statement), Err("b is undefined".to_string()));
	}

	#[test]
	fn defined_variable_in_statement() {
		// a = b
		// b defined
		let statement: Statement<FieldPrime> = Statement::Definition(
			String::from("a"),
			Expression::Identifier(String::from("b"))
		);
		let mut scope = HashSet::new();
		scope.insert(Symbol {
			id: String::from("b"),
			level: 0
		});
		let mut checker = Checker::new_with_args(scope, 1, HashSet::new());
		assert_eq!(checker.check_statement(&statement), Ok(()));
	}

	#[test]
	fn declared_in_other_function() {
		// def foo():
		//   a = 1
		// def bar():
		//   return a
		// should fail
		let foo_args = Vec::<Parameter>::new();
		let mut foo_statements = Vec::<Statement<FieldPrime>>::new();
		foo_statements.push(Statement::Definition(
			String::from("a"),
			Expression::Number(FieldPrime::from(1)))
		);
		let foo = Function {
            id: "foo".to_string(),
            arguments: foo_args,
            statements: foo_statements,
            return_count: 1,
        };

        let bar_args = Vec::<Parameter>::new();
		let mut bar_statements = Vec::<Statement<FieldPrime>>::new();
		bar_statements.push(Statement::Return(
			ExpressionList {
				expressions: vec![Expression::Identifier(String::from("a"))]
			}			
		));
		let bar = Function {
            id: "bar".to_string(),
            arguments: bar_args,
            statements: bar_statements,
            return_count: 1,
        };

        let mut funcs = Vec::<Function<FieldPrime>>::new();
        funcs.push(foo);
        funcs.push(bar);
        let prog = Prog {
			functions: funcs
        };

		let mut checker = Checker::new();
		assert_eq!(checker.check_program(&prog), Err("a is undefined".to_string()));
	}

	#[test]
	fn declared_in_two_scopes() {
		// def foo():
		//   a = 1
		// def bar():
		//   a = 2
		//   return a
		// should pass
		let foo_args = Vec::<Parameter>::new();
		let mut foo_statements = Vec::<Statement<FieldPrime>>::new();
		foo_statements.push(Statement::Definition(
			String::from("a"),
			Expression::Number(FieldPrime::from(1)))
		);
		let foo = Function {
            id: "foo".to_string(),
            arguments: foo_args,
            statements: foo_statements,
            return_count: 1,
        };

        let bar_args = Vec::<Parameter>::new();
		let mut bar_statements = Vec::<Statement<FieldPrime>>::new();
		bar_statements.push(Statement::Definition(
			String::from("a"),
			Expression::Number(FieldPrime::from(2))
		));
		bar_statements.push(Statement::Return(
			ExpressionList {
				expressions: vec![Expression::Identifier(String::from("a"))]
			}
		));
		let bar = Function {
            id: "bar".to_string(),
            arguments: bar_args,
            statements: bar_statements,
            return_count: 1,
        };

        let mut funcs = Vec::<Function<FieldPrime>>::new();
        funcs.push(foo);
        funcs.push(bar);
        let prog = Prog {
			functions: funcs
        };

		let mut checker = Checker::new();
		assert_eq!(checker.check_program(&prog), Ok(()));
	}

	#[test]
	fn for_index_after_end() {
		// def foo():
		//   for i in 0..10 do
		//   endfor
		//   return i
		// should fail
		let mut foo_statements = Vec::<Statement<FieldPrime>>::new();
		foo_statements.push(Statement::For(
			String::from("i"),
			FieldPrime::from(0),
			FieldPrime::from(10),
			Vec::<Statement<FieldPrime>>::new())
		);
		foo_statements.push(Statement::Return(
			ExpressionList {
				expressions: vec![Expression::Identifier(String::from("i"))]
			}
		));
		let foo = Function {
			id: "foo".to_string(),
			arguments: Vec::<Parameter>::new(),
			statements: foo_statements,
            return_count: 1,
		};

		let mut checker = Checker::new();
		assert_eq!(checker.check_function(&foo), Err("i is undefined".to_string()));
	}

	#[test]
	fn for_index_in_for() {
		// def foo():
		//   for i in 0..10 do
		//     a = i
		//   endfor
		// should pass
		let mut foo_statements = Vec::<Statement<FieldPrime>>::new();
		let mut for_statements = Vec::<Statement<FieldPrime>>::new();
		for_statements.push(Statement::Definition(
			String::from("a"),
			Expression::Identifier(String::from("i"))
		));
		foo_statements.push(Statement::For(
			String::from("i"),
			FieldPrime::from(0),
			FieldPrime::from(10),
			for_statements
		));
		let foo = Function {
			id: "foo".to_string(),
			arguments: Vec::<Parameter>::new(),
			statements: foo_statements,
            return_count: 1,
		};

		let mut checker = Checker::new();
		assert_eq!(checker.check_function(&foo), Ok(()));
	}

	#[test]
	fn arity_mismatch() {
		// def foo():
		//   return 1, 2
		// def bar():
		//   c = foo()
		// should fail
		let bar_statements: Vec<Statement<FieldPrime>> = vec![Statement::MultipleDefinition(
			vec!["c".to_string()], 
			Expression::FunctionCall("foo".to_string(), vec![])
		)];

		let foo = FunctionDeclaration {
			id: "foo".to_string(),
			arg_count: 0,
            return_count: 2,
		};

		let mut functions = HashSet::new();
		functions.insert(foo);

		let bar = Function {
			id: "bar".to_string(),
			arguments: vec![],
			statements: bar_statements,
			return_count: 1
		};

		let mut checker = Checker::new_with_args(HashSet::new(), 0, functions);
		assert_eq!(checker.check_function(&bar), Err(("foo returns 2 values but left side is of size 1".to_string())));
	}

	#[test]
	fn multi_return_outside_multidef() {
		// def foo():
		//   return 1, 2
		// def bar():
		//   4 == foo()
		// should fail
		let bar_statements: Vec<Statement<FieldPrime>> = vec![Statement::Condition(
			Expression::Number(FieldPrime::from(2)),
			Expression::FunctionCall("foo".to_string(), vec![])
		)];

		let foo = FunctionDeclaration {
			id: "foo".to_string(),
			arg_count: 0,
            return_count: 2,
		};

		let mut functions = HashSet::new();
		functions.insert(foo);

		let bar = Function {
			id: "bar".to_string(),
			arguments: vec![],
			statements: bar_statements,
			return_count: 1
		};

		let mut checker = Checker::new_with_args(HashSet::new(), 0, functions);
		assert_eq!(checker.check_function(&bar), Err(("foo returns 2 values but is called outside of a definition".to_string())));
	}

	#[test]
	fn function_undefined_in_multidef() {
		// def bar():
		//   c = foo()
		// should fail
		let bar_statements: Vec<Statement<FieldPrime>> = vec![Statement::MultipleDefinition(
			vec!["c".to_string()], 
			Expression::FunctionCall("foo".to_string(), vec![])
		)];

		let bar = Function {
			id: "bar".to_string(),
			arguments: vec![],
			statements: bar_statements,
			return_count: 1
		};

		let mut checker = Checker::new_with_args(HashSet::new(), 0, HashSet::new());
		assert_eq!(checker.check_function(&bar), Err(("Function definition for function foo with 0 argument(s) not found.".to_string())));
	}

	#[test]
	fn function_undefined() {
		// def bar():
		//   1 = foo()
		// should fail
		let bar_statements: Vec<Statement<FieldPrime>> = vec![Statement::Condition(
			Expression::Number(FieldPrime::from(1)), 
			Expression::FunctionCall("foo".to_string(), vec![])
		)];

		let bar = Function {
			id: "bar".to_string(),
			arguments: vec![],
			statements: bar_statements,
			return_count: 1
		};

		let mut checker = Checker::new_with_args(HashSet::new(), 0, HashSet::new());
		assert_eq!(checker.check_function(&bar), Err(("Function definition for function foo with 0 argument(s) not found.".to_string())));
	}

	#[test]
	fn return_undefined() {
		// def bar():
		//   return a, b
		// should fail
		let bar_statements: Vec<Statement<FieldPrime>> = vec![Statement::Return(
			ExpressionList { expressions: vec![
				Expression::Identifier("a".to_string()),
				Expression::Identifier("b".to_string())
			]}
		)];

		let bar = Function {
			id: "bar".to_string(),
			arguments: vec![],
			statements: bar_statements,
			return_count: 2
		};

		let mut checker = Checker::new_with_args(HashSet::new(), 0, HashSet::new());
		assert_eq!(checker.check_function(&bar), Err(("a is undefined".to_string())));
	}

	#[test]
	fn multi_def() {
		// def foo():
		//   return 1, 2
		// def bar():
		//   a, b = foo()
		//   return a + b
		//
		// should pass
		let bar_statements: Vec<Statement<FieldPrime>> = vec![
			Statement::MultipleDefinition(
				vec!["a".to_string(), "b".to_string()], 
				Expression::FunctionCall("foo".to_string(), vec![])
			),
			Statement::Return(
				ExpressionList { expressions: vec![
					Expression::Add(
						box Expression::Identifier("a".to_string()), 
						box Expression::Identifier("b".to_string())
					)]
				}
			)
		];

		let foo = FunctionDeclaration {
			id: "foo".to_string(),
			arg_count: 0,
            return_count: 2,
		};

		let mut functions = HashSet::new();
		functions.insert(foo);

		let bar = Function {
			id: "bar".to_string(),
			arguments: vec![],
			statements: bar_statements,
			return_count: 1
		};

		let mut checker = Checker::new_with_args(HashSet::new(), 0, functions);
		assert_eq!(checker.check_function(&bar), Ok(()));
	}
}