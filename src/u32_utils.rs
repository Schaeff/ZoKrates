use flat_absy::{FlatStatement, FlatExpression, FlatFunction, FlatExpressionList, FlatProg};
use field::Field;
use parameter::Parameter;

use helpers::{DirectiveStatement, Helper, RustHelper};

pub fn field_to_u32<T: Field>() -> FlatProg<T> {

	// GENERIC

	let mut inputs = vec![];
	for i in 0..1 {
		inputs.push(format!("i{}", i));
	}

	let arguments = inputs.iter().map(|id| Parameter {
		private: true,
		id: id.to_owned()
	}).collect();

	let mut outputs = vec![];
	for i in 0..33 {
		outputs.push(format!("o{}", i));
	}

	// CUSTOM

	let mut decomp = FlatExpression::Number(T::zero());
	for i in 0..32 {
		decomp = FlatExpression::Add(
			box decomp, 
			box FlatExpression::Mult(
				box FlatExpression::Identifier(format!("o{}", i)),
				box FlatExpression::Number(T::from(2).pow(31 - i))
			)
		)
	}

	decomp = FlatExpression::Add(
		box decomp, 
		box FlatExpression::Mult(
			box FlatExpression::Number(T::from(2).pow(32)),
			box FlatExpression::Identifier(format!("o32"))
		)
	);

	// GENERIC
	FlatProg {
		functions: vec![FlatFunction {
			id: "main".to_string(),
			arguments: arguments,
			statements: vec![
				FlatStatement::Directive(
					DirectiveStatement {
						outputs: outputs.clone(),
						inputs: inputs,
						helper: Helper::Rust(RustHelper::FieldToU32)
					}
				),
				FlatStatement::Condition(decomp, FlatExpression::Identifier("i0".to_owned())),
				FlatStatement::Return(FlatExpressionList {
					expressions: outputs.iter()
	     				.map(|o| FlatExpression::Identifier(o.to_owned())).collect()
				})
			],
			return_count: 32
		}]
	}
}

pub fn u32_to_field<T: Field>() -> FlatProg<T> {

	// GENERIC

	let mut inputs = vec![];
	for i in 0..32 {
		inputs.push(format!("i{}", i));
	}

	let arguments = inputs.iter().map(|id| Parameter {
		private: true,
		id: id.to_owned()
	}).collect();

	let mut outputs = vec![];
	for i in 0..1 {
		outputs.push(format!("o{}", i));
	}

	let mut decomp = FlatExpression::Number(T::zero());
	for i in 0..32 {
		decomp = FlatExpression::Add(
			box decomp, 
			box FlatExpression::Mult(
				box FlatExpression::Identifier(format!("i{}", i)),
				box FlatExpression::Number(T::from(2).pow(31 - i))
			)
		)
	}

	// GENERIC
	FlatProg {
		functions: vec![FlatFunction {
			id: "main".to_string(),
			arguments: arguments,
			statements: vec![
				FlatStatement::Directive(
					DirectiveStatement {
						outputs: outputs.clone(),
						inputs: inputs,
						helper: Helper::Rust(RustHelper::U32ToField)
					}
				),
				FlatStatement::Condition(decomp, FlatExpression::Identifier("o0".to_owned())),
				FlatStatement::Return(FlatExpressionList {
					expressions: outputs.iter()
	     				.map(|o| FlatExpression::Identifier(o.to_owned())).collect()
				})
			],
			return_count: 1
		}]
	}
}