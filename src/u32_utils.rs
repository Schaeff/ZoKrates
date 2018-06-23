use flat_absy::{FlatStatement, FlatExpression, FlatFunction, FlatExpressionList, FlatProg};
use field::Field;
use parameter::Parameter;

use helpers::{DirectiveStatement, Helper, RustHelper};

pub fn FieldToU32<T: Field>() -> FlatProg<T> {

	// GENERIC

	let mut inputs = vec![];
	for i in 0..1 {
		inputs.push(format!("i{}", i));
	}

	let arguments = inputs.iter().map(|i| Parameter {
		private: true,
		id: format!("i{}", i)
	}).collect();

	let mut outputs = vec![];
	for i in 0..32 {
		outputs.push(format!("o{}", i));
	}

	// CUSTOM

	let mut decomp = FlatExpression::Number(T::zero());
	for i in 0..32 {
		decomp = FlatExpression::Add(
			box decomp, 
			box FlatExpression::Mult(
				box FlatExpression::Identifier(format!("o{}", i)),
				box FlatExpression::Number(T::from(2).pow(i))
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

// #[cfg(test)]
// mod u32 {
// 	// use super::*;
// 	// use field::FieldPrime;

// 	// #[test]
// 	// fn signature() {



// 	// 	let helper = RustHelper {
// 	// 		id: "fieldToU32".to_owned(),
// 	// 		output_count: 32,
// 	// 		input_count: 1,

// 	// 	};

// 	// 	let fun: FlatFunction<FieldPrime> = helper.clone().into();

// 	// 	assert_eq!(fun.arguments.len(), helper.input_count);
// 	// 	assert_eq!(fun.return_count, helper.output_count);
// 	// }

// 	// #[test]
// 	// fn body() {
// 	// 	let helper = RustHelper {
// 	// 		id: "fieldToU32".to_owned(),
// 	// 		output_count: 32,
// 	// 		input_count: 1,
// 	// 	};

// 	// 	let fun: FlatFunction<FieldPrime> = helper.clone().into();

// 	// 	println!("{}", fun);

// 	// 	assert_eq!(fun.statements.len(), 3);
// 	// }
// }