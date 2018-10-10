use types::{Signature, Type};
use std::collections::{BTreeMap, HashSet};
use flat_absy::{FlatStatement, FlatExpression, FlatFunction, FlatExpressionList};
use field::Field;
use flat_absy::{FlatParameter, FlatVariable};
use reduce::Reduce;
use helpers::{DirectiveStatement, Helper};

// for r1cs import, can be moved.
// r1cs data structure reflecting JSON standard format:
// {
//     input_count: count,  // # of inputs to pass
//     outputs: [offset_42, offset_63, offset_55],  // indices of the outputs in the witness
//     constraints: [   // constraints verified by the witness
//         [
//             {offset_1: value_a1, offset_2: value_a2, ...},
//             {offset_1: value_b1, offset_2: value_b2, ...},
//             {offset_1: value_c1, offset_2: value_c2, ...}
//         ]
// }

#[derive(Serialize, Deserialize, Debug)]
struct Package {
    abi: Abi,
    call: Call
}

#[derive(Serialize, Deserialize, Debug)]
struct Abi {
    input_wires: Vec<usize>,
    output_wires: Vec<usize>
}

#[derive(Serialize, Deserialize, Debug)]
enum Call {
    ResolvedCall {
        solver: usize,
        r1cs: R1CS
    },
    ExternalCall {
        package: String,
        input_wires: Vec<usize>,
        output_wires: Vec<usize>
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct R1CS(Vec<Constraint>);

#[derive(Serialize, Deserialize, Debug)]
pub struct Witness {
    pub variables: Vec<usize>
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Constraint {
	a: BTreeMap<usize, String>,
	b: BTreeMap<usize, String>,
	c: BTreeMap<usize, String>,
}

impl<T: Field> Into<FlatStatement<T>> for Constraint {
	fn into(self: Constraint) -> FlatStatement<T> {
		let rhs_a = match self.a.into_iter()
			.map(|(key, val)| FlatExpression::Mult(box FlatExpression::Number(T::from_dec_string(val.to_string())), box FlatExpression::Identifier(FlatVariable::new(key))))
			.reduce(|acc, e| FlatExpression::Add(box acc, box e)) {
                Some(e @ FlatExpression::Mult(..)) => FlatExpression::Add(box FlatExpression::Number(T::zero()), box e), // the R1CS serializer only recognizes Add
                Some(e) => e,
                None => FlatExpression::Number(T::zero())
            };
		
		let rhs_b = match self.b.into_iter()
			.map(|(key, val)| FlatExpression::Mult(box FlatExpression::Number(T::from_dec_string(val.to_string())), box FlatExpression::Identifier(FlatVariable::new(key))))
			.reduce(|acc, e| FlatExpression::Add(box acc, box e)) {
                Some(e @ FlatExpression::Mult(..)) => FlatExpression::Add(box FlatExpression::Number(T::zero()), box e), // the R1CS serializer only recognizes Add
                Some(e) => e,
                None => FlatExpression::Number(T::zero())
            };
		
		let lhs = match self.c.into_iter()
			.map(|(key, val)| FlatExpression::Mult(box FlatExpression::Number(T::from_dec_string(val.to_string())), box FlatExpression::Identifier(FlatVariable::new(key))))
			.reduce(|acc, e| FlatExpression::Add(box acc, box e)) {
                Some(e @ FlatExpression::Mult(..)) => FlatExpression::Add(box FlatExpression::Number(T::zero()), box e), // the R1CS serializer only recognizes Add
                Some(e) => e,
                None => FlatExpression::Number(T::zero())
            };

		FlatStatement::Condition(lhs, FlatExpression::Mult(box rhs_a, box rhs_b))
	}
}

impl<T: Field> Into<FlatFunction<T>> for Package {
	fn into(self: Package) -> FlatFunction<T> {
		// determine the number of variables, assuming there is no i so that column i is only zeroes in a, b and c
        let mut variables_set = HashSet::new();

        let (solver, r1cs) = match self.call {
            Call::ResolvedCall {solver, r1cs} => (solver, r1cs),
            _ => unimplemented!()
        };


        for constraint in r1cs.0.iter() {
            for (key, _) in &constraint.a {
                variables_set.insert(key.clone());
            }
            for (key, _) in &constraint.b {
                variables_set.insert(key.clone());
            }
            for (key, _) in &constraint.c {
                variables_set.insert(key.clone());
            }
        }

        let variables_count = variables_set.len();

		// insert flattened statements to represent constraints
        let mut statements: Vec<FlatStatement<T>> = r1cs.0.into_iter().map(|c| c.into()).collect();

        // define the entire witness
        let variables = vec![0; variables_count].iter().enumerate().map(|(i, _)| FlatVariable::new(i)).collect();

        // define the inputs with dummy variables: arguments to the function and to the directive
        let inputs: Vec<FlatVariable> = self.abi.input_wires.iter().enumerate().map(|(i, _)| FlatVariable::new(i + variables_count)).collect();
        let arguments = inputs.iter().map(|i| FlatParameter { id: i.clone(), private: true }).collect();

        // define which subset of the witness is returned
        let outputs: Vec<FlatExpression<T>> = self.abi.output_wires.into_iter()
         				.map(|o| FlatExpression::Identifier(FlatVariable::new(o))).collect();

        let signature = Signature {
            inputs: vec![Type::FieldElement; inputs.len()],
            outputs: vec![Type::FieldElement; outputs.len()],
        };

        // insert a directive to set the witness based on the inputs
        statements.insert(0, FlatStatement::Directive(
            DirectiveStatement {
                outputs: variables,
                inputs: inputs,
                helper: Helper::Wasm(solver)
            })
        );

        // insert a statement to return the subset of the witness
        statements.push(FlatStatement::Return(
        	FlatExpressionList {
        		expressions: outputs
         	})
        );
        
        FlatFunction { 
            id: "main".to_owned(), 
            arguments, 
            statements, 
            signature,
        }
	}
} 

#[cfg(test)]
mod tests {
	use super::*;
	use field::FieldPrime;
	use serde_json;

	#[test]
	fn deserialize_constraint() {
		let constraint = r#"[{"2026": "1"}, {"0": "1", "2026": "1751751751751751751751751751751751751751751"}, {"0": "0"}]"#;
		let _c: Constraint = serde_json::from_str(constraint).unwrap();
	}

	#[test]
	fn constraint_into_flat_statement() {
		let constraint = r#"[{"2026": "1"}, {"0": "1", "2026": "1751751751751751751751751751751751751751751"}, {"0": "0"}]"#;
		let c: Constraint = serde_json::from_str(constraint).unwrap();
		let _statement: FlatStatement<FieldPrime> = c.into();
	}

    #[test]
    fn package_into_flat_function() {
        let package = r#"{"abi": {"input_wires": [0, 1], "output_wires": [2]}, "call": { "ResolvedCall": {"solver": 42, "r1cs": [[{"0": "1"},{"1": "1"},{"2": "1"}]]}}}"#;
        let p: Package = serde_json::from_str(package).unwrap();
        let f: FlatFunction<FieldPrime> = p.into();
        println!("{}", f);
    }
}

