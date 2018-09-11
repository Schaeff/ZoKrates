use helpers::BellmanDirectiveStatement;
use field::Bls12Field;
use flat_absy_bellman::*;
use field::Field as ZokratesField;

use std::ops::{Add, Sub, Mul};
use std::str::from_utf8;

// Bring in some tools for using pairing-friendly curves
use pairing::{Engine, Field, PrimeField};

// We're going to use the BLS12-381 pairing-friendly elliptic curve.
use pairing::bls12_381::{Bls12, Fr};

// We'll use these interfaces to construct our circuit.
use bellman::{Circuit, ConstraintSystem, LinearCombination, SynthesisError};

use helpers::BellmanExecutable;
use std::fmt;

use std::collections::HashMap;

impl From<Bls12Field> for Fr {
    fn from(e: Bls12Field) -> Fr {
        Fr::from_str(&e.to_dec_string()).unwrap()
    }
}

// data structure to hold a FieldElement variable with its value
// it's fine to clone these
#[derive(Clone)]
struct AssignedLinearCombination {
    combination: LinearCombination<Bls12>,
    value: <Bls12 as Engine>::Fr
}

// data structures for a program
// we don't want to clone these as they contain strings
pub struct Program {
    functions: Vec<Function>,
}

pub struct Function {
    id: String,
    arguments: Vec<String>,
    statements: Vec<Statement>,
    return_variables: Vec<LinCombMemory>
}

#[derive(Clone, Debug, PartialEq)]
pub struct LinCombMemory(pub Vec<(<Bls12 as Engine>::Fr, String)>);

impl fmt::Display for LinCombMemory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}

impl Add<LinCombMemory> for LinCombMemory {
    type Output = LinCombMemory;
    
    fn add(self, other: LinCombMemory) -> LinCombMemory {
        let mut res = HashMap::new();
        for (mult, name) in self.0 {
            res.insert(name, mult);
        }

        for (mult, name) in other.0 {
            let mut target = *res.entry(name).or_insert(<Bls12 as Engine>::Fr::zero());
            target.add_assign(&mult);
        }

        LinCombMemory(res.into_iter().map(|(key, val)| (val, key)).collect())
    }
}

impl Sub<LinCombMemory> for LinCombMemory {
    type Output = LinCombMemory;
    
    fn sub(self, other: LinCombMemory) -> LinCombMemory {
        let mut res = HashMap::new();
        for (mult, name) in self.0 {
            res.insert(name, mult);
        }

        for (mult, name) in other.0 {
            let mut target = *res.entry(name).or_insert(<Bls12 as Engine>::Fr::zero());
            target.sub_assign(&mult);
        }

        LinCombMemory(res.into_iter().map(|(key, val)| (val, key)).collect())
    }
}

impl Mul<<Bls12 as Engine>::Fr> for LinCombMemory {
    type Output = LinCombMemory;
    
    fn mul(self, mult: <Bls12 as Engine>::Fr) -> LinCombMemory {
        LinCombMemory(self.0.into_iter().map(|(m, name)| {
            let mut new_mult = m;
            new_mult.mul_assign(&mult);
            (new_mult, name)
        }).collect())
    }
}

impl From<FlatExpression> for LinCombMemory {
    fn from(e: FlatExpression) -> LinCombMemory {
        match e {
            FlatExpression::Identifier(id) => LinCombMemory(vec![(number_to_fr::<Bls12>(1), id)]),
            FlatExpression::Add(box e1, box e2) => LinCombMemory::from(e1) + LinCombMemory::from(e2),
            FlatExpression::Number(n) => LinCombMemory(vec![(n.into(), "~one".to_string())]),
            FlatExpression::Sub(box e1, box e2) => LinCombMemory::from(e1) - LinCombMemory::from(e2),
            FlatExpression::Mult(box FlatExpression::Number(n), box e1) | FlatExpression::Mult(box e1, box FlatExpression::Number(n)) => {
                LinCombMemory::from(e1) * n.into()
            }
            _ => panic!("die")
        }
    }
}

pub enum Statement {
    // constrain a relationship between some variables
    Constraint(
        LinCombMemory,
        LinCombMemory,
        LinCombMemory,
    ),
    // set some new variables to some values
    Directive(BellmanDirectiveStatement),
    // call a function and assign the results to some new variables
    Definition(Vec<String>, FunctionCall),
}

impl From<FlatFunction> for Function {
    fn from(flat_function: FlatFunction) -> Function {
        let mut statements = vec![];
        let mut return_variables = vec![];

        for s in flat_function.statements {
            match s {
                FlatStatement::Return(ref r) => {
                    return_variables = r.expressions.iter().map(|r| 
                        r.clone().into()
                    ).collect();
                },
                _ => {}
            }
            match s {
                FlatStatement::Condition(e1, e2) => {
                    let stat = match e2 {
                        FlatExpression::Mult(box e21, box e22) => {
                            Statement::Constraint(e21.into(), e22.into(), e1.into())
                        },
                        e @ FlatExpression::Identifier(..) => {
                            Statement::Constraint(e.into(), LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]), e1.into())
                        },
                        _ => panic!("wrong format in constraint")
                    };
                    statements.push(stat);
                },
                FlatStatement::FunctionCall(outputs, f_id, inputs) => {
                    let stat = Statement::Definition(
                        outputs, 
                        FunctionCall(f_id,
                            inputs.iter().map(|input| 
                                input.clone().into()
                            ).collect()
                        )
                    );
                    statements.push(stat);
                },
                FlatStatement::Return(..) => {
                },
                FlatStatement::Directive(d) => {
                    statements.push(Statement::Directive(d));
                },
                e => panic!("unknown {:?}", e)
            }
        }

        Function {
            id: flat_function.id,
            arguments: flat_function.arguments.iter().map(|p| p.id.clone()).collect(),
            statements: statements,
            return_variables: return_variables,
        }
    }
}

impl From<FlatProg> for Program {
    fn from(flat_prog: FlatProg) -> Program {
        Program {
            functions: flat_prog.functions.into_iter().map(|f| f.into()).collect()
        }
    }
}

pub struct FunctionCall(String, Vec<LinCombMemory>);

impl Statement {
    fn flatten<CS: ConstraintSystem<Bls12>>(
        &self,
        cs: &mut CS,
        functions: &Vec<Function>,
        symbols: &mut HashMap<String, AssignedLinearCombination>,
    ) -> Result<(), SynthesisError> {
        match *self {
            Statement::Definition(ref output_names, FunctionCall(ref fun_id, ref input_combinations)) => {
                let mut inputs = vec![];
                for comb in input_combinations {
                    let mut c = LinearCombination::zero();
                    let mut value = <Bls12 as Engine>::Fr::zero();
                    for (mult, var) in comb.0.iter() {
                        let assigned_variable = symbols.get(var).unwrap();
                        let mut v = assigned_variable.value;
                        v.mul_assign(&mult);
                        c = c + (*mult, &assigned_variable.combination);
                        value.add_assign(&v);
                    } 

                    inputs.push(AssignedLinearCombination {
                        combination: c,
                        value: value
                    });
                }

                let fun = functions.iter().find(|f| &f.id == fun_id).unwrap();

                let assigned_outputs = fun.flatten(cs, &functions, &inputs)?;

                // no need to create a new variable, just register `id` is worth e.flattened
                for (index, assignment) in assigned_outputs.iter().enumerate() {
                    symbols.insert(output_names[index].clone(), assignment.clone());
                }
            }
            Statement::Directive(ref d) => {
                // get argument values
                let argument_values = &d.helper.get_input_names()
                    .iter()
                    .map(|a| symbols.get(a).unwrap().value)
                    .collect::<Vec<_>>();

                // apply solver to find result
                let res = d.helper.execute_bellman(&argument_values).unwrap();

                for (index, r) in res.iter().enumerate() {
                    let var = cs.alloc(|| "directive result", || Ok(*r))?;
                    symbols.insert(
                        d.outputs[index].clone(),
                        AssignedLinearCombination {
                            combination: LinearCombination::zero() + var,
                            value: *r,
                        },
                    );
                }
            }
            Statement::Constraint(ref a, ref b, ref c) => {
                let a_comb = a.0
                    .iter()
                    .map(|(mult, var)| (*mult, &symbols.get(var).unwrap().combination))
                    .fold(LinearCombination::zero(), |acc, term| acc + term);
                let b_comb = b.0
                    .iter()
                    .map(|(mult, var)| (*mult, &symbols.get(var).unwrap().combination))
                    .fold(LinearCombination::zero(), |acc, term| acc + term);
                let c_comb = c.0
                    .iter()
                    .map(|(mult, var)| (*mult, &symbols.get(var).unwrap().combination))
                    .fold(LinearCombination::zero(), |acc, term| acc + term);

                cs.enforce(
                    || "inline constraint",
                    |lc| lc + &a_comb,
                    |lc| lc + &b_comb,
                    |lc| lc + &c_comb,
                );
            }
        }
        Ok(())
    }
}

impl Function {
    fn flatten<CS: ConstraintSystem<Bls12>>(
        &self,
        cs: &mut CS,
        functions: &Vec<Function>,
        arguments: &Vec<AssignedLinearCombination>,
    ) -> Result<Vec<AssignedLinearCombination>, SynthesisError> {
        let mut cs = &mut cs.namespace(|| self.id.to_string());

        // map from identifier to CS wire
        let mut symbols = HashMap::new();

        symbols.insert(
            "~one".to_string(),
            AssignedLinearCombination {
                combination: LinearCombination::zero() + CS::one(),
                value: number_to_fr::<Bls12>(1),
            },
        );

        for (i, assignment) in arguments.iter().enumerate() {
            symbols.insert(self.arguments[i].clone(), assignment.clone());
        }

        let is_main = self.id == "main";

        for statement in &self.statements {
            statement.flatten(&mut cs, &functions, &mut symbols)?;
        }

        let out = if is_main {
            self.return_variables
                .iter()
                .map(|comb| {
                    let assigned_comb = comb.0.iter().map(|(mult, var)| (mult, symbols.get(var).unwrap())).fold(AssignedLinearCombination { combination: LinearCombination::zero(), value: <Bls12 as Engine>::Fr::zero()}, |acc, (mult, comb)| {
                        let mut val = comb.value;
                        val.mul_assign(mult);

                        AssignedLinearCombination {
                            combination: acc.combination + (*mult, &comb.combination),
                            value: val
                        }
                    });
                    // if we're in the main function, we need to make the return variable a public input in the CS
                    let out = cs.alloc_input(|| "out", || Ok(assigned_comb.value)).unwrap();

                    // CONSTRAINT
                    cs.enforce(
                        || "out = e * ~one",
                        |lc| lc + &assigned_comb.combination,
                        |lc| lc + CS::one(),
                        |lc| lc + out,
                    );

                    AssignedLinearCombination {
                        combination: LinearCombination::zero() + out,
                        value: assigned_comb.value,
                    }
                }).collect()
        }
        // otherwise, we already have everything we need to return
        else {
            self.return_variables
                .iter()
                .map(|comb| {
                    let assigned_comb = comb.0.iter().map(|(mult, var)| (mult, symbols.get(var).unwrap())).fold(AssignedLinearCombination { combination: LinearCombination::zero(), value: <Bls12 as Engine>::Fr::zero()}, |acc, (mult, comb)| {
                        let mut val = comb.value;
                        val.mul_assign(mult);

                        AssignedLinearCombination {
                            combination: acc.combination + (*mult, &comb.combination),
                            value: val
                        }
                    });

                    assigned_comb
                }).collect()        
        };

        Ok(out)
    }
}

impl Program {
    fn synthesize<CS: ConstraintSystem<Bls12>>(self, cs: &mut CS, arguments: Vec<<Bls12 as Engine>::Fr>) -> Result<(), SynthesisError> {
        let argument_combs = arguments.iter().enumerate().map(|(index, arg)| {
            let var = cs.alloc(|| format!("INPUT{}", index), || Ok(*arg)).unwrap();
            AssignedLinearCombination {
                combination: LinearCombination::zero() + var,
                value: *arg
            }
        }).collect();
        match self.functions.iter().find(|f| f.id == "main".to_string()).unwrap().flatten(cs, &self.functions, &argument_combs) {
            Ok(..) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

pub struct Computation {
    pub program: Program,
    pub inputs: Vec<<Bls12 as Engine>::Fr>
}

impl Circuit<Bls12> for Computation {
    fn synthesize<CS: ConstraintSystem<Bls12>>(self, cs: &mut CS) -> Result<(), SynthesisError> {
        self.program.synthesize(cs, self.inputs)
    }
}

pub fn number_to_fr<E: Engine>(number: usize) -> E::Fr {
    E::Fr::from_str(&number.to_string()).unwrap()
}

#[cfg(test)]
mod tests {
	use super::*;
	// We're going to use the Groth16 proving system.
	use bellman::groth16::{
	    create_random_proof, generate_random_parameters, prepare_verifying_key, verify_proof, Proof,
	};

	extern crate rand;
	// For randomness (during paramgen and proof generation)
	use self::rand::thread_rng;

    use helpers::*;

	#[test]
	fn function_calls() {
	    let rng = &mut thread_rng();

	    // our toy program
	    // def main():
	    //	  # x = 3
	    //	  x == 3
	    //    a = foo(x)
	    //    return a
	    //
	    // def foo(b):
	    //	  # c = b + 1
	    //	  c == b + 1
	    //    return c
	    //
	    // should return 4

	    let foo = Function {
	        id: "foo".to_string(),
	        arguments: vec!["b".to_string()],
	        return_variables: vec![LinCombMemory(vec![(number_to_fr::<Bls12>(1), "c".to_string())])],
	        statements: vec![
	            Statement::Directive(
	                BellmanDirectiveStatement {
                        outputs: vec![String::from("c")],
                        helper: BellmanHelper::RankOne(RankOneHelper {
                            l1: LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]),
                            l2: LinCombMemory(vec![
                                (number_to_fr::<Bls12>(1), "b".to_string()),
                                (number_to_fr::<Bls12>(1), "~one".to_string()),
                            ]),
                        })
                    }
	            ),
	            Statement::Constraint(
	                LinCombMemory(vec![(number_to_fr::<Bls12>(1), "c".to_string())]),
	                LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]),
	                LinCombMemory(vec![
	                    (number_to_fr::<Bls12>(1), "b".to_string()),
	                    (number_to_fr::<Bls12>(1), "~one".to_string()),
	                ]),
	            ),
	        ],
	    };

	    let main = Function {
	        id: "main".to_string(),
	        arguments: vec![],
	        return_variables: vec![LinCombMemory(vec![(number_to_fr::<Bls12>(1), "a".to_string())])],
	        statements: vec![
	            Statement::Directive(
                    BellmanDirectiveStatement {
                        outputs: vec![String::from("x")],
                        helper: BellmanHelper::RankOne(RankOneHelper {
                            l1: LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]),
                            l2: LinCombMemory(vec![(number_to_fr::<Bls12>(3), "~one".to_string())]),
                        }),
                    }
                ),
	            Statement::Constraint(
	                LinCombMemory(vec![(number_to_fr::<Bls12>(1), "x".to_string())]),
	                LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]),
	                LinCombMemory(vec![(number_to_fr::<Bls12>(3), "~one".to_string())]),
	            ),
	            Statement::Definition(
	                vec![String::from("a")],
	                FunctionCall("foo".to_string(), vec![LinCombMemory(vec![(number_to_fr::<Bls12>(1), "x".to_string())])]),
	            ),
	        ],
	    };

	    let program = Program { functions: vec![foo, main] };

        let computation = Computation {
            program: program,
            inputs: vec![]
        };

	    println!("Creating parameters...");

	    let params = generate_random_parameters(computation, rng).unwrap();

	    // Prepare the verification key (for proof verification)
	    let pvk = prepare_verifying_key(&params.vk);

	    println!("Creating proofs...");

	    let mut proof_vec = vec![];

	    // Create an instance of our circuit (pass inputs, they were not needed for the setup)
        let foo = Function {
            id: "foo".to_string(),
            arguments: vec!["b".to_string()],
            return_variables: vec![LinCombMemory(vec![(number_to_fr::<Bls12>(1), "c".to_string())])],
            statements: vec![
                Statement::Directive(
                    BellmanDirectiveStatement {
                        outputs: vec![String::from("c")],
                        helper: BellmanHelper::RankOne(RankOneHelper {
                            l1: LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]),
                            l2: LinCombMemory(vec![
                                (number_to_fr::<Bls12>(1), "b".to_string()),
                                (number_to_fr::<Bls12>(1), "~one".to_string()),
                            ]),
                        })
                    }
                ),
                Statement::Constraint(
                    LinCombMemory(vec![(number_to_fr::<Bls12>(1), "c".to_string())]),
                    LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]),
                    LinCombMemory(vec![
                        (number_to_fr::<Bls12>(1), "b".to_string()),
                        (number_to_fr::<Bls12>(1), "~one".to_string()),
                    ]),
                ),
            ],
        };

        let main = Function {
            id: "main".to_string(),
            arguments: vec![],
            return_variables: vec![LinCombMemory(vec![(number_to_fr::<Bls12>(1), "a".to_string())])],
            statements: vec![
                Statement::Directive(
                    BellmanDirectiveStatement {
                        outputs: vec![String::from("x")],
                        helper: BellmanHelper::RankOne(RankOneHelper {
                            l1: LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]),
                            l2: LinCombMemory(vec![(number_to_fr::<Bls12>(3), "~one".to_string())]),
                        }),
                    }
                ),
                Statement::Constraint(
                    LinCombMemory(vec![(number_to_fr::<Bls12>(1), "x".to_string())]),
                    LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]),
                    LinCombMemory(vec![(number_to_fr::<Bls12>(3), "~one".to_string())]),
                ),
                Statement::Definition(
                    vec![String::from("a")],
                    FunctionCall("foo".to_string(), vec![LinCombMemory(vec![(number_to_fr::<Bls12>(1), "x".to_string())])]),
                ),
            ],
        };
	    let program = Program { functions: vec![foo, main] };

        let computation = Computation {
            program,
            inputs: vec![]
        };

	    // Create a groth16 proof with our parameters.
	    let proof = create_random_proof(computation, &params, rng).unwrap();

	    proof.write(&mut proof_vec).unwrap();

	    let proof = Proof::read(&proof_vec[..]).unwrap();

	    // Check the proof
	    assert!(verify_proof(&pvk, &proof, &[number_to_fr::<Bls12>(4)]).unwrap());
	}

    #[test]
    fn primefield_conversion() {
        // for 123, the field size should not matter
        let expected = <Bls12 as Engine>::Fr::from_str("123").unwrap();
        let original = Bls12Field::from("123");
        assert_eq!(expected, original.into());

        //for -1, the field size matches so it should succeed
        let mut expected = <Bls12 as Engine>::Fr::from_str("0").unwrap();
        expected.sub_assign(&<Bls12 as Engine>::Fr::one());
        let original = Bls12Field::from("0") - Bls12Field::from("1");
        assert_eq!(expected, original.into());
    }
} 
