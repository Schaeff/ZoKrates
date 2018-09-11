//! Module containing structs and enums to represent a program.
//!
//! @file absy.rs
//! @author Dennis Kuhnert <dennis.kuhnert@campus.tu-berlin.de>
//! @author Jacob Eberhardt <jacob.eberhardt@tu-berlin.de>
//! @date 2017

pub mod flat_parameter;

const BINARY_SEPARATOR: &str = "_b";

use field::Bls12Field;
use pairing::bls12_381::Bls12;
use pairing::Engine;
use self::flat_parameter::FlatParameter;
use std::fmt;
use std::collections::{BTreeMap};
use field::*;
use substitution::Substitution;
#[cfg(feature = "libsnark")]
use standard;
use helpers::{BellmanDirectiveStatement, BellmanExecutable};

use num::{One, Zero};

#[derive(Clone)]
pub struct FlatProg {
    /// FlatFunctions of the program
    pub functions: Vec<FlatFunction>,
}


impl FlatProg {
    // only main flattened function is relevant here, as all other functions are unrolled into it
    // #[allow(dead_code)] // I don't want to remove this
    // pub fn get_witness(&self, inputs: Vec<Bls12Field>) -> Result<BTreeMap<String, Bls12Field>, Error> {
    //     let main = self.functions.iter().find(|x| x.id == "main").unwrap();
    //     main.get_witness(inputs)
    // }
}

impl fmt::Display for FlatProg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.functions
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl fmt::Debug for FlatProg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "flat_program(functions: {}\t)",
            self.functions
                .iter()
                .map(|x| format!("\t{:?}", x))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[cfg(feature = "libsnark")]
impl<T: Field> From<standard::R1CS> for FlatProg<Bls12Field> {
    fn from(r1cs: standard::R1CS) -> Self {
        FlatProg {
            functions: vec![r1cs.into()]
        }
    }
}


#[derive(Clone, PartialEq)]
pub struct FlatFunction {
    /// Name of the program
    pub id: String,
    /// Arguments of the function
    pub arguments: Vec<FlatParameter>,
    /// Vector of statements that are executed when running the function
    pub statements: Vec<FlatStatement>,
    /// number of returns
    pub return_count: usize,
}

impl FlatFunction {
    // pub fn get_witness(&self, inputs: Vec<Bls12Field>) -> Result<BTreeMap<String, Bls12Field>, Error> {
    //     assert!(self.arguments.len() == inputs.len());
    //     let mut witness = BTreeMap::new();
    //     witness.insert("~one".to_string(), Bls12Field::one());
    //     for (i, arg) in self.arguments.iter().enumerate() {
    //         witness.insert(arg.id.to_string(), inputs[i].clone());
    //     }
    //     for statement in &self.statements {
    //         println!("{}", statement);
    //         match *statement {
    //             FlatStatement::Return(ref list) => {
    //                 for (i, val) in list.expressions.iter().enumerate() {
    //                     let s = val.solve(&mut witness);
    //                     witness.insert(format!("~out_{}", i).to_string(), s);
    //                 }
    //             }
    //             FlatStatement::Definition(ref id, ref expr) => {
    //                 let s = expr.solve(&mut witness);
    //                 witness.insert(id.to_string(), s);
    //             },
    //             FlatStatement::Condition(ref lhs, ref rhs) => {
    //                 if lhs.solve(&mut witness) != rhs.solve(&mut witness) {
    //                     return Err(Error {
    //                         message: format!("Condition not satisfied: {} should equal {}", lhs, rhs)
    //                     });
    //                 }
    //             },
    //             FlatStatement::Directive(ref d) => {
    //                 let input_values: Vec<Bls12Field> = d.inputs.iter().map(|i| witness.get(i).unwrap().clone()).collect();
    //                 match d.helper.execute_bellman(&input_values.into_iter().map(|v| v.into()).collect()) {
    //                     Ok(res) => {
    //                         for (i, o) in d.outputs.iter().enumerate() {
    //                             witness.insert(o.to_string(), res[i].clone());
    //                         }
    //                         continue;
    //                     },
    //                     Err(message) => {
    //                         return Err(Error {
    //                             message: message
    //                         })
    //                     }
    //                 };
    //             },
    //             FlatStatement::FunctionCall(..) => {
    //                 unimplemented!()
    //             }
    //         }
    //     }
    //     Ok(witness)
    // }
}

impl fmt::Display for FlatFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "def {}({}):\n{}",
            self.id,
            self.arguments
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>()
                .join(","),
            self.statements
                .iter()
                .map(|x| format!("\t{}", x))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl fmt::Debug for FlatFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "FlatFunction(id: {:?}, arguments: {:?}, ...):\n{}",
            self.id,
            self.arguments,
            self.statements
                .iter()
                .map(|x| format!("\t{:?}", x))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

/// Calculates a flattened function based on a R1CS (A, B, C) and returns that flattened function:
/// * The Rank 1 Constraint System (R1CS) is defined as:
/// * `<A,x>*<B,x> = <C,x>` for a witness `x`
/// * Since the matrices in R1CS are usually sparse, the following encoding is used:
/// * For each constraint (i.e., row in the R1CS), only non-zero values are supplied and encoded as a tuple (index, value).
///
/// # Arguments
///
/// * r1cs - R1CS in standard JSON data format

#[derive(Clone, PartialEq)]
pub enum FlatStatement {
    Return(FlatExpressionList),
    Condition(FlatExpression, FlatExpression),
    Definition(String, FlatExpression),
    Directive(BellmanDirectiveStatement),
    FunctionCall(Vec<String>, String, Vec<FlatExpression>),
}

impl fmt::Display for FlatStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FlatStatement::Definition(ref lhs, ref rhs) => write!(f, "{} = {}", lhs, rhs),
            FlatStatement::Return(ref expr) => write!(f, "return {}", expr),
            FlatStatement::Condition(ref lhs, ref rhs) => write!(f, "{} == {}", lhs, rhs),
            FlatStatement::Directive(ref d) => write!(f, "{}", d),
            FlatStatement::FunctionCall(ref o, ref fun, ref i) => write!(f, "{:?} = {}({:?})", o, fun, i),
        }
    }
}

impl fmt::Debug for FlatStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FlatStatement::Definition(ref lhs, ref rhs) => write!(f, "{} = {}", lhs, rhs),
            FlatStatement::Return(ref expr) => write!(f, "FlatReturn({:?})", expr),
            FlatStatement::Condition(ref lhs, ref rhs) => write!(f, "FlatCondition({:?}, {:?})", lhs, rhs),
            FlatStatement::Directive(ref d) => write!(f, "{:?}", d),
            FlatStatement::FunctionCall(ref o, ref fun, ref i) => write!(f, "FunctionCall({:?},{}({:?})", o, fun, i),
        }
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum FlatExpression {
    Number(Bls12Field),
    Identifier(String),
    Add(Box<FlatExpression>, Box<FlatExpression>),
    Sub(Box<FlatExpression>, Box<FlatExpression>),
    Div(Box<FlatExpression>, Box<FlatExpression>),
    Mult(Box<FlatExpression>, Box<FlatExpression>)
}

impl FlatExpression {
    pub fn apply_substitution(self, substitution: &Substitution) -> FlatExpression {
        match self {
            e @ FlatExpression::Number(_) => e,
            FlatExpression::Identifier(v) => {
                let mut new_name = v.to_string();
                loop {
                    match substitution.get(&new_name) {
                        Some(x) => new_name = x.to_string(),
                        None => return FlatExpression::Identifier(new_name),
                    }
                }
            }
            FlatExpression::Add(e1, e2) => FlatExpression::Add(
                box e1.apply_substitution(substitution),
                box e2.apply_substitution(substitution),
            ),
            FlatExpression::Sub(e1, e2) => FlatExpression::Sub(
                box e1.apply_substitution(substitution),
                box e2.apply_substitution(substitution),
            ),
            FlatExpression::Mult(e1, e2) => FlatExpression::Mult(
                box e1.apply_substitution(substitution),
                box e2.apply_substitution(substitution),
            ),
            FlatExpression::Div(e1, e2) => FlatExpression::Div(
                box e1.apply_substitution(substitution),
                box e2.apply_substitution(substitution),
            )

        }
    }

    fn solve(&self, inputs: &mut BTreeMap<String, Bls12Field>) -> Bls12Field {
        match *self {
            FlatExpression::Number(ref x) => x.clone(),
            FlatExpression::Identifier(ref var) => {
                if let None = inputs.get(var) {
                    if var.contains(BINARY_SEPARATOR) {
                        let var_name = var.split(BINARY_SEPARATOR).collect::<Vec<_>>()[0];
                        let mut num = inputs[var_name].clone();
                        let bits = Bls12Field::get_required_bits();
                        for i in (0..bits).rev() {
                            if Bls12Field::from(2).pow(i) <= num {
                                num = num - Bls12Field::from(2).pow(i);
                                inputs.insert(format!("{}{}{}", &var_name, BINARY_SEPARATOR, i), Bls12Field::one());
                            } else {
                                inputs.insert(format!("{}{}{}", &var_name, BINARY_SEPARATOR, i), Bls12Field::zero());
                            }
                        }
                        assert_eq!(num, Bls12Field::zero());
                    } else {
                        panic!(
                            "Variable {:?} is undeclared in inputs: {:?}",
                            var,
                            inputs
                        );
                    }
                }
                inputs[var].clone()
            }
            FlatExpression::Add(ref x, ref y) => x.solve(inputs) + y.solve(inputs),
            FlatExpression::Sub(ref x, ref y) => x.solve(inputs) - y.solve(inputs),
            FlatExpression::Mult(ref x, ref y) => x.solve(inputs) * y.solve(inputs),
            FlatExpression::Div(ref x, ref y) => x.solve(inputs) / y.solve(inputs),
        }
    }

    pub fn is_linear(&self) -> bool {
        match *self {
            FlatExpression::Number(_) | FlatExpression::Identifier(_) => true,
            FlatExpression::Add(ref x, ref y) | FlatExpression::Sub(ref x, ref y) => {
                x.is_linear() && y.is_linear()
            }
            FlatExpression::Mult(ref x, ref y) | FlatExpression::Div(ref x, ref y) => {
                match (x.clone(), y.clone()) {
                    (box FlatExpression::Number(_), box FlatExpression::Number(_)) |
                    (box FlatExpression::Number(_), box FlatExpression::Identifier(_)) |
                    (box FlatExpression::Identifier(_), box FlatExpression::Number(_)) => true,
                    _ => false,
                }
            }
        }
    }
}

impl fmt::Display for FlatExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FlatExpression::Number(ref i) => write!(f, "{}", i),
            FlatExpression::Identifier(ref var) => write!(f, "{}", var),
            FlatExpression::Add(ref lhs, ref rhs) => write!(f, "({} + {})", lhs, rhs),
            FlatExpression::Sub(ref lhs, ref rhs) => write!(f, "({} - {})", lhs, rhs),
            FlatExpression::Mult(ref lhs, ref rhs) => write!(f, "({} * {})", lhs, rhs),
            FlatExpression::Div(ref lhs, ref rhs) => write!(f, "({} / {})", lhs, rhs),
        }
    }
}

impl fmt::Debug for FlatExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FlatExpression::Number(ref i) => write!(f, "Num({})", i),
            FlatExpression::Identifier(ref var) => write!(f, "Ide({})", var),
            FlatExpression::Add(ref lhs, ref rhs) => write!(f, "Add({:?}, {:?})", lhs, rhs),
            FlatExpression::Sub(ref lhs, ref rhs) => write!(f, "Sub({:?}, {:?})", lhs, rhs),
            FlatExpression::Mult(ref lhs, ref rhs) => write!(f, "Mult({:?}, {:?})", lhs, rhs),
            FlatExpression::Div(ref lhs, ref rhs) => write!(f, "Div({:?}, {:?})", lhs, rhs),
        }
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct FlatExpressionList {
    pub expressions: Vec<FlatExpression>
}

impl fmt::Display for FlatExpressionList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, param) in self.expressions.iter().enumerate() {
            try!(write!(f, "{}", param));
            if i < self.expressions.len() - 1 {
                try!(write!(f, ", "));
            }
        }
        write!(f, "")
    }
}

impl FlatExpressionList {
    pub fn apply_substitution(self, substitution: &Substitution) -> FlatExpressionList {
        FlatExpressionList {
            expressions: self.expressions.into_iter().map(|e| e.apply_substitution(substitution)).collect()
        }
    }
}

impl fmt::Debug for FlatExpressionList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ExpressionList({:?})", self.expressions)
    }
}

#[derive(PartialEq, Debug)]
pub struct Error {
    message: String
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
