use pairing::bls12_381::Bls12;
use helpers::BellmanExecutable;
use pairing::Engine;
use std::fmt;
use helpers::{Signed, Executable};
use field::{Field};
use pairing::Field as BellmanField;

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum RustHelper {
	Identity(Vec<String>),
	ConditionEq(String),
}

impl fmt::Display for RustHelper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match *self {
    		RustHelper::Identity(ref names) => write!(f, "Identity({})", names.join(", ")),
    		RustHelper::ConditionEq(ref name) => write!(f, "ConditionEq({})", name),
    	}
    }
}

impl Signed for RustHelper {
	fn get_signature(&self) -> (usize, usize) {
		match self {
			RustHelper::Identity(names) => (names.len(), names.len()),
			RustHelper::ConditionEq(..) => (1, 2),
		}
	}
}


impl<T: Field> Executable<T> for RustHelper {
	fn execute(&self, inputs: &Vec<T>) -> Result<Vec<T>, String> {
		match self {
			// having the right output size is ensured by the caller
			RustHelper::Identity(..) => Ok(inputs.clone()),
			RustHelper::ConditionEq(..) => {
				match inputs[0].is_zero() {
					true => Ok(vec![T::zero(), T::one()]),
					false => Ok(vec![T::one(), T::one() / inputs[0].clone()])
				}
			},
		}
	}
}

impl BellmanExecutable for RustHelper {
	fn execute_bellman(&self, inputs: &Vec<<Bls12 as Engine>::Fr>) -> Result<Vec<<Bls12 as Engine>::Fr>, String> {
		match self {
			RustHelper::Identity(..) => Ok(inputs.clone()),
			RustHelper::ConditionEq(..) => {
				match inputs[0].is_zero() {
					true => Ok(vec![<Bls12 as Engine>::Fr::zero(), <Bls12 as Engine>::Fr::one()]),
					false => Ok(vec![<Bls12 as Engine>::Fr::one(), inputs[0].inverse().unwrap()])
				}
			},
		}
	}

	fn get_input_names(&self) -> Vec<String> {
		match *self {
			RustHelper::Identity(ref names) => names.clone(),
			RustHelper::ConditionEq(ref name) => vec![name.clone()],
		}
	}
}