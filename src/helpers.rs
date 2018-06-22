use std::fmt;
use field::{Field};

use libsnark::*;
use serde_json;
use standard;

use byteorder::{BigEndian, WriteBytesExt};

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct DirectiveStatement {
	pub inputs: Vec<String>,
	pub outputs: Vec<String>,
	pub helper: Helper
}

impl fmt::Display for DirectiveStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	write!(f, "# {} = {}({})", self.outputs.join(", "), self.helper, self.inputs.join(", "))
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum Helper {
	LibsnarkGadget(LibsnarkGadgetHelper),
	Rust(RustHelper)
}

impl fmt::Display for Helper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match *self {
    		Helper::LibsnarkGadget(ref h) => write!(f, "LibsnarkGadget::{}", h),
    		Helper::Rust(ref h) => write!(f, "Rust::{}", h)
    	}
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum LibsnarkGadgetHelper {
	Sha256Compress,
}

impl fmt::Display for LibsnarkGadgetHelper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match *self {
    		LibsnarkGadgetHelper::Sha256Compress => write!(f, "Sha256Compress"),
    	}
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum RustHelper {
	Identity
}

impl fmt::Display for RustHelper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match *self {
    		RustHelper::Identity => write!(f, "Identity"),
    	}
    }
}

pub trait Executable<T: Field>
	: Signed {
	fn execute(&self, inputs: &Vec<T>) -> Result<Vec<T>, String>;
}

pub trait Signed {
	fn get_signature(&self) -> (usize, usize);
}

impl<T: Field> Executable<T> for LibsnarkGadgetHelper {
	fn execute(&self, inputs: &Vec<T>) -> Result<Vec<T>, String> {
		match self {
			LibsnarkGadgetHelper::Sha256Compress => {
				let witness_result: Result<standard::Witness, serde_json::Error> = serde_json::from_str(&get_sha256_witness(inputs));

				if let Err(e) = witness_result {
					return Err(format!("{}", e));
				}

				Ok(witness_result.unwrap().variables.iter().map(|&i| T::from(i)).collect())
			},
		}
	}
}

impl Signed for LibsnarkGadgetHelper {
	fn get_signature(&self) -> (usize, usize) {
		match self {
			LibsnarkGadgetHelper::Sha256Compress => (512, 25561),
		}
	}
}

impl<T: Field> Executable<T> for RustHelper {
	fn execute(&self, inputs: &Vec<T>) -> Result<Vec<T>, String> {
		match self {
			RustHelper::Identity => Ok(inputs.clone()),
			//RustHelper::U32toField => Ok(inputs.iter().rev().enumerate().fold(0, |acc, (index, bit)| acc + 2 ** i))
			RustHelper::FieldToU32 => {
				let input = inputs[0];
				let u = input.to_dec_string().parse::<u32>().unwrap();
				let mut rdr = Cursor::new(vec![u]);
				
			}
		}
	}
}

impl Signed for RustHelper {
	fn get_signature(&self) -> (usize, usize) {
		match self {
			RustHelper::Identity => (1, 1),
		}
	}
}

impl<T: Field> Executable<T> for Helper {
	fn execute(&self, inputs: &Vec<T>) -> Result<Vec<T>, String> {
		let (expected_input_count, expected_output_count) = self.get_signature();
		assert!(inputs.len() == expected_input_count);

		let result = match self {
			Helper::LibsnarkGadget(helper) => helper.execute(inputs),
			Helper::Rust(helper) => helper.execute(inputs)
		};

		match result {
			Ok(ref r) if r.len() != expected_output_count => Err(format!("invalid witness size: is {} but should be {}", r.len(), expected_output_count).to_string()),
			r => r
		}
	}
}

impl Signed for Helper {
	fn get_signature(&self) -> (usize, usize) {
		match self {
			Helper::LibsnarkGadget(helper) => helper.get_signature(),
			Helper::Rust(helper) => helper.get_signature()
		}
	}
}

#[cfg(test)]
mod tests {
	use field::FieldPrime;
	use super::*;

	#[test]
	fn execute_sha() {
		let sha = LibsnarkGadgetHelper::Sha256Compress;
		// second vector here https://homes.esat.kuleuven.be/~nsmart/MPC/sha-256-test.txt
		let inputs = vec![0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,0,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,0,0,0,0,1,1,0,1,1,0,0,0,1,1,1,0,0,0,0,0,1,1,1,0,1,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,0,1,0,0,1,1,1,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,1,1,0,1,0,0,1,0,1,1,1,0,0,0,1,0,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,1,0,0,1,1,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,1,0,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,1,0,1,1,1,0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,1,0,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,1,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,1];
		let r = sha.execute(&inputs.iter().map(|&i| FieldPrime::from(i)).collect()).unwrap();
		let r1 = &r[513..769]; // index of the result
		let res: Vec<FieldPrime> = vec![1,1,1,1,1,1,0,0,1,0,0,1,1,0,0,1,1,0,1,0,0,0,1,0,1,1,0,1,1,1,1,1,1,0,0,0,1,0,0,0,1,1,1,1,0,1,0,0,0,0,1,0,1,0,1,0,0,1,1,1,1,0,1,0,0,1,1,1,1,0,1,1,1,0,1,1,1,0,0,1,1,1,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,1,1,0,0,1,1,0,1,1,1,0,0,0,1,1,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,1,0,1,1,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,1,1,1,0,0,1,1,1,0,1,0,1,0,1,1,0,1,1,1,0,0,1,1,0,1,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,1,0,1,0,1,0,0,1,1,1,0,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1,0,1,1,0,1,0,1,0,1,1,1,1,1,0,1,0,0,0,0,1,0,0,1,0,1,0,0,1,1,1].iter().map(|&i| FieldPrime::from(i)).collect();
		assert_eq!(r1, &res[..]);
	}
}



