#[cfg(feature = "libsnark")]
mod libsnark_gadget;
mod rust;
mod rank_one;

#[cfg(feature = "libsnark")]
pub use self::libsnark_gadget::LibsnarkGadgetHelper;
use pairing::Engine;
pub use self::rust::RustHelper;
pub use self::rank_one::RankOneHelper;
use pairing::bls12_381::{Bls12, Fr};
use std::fmt;
use field::{Field};


#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct DirectiveStatement {
	pub inputs: Vec<String>,
	pub outputs: Vec<String>,
	pub helper: Helper
}

#[derive(Clone, PartialEq, Debug)]
pub struct BellmanDirectiveStatement {
	pub outputs: Vec<String>,
	pub helper: BellmanHelper
}

impl fmt::Display for BellmanDirectiveStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	write!(f, "# {} = {}", self.outputs.join(", "), self.helper)
    }
}

impl fmt::Display for DirectiveStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	write!(f, "# {} = {}", self.outputs.join(", "), self.helper)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum BellmanHelper {
	#[cfg(feature = "libsnark")]
	LibsnarkGadget(LibsnarkGadgetHelper),
	Rust(RustHelper),
	RankOne(RankOneHelper),
}

impl fmt::Display for BellmanHelper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match *self {
    		#[cfg(feature = "libsnark")]
    		BellmanHelper::LibsnarkGadget(ref h) => write!(f, "LibsnarkGadget::{}", h),
    		BellmanHelper::Rust(ref h) => write!(f, "Rust::{}", h),
    		BellmanHelper::RankOne(ref h) => write!(f, "RankOnBls12::{}", h),
    	}
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum Helper {
	#[cfg(feature = "libsnark")]
	LibsnarkGadget(LibsnarkGadgetHelper),
	Rust(RustHelper),
}

impl fmt::Display for Helper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match *self {
    		#[cfg(feature = "libsnark")]
    		Helper::LibsnarkGadget(ref h) => write!(f, "LibsnarkGadget::{}", h),
    		Helper::Rust(ref h) => write!(f, "Rust::{}", h),
    	}
    }
}

pub trait BellmanExecutable
	: Signed {
	fn execute_bellman(&self, inputs: &Vec<<Bls12 as Engine>::Fr>) -> Result<Vec<<Bls12 as Engine>::Fr>, String>;
	fn get_input_names(&self) -> Vec<String>;
}

pub trait Executable<T: Field>
	: Signed {
	fn execute(&self, inputs: &Vec<T>) -> Result<Vec<T>, String>;
}

pub trait Signed {
	fn get_signature(&self) -> (usize, usize);
}

impl<T: Field> Executable<T> for Helper {
	fn execute(&self, inputs: &Vec<T>) -> Result<Vec<T>, String> {
		let (expected_input_count, expected_output_count) = self.get_signature();
		assert!(inputs.len() == expected_input_count);

		let result = match self {
			#[cfg(feature = "libsnark")]
			Helper::LibsnarkGadget(helper) => helper.execute(inputs),
			Helper::Rust(helper) => helper.execute(inputs),
		};

		match result {
			Ok(ref r) if r.len() != expected_output_count => Err(format!("invalid witness size: is {} but should be {}", r.len(), expected_output_count).to_string()),
			r => r
		}
	}
}

impl BellmanExecutable for BellmanHelper {
	fn execute_bellman(&self, inputs: &Vec<<Bls12 as Engine>::Fr>) -> Result<Vec<<Bls12 as Engine>::Fr>, String> {
		let (expected_input_count, expected_output_count) = self.get_signature();
		assert!(inputs.len() == expected_input_count);

		let result = match self {
			#[cfg(feature = "libsnark")]
			BellmanHelper::LibsnarkGadget(helper) => helper.execute_bellman(inputs),
			BellmanHelper::Rust(helper) => helper.execute_bellman(inputs),
			BellmanHelper::RankOne(helper) => helper.execute_bellman(inputs),
		};

		match result {
			Ok(ref r) if r.len() != expected_output_count => Err(format!("invalid witness size: is {} but should be {}", r.len(), expected_output_count).to_string()),
			r => r
		}
	}
	fn get_input_names(&self) -> Vec<String> {
		match self {
			#[cfg(feature = "libsnark")]
			BellmanHelper::LibsnarkGadget(helper) => helper.get_input_names(),
			BellmanHelper::Rust(helper) => helper.get_input_names(),
			BellmanHelper::RankOne(helper) => helper.get_input_names(),
		}
	}
}

impl Signed for BellmanHelper {
	fn get_signature(&self) -> (usize, usize) {
		match self {
			#[cfg(feature = "libsnark")]
			BellmanHelper::LibsnarkGadget(helper) => helper.get_signature(),
			BellmanHelper::Rust(helper) => helper.get_signature(),
			BellmanHelper::RankOne(helper) => helper.get_signature(),
		}
	}
}

impl Signed for Helper {
	fn get_signature(&self) -> (usize, usize) {
		match self {
			#[cfg(feature = "libsnark")]
			Helper::LibsnarkGadget(helper) => helper.get_signature(),
			Helper::Rust(helper) => helper.get_signature(),
		}
	}
}

#[cfg(test)]
mod tests {
	use field::PrimeField;
	use super::*;

	#[cfg(feature = "libsnark")]
	mod sha256libsnark {
		use super::*;

		#[test]
		fn execute() {
			let sha = LibsnarkGadgetHelper::Sha256Compress;
			// second vector here https://homes.esat.kuleuven.be/~nsmart/MPC/sha-256-test.txt
			let inputs = vec![0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,0,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,0,0,0,0,1,1,0,1,1,0,0,0,1,1,1,0,0,0,0,0,1,1,1,0,1,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,0,1,0,0,1,1,1,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,1,1,0,1,0,0,1,0,1,1,1,0,0,0,1,0,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,1,0,0,1,1,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,1,0,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,1,0,1,1,1,0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,1,0,0,1,1,1,0,1,0,0,0,1,1,1,0,1,1,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,1,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,1];
			let r = sha.execute(&inputs.iter().map(|&i| PrimeField::from(i)).collect()).unwrap();
			let r1 = &r[513..769]; // index of the result
			let res: Vec<PrimeField> = vec![1,1,1,1,1,1,0,0,1,0,0,1,1,0,0,1,1,0,1,0,0,0,1,0,1,1,0,1,1,1,1,1,1,0,0,0,1,0,0,0,1,1,1,1,0,1,0,0,0,0,1,0,1,0,1,0,0,1,1,1,1,0,1,0,0,1,1,1,1,0,1,1,1,0,1,1,1,0,0,1,1,1,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,1,1,0,0,1,1,0,1,1,1,0,0,0,1,1,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,1,0,1,1,0,0,1,1,1,0,1,0,1,0,1,0,1,1,1,1,1,1,0,0,1,1,1,0,1,0,1,0,1,1,0,1,1,1,0,0,1,1,0,1,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,1,0,1,0,1,0,0,1,1,1,0,0,1,1,0,0,0,0,1,1,0,0,0,1,0,1,0,1,1,0,1,0,1,0,1,1,1,1,1,0,1,0,0,0,0,1,0,0,1,0,1,0,0,1,1,1].iter().map(|&i| PrimeField::from(i)).collect();
			assert_eq!(r1, &res[..]);
		}
	}


	mod eq_condition {

		// Wanted: (Y = (X != 0) ? 1 : 0)
        // # Y = if X == 0 then 0 else 1 fi
        // # M = if X == 0 then 1 else 1/X fi

		use super::*;

		#[test]
		fn execute() {
			let cond_eq = RustHelper::ConditionEq(String::from("a"));
			let inputs = vec![0];
			let r = cond_eq.execute(&inputs.iter().map(|&i| PrimeField::from(i)).collect()).unwrap();
			let res: Vec<PrimeField> = vec![0, 1].iter().map(|&i| PrimeField::from(i)).collect();
			assert_eq!(r, &res[..]);
		}

		#[test]
		fn execute_non_eq() {
			let cond_eq = RustHelper::ConditionEq(String::from("a"));
			let inputs = vec![1];
			let r = cond_eq.execute(&inputs.iter().map(|&i| PrimeField::from(i)).collect()).unwrap();
			let res: Vec<PrimeField> = vec![1, 1].iter().map(|&i| PrimeField::from(i)).collect();
			assert_eq!(r, &res[..]);
		}
	}
}



