use std::fmt;
use field::{Field};

#[cfg(not(feature = "nolibsnark"))]
use libsnark::*;

use serde_json;
use standard;

use byteorder::{BigEndian, WriteBytesExt};
use bitreader::BitReader;

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
	Identity,
	FieldToU32,
	U32ToField
}

impl fmt::Display for RustHelper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match *self {
    		RustHelper::Identity => write!(f, "Identity"),
    		RustHelper::FieldToU32 => write!(f, "FieldToU32"),
    		RustHelper::U32ToField => write!(f, "U32ToField"),
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
				#[cfg(feature = "nolibsnark")]
				{
					Err(format!("Libsnark is not available"))
				}

				#[cfg(not(feature = "nolibsnark"))]
				{
					let witness_result: Result<standard::Witness, serde_json::Error> = serde_json::from_str(&get_sha256_witness(inputs));

					if let Err(e) = witness_result {
						return Err(format!("{}", e));
					}

					Ok(witness_result.unwrap().variables.iter().map(|&i| T::from(i)).collect())
				}
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
			RustHelper::FieldToU32 => {
				// only expected to work if the input is smaller than 2^64
				// get the input
				let input = &inputs[0];

				// convert it to u64 as it could be bigger than 2^32
				let r_overflow = input.to_dec_string().parse::<u64>().unwrap();

				// only keep the u32 part
				let r = r_overflow % (2u64.pow(32));

				// determine whether an overflow happened
				let has_overflow = r != r_overflow;

				// convert to vector of bytes
				let mut wtr = vec![];
				wtr.write_u32::<BigEndian>(r as u32).unwrap();

				let mut reader = BitReader::new(&wtr);

				// convert to vector of bits
				let mut bit_vect = vec![];
				for i in 0..32 {
					bit_vect.push(reader.read_u32(1).unwrap());
				}

				bit_vect.push(has_overflow as u32);

				// convert to Field elements and return
				Ok(bit_vect.iter().map(|&i| T::from(i)).collect())
			},
			RustHelper::U32ToField => {
				Ok(vec![
					T::from(
						inputs.iter().enumerate()
							.map(|(i, v)| {
								v.to_dec_string().parse::<u32>().unwrap() * 2u32.pow((31 - i) as u32)
							})
							.fold(0, |acc, v| acc + v))
					]
				)
			}
		}
	}
}

impl Signed for RustHelper {
	fn get_signature(&self) -> (usize, usize) {
		match self {
			RustHelper::Identity => (1, 1),
			RustHelper::FieldToU32 => (1, 33),
			RustHelper::U32ToField => (32, 1)
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

	#[test]
	fn execute_field_to_u32() {
		let f2u = RustHelper::FieldToU32;
		let inputs = vec![4294967295u32];
		let r = f2u.execute(&inputs.iter().map(|&i| FieldPrime::from(i)).collect()).unwrap();
		let res: Vec<FieldPrime> = vec![1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0].iter().map(|&i| FieldPrime::from(i)).collect();
		assert_eq!(r, &res[..]);
	}

	#[test]
	fn execute_field_to_u32_1() {
		let f2u = RustHelper::FieldToU32;
		let inputs = vec![4294967294u32];
		let r = f2u.execute(&inputs.iter().map(|&i| FieldPrime::from(i)).collect()).unwrap();
		let res: Vec<FieldPrime> = vec![1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0].iter().map(|&i| FieldPrime::from(i)).collect();
		assert_eq!(r, &res[..]);
	}

	#[test]
	fn execute_u32_to_field() {
		let u2f = RustHelper::U32ToField;
		let inputs = vec![0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0];
		let r = u2f.execute(&inputs.iter().map(|&i| FieldPrime::from(i)).collect()).unwrap();
		let res: Vec<FieldPrime> = vec![2].iter().map(|&i| FieldPrime::from(i)).collect();
		assert_eq!(r, &res[..]);
	}
}



