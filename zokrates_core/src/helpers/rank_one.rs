use bellman_absy::LinCombMemory;
use flat_absy_bellman::FlatExpression;
use helpers::BellmanExecutable;
use pairing::Engine;
use std::fmt;
use helpers::{Signed, Executable};
use field::{Field};
use pairing::Field as BellmanField;
use pairing::bls12_381::Bls12;
use std::collections::{HashSet, HashMap};
use bellman_absy::number_to_fr;


#[derive(Clone, PartialEq, Debug)]
pub struct RankOneHelper {
	pub l1: LinCombMemory,
	pub l2: LinCombMemory,
}

impl fmt::Display for RankOneHelper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	write!(f, "{} * {}", self.l1, self.l2)
    }
}

impl Signed for RankOneHelper {
	fn get_signature(&self) -> (usize, usize) {
		let mut variables = HashSet::new();
		for (_, s) in &self.l1.0 {
			variables.insert(s);
		}

		for (_, s) in &self.l2.0 {
			variables.insert(s);
		}
		(variables.len(), 1)
	}
}

impl BellmanExecutable for RankOneHelper {
	fn execute_bellman(&self, inputs: &Vec<<Bls12 as Engine>::Fr>) -> Result<Vec<<Bls12 as Engine>::Fr>, String> {
		// build mapping from variable name to input index
		let mut index = HashMap::new();
		let mut inputs = inputs.iter();

		let mut l1 = self.l1.0.iter().map(|(m, s)| {
			// unse or_insert_with to avoid iterating when the entry is already there
			let mut val = index.entry(s).or_insert_with(|| inputs.next().unwrap()).clone();
			val.mul_assign(&m);
			val
		}).fold(number_to_fr::<Bls12>(0), |mut acc, e| {
			acc.add_assign(&e);
			acc
		});

		let l2 = self.l2.0.iter().map(|(m, s)| {
			let mut val = index.entry(s).or_insert_with(|| inputs.next().unwrap()).clone();
			val.mul_assign(&m);
			val
		}).fold(number_to_fr::<Bls12>(0), |mut acc, e| {
			acc.add_assign(&e);
			acc
		});

		l1.mul_assign(&l2);

		Ok(vec![l1])
	}

	fn get_input_names(&self) -> Vec<String> {
		// build mapping from variable name to input index
		let mut variable_set = HashSet::new();
		let mut variables = vec![];

		for (_, s) in &self.l1.0 {
			match variable_set.contains(s) {
				false => {
					variable_set.insert(s);
					variables.push(s.clone());
				},
				true => {}
			}
		}

		for (_, s) in &self.l2.0 {
			match variable_set.contains(s) {
				false => {
					variable_set.insert(s);
					variables.push(s.clone());
				},
				true => {}
			}	
		}

		variables
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn empty_lin_combs() {
		let l1 = LinCombMemory(vec![]);
		let l2 = LinCombMemory(vec![]);
		let helper = RankOneHelper { l1, l2 };

		assert_eq!(helper.get_signature(), (0, 1));
		assert_eq!(helper.execute_bellman(&vec![]), Ok(vec![number_to_fr::<Bls12>(0)]));
		assert_eq!(helper.get_input_names(), vec![String::new(); 0]);
	}

	#[test]
	fn lin_combs() {
		let l1 = LinCombMemory(vec![(number_to_fr::<Bls12>(1), String::from("foo")), (number_to_fr::<Bls12>(2), String::from("bar"))]);
		let l2 = LinCombMemory(vec![(number_to_fr::<Bls12>(1), String::from("bar")), (number_to_fr::<Bls12>(2), String::from("baz"))]);
		let helper = RankOneHelper { l1, l2 };

		// expected: (1*1 + 2*2)(2*1 + 3*2) = 40

		assert_eq!(helper.get_signature(), (3, 1));
		assert_eq!(helper.execute_bellman(&vec![number_to_fr::<Bls12>(1), number_to_fr::<Bls12>(2), number_to_fr::<Bls12>(3)]), Ok(vec![number_to_fr::<Bls12>(40)]));
		assert_eq!(helper.get_input_names(), vec![String::from("foo"), String::from("bar"), String::from("baz")]);
	}
}