#![feature(box_patterns, box_syntax)]

#[macro_use]
extern crate lazy_static;
extern crate num;
extern crate reduce; // better reduce function than Iter.fold
extern crate serde; // serialization deserialization
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate bincode;
extern crate regex;
extern crate bellman;
extern crate pairing;

mod parser;
mod imports;
mod semantics;
mod substitution;
mod flatten;
mod flatten_bellman;
mod optimizer;
#[cfg(feature = "libsnark")]
mod standard;
mod helpers;
mod types;
mod typed_absy;
mod bellman_absy;

pub mod absy;
pub mod flat_absy;
pub mod flat_absy_bellman;
pub mod compile;
pub mod r1cs;
pub mod field;
pub mod verification;
#[cfg(feature = "libsnark")]
pub mod libsnark;