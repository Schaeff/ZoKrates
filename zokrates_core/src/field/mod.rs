//
// @file field.rs
// @author Dennis Kuhnert <dennis.kuhnert@campus.tu-berlin.de>
// @author Jacob Eberhardt <jacob.eberhardt@tu-berlin.de>
// @date 2017

use num::{One, Zero};
use num::bigint::{BigInt};
use std::convert::From;
use std::ops::{Add, Div, Mul, Sub};
use std::fmt::{Debug, Display};

mod bn;
mod bls12;

pub use self::bn::PrimeField;
pub use self::bls12::Bls12Field;

pub trait Pow<RHS> {
    type Output;
    fn pow(self, RHS) -> Self::Output;
}

pub trait Field
    : From<i32>
    + From<u32>
    + From<usize>
    + for<'a> From<&'a str>
    + Zero
    + One
    + Clone
    + PartialEq
    + PartialOrd
    + Display
    + Debug
    + Add<Self, Output = Self>
    + for<'a> Add<&'a Self, Output = Self>
    + Sub<Self, Output = Self>
    + for<'a> Sub<&'a Self, Output = Self>
    + Mul<Self, Output = Self>
    + for<'a> Mul<&'a Self, Output = Self>
    + Div<Self, Output = Self>
    + for<'a> Div<&'a Self, Output = Self>
    + Pow<usize, Output = Self>
    + Pow<Self, Output = Self>
    + for<'a> Pow<&'a Self, Output = Self> {

    /// Returns this `Field`'s contents as little-endian byte vector
    fn into_byte_vector(&self) -> Vec<u8>;
    /// Returns an element of this `Field` from a little-endian byte vector
    fn from_byte_vector(Vec<u8>) -> Self;
    /// Returns this `Field`'s contents as decimal string
    fn to_dec_string(&self) -> String;
    /// Returns an element of this `Field` from a decimal string
    fn from_dec_string(val: String) -> Self;
    /// Returns the multiplicative inverse, i.e.: self * self.inverse_mul() = Self::one()
    fn inverse_mul(&self) -> Self;
    /// Returns the smallest value that can be represented by this field type.
    fn min_value() -> Self;
    /// Returns the largest value that can be represented by this field type.
    fn max_value() -> Self;
    /// Returns the number of required bits to represent this field type.
    fn get_required_bits() -> usize;
}



/// Calculates the gcd using a iterative implementation of the extended euclidian algorithm.
/// Returning `(d, s, t)` so that `d = s * a + t * b`
///
/// # Arguments
/// * `a` - First number as `BigInt`
/// * `b` - Second number as `BigInt`
fn extended_euclid(a: &BigInt, b: &BigInt) -> (BigInt, BigInt, BigInt) {
    let (mut s, mut old_s) = (BigInt::zero(), BigInt::one());
    let (mut t, mut old_t) = (BigInt::one(), BigInt::zero());
    let (mut r, mut old_r) = (b.clone(), a.clone());
    while !&r.is_zero() {
        let quotient = &old_r / &r;
        let tmp_r = old_r.clone();
        old_r = r.clone();
        r = &tmp_r - &quotient * &r;
        let tmp_s = old_s.clone();
        old_s = s.clone();
        s = &tmp_s - &quotient * &s;
        let tmp_t = old_t.clone();
        old_t = t.clone();
        t = &tmp_t - &quotient * &t;
    }
    return (old_r, old_s, old_t);
}
