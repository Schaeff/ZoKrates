use std::fmt::Display;
use std::fmt::Debug;
use field::Pow;
use field::Field;
use field::extended_euclid;
use std::convert::From;
use std::ops::{Add, Div, Mul, Sub};
use std::fmt;
use num::{Num, Integer, One, Zero};
use num::bigint::{BigInt, BigUint, Sign, ToBigInt};
use serde::{Serialize, Serializer};
use serde::de::{Deserialize, Deserializer, Visitor};

lazy_static! {
    static ref P: BigInt = BigInt::parse_bytes(b"21888242871839275222246405745257275088548364400416034343698204186575808495617", 10).unwrap();
}

#[derive(PartialEq, PartialOrd, Clone, Eq, Ord)]
pub struct PrimeField {
    value: BigInt,
}

impl Field for PrimeField {
    fn into_byte_vector(&self) -> Vec<u8> {
        match self.value.to_biguint() {
            Option::Some(val) => val.to_bytes_le(),
            Option::None => panic!("Should never happen."),
        }
    }

    fn from_byte_vector(bytes: Vec<u8>) -> Self {
        let uval = BigUint::from_bytes_le(bytes.as_slice());
        PrimeField{value: BigInt::from_biguint(Sign::Plus, uval)}
    }

    fn to_dec_string(&self) -> String {
        self.value.to_str_radix(10)
    }

    fn from_dec_string(val: String) -> Self {
        PrimeField{value: BigInt::from_str_radix(val.as_str(), 10).unwrap()}
    }

    fn inverse_mul(&self) -> PrimeField {
        let (b, s, _) = extended_euclid(&self.value, &*P);
        assert_eq!(b, BigInt::one());
        PrimeField {
            value: &s - s.div_floor(&*P) * &*P,
        }
    }
    fn min_value() -> PrimeField {
        PrimeField {
            value: ToBigInt::to_bigint(&0).unwrap(),
        }
    }
    fn max_value() -> PrimeField {
        PrimeField {
            value: &*P - ToBigInt::to_bigint(&1).unwrap(),
        }
    }
    fn get_required_bits() -> usize {
        (*P).bits()
    }
}

impl Display for PrimeField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value.to_str_radix(10))
    }
}

impl Debug for PrimeField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value.to_str_radix(10))
    }
}

impl From<i32> for PrimeField {
    fn from(num: i32) -> Self {
        let x = ToBigInt::to_bigint(&num).unwrap();
        PrimeField {
            value: &x - x.div_floor(&*P) * &*P,
        }
    }
}

impl From<u32> for PrimeField {
    fn from(num: u32) -> Self {
        let x = ToBigInt::to_bigint(&num).unwrap();
        PrimeField {
            value: &x - x.div_floor(&*P) * &*P,
        }
    }
}

impl From<usize> for PrimeField {
    fn from(num: usize) -> Self {
        let x = ToBigInt::to_bigint(&num).unwrap();
        PrimeField {
            value: &x - x.div_floor(&*P) * &*P,
        }
    }
}

impl<'a> From<&'a str> for PrimeField {
    fn from(s: &'a str) -> Self {
        let x = match BigInt::parse_bytes(s.as_bytes(), 10) {
            Some(x) => x,
            None => panic!("Could not parse {:?} to BigInt!", &s),
        };
        PrimeField {
            value: &x - x.div_floor(&*P) * &*P,
        }
    }
}

impl Zero for PrimeField {
    fn zero() -> PrimeField {
        PrimeField {
            value: ToBigInt::to_bigint(&0).unwrap(),
        }
    }
    fn is_zero(&self) -> bool {
        self.value == ToBigInt::to_bigint(&0).unwrap()
    }
}

impl One for PrimeField {
    fn one() -> PrimeField {
        PrimeField {
            value: ToBigInt::to_bigint(&1).unwrap(),
        }
    }
}

impl Add<PrimeField> for PrimeField {
    type Output = PrimeField;

    fn add(self, other: PrimeField) -> PrimeField {
        PrimeField {
            value: (self.value + other.value) % &*P,
        }
    }
}

impl<'a> Add<&'a PrimeField> for PrimeField {
    type Output = PrimeField;

    fn add(self, other: &PrimeField) -> PrimeField {
        PrimeField {
            value: (self.value + other.value.clone()) % &*P,
        }
    }
}

impl Sub<PrimeField> for PrimeField {
    type Output = PrimeField;

    fn sub(self, other: PrimeField) -> PrimeField {
        let x = self.value - other.value;
        PrimeField {
            value: &x - x.div_floor(&*P) * &*P,
        }
    }
}

impl<'a> Sub<&'a PrimeField> for PrimeField {
    type Output = PrimeField;

    fn sub(self, other: &PrimeField) -> PrimeField {
        let x = self.value - other.value.clone();
        PrimeField {
            value: &x - x.div_floor(&*P) * &*P,
        }
    }
}

impl Mul<PrimeField> for PrimeField {
    type Output = PrimeField;

    fn mul(self, other: PrimeField) -> PrimeField {
        PrimeField {
            value: (self.value * other.value) % &*P,
        }
    }
}

impl<'a> Mul<&'a PrimeField> for PrimeField {
    type Output = PrimeField;

    fn mul(self, other: &PrimeField) -> PrimeField {
        PrimeField {
            value: (self.value * other.value.clone()) % &*P,
        }
    }
}

impl Div<PrimeField> for PrimeField {
    type Output = PrimeField;

    fn div(self, other: PrimeField) -> PrimeField {
        self * other.inverse_mul()
    }
}

impl<'a> Div<&'a PrimeField> for PrimeField {
    type Output = PrimeField;

    fn div(self, other: &PrimeField) -> PrimeField {
        self / other.clone()
    }
}

impl Pow<usize> for PrimeField {
    type Output = PrimeField;

    fn pow(self, exp: usize) -> PrimeField {
        let mut res = PrimeField::from(1);
        for _ in 0..exp {
            res = res * &self;
        }
        res
    }
}

impl Pow<PrimeField> for PrimeField {
    type Output = PrimeField;

    fn pow(self, exp: PrimeField) -> PrimeField {
        let mut res = PrimeField::one();
        let mut current = PrimeField::zero();
        loop {
            if current >= exp {
                return res;
            }
            res = res * &self;
            current = current + PrimeField::one();
        }
    }
}

impl<'a> Pow<&'a PrimeField> for PrimeField {
    type Output = PrimeField;

    fn pow(self, exp: &'a PrimeField) -> PrimeField {
        let mut res = PrimeField::one();
        let mut current = PrimeField::zero();
        loop {
            if &current >= exp {
                return res;
            }
            res = res * &self;
            current = current + PrimeField::one();
        }
    }
}

// custom serde serialization
impl Serialize for PrimeField {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        // serializer.serialize_bytes(&(*self.value.to_biguint().to_bytes_le().as_slice()))
        serializer.serialize_bytes(&(*self.into_byte_vector().as_slice()))
    }
}

// custom serde deserialization

struct FieldPrimeVisitor;

impl FieldPrimeVisitor {
    fn new() -> Self {
        FieldPrimeVisitor{}
    }
}

impl<'de> Visitor<'de> for FieldPrimeVisitor {
    type Value = PrimeField;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("struct PrimeField")
    }

    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E> {
        let val = BigUint::from_bytes_le(v).to_bigint().unwrap();
        Ok(PrimeField{value: val})
    }
}

impl<'de> Deserialize<'de> for PrimeField {

    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where D: Deserializer<'de>
    {
        deserializer.deserialize_bytes(FieldPrimeVisitor::new())
    }
}

#[cfg(test)]
mod field_prime {
    use super::*;
    use bincode::{serialize, deserialize , Infinite};

    #[test]
    fn positive_number() {
        assert_eq!(
            "1234245612".parse::<BigInt>().unwrap(),
            PrimeField::from("1234245612").value
        );
    }

    #[test]
    fn negative_number() {
        assert_eq!(
            P.checked_sub(&"12".parse::<BigInt>().unwrap()).unwrap(),
            PrimeField::from("-12").value
        );
    }

    #[test]
    fn addition() {
        assert_eq!(
            "65484493".parse::<BigInt>().unwrap(),
            (PrimeField::from("65416358") + PrimeField::from("68135")).value
        );
        assert_eq!(
            "65484493".parse::<BigInt>().unwrap(),
            (PrimeField::from("65416358") + &PrimeField::from("68135")).value
        );
    }

    #[test]
    fn addition_negative_small() {
        assert_eq!(
            "3".parse::<BigInt>().unwrap(),
            (PrimeField::from("5") + PrimeField::from("-2")).value
        );
        assert_eq!(
            "3".parse::<BigInt>().unwrap(),
            (PrimeField::from("5") + &PrimeField::from("-2")).value
        );
    }

    #[test]
    fn addition_negative() {
        assert_eq!(
            "65348223".parse::<BigInt>().unwrap(),
            (PrimeField::from("65416358") + PrimeField::from("-68135")).value
        );
        assert_eq!(
            "65348223".parse::<BigInt>().unwrap(),
            (PrimeField::from("65416358") + &PrimeField::from("-68135")).value
        );
    }

    #[test]
    fn subtraction() {
        assert_eq!(
            "65348223".parse::<BigInt>().unwrap(),
            (PrimeField::from("65416358") - PrimeField::from("68135")).value
        );
        assert_eq!(
            "65348223".parse::<BigInt>().unwrap(),
            (PrimeField::from("65416358") - &PrimeField::from("68135")).value
        );
    }

    #[test]
    fn subtraction_negative() {
        assert_eq!(
            "65484493".parse::<BigInt>().unwrap(),
            (PrimeField::from("65416358") - PrimeField::from("-68135")).value
        );
        assert_eq!(
            "65484493".parse::<BigInt>().unwrap(),
            (PrimeField::from("65416358") - &PrimeField::from("-68135")).value
        );
    }

    #[test]
    fn subtraction_overflow() {
        assert_eq!(
            "21888242871839275222246405745257275088548364400416034343698204186575743147394"
                .parse::<BigInt>()
                .unwrap(),
            (PrimeField::from("68135") - PrimeField::from("65416358")).value
        );
        assert_eq!(
            "21888242871839275222246405745257275088548364400416034343698204186575743147394"
                .parse::<BigInt>()
                .unwrap(),
            (PrimeField::from("68135") - &PrimeField::from("65416358")).value
        );
    }

    #[test]
    fn multiplication() {
        assert_eq!(
            "13472".parse::<BigInt>().unwrap(),
            (PrimeField::from("32") * PrimeField::from("421")).value
        );
        assert_eq!(
            "13472".parse::<BigInt>().unwrap(),
            (PrimeField::from("32") * &PrimeField::from("421")).value
        );
    }

    #[test]
    fn multiplication_negative() {
        assert_eq!(
            "21888242871839275222246405745257275088548364400416034343698204186575808014369"
                .parse::<BigInt>()
                .unwrap(),
            (PrimeField::from("54") * PrimeField::from("-8912")).value
        );
        assert_eq!(
            "21888242871839275222246405745257275088548364400416034343698204186575808014369"
                .parse::<BigInt>()
                .unwrap(),
            (PrimeField::from("54") * &PrimeField::from("-8912")).value
        );
    }

    #[test]
    fn multiplication_two_negative() {
        assert_eq!(
            "648".parse::<BigInt>().unwrap(),
            (PrimeField::from("-54") * PrimeField::from("-12")).value
        );
        assert_eq!(
            "648".parse::<BigInt>().unwrap(),
            (PrimeField::from("-54") * &PrimeField::from("-12")).value
        );
    }

    #[test]
    fn multiplication_overflow() {
        assert_eq!(
            "6042471409729479866150380306128222617399890671095126975526159292198160466142"
                .parse::<BigInt>()
                .unwrap(),
            (PrimeField::from(
                "21888242871839225222246405785257275088694311157297823662689037894645225727"
            ) *
                PrimeField::from("218882428715392752222464057432572755886923"))
                .value
        );
        assert_eq!(
            "6042471409729479866150380306128222617399890671095126975526159292198160466142"
                .parse::<BigInt>()
                .unwrap(),
            (PrimeField::from(
                "21888242871839225222246405785257275088694311157297823662689037894645225727"
            ) *
                &PrimeField::from("218882428715392752222464057432572755886923"))
                .value
        );
    }

    #[test]
    fn division() {
        assert_eq!(
            PrimeField::from(4),
            PrimeField::from(48) / PrimeField::from(12)
        );
        assert_eq!(
            PrimeField::from(4),
            PrimeField::from(48) / &PrimeField::from(12)
        );
    }

    #[test]
    fn division_negative() {
        let res = PrimeField::from(-54) / PrimeField::from(12);
        assert_eq!(PrimeField::from(-54), PrimeField::from(12) * res);
    }

    #[test]
    fn division_two_negative() {
        let res = PrimeField::from(-12) / PrimeField::from(-85);
        assert_eq!(PrimeField::from(-12), PrimeField::from(-85) * res);
    }

    #[test]
    fn pow_small() {
        assert_eq!(
            "8".parse::<BigInt>().unwrap(),
            (PrimeField::from("2").pow(PrimeField::from("3"))).value
        );
        assert_eq!(
            "8".parse::<BigInt>().unwrap(),
            (PrimeField::from("2").pow(&PrimeField::from("3"))).value
        );
    }

    #[test]
    fn pow_usize() {
        assert_eq!(
            "614787626176508399616".parse::<BigInt>().unwrap(),
            (PrimeField::from("54").pow(12)).value
        );
    }

    #[test]
    fn pow() {
        assert_eq!(
            "614787626176508399616".parse::<BigInt>().unwrap(),
            (PrimeField::from("54").pow(PrimeField::from("12"))).value
        );
        assert_eq!(
            "614787626176508399616".parse::<BigInt>().unwrap(),
            (PrimeField::from("54").pow(&PrimeField::from("12"))).value
        );
    }

    #[test]
    fn pow_negative() {
        assert_eq!(
            "21888242871839275222246405745257275088548364400416034343686819230535502784513"
                .parse::<BigInt>()
                .unwrap(),
            (PrimeField::from("-54").pow(PrimeField::from("11"))).value
        );
        assert_eq!(
            "21888242871839275222246405745257275088548364400416034343686819230535502784513"
                .parse::<BigInt>()
                .unwrap(),
            (PrimeField::from("-54").pow(&PrimeField::from("11"))).value
        );
    }

    #[test]
    fn serde_ser_deser() {
        let serialized = &serialize(&PrimeField::from("11"), Infinite).unwrap();
        let deserialized = deserialize(serialized).unwrap();
        assert_eq!(PrimeField::from("11"), deserialized);
    }

    #[test]
    fn bytes_ser_deser() {
        let fp = PrimeField::from("101");
        let bv = fp.into_byte_vector();
        assert_eq!(fp, PrimeField::from_byte_vector(bv));
    }

    #[test]
    fn dec_string_ser_deser() {
        let fp = PrimeField::from("101");
        let bv = fp.to_dec_string();
        assert_eq!(fp, PrimeField::from_dec_string(bv));
    }
}

#[test]
fn bigint_assertions() {
    let x = BigInt::parse_bytes(b"65", 10).unwrap();
    assert_eq!(&x + &x, BigInt::parse_bytes(b"130", 10).unwrap());
    assert_eq!(
        "1".parse::<BigInt>().unwrap(),
        "3".parse::<BigInt>()
            .unwrap()
            .div_floor(&"2".parse::<BigInt>().unwrap())
    );
    assert_eq!(
        "-2".parse::<BigInt>().unwrap(),
        "-3".parse::<BigInt>()
            .unwrap()
            .div_floor(&"2".parse::<BigInt>().unwrap())
    );
}

#[test]
fn test_extended_euclid() {
    assert_eq!(
        (
            ToBigInt::to_bigint(&1).unwrap(),
            ToBigInt::to_bigint(&-9).unwrap(),
            ToBigInt::to_bigint(&47).unwrap()
        ),
        extended_euclid(
            &ToBigInt::to_bigint(&120).unwrap(),
            &ToBigInt::to_bigint(&23).unwrap()
        )
    );
    assert_eq!(
        (
            ToBigInt::to_bigint(&2).unwrap(),
            ToBigInt::to_bigint(&2).unwrap(),
            ToBigInt::to_bigint(&-11).unwrap()
        ),
        extended_euclid(
            &ToBigInt::to_bigint(&122).unwrap(),
            &ToBigInt::to_bigint(&22).unwrap()
        )
    );
    assert_eq!(
        (
            ToBigInt::to_bigint(&2).unwrap(),
            ToBigInt::to_bigint(&-9).unwrap(),
            ToBigInt::to_bigint(&47).unwrap()
        ),
        extended_euclid(
            &ToBigInt::to_bigint(&240).unwrap(),
            &ToBigInt::to_bigint(&46).unwrap()
        )
    );
    let (b, s, _) = extended_euclid(&ToBigInt::to_bigint(&253).unwrap(), &*P);
    assert_eq!(b, BigInt::one());
    let s_field = PrimeField {
        value: &s - s.div_floor(&*P) * &*P,
    };
    assert_eq!(
        PrimeField::from(
            "12717674712096337777352654721552646000065650461901806515903699665717959876900"
        ),
        s_field
    );
}