use bstr::{BStr, BString, ByteSlice, ByteVec};
use std::collections::HashMap;

use lazy_static::lazy_static;
use regex::bytes::Regex;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum OptFieldVal {
    A(u8),
    Int(i64),
    Float(f32),
    Z(BString),
    J(BString),
    H(Vec<u32>),
    BInt(Vec<i64>),
    BFloat(Vec<f32>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct OptField {
    pub tag: [u8; 2],
    pub value: OptFieldVal,
}

impl OptField {
    pub fn parse(input: &[u8]) -> Option<Self> {
        lazy_static! {
            static ref RE_TYPE: Regex = Regex::new(r"(?-u)[AifZJHB]").unwrap();
            static ref RE_TAG: Regex =
                Regex::new(r"(?-u)[A-Za-z][A-Za-z0-9]").unwrap();
            static ref RE_CHAR: Regex = Regex::new(r"(?-u)[!-~]").unwrap();
            static ref RE_INT: Regex = Regex::new(r"(?-u)[-+]?[0-9]+").unwrap();
            static ref RE_FLOAT: Regex =
                Regex::new(r"(?-u)[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?")
                    .unwrap();
            static ref RE_STRING: Regex = Regex::new(r"(?-u)[ !-~]+").unwrap();
            static ref RE_BYTES: Regex = Regex::new(r"(?-u)[0-9A-F]+").unwrap();
        }

        use std::str::from_utf8;
        use OptFieldVal::*;

        let mut fields = input.split_str(b":");

        // let o_tag = fields.next().and_then(OptTag::from_bytes)?;
        let o_tag = fields.next()?;

        let o_type = fields
            .next()
            .and_then(|s| RE_TYPE.find(s))
            .map(|s| s.as_bytes()[0])?;

        let o_contents = fields.next()?;

        let o_val = match o_type {
            // char
            b'A' => RE_CHAR.find(o_contents).map(|s| s.as_bytes()[0]).map(A),
            // int
            b'i' => RE_INT
                .find(o_contents)
                .and_then(|s| from_utf8(s.as_bytes()).ok())
                .and_then(|s| s.parse().ok())
                .map(Int),
            // float
            b'f' => RE_FLOAT
                .find(o_contents)
                .and_then(|s| from_utf8(s.as_bytes()).ok())
                .and_then(|s| s.parse().ok())
                .map(Float),
            // string
            b'Z' => RE_STRING
                .find(o_contents)
                .map(|s| s.as_bytes().into())
                .map(Z),
            // JSON string
            b'J' => RE_STRING
                .find(o_contents)
                .map(|s| s.as_bytes().into())
                .map(J),
            // bytearray
            b'H' => RE_BYTES
                .find(o_contents)
                .and_then(|s| from_utf8(s.as_bytes()).ok())
                .map(|s| s.chars().filter_map(|c| c.to_digit(16)))
                .map(|s| H(s.collect())),
            // float or int array
            b'B' => {
                let first = o_contents[0];
                let rest = o_contents[1..]
                    .split_str(b",")
                    .filter_map(|s| from_utf8(s.as_bytes()).ok());
                if first == b'f' {
                    Some(BFloat(rest.filter_map(|s| s.parse().ok()).collect()))
                } else {
                    Some(BInt(rest.filter_map(|s| s.parse().ok()).collect()))
                }
            }
            _ => panic!(
                "Tried to parse optional field with unknown type '{}'",
                o_type,
            ),
        }?;

        Some(OptField {
            tag: [o_tag[0], o_tag[1]],
            value: o_val,
        })
    }
}

impl std::fmt::Display for OptField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OptFieldVal::*;

        write!(f, "{}{}:", char::from(self.tag[0]), char::from(self.tag[1]))?;

        match &self.value {
            A(x) => write!(f, "A:{}", x),
            Int(x) => write!(f, "i:{}", x),
            Float(x) => write!(f, "f:{}", x),
            Z(x) => write!(f, "Z:{}", x),
            J(x) => write!(f, "J:{}", x),
            H(x) => {
                write!(f, "H:")?;
                for a in x {
                    write!(f, "{:x}", a)?
                }
                Ok(())
            }
            BInt(x) => {
                write!(f, "B:I{}", x[0])?;
                for a in x[1..].iter() {
                    write!(f, ",{}", a)?
                }
                Ok(())
            }
            BFloat(x) => {
                write!(f, "B:F{}", x[0])?;
                for a in x[1..].iter() {
                    write!(f, ",{}", a)?
                }
                Ok(())
            }
        }
    }
}

pub type OptionalFields = Vec<OptField>;

// parse_optionals returns Self, not Option, since, well, they're optional anyway
pub trait OptFields: Sized + Default {
    fn get_field(&self, tag: &[u8]) -> Option<&OptField>;

    fn fields(&self) -> &[OptField];

    fn parse<T>(input: T) -> Self
    where
        T: IntoIterator,
        T::Item: AsRef<[u8]>;
}

impl OptFields for () {
    fn get_field(&self, _: &[u8]) -> Option<&OptField> {
        None
    }

    fn fields(&self) -> &[OptField] {
        &[]
    }

    fn parse<T>(_input: T) -> Self
    where
        T: IntoIterator,
        T::Item: AsRef<[u8]>,
    {
        ()
    }
}

impl OptFields for Vec<OptField> {
    fn get_field(&self, tag: &[u8]) -> Option<&OptField> {
        self.iter().find(|o| o.tag == tag).map(|v| v)
    }

    fn fields(&self) -> &[OptField] {
        self.as_slice()
    }

    fn parse<T>(input: T) -> Self
    where
        T: IntoIterator,
        T::Item: AsRef<[u8]>,
    {
        input
            .into_iter()
            .filter_map(|f| OptField::parse(f.as_ref()))
            .collect()
    }
}
