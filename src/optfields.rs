use bstr::{BString, ByteSlice};

use lazy_static::lazy_static;
use regex::bytes::Regex;

/// These type aliases are useful for configuring the parsers
pub type OptionalFields = Vec<OptField>;
pub type NoOptionalFields = ();

/// An optional field a la SAM
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct OptField {
    pub tag: [u8; 2],
    pub value: OptFieldVal,
}

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

impl OptField {
    /// Panics if the provided tag doesn't match the regex [A-Z][A-Za-z]
    pub fn tag(t: &[u8]) -> [u8; 2] {
        assert_eq!(t.len(), 2);
        assert!(t[0].is_ascii_alphabetic());
        assert!(t[1].is_ascii_alphanumeric());
        [t[0], t[1]]
    }

    /// Create a new OptField from a tag name and a value.
    pub fn new(tag: &[u8], value: OptFieldVal) -> Self {
        let tag = OptField::tag(tag);
        OptField { tag, value }
    }

    /// Parses an optional field from a bytestring in the format
    /// <TAG>:<TYPE>:<VALUE>
    pub fn parse(input: &[u8]) -> Option<Self> {
        lazy_static! {
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

        let o_tag = &input[0..=1];

        let o_type = input[3];
        if !b"AifZJHB".contains(&o_type) {
            return None;
        }

        let o_contents = &input[5..];

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

        Some(Self::new(o_tag, o_val))
    }
}

/// The Display implementation output that can be parsed back to OptField
impl std::fmt::Display for OptField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OptFieldVal::*;

        write!(f, "{}{}:", char::from(self.tag[0]), char::from(self.tag[1]))?;

        match &self.value {
            A(x) => write!(f, "A:{}", char::from(*x)),
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

/// OptFields describes how to parse, store, and query optional fields
pub trait OptFields: Sized + Default + Clone {
    /// Return the optional field with the given tag, if it exists.
    fn get_field(&self, tag: &[u8]) -> Option<&OptField>;

    /// Return all optional fields.
    fn fields(&self) -> &[OptField];

    /// Given a sequence of bytestrings, parse them as optional fields
    /// to create a collection
    fn parse<T>(input: T) -> Self
    where
        T: IntoIterator,
        T::Item: AsRef<[u8]>;
}

/// This implementation is useful for performance if we don't actually
/// need any optional fields
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
    }
}

/// Stores all the optional fields in a vector
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
