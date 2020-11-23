use crate::parser::ParseFieldError;

use bstr::ByteSlice;
use lazy_static::lazy_static;
use regex::bytes::Regex;

/// Trait for the types that can be parsed and used as segment IDs;
/// will probably only be usize and Vec<u8>.
pub trait SegmentId: Sized + Default {
    const ERROR: ParseFieldError;

    fn parse_id(input: &[u8]) -> Option<Self>;

    #[inline]
    fn parse_next<I>(mut input: I) -> Result<Self, ParseFieldError>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let next = input.next().ok_or(ParseFieldError::MissingFields)?;
        Self::parse_id(next.as_ref()).ok_or(Self::ERROR)
    }

    fn display(&self) -> String;
}

impl SegmentId for usize {
    const ERROR: ParseFieldError = ParseFieldError::UintIdError;

    #[inline]
    fn parse_id(input: &[u8]) -> Option<Self> {
        input.to_str().ok()?.parse::<usize>().ok()
    }

    #[inline]
    fn display(&self) -> String {
        self.to_string()
    }
}

impl SegmentId for Vec<u8> {
    const ERROR: ParseFieldError = ParseFieldError::Utf8Error;

    #[inline]
    fn parse_id(input: &[u8]) -> Option<Self> {
        lazy_static! {
            static ref RE: Regex =
                Regex::new(r"(?-u)[!-)+-<>-~][!-~]*").unwrap();
        }
        RE.find(input).map(|s| Vec::from(s.as_bytes()))
    }

    #[inline]
    fn display(&self) -> String {
        self.as_bstr().to_string()
    }
}
