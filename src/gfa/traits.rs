use crate::parser::ParseFieldError;

use bstr::{BString, ByteSlice};
use lazy_static::lazy_static;
use regex::bytes::Regex;

/// Trait for the types that can be parsed and used as segment IDs;
/// will probably only be usize and BString.
pub trait SegmentId: Sized + Default {
    const ERROR: ParseFieldError;

    fn parse_id(input: &[u8]) -> Option<Self>;

    fn parse_next<I>(mut input: I) -> Result<Self, ParseFieldError>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let next = input.next().ok_or(ParseFieldError::MissingFields)?;
        Self::parse_id(next.as_ref()).ok_or(Self::ERROR)
    }
}

impl SegmentId for usize {
    const ERROR: ParseFieldError = ParseFieldError::UintIdError;

    fn parse_id(input: &[u8]) -> Option<Self> {
        input.to_str().ok()?.parse::<usize>().ok()
    }
}

impl SegmentId for BString {
    const ERROR: ParseFieldError = ParseFieldError::Utf8Error;

    fn parse_id(input: &[u8]) -> Option<Self> {
        lazy_static! {
            static ref RE: Regex =
                Regex::new(r"(?-u)[!-)+-<>-~][!-~]*").unwrap();
        }
        RE.find(input).map(|s| BString::from(s.as_bytes()))
    }
}
