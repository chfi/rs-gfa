use bstr::{BStr, BString, ByteSlice, ByteVec};
use lazy_static::lazy_static;
use regex::bytes::Regex;
use std::ops::Range;

use crate::gfa::*;
use crate::optfields::*;

// TODO the path in this definitely needs to be redefined, and the parser fixed
#[derive(Debug)]
pub struct GAF<T: OptFields> {
    pub seq_name: BString,
    pub seq_len: usize,
    pub seq_range: Range<usize>,
    pub strand: Orientation,
    pub path: BString,
    pub path_len: usize,
    pub path_range: Range<usize>,
    pub residue_matches: usize,
    pub block_length: usize,
    pub quality: u8,
    pub optional: T,
}

#[derive(Debug)]
pub struct PAF<T: OptFields> {
    pub query_seq_name: BString,
    pub query_seq_len: usize,
    pub query_seq_range: Range<usize>,
    pub strand: Orientation,
    pub target_seq_name: BString,
    pub target_seq_len: usize,
    pub target_seq_range: Range<usize>,
    pub residue_matches: usize,
    pub block_length: usize,
    pub quality: u8,
    pub optional: T,
}

fn parse_next<I, T>(mut input: I) -> Option<T>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
    T: std::str::FromStr,
{
    let tmp = input.next()?;
    let bytes = tmp.as_ref();
    std::str::from_utf8(bytes).ok().and_then(|p| p.parse().ok())
}

fn parse_seq_fields<I>(mut input: I) -> Option<(BString, usize, Range<usize>)>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
{
    let name = input.next()?.as_ref().into();
    let len = parse_next(&mut input)?;
    let start = parse_next(&mut input)?;
    let end = parse_next(&mut input)?;

    Some((name, len, start..end))
}

pub fn parse_paf<I, T>(mut input: I) -> Option<PAF<T>>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
    T: OptFields,
{
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"(?-u)/([><][^\s><]+(:\d+-\d+)?)+|([^\s><]+)/")
                .unwrap();
    }
    let (query_seq_name, query_seq_len, query_seq_range) =
        parse_seq_fields(&mut input)?;

    let strand = input.next().and_then(Orientation::from_bytes)?;

    let (target_seq_name, target_seq_len, target_seq_range) =
        parse_seq_fields(&mut input)?;

    let residue_matches = parse_next(&mut input)?;
    let block_length = parse_next(&mut input)?;
    let quality = parse_next(&mut input)?;

    let optional = T::parse(input);

    Some(PAF {
        query_seq_name,
        query_seq_len,
        query_seq_range,
        strand,
        target_seq_name,
        target_seq_len,
        target_seq_range,
        residue_matches,
        block_length,
        quality,
        optional,
    })
}

// Since GAF and PAF are *essentially* the same, we just reuse the PAF
// parser and add a check that the path matches the spec regex
pub fn parse_gaf<I, T>(input: I) -> Option<GAF<T>>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
    T: OptFields,
{
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"(?-u)/([><][^\s><]+(:\d+-\d+)?)+|([^\s><]+)/")
                .unwrap();
    }

    let paf: PAF<T> = parse_paf(input)?;
    let path = paf.target_seq_name;

    if !RE.is_match(&path) {
        return None;
    }

    Some(GAF {
        path,
        seq_name: paf.query_seq_name,
        seq_len: paf.query_seq_len,
        seq_range: paf.query_seq_range,
        strand: paf.strand,
        path_len: paf.target_seq_len,
        path_range: paf.target_seq_range,
        residue_matches: paf.residue_matches,
        block_length: paf.block_length,
        quality: paf.quality,
        optional: paf.optional,
    })
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CIGAROp {
    M = 0,
    I = 1,
    D = 2,
    N = 3,
    S = 4,
    H = 5,
    P = 6,
    E = 7,
    X = 8,
}

impl std::fmt::Display for CIGAROp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CIGAROp::*;
        let sym = match self {
            M => 'M',
            I => 'I',
            D => 'D',
            N => 'N',
            S => 'S',
            H => 'H',
            P => 'P',
            E => '=',
            X => 'X',
        };

        write!(f, "{}", sym)
    }
}

impl CIGAROp {
    fn from_u8(byte: u8) -> Option<CIGAROp> {
        match byte {
            b'M' => Some(CIGAROp::M),
            b'I' => Some(CIGAROp::I),
            b'D' => Some(CIGAROp::D),
            b'N' => Some(CIGAROp::N),
            b'S' => Some(CIGAROp::S),
            b'H' => Some(CIGAROp::H),
            b'P' => Some(CIGAROp::P),
            b'=' => Some(CIGAROp::E),
            b'X' => Some(CIGAROp::X),
            _ => None,
        }
    }
}

impl std::str::FromStr for CIGAROp {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "M" => Ok(CIGAROp::M),
            "I" => Ok(CIGAROp::I),
            "D" => Ok(CIGAROp::D),
            "N" => Ok(CIGAROp::N),
            "S" => Ok(CIGAROp::S),
            "H" => Ok(CIGAROp::H),
            "P" => Ok(CIGAROp::P),
            "=" => Ok(CIGAROp::E),
            "X" => Ok(CIGAROp::X),
            _ => Err("Could not parse CIGAR operation"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CIGAR(pub Vec<(CIGAROp, u32)>);

impl CIGAR {
    fn parse_first(bytes: &[u8]) -> Option<((CIGAROp, u32), &[u8])> {
        if bytes[0].is_ascii_digit() {
            let op_ix = bytes.find_byteset(b"MIDNSHP=X")?;
            let num = std::str::from_utf8(&bytes[0..op_ix]).ok()?;
            let num: u32 = num.parse().ok()?;
            let op = CIGAROp::from_u8(bytes[op_ix])?;
            let rest = &bytes[op_ix + 1..];
            Some(((op, num), rest))
        } else {
            None
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        if bytes.is_empty() {
            return None;
        }
        let mut cigar: Vec<(CIGAROp, u32)> = Vec::new();
        let mut bytes = bytes;
        while bytes.len() > 0 {
            let (cg, rest) = Self::parse_first(bytes)?;
            cigar.push(cg);
            bytes = rest;
        }

        Some(CIGAR(cigar))
    }
}

impl std::fmt::Display for CIGAR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (op, len) in self.0.iter() {
            write!(f, "{}{}", len, op)?
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cigar_display() {
        let input = b"20M12D3M4N9S10H5P11=9X";
        let input_str = std::str::from_utf8(input).unwrap();
        let cigar = CIGAR::from_bytes(input).unwrap();
        let cigstr = cigar.to_string();
        assert_eq!(input_str, cigstr);
    }

    #[test]
    fn cigar_parser() {
        use CIGAROp::*;

        let input = b"20M12D3M4N9S10H5P11=9X";
        let cigar = CIGAR::from_bytes(input);
        assert_eq!(
            Some(CIGAR(vec![
                (M, 20),
                (D, 12),
                (M, 3),
                (N, 4),
                (S, 9),
                (H, 10),
                (P, 5),
                (E, 11),
                (X, 9)
            ])),
            cigar
        );

        assert_eq!(None, CIGAR::from_bytes(b"M20"));
        assert_eq!(None, CIGAR::from_bytes(b"20"));
        assert_eq!(None, CIGAR::from_bytes(b""));
    }
}
