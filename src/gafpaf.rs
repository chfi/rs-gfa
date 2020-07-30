use bstr::{BStr, BString, ByteSlice, ByteVec};
use lazy_static::lazy_static;
use regex::bytes::Regex;
use std::ops::Range;

use nom::{
    branch::{alt, permutation},
    bytes::complete::*,
    character::complete::digit1,
    combinator, multi,
    sequence::{preceded, separated_pair},
    IResult,
};

use crate::gfa::*;
use crate::optfields::*;

// TODO the path in this definitely needs to be redefined, and the parser fixed
#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum GAFStep {
    SegId(Orientation, BString),
    StableIntv(Orientation, BString, Range<usize>),
}

impl GAFStep {
    fn parse_orient(bytes: &[u8]) -> IResult<&[u8], Orientation> {
        use Orientation::*;
        let fwd = combinator::map(tag(">"), |_| Forward);
        let bwd = combinator::map(tag("<"), |_| Backward);
        alt((fwd, bwd))(bytes)
    }

    pub fn parse_step(i: &[u8]) -> IResult<&[u8], GAFStep> {
        let (i, orient) = Self::parse_orient(i)?;
        let (i, name) = is_not("<>: \t\r\n")(i)?;
        let name = name.into();

        let parse_digits = combinator::map(digit1, |bs| {
            let s = unsafe { std::str::from_utf8_unchecked(bs) };
            s.parse::<usize>().unwrap()
        });

        let parse_range = preceded(
            tag(":"),
            separated_pair(&parse_digits, tag("-"), &parse_digits),
        );

        let (i, range) = combinator::opt(parse_range)(i)?;
        if let Some((start, end)) = range {
            let range = start..end;
            Ok((i, GAFStep::StableIntv(orient, name, range)))
        } else {
            Ok((i, GAFStep::SegId(orient, name)))
        }
    }
}

pub enum GAFPath {
    StableId(BString),
    OrientIntv(Vec<GAFStep>),
}

impl GAFPath {
    pub fn parse_path(i: &[u8]) -> IResult<&[u8], GAFPath> {
        let (i, step) = combinator::opt(GAFStep::parse_step)(i)?;

        if let Some(step) = step {
            let (i, rest) = multi::many0(GAFStep::parse_step)(i)?;
            let step: GAFStep = step;
            let mut rest: Vec<GAFStep> = rest;
            rest.insert(0, step);

            Ok((i, GAFPath::OrientIntv(rest)))
        } else {
            let (i, stable_id) = is_not("\t")(i)?;
            Ok((i, GAFPath::StableId(stable_id.into())))
        }
    }
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

impl CIGAROp {
    fn to_u8(self) -> u8 {
        use CIGAROp::*;
        match self {
            M => b'M',
            I => b'I',
            D => b'D',
            N => b'N',
            S => b'S',
            H => b'H',
            P => b'P',
            E => b'=',
            X => b'X',
        }
    }

    fn from_u8(byte: u8) -> Option<CIGAROp> {
        use CIGAROp::*;
        match byte {
            b'M' => Some(M),
            b'I' => Some(I),
            b'D' => Some(D),
            b'N' => Some(N),
            b'S' => Some(S),
            b'H' => Some(H),
            b'P' => Some(P),
            b'=' => Some(E),
            b'X' => Some(X),
            _ => None,
        }
    }
}

impl std::fmt::Display for CIGAROp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sym = char::from(self.to_u8());
        write!(f, "{}", sym)
    }
}

impl std::str::FromStr for CIGAROp {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.as_bytes()
            .get(0)
            .cloned()
            .and_then(CIGAROp::from_u8)
            .ok_or("Could not parse CIGAR operation")
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
    fn parse_gaf_step() {
        use GAFStep::*;
        use Orientation::*;

        // segment ids
        let s1 = b">s1";
        let s2 = b"<segmentid>s1<s2";

        let (i1, step1) = GAFStep::parse_step(s1).unwrap();
        // The step is parsed as an oriented segment ID
        assert_eq!(SegId(Forward, "s1".into()), step1);
        // If there's just one segment ID to parse, it consumes the entire input
        assert_eq!(b"", i1);

        let (i2, step2) = GAFStep::parse_step(s2).unwrap();
        assert_eq!(SegId(Backward, "segmentid".into()), step2);
        assert_eq!(b">s1<s2", i2);

        // Can parse another step from the remaining bytes
        let (i2_2, step2_2) = GAFStep::parse_step(i2).unwrap();
        assert_eq!(b"<s2", i2_2);
        assert_eq!(SegId(Forward, "s1".into()), step2_2);

        // stable intervals
        let s3 = b">chr1:123-456";
        let s4 = b"<chr2:123-456<chr2:455-780";

        let (i3, step3) = GAFStep::parse_step(s3).unwrap();
        assert_eq!(b"", i3);
        assert_eq!(StableIntv(Forward, "chr1".into(), 123..456), step3);

        let (i4, step4) = GAFStep::parse_step(s4).unwrap();
        assert_eq!(b"<chr2:455-780", i4);
        assert_eq!(StableIntv(Backward, "chr2".into(), 123..456), step4);

        let (i4_2, step4_2) = GAFStep::parse_step(i4).unwrap();
        assert_eq!(b"", i4_2);
        assert_eq!(StableIntv(Backward, "chr2".into(), 455..780), step4_2);

        // Stops at tabs
        let with_tab = b"<s2\t266";
        let (i, s) = GAFStep::parse_step(with_tab).unwrap();
        assert_eq!(b"\t266", i);
        assert_eq!(SegId(Backward, "s2".into()), s);
    }

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
