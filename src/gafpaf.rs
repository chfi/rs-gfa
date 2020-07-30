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

        let parse_range =
            // preceded(tag(":"), separated_pair(digit1, tag("-"), digit1));
            preceded(tag(":"), separated_pair(&parse_digits, tag("-"), &parse_digits));

        let (i, range) = combinator::opt(parse_range)(i)?;
        if let Some((start, end)) = range {
            let range = start..end;
            Ok((i, GAFStep::StableIntv(orient, name, range)))
        } else {
            Ok((i, GAFStep::SegId(orient, name)))
        }
    }
    // fn parse_orient(bytes: &[u8]) -> Option<Orientation> {
    //     match bytes {
    //         b">" => Some(Orientation::Forward),
    //         b"<" => Some(Orientation::Backward),
    //         _ => None,
    //     }
    // }

    // fn parse_step(bytes: &[u8]) -> IResult<&[u8], GAFStep> {}

    /*
    fn parse_step(bytes: &[u8]) -> Option<(GAFStep, &[u8])> {
        // if we're trying to parse a step, we know it's oriented
        let orient = Self::parse_orient(&bytes[0..=0])?;
        // next, it's either a GFA segment ID, or a stable ID

        // if there's a colon, we're dealing with a stable ID and its interval
        if let Some(ix) = bytes.find_byte(b':') {
            let iv_ix = bytes.find_byte(b'-')?;
            None
        } else {
            // if there's no :, it's a GFA segment ID
            if let Some(seg_end) = bytes.find_byteset(b"><") {
                let seg_id: BString = bytes[1..seg_end].into();
                let rest = &bytes[seg_end..];
                let step = GAFStep::SegId(seg_id, orient);
                Some((step, rest))
            } else {
                let seg_id: BString = bytes[1..].into();
                let rest = &[]
                None
            }
            // seg_range.or
            // if let Some(end_ix) = bytes.find_byteset(b"><") {
            //     let seg_id = bytes[1..end_ix]
            // } else {
            // }
        }
    }
    */
}

pub enum GAFPath {
    StableId(BString),
    OrientIntv(Vec<GAFStep>),
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
