use bytemuck::{Contiguous, Pod, Zeroable};

use nom::{bytes::complete::*, IResult};

#[cfg(feature = "serde1")]
use serde::{Deserialize, Serialize};

#[repr(u8)]
#[derive(
    Contiguous, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
#[cfg_attr(feature = "serde1", derive(Serialize, Deserialize))]
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
    fn from_u8_byte(value: u8) -> Option<Self> {
        Self::from_integer(value)
    }

    fn to_u8_char(self) -> u8 {
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

    fn from_u8_char(byte: u8) -> Option<CIGAROp> {
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

    #[inline]
    pub fn consumes_query(&self) -> bool {
        use CIGAROp::*;
        matches!(self, M | E | X | I | S)
    }

    #[inline]
    pub fn consumes_reference(&self) -> bool {
        use CIGAROp::*;
        matches!(self, M | E | X | D | N)
    }

    #[inline]
    pub fn is_match_or_mismatch(&self) -> bool {
        use CIGAROp::*;
        matches!(self, M | E | X)
    }
}

impl std::fmt::Display for CIGAROp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sym = char::from(self.to_u8_char());
        write!(f, "{}", sym)
    }
}

impl std::str::FromStr for CIGAROp {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.as_bytes()
            .get(0)
            .cloned()
            .and_then(CIGAROp::from_u8_char)
            .ok_or("Could not parse CIGAR operation")
    }
}

/// A memory-efficient representation of a single CIGAR op + length, as
/// a u32.
#[repr(transparent)]
#[derive(
    Zeroable, Pod, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
#[cfg_attr(feature = "serde1", derive(Serialize, Deserialize))]
#[allow(clippy::identity_op)]
pub struct CIGARPair(u32);

#[allow(clippy::len_without_is_empty)]
impl CIGARPair {
    pub fn new(len: u32, op: CIGAROp) -> Option<Self> {
        if len < (1 << 28) {
            Some(CIGARPair((len << 4) | (op as u32)))
        } else {
            None
        }
    }

    #[inline]
    pub fn zero(op: CIGAROp) -> Self {
        CIGARPair(op.into_integer() as u32)
    }

    #[inline]
    pub fn len(&self) -> u32 {
        self.0 >> 4
    }

    #[inline]
    pub fn set_len(&mut self, len: u32) {
        assert!(len < (1 << 28));
        self.0 = len << 4 | self.op() as u32;
    }

    #[inline]
    pub fn op(&self) -> CIGAROp {
        let op = (self.0 & 0xF) as u8;
        CIGAROp::from_u8_byte(op).unwrap()
    }

    pub fn into_pair(&self) -> (u32, CIGAROp) {
        let len = self.len();
        let op = self.op();
        (len, op)
    }

    pub fn from_pair((len, op): (u32, CIGAROp)) -> Self {
        CIGARPair((len << 4) | (op.into_integer()) as u32)
    }
}

impl From<u32> for CIGARPair {
    fn from(bytes: u32) -> Self {
        bytemuck::cast(bytes)
    }
}

impl From<CIGARPair> for u32 {
    fn from(cg: CIGARPair) -> Self {
        bytemuck::cast(cg)
    }
}

impl std::fmt::Display for CIGARPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let len = self.len();
        let op = self.op();
        write!(f, "{}{}", len, op)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct CIGAR(pub Vec<(u32, CIGAROp)>);
#[cfg_attr(feature = "serde1", derive(Serialize, Deserialize))]
pub struct CIGAR(pub Vec<CIGARPair>);

impl CIGAR {
    pub fn from_pairs<I>(pairs: I) -> Self
    where
        I: IntoIterator<Item = (u32, CIGAROp)>,
    {
        CIGAR(pairs.into_iter().map(CIGARPair::from_pair).collect())
    }

    fn parse_op_cmd(input: &[u8]) -> IResult<&[u8], CIGAROp> {
        use nom::{branch::alt, combinator::map};
        use CIGAROp::*;
        alt((
            map(tag("M"), |_| M),
            map(tag("I"), |_| I),
            map(tag("D"), |_| D),
            map(tag("N"), |_| N),
            map(tag("S"), |_| S),
            map(tag("H"), |_| H),
            map(tag("P"), |_| P),
            map(tag("="), |_| E),
            map(tag("X"), |_| X),
        ))(input)
    }

    pub(crate) fn parser_bytestring(i: &[u8]) -> IResult<&[u8], Self> {
        use nom::{
            character::complete::digit1, combinator::map, multi::many1,
            sequence::pair,
        };
        map(
            many1(map(
                pair(
                    map(digit1, |bs| {
                        let s = unsafe { std::str::from_utf8_unchecked(bs) };
                        s.parse::<u32>().unwrap()
                    }),
                    Self::parse_op_cmd,
                ),
                CIGARPair::from_pair,
            )),
            CIGAR,
        )(i)
    }

    /// Parse a CIGAR object from an ASCII byte slice
    pub fn from_bytestring(i: &[u8]) -> Option<Self> {
        Self::parser_bytestring(i).ok().map(|(_, cg)| cg)
    }

    pub fn len(&self) -> usize {
        self.0.iter().fold(0, |s, pair| s + pair.len() as usize)
    }

    /// is_empty corresponds to whether or not the contained vector is
    /// empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (u32, CIGAROp)> + '_ {
        self.0.iter().map(CIGARPair::into_pair)
    }

    /// Produces an iterator over the individual CIGAR operations in
    /// the string, e.g. an iterator over "3M2D" would produce [M, M,
    /// M, D, D]
    pub fn iter_single(&self) -> impl Iterator<Item = CIGAROp> + '_ {
        self.0.iter().copied().flat_map(|pair| {
            std::iter::repeat(pair.op()).take(pair.len() as usize)
        })
    }

    /// Given an index along the cigar string, return a pair of
    /// indices, where the first is the index to the cigar operation
    /// in this cigar that includes the given index, and the second is
    /// the remainder of the index, or the number of operations at the
    /// index that will be kept
    pub fn index(&self, i: usize) -> (usize, usize) {
        self.0
            .iter()
            .try_fold((0, i), |(v_ix, o_ix), pair| {
                let count = pair.len() as usize;
                if o_ix < count || v_ix >= self.0.len() {
                    Err((v_ix, o_ix))
                } else {
                    Ok((v_ix + 1, o_ix - count))
                }
            })
            .unwrap_or_else(|x| x)
    }

    pub fn query_index(&self, i: usize) -> (usize, usize) {
        self.0
            .iter()
            .try_fold((0, i), |(v_ix, o_ix), pair| {
                let count = pair.len() as usize;
                let op = pair.op();
                if op.consumes_query() {
                    if o_ix < count || v_ix >= self.0.len() {
                        Err((v_ix, o_ix))
                    } else {
                        Ok((v_ix + 1, o_ix - count))
                    }
                } else {
                    Ok((v_ix + 1, o_ix))
                }
            })
            .unwrap_or_else(|x| x)
    }

    pub fn ref_index(&self, i: usize) -> (usize, usize) {
        self.0
            .iter()
            .try_fold((0, i), |(v_ix, o_ix), pair| {
                let count = pair.len() as usize;
                let op = pair.op();
                if op.consumes_reference() {
                    if o_ix < count || v_ix >= self.0.len() {
                        Err((v_ix, o_ix))
                    } else {
                        Ok((v_ix + 1, o_ix - count))
                    }
                } else {
                    Ok((v_ix + 1, o_ix))
                }
            })
            .unwrap_or_else(|x| x)
    }

    pub fn split_with_index(
        &self,
        (v_ix, o_ix): (usize, usize),
    ) -> (Self, Self) {
        let mut left_cg = self.0.clone();
        let mut right_cg = left_cg.split_off(v_ix);

        if o_ix != 0 {
            if let Some(r_first) = right_cg.first_mut() {
                let ix = o_ix as u32;
                left_cg.push(CIGARPair::from_pair((ix, r_first.op())));
                r_first.set_len(r_first.len() - ix);
            }
        }
        (CIGAR(left_cg), CIGAR(right_cg))
    }

    /// Split a cigar at the provided index, returning two new cigars;
    /// e.g. splitting 4M at index 1 produces (1M, 3M); splitting
    /// 6M3I4D at index 8 produces (6M2I, 1I4D)
    pub fn split_at(&self, i: usize) -> (Self, Self) {
        self.split_with_index(self.index(i))
    }
}

impl std::fmt::Display for CIGAR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for pair in self.0.iter() {
            let (len, op) = pair.into_pair();
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
        let cigar = CIGAR::parser_bytestring(input).unwrap().1;
        let cigstr = cigar.to_string();
        assert_eq!(input_str, cigstr);
    }

    #[test]
    fn cigar_parser_bytestring() {
        use CIGAROp::*;

        let input = b"20M12D3M4N9S10H5P11=9X";
        let (i, cigar) = CIGAR::parser_bytestring(input).unwrap();
        assert_eq!(b"", i);
        assert_eq!(
            CIGAR::from_pairs(vec![
                (20, M),
                (12, D),
                (3, M),
                (4, N),
                (9, S),
                (10, H),
                (5, P),
                (11, E),
                (9, X)
            ]),
            cigar
        );

        let input = b"20M12D93  X";
        let (i, cigar) = CIGAR::parser_bytestring(input).unwrap();
        assert_eq!(b"93  X", i);
        assert_eq!(CIGAR::from_pairs(vec![(20, M), (12, D)]), cigar);

        assert!(CIGAR::parser_bytestring(b"M20").is_err());
        assert!(CIGAR::parser_bytestring(b"20").is_err());
        assert!(CIGAR::parser_bytestring(b"").is_err());
    }

    #[test]
    fn temp_split_test() {
        let input = b"6M3I4D";
        let (_, cigar) = CIGAR::parser_bytestring(input).unwrap();

        let (l, r) = cigar.split_at(8);
        println!("{}, {}", l, r);
    }

    #[test]
    fn split_cigars() {
        let input = b"20M12D3M4N9S10H5P11=9X";
        let (_i, cigar) = CIGAR::parser_bytestring(input).unwrap();

        // assert_eq!(
        // println!("{}", cigar.to_string());
        // assert_eq!("20M12D3M4N9S10H5P11=9X", cigar.to_string());

        let (l, r) = cigar.split_at(0);
        assert_eq!("", l.to_string());
        assert_eq!("20M12D3M4N9S10H5P11=9X", r.to_string());

        let (l, r) = cigar.split_at(10);
        assert_eq!("10M", l.to_string());
        assert_eq!("10M12D3M4N9S10H5P11=9X", r.to_string());

        let (l, r) = cigar.split_at(20);
        assert_eq!("20M", l.to_string());
        assert_eq!("12D3M4N9S10H5P11=9X", r.to_string());

        let (l, r) = cigar.split_at(25);
        assert_eq!("20M5D", l.to_string());
        assert_eq!("7D3M4N9S10H5P11=9X", r.to_string());

        let (l, r) = cigar.split_at(80);
        assert_eq!("20M12D3M4N9S10H5P11=6X", l.to_string());
        assert_eq!("3X", r.to_string());

        let (l, r) = cigar.split_at(85);
        assert_eq!("20M12D3M4N9S10H5P11=9X", l.to_string());
        assert_eq!("", r.to_string());
    }

    #[test]
    fn indexing_test() {
        let input = b"1M1I1M1I2M";
        let (_i, cigar) = CIGAR::parser_bytestring(input).unwrap();

        let r_ix = cigar.ref_index(3);
        let q_ix = cigar.query_index(3);

        println!("ref:   {}, {}", r_ix.0, r_ix.1);
        println!("query: {}, {}", q_ix.0, q_ix.1);

        let r_cg = cigar.split_with_index(r_ix);
        let q_cg = cigar.split_with_index(q_ix);

        println!("ref:   {}, {}", r_cg.0, r_cg.1);
        println!("query: {}, {}", q_cg.0, q_cg.1);
    }
}
