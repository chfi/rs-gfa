use bstr::{BString, ByteSlice};

use std::convert::{TryFrom, TryInto};
use std::fmt::Display;

use nom::{bytes::complete::*, IResult};

use crate::gfa::*;
use crate::optfields::*;

/// A GAF record, with optional fields T. Can be created by using
/// `parse_gaf`, and the Display implementation produces
/// spec-compliant tab-delimited output.
#[derive(Debug, Clone, PartialEq)]
pub struct GAF<T: OptFields> {
    pub seq_name: BString,
    pub seq_len: usize,
    pub seq_range: (usize, usize),
    pub strand: Orientation,
    pub path: GAFPath,
    pub path_len: usize,
    pub path_range: (usize, usize),
    pub residue_matches: usize,
    pub block_length: usize,
    pub quality: u8,
    pub optional: T,
}

impl<T: OptFields> Display for GAF<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\t{}\t{}\t{}\t{}",
            self.seq_name,
            self.seq_len,
            self.seq_range.0,
            self.seq_range.1,
            self.strand
        )?;

        write!(
            f,
            "\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            self.path,
            self.path_len,
            self.path_range.0,
            self.path_range.1,
            self.residue_matches,
            self.block_length,
            self.quality
        )?;

        for opt in self.optional.fields() {
            write!(f, "\t{}", opt)?;
        }

        Ok(())
    }
}

/// enum representing the two kinds of step in a GAF path; either an
/// oriented GFA segment ID, or an oriented interval on a stable rGFA
/// reference.
#[derive(Debug, Clone, PartialEq)]
pub enum GAFStep {
    SegId(Orientation, BString),
    StableIntv(Orientation, BString, usize, usize),
}

impl Display for GAFStep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Orientation::*;
        let o_str = |o: &Orientation| match o {
            Forward => ">",
            Backward => "<",
        };

        match self {
            GAFStep::SegId(o, seg) => write!(f, "{}{}", o_str(o), seg),
            GAFStep::StableIntv(o, id, from, to) => {
                write!(f, "{}{}:{}-{}", o_str(o), id, from, to)
            }
        }
    }
}

impl GAFStep {
    // The steps in a GAF path use '>' and '<' to denote relative
    // strand of a path step, so we need another Orientation parser to
    // reuse that type here
    fn parse_orient(bytes: &[u8]) -> IResult<&[u8], Orientation> {
        use nom::{branch::alt, combinator::map};
        use Orientation::*;

        let fwd = map(tag(">"), |_| Forward);
        let bwd = map(tag("<"), |_| Backward);
        alt((fwd, bwd))(bytes)
    }

    pub(crate) fn parse_step(i: &[u8]) -> IResult<&[u8], GAFStep> {
        use nom::{
            character::complete::digit1,
            combinator::{map, opt},
            sequence::{preceded, separated_pair},
        };

        let (i, orient) = Self::parse_orient(i)?;
        let (i, name) = is_not("<>: \t\r\n")(i)?;
        let name = name.into();

        let parse_digits = map(digit1, |bs| {
            let s = unsafe { std::str::from_utf8_unchecked(bs) };
            s.parse::<usize>().unwrap()
        });

        let parse_range = preceded(
            tag(":"),
            separated_pair(&parse_digits, tag("-"), &parse_digits),
        );

        let (i, range) = opt(parse_range)(i)?;
        if let Some((start, end)) = range {
            Ok((i, GAFStep::StableIntv(orient, name, start, end)))
        } else {
            Ok((i, GAFStep::SegId(orient, name)))
        }
    }
}

/// enum representing the two kinds of GAF path; either the ID of a
/// stable rGFA identifier, or a list of oriented steps.
// NB: it may be the case that the GAFStep enum could be replaced with
// a third variant here, as I doubt a path would mix GFA segment IDs
// and rGFA stable intervals, but I'm not sure, so I'm keeping this as
// is for now.
#[derive(Debug, Clone, PartialEq)]
pub enum GAFPath {
    StableId(BString),
    OrientIntv(Vec<GAFStep>),
}

impl GAFPath {
    pub(crate) fn parse_path(i: &[u8]) -> IResult<&[u8], GAFPath> {
        use nom::{
            combinator::{opt, verify},
            multi::many1,
        };
        let (i, path) = opt(many1(GAFStep::parse_step))(i)?;

        if let Some(path) = path {
            Ok((i, GAFPath::OrientIntv(path)))
        } else {
            let (i, stable_id) = verify(is_not("\t"), |bs: &[u8]| {
                bs.find_byteset("><").is_none()
            })(i)?;
            Ok((i, GAFPath::StableId(stable_id.into())))
        }
    }
}

impl Display for GAFPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GAFPath::StableId(id) => write!(f, "{}", id),
            GAFPath::OrientIntv(steps) => {
                for s in steps {
                    write!(f, "{}", s)?;
                }
                Ok(())
            }
        }
    }
}

/// A PAF record, with optional fields T. Can be created by using
/// `parse_gaf`, and the Display implementation produces
/// spec-compliant tab-delimited output.
#[derive(Debug, Clone)]
pub struct PAF<T: OptFields> {
    pub query_seq_name: BString,
    pub query_seq_len: usize,
    pub query_seq_range: (usize, usize),
    pub strand: Orientation,
    pub target_seq_name: BString,
    pub target_seq_len: usize,
    pub target_seq_range: (usize, usize),
    pub residue_matches: usize,
    pub block_length: usize,
    pub quality: u8,
    pub optional: T,
}

impl<T: OptFields> Display for PAF<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\t{}\t{}\t{}\t{}",
            self.query_seq_name,
            self.query_seq_len,
            self.query_seq_range.0,
            self.query_seq_range.1,
            self.strand
        )?;

        write!(
            f,
            "\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            self.target_seq_name,
            self.target_seq_len,
            self.target_seq_range.0,
            self.target_seq_range.1,
            self.residue_matches,
            self.block_length,
            self.quality
        )?;

        for opt in self.optional.fields() {
            write!(f, "\t{}", opt)?;
        }

        Ok(())
    }
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

fn parse_seq_fields<I>(mut input: I) -> Option<(BString, usize, (usize, usize))>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
{
    let name = input.next()?.as_ref().into();
    let len = parse_next(&mut input)?;
    let start = parse_next(&mut input)?;
    let end = parse_next(&mut input)?;

    Some((name, len, (start, end)))
}

/// Parse a PAF record from an iterator over the tab-delimited fields
/// of bytes
pub fn parse_paf<I, T>(mut input: I) -> Option<PAF<T>>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
    T: OptFields,
{
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
/// Parse a GAF record from an iterator over the tab-delimited fields
/// of bytes
pub fn parse_gaf<I, T>(input: I) -> Option<GAF<T>>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
    T: OptFields,
{
    let paf: PAF<T> = parse_paf(input)?;
    let (_, path) = GAFPath::parse_path(&paf.target_seq_name).ok()?;

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

impl TryFrom<u8> for CIGAROp {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use CIGAROp::*;
        match value {
            b'M' => Ok(M),
            b'I' => Ok(I),
            b'D' => Ok(D),
            b'N' => Ok(N),
            b'S' => Ok(S),
            b'H' => Ok(H),
            b'P' => Ok(P),
            b'=' => Ok(E),
            b'X' => Ok(X),
            _ => Err(()),
        }
    }
}

/// A memory-efficient representation of a single CIGAR op + length, as
/// a u32.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

    pub fn len(&self) -> u32 {
        self.0 >> 4
    }

    pub fn op(&self) -> CIGAROp {
        let op = (self.0 & 0x4) as u8;
        op.try_into().unwrap()
    }

    pub fn into_pair(&self) -> (u32, CIGAROp) {
        let len = self.len();
        let op = self.op();
        (len, op)
    }

    pub fn from_pair((len, op): (u32, CIGAROp)) -> Self {
        CIGARPair((len << 4) | (op as u32))
    }
}

impl From<u32> for CIGARPair {
    fn from(bytes: u32) -> Self {
        CIGARPair(bytes)
    }
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

    pub fn consumes_query(&self) -> bool {
        use CIGAROp::*;
        match self {
            M | E | X | I | S => true,
            _ => false,
        }
    }

    pub fn consumes_reference(&self) -> bool {
        use CIGAROp::*;
        match self {
            M | E | X | D | N => true,
            _ => false,
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

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct CIGAR(pub Vec<(u32, CIGAROp)>);

impl CIGAR {
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

    pub(crate) fn parser(i: &[u8]) -> IResult<&[u8], Self> {
        use nom::{
            character::complete::digit1, combinator::map, multi::many1,
            sequence::pair,
        };
        map(
            many1(pair(
                map(digit1, |bs| {
                    let s = unsafe { std::str::from_utf8_unchecked(bs) };
                    s.parse::<u32>().unwrap()
                }),
                Self::parse_op_cmd,
            )),
            CIGAR,
        )(i)
    }

    /// Parse a CIGAR object from an ASCII byte slice
    pub fn from_bytes(i: &[u8]) -> Option<Self> {
        Self::parser(i).ok().map(|(_, cg)| cg)
    }

    pub fn len(&self) -> usize {
        self.0
            .iter()
            .fold(0, |s, (op_len, _op)| s + *op_len as usize)
    }

    /// is_empty corresponds to whether or not the contained vector is
    /// empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Produces an iterator over the individual CIGAR operations in
    /// the string, e.g. an iterator over "3M2D" would produce [M, M,
    /// M, D, D]
    pub fn iter(&self) -> impl Iterator<Item = CIGAROp> + '_ {
        self.0
            .iter()
            .copied()
            .flat_map(|(i, c)| std::iter::repeat(c).take(i as usize))
    }

    /// Given an index along the cigar string, return a pair of
    /// indices, where the first is the index to the cigar operation
    /// in this cigar that includes the given index, and the second is
    /// the remainder of the index, or the number of operations at the
    /// index that will be kept
    pub fn index(&self, i: usize) -> (usize, usize) {
        self.0
            .iter()
            .try_fold((0, i), |(v_ix, o_ix), (count, _)| {
                let count = *count as usize;
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
            .try_fold((0, i), |(v_ix, o_ix), (count, op)| {
                let count = *count as usize;
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
            .try_fold((0, i), |(v_ix, o_ix), (count, op)| {
                let count = *count as usize;
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
                left_cg.push((ix, r_first.1));
                r_first.0 -= ix;
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
        for (len, op) in self.0.iter() {
            write!(f, "{}{}", len, op)?
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_gaf_lines() {
        use GAFStep::*;
        use Orientation::*;

        type GAF = super::GAF<OptionalFields>;
        let gaf_in1 =
            b"read1\t6\t0\t6\t+\t>s2>s3>s4\t12\t2\t8\t6\t6\t60\tcg:Z:6M";

        let path_i1: Vec<GAFStep> = ["s2", "s3", "s4"]
            .iter()
            .map(|&s| SegId(Forward, s.into()))
            .collect();

        let expected_1 = GAF {
            seq_name: "read1".into(),
            seq_len: 6,
            seq_range: (0, 6),
            strand: Forward,
            path: GAFPath::OrientIntv(path_i1),
            path_len: 12,
            path_range: (2, 8),
            residue_matches: 6,
            block_length: 6,
            quality: 60,
            optional: vec![OptField::new(b"cg", OptFieldVal::Z("6M".into()))],
        };

        let gaf_1: Option<GAF> = parse_gaf(gaf_in1.split_str("\t"));

        assert_eq!(Some(expected_1.clone()), gaf_1);

        let gaf_in2 = b"read1\t6\t0\t6\t+\tchr1\t12\t2\t8\t6\t6\t60\tcg:Z:6M";

        let expected_2 = GAF {
            path: GAFPath::StableId("chr1".into()),
            ..expected_1
        };

        let gaf_2: Option<GAF> = parse_gaf(gaf_in2.split_str("\t"));

        assert_eq!(Some(expected_2.clone()), gaf_2);

        let gaf_in3 =
            b"read2\t7\t0\t7\t-\t>chr1:5-8>foo:8-16\t11\t1\t8\t7\t7\t60\tcg:Z:7M";

        let path_i3: Vec<GAFStep> = vec![
            StableIntv(Forward, "chr1".into(), 5, 8),
            StableIntv(Forward, "foo".into(), 8, 16),
        ];

        let expected_3 = GAF {
            seq_name: "read2".into(),
            seq_len: 7,
            seq_range: (0, 7),
            strand: Backward,
            path: GAFPath::OrientIntv(path_i3),
            path_len: 11,
            path_range: (1, 8),
            residue_matches: 7,
            block_length: 7,
            quality: 60,
            optional: vec![OptField::new(b"cg", OptFieldVal::Z("7M".into()))],
        };

        let gaf_3: Option<GAF> = parse_gaf(gaf_in3.split_str("\t"));

        assert_eq!(Some(expected_3), gaf_3);
    }

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
        assert_eq!(StableIntv(Forward, "chr1".into(), 123, 456), step3);

        let (i4, step4) = GAFStep::parse_step(s4).unwrap();
        assert_eq!(b"<chr2:455-780", i4);
        assert_eq!(StableIntv(Backward, "chr2".into(), 123, 456), step4);

        let (i4_2, step4_2) = GAFStep::parse_step(i4).unwrap();
        assert_eq!(b"", i4_2);
        assert_eq!(StableIntv(Backward, "chr2".into(), 455, 780), step4_2);

        // Stops at tabs
        let with_tab = b"<s2\t266";
        let (i, s) = GAFStep::parse_step(with_tab).unwrap();
        assert_eq!(b"\t266", i);
        assert_eq!(SegId(Backward, "s2".into()), s);
    }

    #[test]
    fn parse_gaf_paths() {
        use GAFPath::*;
        use GAFStep::*;
        use Orientation::*;

        let seg_fwd = |bs: &str| SegId(Forward, bs.into());
        let seg_bwd = |bs: &str| SegId(Backward, bs.into());
        let stbl_fwd = |bs: &str, r: (usize, usize)| {
            StableIntv(Forward, bs.into(), r.0, r.1)
        };
        let stbl_bwd = |bs: &str, r: (usize, usize)| {
            StableIntv(Backward, bs.into(), r.0, r.1)
        };

        // stable IDs
        let p_id1 = b"some_id1";
        let p_id2 = b"chr1\t123";

        let (i, p) = GAFPath::parse_path(p_id1).unwrap();
        assert_eq!(b"", i);
        assert_eq!(StableId("some_id1".into()), p);

        let (i, p) = GAFPath::parse_path(p_id2).unwrap();
        assert_eq!(b"\t123", i);
        assert_eq!(StableId("chr1".into()), p);

        // oriented paths

        let p_orient1 = b">s1>s2<s3<s4";
        let p_orient2 = b">chr1:5-8>foo:8-16<bar:16-20\t298";

        let (i, p) = GAFPath::parse_path(p_orient1).unwrap();
        assert_eq!(b"", i);
        assert_eq!(
            OrientIntv(vec![
                seg_fwd("s1"),
                seg_fwd("s2"),
                seg_bwd("s3"),
                seg_bwd("s4")
            ]),
            p
        );

        let (i, p) = GAFPath::parse_path(p_orient2).unwrap();
        assert_eq!(b"\t298", i);
        assert_eq!(
            OrientIntv(vec![
                stbl_fwd("chr1", (5, 8)),
                stbl_fwd("foo", (8, 16)),
                stbl_bwd("bar", (16, 20)),
            ]),
            p
        );

        // If the path doesn't start with an orientation, it must be a
        // stable ID, and thus cannot contain any > or <
        let err_input = b"s1>s2<s3\t123";
        let parse_error: IResult<&[u8], GAFPath> =
            GAFPath::parse_path(err_input);
        assert!(parse_error.is_err());
    }

    #[test]
    fn cigar_display() {
        let input = b"20M12D3M4N9S10H5P11=9X";
        let input_str = std::str::from_utf8(input).unwrap();
        let cigar = CIGAR::parser(input).unwrap().1;
        let cigstr = cigar.to_string();
        assert_eq!(input_str, cigstr);
    }

    #[test]
    fn cigar_parser() {
        use CIGAROp::*;

        let input = b"20M12D3M4N9S10H5P11=9X";
        let (i, cigar) = CIGAR::parser(input).unwrap();
        assert_eq!(b"", i);
        assert_eq!(
            CIGAR(vec![
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
        let (i, cigar) = CIGAR::parser(input).unwrap();
        assert_eq!(b"93  X", i);
        assert_eq!(CIGAR(vec![(20, M), (12, D)]), cigar);

        assert!(CIGAR::parser(b"M20").is_err());
        assert!(CIGAR::parser(b"20").is_err());
        assert!(CIGAR::parser(b"").is_err());
    }

    #[test]
    fn temp_split_test() {
        let input = b"6M3I4D";
        let (_, cigar) = CIGAR::parser(input).unwrap();

        let (l, r) = cigar.split_at(8);
        println!("{}, {}", l, r);
    }

    #[test]
    fn split_cigars() {
        let input = b"20M12D3M4N9S10H5P11=9X";
        let (_i, cigar) = CIGAR::parser(input).unwrap();

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
        let (_i, cigar) = CIGAR::parser(input).unwrap();

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
