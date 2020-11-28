use bstr::ByteSlice;

use std::fmt::Display;

use nom::{bytes::complete::*, IResult};

use crate::{gfa::*, optfields::*};

/// A GAF record, with optional fields T. Can be created by using
/// `parse_gaf`, and the Display implementation produces
/// spec-compliant tab-delimited output.
#[derive(Debug, Clone, PartialEq)]
pub struct GAF<T: OptFields> {
    pub seq_name: Vec<u8>,
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
            self.seq_name.as_bstr(),
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
    SegId(Orientation, Vec<u8>),
    StableIntv(Orientation, Vec<u8>, usize, usize),
}

impl Display for GAFStep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GAFStep::SegId(o, seg) => {
                o.write_gt_ln(f)?;
                write!(f, "{}", seg.as_bstr())
            }
            GAFStep::StableIntv(o, id, from, to) => {
                o.write_gt_ln(f)?;
                write!(f, "{}:{}-{}", id.as_bstr(), from, to)
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
    StableId(Vec<u8>),
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
            GAFPath::StableId(id) => write!(f, "{}", id.as_bstr()),
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
    pub query_seq_name: Vec<u8>,
    pub query_seq_len: usize,
    pub query_seq_range: (usize, usize),
    pub strand: Orientation,
    pub target_seq_name: Vec<u8>,
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
            self.query_seq_name.as_bstr(),
            self.query_seq_len,
            self.query_seq_range.0,
            self.query_seq_range.1,
            self.strand
        )?;

        write!(
            f,
            "\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            self.target_seq_name.as_bstr(),
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
    bytes.to_str().ok().and_then(|p| p.parse().ok())
}

fn parse_seq_fields<I>(mut input: I) -> Option<(Vec<u8>, usize, (usize, usize))>
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

    let strand = input.next().and_then(Orientation::from_bytes_plus_minus)?;

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
}
