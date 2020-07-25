use bstr::BString;
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
