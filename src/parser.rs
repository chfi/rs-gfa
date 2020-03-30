use nom::branch::alt;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::map;
use nom::error::ErrorKind;
use nom::number::complete::be_u16;
use nom::sequence::terminated;
use nom::Err;
use nom::IResult;

// #[macro_use]
use nom::regex::Regex;

use crate::gfa::*;

fn segment_ex() -> String {
    format!("S\t11\tACCTT\tRC:i:123")
}

lazy_static! {
    static ref RE_ORIENT: Regex = Regex::new(r"+|-").unwrap();
    static ref RE_OVERLAP: Regex = Regex::new(r"\*|([0-9]+[MIDNSHPX=])+").unwrap();
}

fn parse_name(input: &str) -> IResult<&str, String> {
    let (i, name) = re_find!(input, r"^[!-)+-<>-~][!-~]*")?;
    Ok((i, name.to_string()))
}

fn parse_sequence(input: &str) -> IResult<&str, String> {
    let (i, seq) = re_find!(input, r"\*|[A-Za-z=.]+")?;
    Ok((i, seq.to_string()))
}

fn parse_orient(input: &str) -> IResult<&str, Orientation> {
    let fwd = map(tag("+"), |_| Orientation::Forward);
    let bkw = map(tag("-"), |_| Orientation::Backward);
    alt((fwd, bkw))(input)
}

fn parse_overlap(input: &str) -> IResult<&str, String> {
    let (i, overlap) = re_find!(input, r"\*|([0-9]+[MIDNSHPX=])+")?;
    Ok((i, overlap.to_string()))
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    let tab = tag("\t");
    let (input, _line_type) = terminated(tag("S"), &tab)(input)?;

    let (input, name) = terminated(parse_name, &tab)(input)?;

    let (input, seq) = terminated(parse_sequence, &tab)(input)?;

    let result = Segment {
        name: name,
        sequence: seq,
        read_count: None,
        fragment_count: None,
        kmer_count: None,
        uri: None,
    };

    Ok((input, result))
}

fn parse_line(input: &str) -> IResult<&str, Line> {
    let tab = tag("\t");
    let (i, _line_type) = terminated(tag("L"), &tab)(input)?;

    let seg = terminated(parse_name, &tab);
    let orient = terminated(parse_orient, &tab);

    let (i, from_segment) = seg(i)?;
    let (i, from_orient) = orient(i)?;
    let (i, to_segment) = seg(i)?;
    let (i, to_orient) = orient(i)?;
    let (i, overlap) = terminated(parse_overlap, &tab)(i)?;

    let result = Line {
        from_segment,
        from_orient,
        to_segment,
        to_orient,
        overlap,
        map_quality: None,
        num_mismatches: None,
        read_count: None,
        fragment_count: None,
        kmer_count: None,
        edge_id: None,
    };

    Ok((i, result))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_parse_segment() {
        let seg = "S	11	ACCTT	";
        let seg_ = Segment {
            name: "11".to_string(),
            sequence: "ACCTT".to_string(),
            read_count: None,
            fragment_count: None,
            kmer_count: None,
            uri: None,
        };
        match parse_segment(seg) {
            Err(err) => {
                println!("{:?}", err);
                assert_eq!(true, false)
            }
            Ok((res, s)) => {
                println!("{:?}", s);
                assert_eq!(s, seg_)
            }
        }
    }

    #[test]
    fn can_parse_line() {
        let line = "L	11	+	12	-	4M	";
        let line_ = Line {
            from_segment: "11".to_string(),
            from_orient: Orientation::Forward,
            to_segment: "12".to_string(),
            to_orient: Orientation::Backward,
            overlap: "4M".to_string(),
            map_quality: None,
            num_mismatches: None,
            read_count: None,
            fragment_count: None,
            kmer_count: None,
            edge_id: None,
        };
        match parse_line(line) {
            Err(err) => {
                println!("{:?}", err);
                assert_eq!(true, false)
            }
            Ok((res, l)) => {
                println!("{:?}", l);
                assert_eq!(l, line_)
            }
        }
    }
}
