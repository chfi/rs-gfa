use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::number::complete::be_u16;
use nom::IResult;

use crate::gfa::*;

fn segment_ex() -> String {
    format!("S\t11\tACCTT\tRC:i:123")
}

// fn

// Names are printable ASCII, but no whitespace, do not start with *
// or =, and they cannot contain the strings "+," or "-,"

// for now, just check the prefix
fn is_name(i: &str) -> bool {
    !(i.starts_with("*") || i.starts_with("="))
    //     false
    // else
    //     true
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    let (input, line_type) = tag("S\t")(input)?;
    println!("after type: {}", input);
    // TODO use is_name
    let (input, name) = take_until("\t")(input)?;
    let (input, _) = tag("\t")(input)?;
    println!("after name: {}", input);
    // TODO the spec is a bit more constraining on sequences
    let (input, seq) = take_until("\t")(input)?;
    // let (input, _) = tag("\t")(input)?;
    println!("after seq: {}", input);

    let result = Segment {
        name: name.to_string(),
        sequence: seq.to_string(),
        read_count: None,
        fragment_count: None,
        kmer_count: None,
        uri: None,
    };

    Ok((input, result))
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
            Err(_) => assert_eq!(true, false),
            Ok((res, s)) => assert_eq!(s, seg_),
        }
        // let Ok((res, s)) = parse_segment(seg);
        // let ss: Segment = s;

        // assert_eq!(ss, seg_);
    }
}
