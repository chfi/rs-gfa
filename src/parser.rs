use nom::branch::alt;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::map;
use nom::sequence::terminated;
use nom::IResult;
use std::fs::File;
use std::io::prelude::*;
use std::io::{BufReader, Lines};
use std::path::PathBuf;

use lazy_static::lazy_static;
use regex::Regex;

use crate::gfa::*;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct GFAParsingConfig {
    pub segments: bool,
    pub links: bool,
    pub containments: bool,
    pub paths: bool,
}

impl GFAParsingConfig {
    pub fn none() -> Self {
        GFAParsingConfig {
            segments: false,
            links: false,
            containments: false,
            paths: false,
        }
    }

    pub fn all() -> Self {
        GFAParsingConfig {
            segments: true,
            links: true,
            containments: true,
            paths: true,
        }
    }

    fn make_filter(&self) -> Box<dyn for<'a> Fn(&'a str) -> Option<&'a str>> {
        let string = self.filter_chars();
        Box::new(move |s| {
            if string.contains(&s[0..1]) {
                Some(s)
            } else {
                None
            }
        })
    }

    fn filter_chars(&self) -> String {
        let mut result = "H".to_string();
        if self.segments {
            result.push('S');
        }
        if self.links {
            result.push('L');
        }
        if self.containments {
            result.push('C');
        }
        if self.paths {
            result.push('P');
        }

        result
    }
}

pub struct GFAParser<T> {
    filter: Box<dyn Fn(&'_ str) -> Option<&'_ str>>,
    _optional_fields: std::marker::PhantomData<T>,
}

impl<T> GFAParser<T> {
    pub fn new(config: GFAParsingConfig) -> Self {
        let filter = config.make_filter();

        GFAParser {
            filter,
            _optional_fields: std::marker::PhantomData,
        }
    }

    pub fn filter_line<'a>(&self, line: &'a str) -> Option<&'a str> {
        (self.filter)(line)
    }
}

impl<T: OptFields> GFAParser<T> {
    pub fn parse_buf<'a>(
        &self,
        buf: &mut Vec<&'a str>,
        line: &'a str,
    ) -> Option<Line<T>> {
        use Line::*;
        if let Some(line) = self.filter_line(line) {
            buf.clear();
            line.split_terminator('\t').for_each(|f| buf.push(f));
            let fields = &buf[1..];
            match buf[0] {
                "H" => parse_header(buf[0]).map(Header),
                "S" => ParseGFA::parse_line(fields).map(Segment),
                "L" => ParseGFA::parse_line(fields).map(Link),
                "C" => ParseGFA::parse_line(fields).map(Containment),
                "P" => ParseGFA::parse_line(fields).map(Path),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn parse_line(&self, line: &str) -> Option<Line<T>> {
        use Line::*;
        if let Some(line) = self.filter_line(line) {
            let line: Vec<_> = line.split_terminator('\t').collect();
            let fields = &line[1..];
            match line[0] {
                "H" => parse_header(fields[0]).map(Header),
                "S" => ParseGFA::parse_line(fields).map(Segment),
                "L" => ParseGFA::parse_line(fields).map(Link),
                "C" => ParseGFA::parse_line(fields).map(Containment),
                "P" => ParseGFA::parse_line(fields).map(Path),
                _ => None,
            }
        } else {
            None
        }
    }
}

// parse_optionals returns Self, not Option, since, well, they're optional anyway
pub trait OptFields: Sized + Default {
    fn parse(skip: usize, input: &[&str]) -> Self {
        if input.len() > skip {
            Self::parse_optionals(&input[skip..])
        } else {
            Default::default()
        }
    }

    // fn parse_iter<'a>(fields: impl Iterator<Item = &'a>) -> Self;
    fn parse_optionals(input: &[&str]) -> Self;

    fn get_field(&self, tag: OptTag) -> Option<&OptionalFieldValue>;
}

impl OptFields for () {
    fn parse(_: usize, _: &[&str]) -> () {
        ()
    }
    fn parse_optionals(_: &[&str]) -> () {
        ()
    }

    fn get_field(&self, _: OptTag) -> Option<&OptionalFieldValue> {
        None
    }
}

impl OptFields for Vec<OptionalField> {
    fn parse_optionals(input: &[&str]) -> Self {
        input
            .into_iter()
            .filter_map(|f| parse_optional_field(*f))
            .collect()
    }

    fn get_field(&self, tag: OptTag) -> Option<&OptionalFieldValue> {
        self.iter().find(|o| o.tag == tag).map(|v| &v.content)
    }
}

// type StrIter<'a> = impl Iterator<Item = &'a str>;

// trait Test: Sized {
//     fn parse_iter<'a>(input: impl Iterator<Item = &'a str>) -> Option<Self>;
// }

trait ParseGFA: Sized + Default {
    fn parse_line(input: &[&str]) -> Option<Self>;

    // fn parse_iter<'a>(input: impl Iterator<Item = &'a str>) -> Option<Self> {
    //     let fields: Vec<_> = input.collect();
    //     Self::parse_line(&fields)
    // }
}

impl<T: OptFields> ParseGFA for Segment<T> {
    fn parse_line(input: &[&str]) -> Option<Self> {
        let name = parse_name(input[0])?;
        let sequence = parse_sequence(input[1])?;
        Some(Segment {
            name,
            sequence,
            optional: T::parse(2, input),
        })
    }
}

impl<T: OptFields> ParseGFA for Link<T> {
    fn parse_line(fields: &[&str]) -> Option<Self> {
        let from_segment = parse_name(fields[0])?;
        let from_orient = parse_orient(fields[1])?;
        let to_segment = parse_name(fields[2])?;
        let to_orient = parse_orient(fields[3])?;
        let overlap = fields[4].bytes().collect();

        Some(Link {
            from_segment,
            from_orient,
            to_segment,
            to_orient,
            overlap,
            optional: T::parse(5, fields),
        })
    }
}

impl<T: OptFields> ParseGFA for Containment<T> {
    fn parse_line(fields: &[&str]) -> Option<Self> {
        let container_name = parse_name(fields[0])?;
        let container_orient = parse_orient(fields[1])?;
        let contained_name = parse_name(fields[2])?;
        let contained_orient = parse_orient(fields[3])?;
        let pos = fields[4].parse().ok()?;
        let overlap = fields[5].bytes().collect();

        Some(Containment {
            container_name,
            container_orient,
            contained_name,
            contained_orient,
            overlap,
            pos,
            optional: T::parse(6, fields),
        })
    }
}

impl<T: OptFields> ParseGFA for Path<T> {
    fn parse_line(fields: &[&str]) -> Option<Self> {
        let path_name = parse_name(fields[0])?;

        let segment_names = fields[1]
            .split_terminator(',')
            .map(|s| {
                let (n, o) = s.split_at(s.len() - 1);
                let orient = match o {
                    "+" => Orientation::Forward,
                    "-" => Orientation::Backward,
                    _ => panic!("Path segment did not include orientation"),
                };
                let name = n.to_string();
                (name, orient)
            })
            .collect();

        let overlaps = fields[2]
            .split_terminator(',')
            .map(|s| s.bytes().collect())
            .collect();

        Some(Path {
            path_name,
            segment_names,
            overlaps,
            optional: T::parse(3, fields),
        })
    }
}

macro_rules! unwrap {
    ($path:path, $opt:expr) => {
        if let $path(x) = $opt {
            Some(x)
        } else {
            None
        }
    };
}

fn parse_optional_tag(input: &str) -> Option<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"[A-Za-z][A-Za-z0-9]").unwrap();
    }
    RE.find(input).map(|s| s.as_str().to_string())
}

fn parse_optional_char(input: &str) -> Option<char> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"[!-~]").unwrap();
    }

    RE.find(input).and_then(|s| s.as_str().chars().nth(0))
}

fn parse_optional_int(input: &str) -> Option<i64> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"[-+]?[0-9]+").unwrap();
    }

    RE.find(input).and_then(|s| s.as_str().parse().ok())
}

fn parse_optional_float(input: &str) -> Option<f32> {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?").unwrap();
    }

    RE.find(input).and_then(|s| s.as_str().parse().ok())
}

fn parse_optional_string(input: &str) -> Option<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"[ !-~]+").unwrap();
    }
    let result = RE.find(input).map(|s| s.as_str().to_string());
    result
}

// TODO I'm not entirely sure if this works as it should; I assume it
// should actually parse pairs of digits
fn parse_optional_bytearray(input: &str) -> Option<Vec<u32>> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"[0-9A-F]+").unwrap();
    }

    RE.find(input)
        .map(|s| s.as_str().chars().filter_map(|c| c.to_digit(16)).collect())
}

fn parse_optional_array<T: std::str::FromStr>(input: &str) -> Option<Vec<T>> {
    input
        .split_terminator(',')
        .map(|f| f.parse().ok())
        .collect()
}

fn parse_optional_field(input: &str) -> Option<OptionalField> {
    use OptionalFieldValue::*;

    lazy_static! {
        static ref RE: Regex = Regex::new(r"[AifZJHB]").unwrap();
    }

    let fields: Vec<_> = input.split_terminator(':').collect();

    // let field_type = RE.find(fields[1]).map(|s| s.as_str())?;
    let field_type = RE.find(&input[3..=3]).map(|s| s.as_str())?;
    // let field_tag = parse_optional_tag(&input[0..=1])?;
    let field_tag = OptTag::from_str(&input[0..=1])?;
    let field_contents = &input[5..];
    let field_value = match field_type {
        // char
        "A" => parse_optional_char(field_contents).map(PrintableChar),
        // int
        "i" => parse_optional_int(field_contents).map(SignedInt),
        // float
        "f" => parse_optional_float(field_contents).map(Float),
        // string
        "Z" => parse_optional_string(field_contents).map(PrintableString),
        // JSON string
        "J" => parse_optional_string(field_contents).map(JSON),
        // bytearray
        "H" => parse_optional_bytearray(field_contents).map(ByteArray),
        // float or int array
        "B" => {
            if field_contents.starts_with('f') {
                parse_optional_array(&field_contents[1..]).map(FloatArray)
            } else {
                parse_optional_array(&field_contents[1..]).map(IntArray)
            }
        }
        _ => panic!(
            "Tried to parse optional field with unknown type '{}'",
            fields[1]
        ),
    }?;

    Some(OptionalField {
        tag: field_tag,
        content: field_value,
    })
}

fn parse_header(input: &str) -> Option<Header> {
    use OptionalFieldValue::PrintableString;
    let version = parse_optional_field(input)
        .map(|o| o.content)
        .and_then(|o| unwrap!(PrintableString, o));
    Some(Header { version })
}

fn parse_orient(input: &str) -> Option<Orientation> {
    let fwd = map(tag("+"), |_| Orientation::Forward);
    let bkw = map(tag("-"), |_| Orientation::Backward);
    let result: IResult<_, _> = alt((fwd, bkw))(input);
    match result {
        Ok((_, o)) => Some(o),
        _ => None,
    }
}

fn parse_name(input: &str) -> Option<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"[!-)+-<>-~][!-~]*").unwrap();
    }

    RE.find(input).map(|s| s.as_str().to_string())
}

fn parse_sequence(input: &str) -> Option<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"\*|[A-Za-z=.]+").unwrap();
    }

    RE.find(input).map(|s| s.as_str().to_string())
}

pub fn parse_gfa_line<T: OptFields>(
    line: &str,
    config: &GFAParsingConfig,
) -> Option<Line<T>> {
    let fields: Vec<_> = line.split_terminator('\t').collect();
    match fields[0] {
        "H" => {
            let h = parse_header(&fields[1])?;
            Some(Line::Header(h))
        }
        "#" => Some(Line::Comment),
        "S" => {
            if config.segments {
                let s: Segment<T> = ParseGFA::parse_line(&fields[1..])?;
                Some(Line::Segment(s))
            } else {
                None
            }
        }
        "L" => {
            if config.links {
                let l = ParseGFA::parse_line(&fields[1..])?;
                Some(Line::Link(l))
            } else {
                None
            }
        }
        "C" => {
            if config.containments {
                let c = ParseGFA::parse_line(&fields[1..])?;
                Some(Line::Containment(c))
            } else {
                None
            }
        }
        "P" => {
            if config.paths {
                let p = ParseGFA::parse_line(&fields[1..])?;
                Some(Line::Path(p))
            } else {
                None
            }
        }
        _ => Some(Line::Comment),
    }
}

// pub fn parse_line(line: &str) -> Option<Line<OptionalFields>> {
//     parse_line_config(line, &GFAParsingConfig::all())
// }

/*
pub fn parse_gfa_stream<'a, B: BufRead>(
    input: &'a mut Lines<B>,
) -> impl Iterator<Item = Line<OptionalFields>> + 'a {
    parse_gfa_stream_config(input, GFAParsingConfig::all())
}

pub fn parse_gfa_stream_config<'a, B: BufRead>(
    input: &'a mut Lines<B>,
    config: GFAParsingConfig,
) -> impl Iterator<Item = Line<OptionalFields>> + 'a {
    input.filter_map(move |l| {
        let l = l.expect("Error parsing file");
        parse_line_config(&l, &config)
    })
}
*/

// pub fn parse_gfa_stream_config_iter<'a, T: OptFields> (
//     input: &'a impl Iterator<
// }

pub fn parse_gfa_stream_config<'a, B: BufRead, T: OptFields>(
    input: &'a mut Lines<B>,
    config: GFAParsingConfig,
) -> impl Iterator<Item = Line<T>> + 'a {
    input.filter_map(move |l| {
        let l = l.expect("Error parsing file");
        parse_gfa_line(&l, &config)
    })
}

pub fn parse_gfa_with_config<T: OptFields>(
    path: &PathBuf,
    config: GFAParsingConfig,
) -> Option<GFA<T>> {
    let file = File::open(path)
        .unwrap_or_else(|_| panic!("Error opening file {:?}", path));

    let mut buf = String::with_capacity(1024);
    let mut reader = BufReader::new(file);
    let mut gfa = GFA::new();

    loop {
        buf.clear();
        let res = reader.read_line(&mut buf);
        if res.is_err() || res.unwrap() == 0 {
            break;
        }
        let p = parse_gfa_line(&buf, &config);

        if let Some(Line::Header(h)) = p {
            gfa.version = h.version;
        } else if let Some(Line::Segment(s)) = p {
            gfa.segments.push(s);
        } else if let Some(Line::Link(l)) = p {
            gfa.links.push(l);
        } else if let Some(Line::Containment(c)) = p {
            gfa.containments.push(c);
        } else if let Some(Line::Path(pt)) = p {
            gfa.paths.push(pt);
        }
    }

    Some(gfa)
}

pub fn parse_gfa_no_opt(path: &PathBuf) -> Option<GFA<()>> {
    parse_gfa_with_config(path, GFAParsingConfig::all())
}

pub fn parse_gfa_with_opt(path: &PathBuf) -> Option<GFA<OptionalFields>> {
    parse_gfa_with_config(path, GFAParsingConfig::all())
}

pub fn parse_gfa<T: OptFields>(path: &PathBuf) -> Option<GFA<T>> {
    parse_gfa_with_config(path, GFAParsingConfig::all())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_parse_header() {
        let hdr = "VN:Z:1.0";
        let hdr_ = Header {
            version: Some("1.0".to_string()),
        };

        match parse_header(hdr) {
            None => {
                panic!("Error parsing header");
            }
            Some(h) => assert_eq!(h, hdr_),
        }
    }

    #[test]
    fn can_parse_link() {
        let link = "11	+	12	-	4M";
        let link_ = Link {
            from_segment: "11".to_string(),
            from_orient: Orientation::Forward,
            to_segment: "12".to_string(),
            to_orient: Orientation::Backward,
            overlap: b"4M".to_vec(),
            optional: (),
        };
        let fields: Vec<_> = link.split_terminator('\t').collect();
        let parsed: Option<Link<()>> = ParseGFA::parse_line(fields.as_slice());
        match parsed {
            None => {
                panic!("Error parsing link");
            }
            Some(l) => assert_eq!(l, link_),
        }
    }

    #[test]
    fn can_parse_containment() {
        let cont = "1\t-\t2\t+\t110\t100M";

        let cont_ = Containment {
            container_name: "1".to_string(),
            container_orient: Orientation::Backward,
            contained_name: "2".to_string(),
            contained_orient: Orientation::Forward,
            overlap: b"100M".to_vec(),
            pos: 110,
            optional: (),
        };

        let fields: Vec<_> = cont.split_terminator('\t').collect();
        let parsed: Option<Containment<()>> =
            ParseGFA::parse_line(fields.as_slice());
        match parsed {
            None => {
                panic!("Error parsing containment");
            }
            Some(c) => assert_eq!(c, cont_),
        }
    }

    #[test]
    fn can_parse_path() {
        let path = "14\t11+,12-,13+\t4M,5M";

        let path_ = Path {
            path_name: "14".to_string(),
            segment_names: vec![
                ("11".to_string(), Orientation::Forward),
                ("12".to_string(), Orientation::Backward),
                ("13".to_string(), Orientation::Forward),
            ],
            overlaps: vec![b"4M".to_vec(), b"5M".to_vec()],
            optional: (),
        };

        let fields: Vec<_> = path.split_terminator('\t').collect();

        let result: Option<Path<()>> = ParseGFA::parse_line(fields.as_slice());

        match result {
            None => {
                panic!("Error parsing path");
            }
            Some(p) => assert_eq!(p, path_),
        }
    }

    /*
        #[test]
        fn can_parse_lines() {
            let input = "H	VN:Z:1.0
    S	1	CAAATAAG
    S	2	A
    S	3	G
    S	4	T
    S	5	C
    L	1	+	2	+	0M
    L	1	+	3	+	0M
    P	x	1+,3+,5+,6+,8+,9+,11+,12+,14+,15+	8M,1M,1M,3M,1M,19M,1M,4M,1M,11M";

            let lines = input.lines();
            let mut gfa = GFA::new();

            let gfa_correct = GFA {
                version: Some("1.0".to_string()),
                segments: vec![
                    Segment::new("1", "CAAATAAG"),
                    Segment::new("2", "A"),
                    Segment::new("3", "G"),
                    Segment::new("4", "T"),
                    Segment::new("5", "C"),
                ],
                links: vec![
                    Link::new(
                        "1",
                        Orientation::Forward,
                        "2",
                        Orientation::Forward,
                        "0M",
                    ),
                    Link::new(
                        "1",
                        Orientation::Forward,
                        "3",
                        Orientation::Forward,
                        "0M",
                    ),
                ],
                paths: vec![Path::new(
                    "x",
                    vec![
                        "1+", "3+", "5+", "6+", "8+", "9+", "11+", "12+", "14+",
                        "15+",
                    ],
                    vec![
                        "8M", "1M", "1M", "3M", "1M", "19M", "1M", "4M", "1M",
                        "11M",
                    ]
                    .into_iter()
                    .map(|b| b.bytes().collect())
                    .collect(),
                )],
                containments: vec![],
            };

            for l in lines {
                let p = parse_gfa_line(l);

                if let Some(Line::Header(h)) = p {
                    gfa.version = h.version;
                } else if let Some(Line::Segment(s)) = p {
                    gfa.segments.push(s);
                } else if let Some(Line::Link(l)) = p {
                    gfa.links.push(l);
                } else if let Some(Line::Path(pt)) = p {
                    gfa.paths.push(pt);
                }
            }

            assert_eq!(gfa_correct, gfa);
        }
        */

    #[test]
    fn can_parse_gfa_file() {
        let gfa = parse_gfa::<()>(&PathBuf::from("./lil.gfa"));

        match gfa {
            None => panic!("Error parsing GFA file"),
            Some(g) => {
                let num_segs = g.segments.len();
                let num_links = g.links.len();
                let num_paths = g.paths.len();
                let num_conts = g.containments.len();

                assert_eq!(num_segs, 15);
                assert_eq!(num_links, 20);
                assert_eq!(num_conts, 0);
                assert_eq!(num_paths, 3);
            }
        }
    }

    /*
    #[test]
    fn can_stream_gfa_lines() {
        let file = File::open(&PathBuf::from("./lil.gfa")).unwrap();

        let reader = BufReader::new(file);
        let mut lines = reader.lines();

        let mut gfa_lines = parse_gfa_stream(&mut lines);

        assert_eq!(
            gfa_lines.next(),
            Some(Line::Header(Header {
                version: Some("1.0".to_string())
            }))
        );

        assert_eq!(
            gfa_lines.next(),
            Some(Line::Segment(Segment::new("1", "CAAATAAG")))
        );
        assert_eq!(
            gfa_lines.next(),
            Some(Line::Segment(Segment::new("2", "A")))
        );

        assert_eq!(
            gfa_lines.next(),
            Some(Line::Segment(Segment::new("3", "G")))
        );
    }
    */

    #[test]
    fn new_parsers_work() {
        use OptionalFieldValue::*;
        let name = "11";
        let seq = "ACCTT";
        let seg = "11\tACCTT\tLN:i:123\tSH:H:AACCFF05\tRC:i:123\tUR:Z:http://test.com/\tIJ:A:x\tAB:B:I1,2,3,52124";
        let fields: Vec<_> = seg.split_terminator('\t').collect();

        let optional_fields: Vec<_> = vec![
            (b"LN", SignedInt(123)),
            (
                b"SH",
                ByteArray(vec![0xA, 0xA, 0xC, 0xC, 0xF, 0xF, 0x0, 0x5]),
            ),
            (b"RC", SignedInt(123)),
            (b"UR", PrintableString("http://test.com/".to_string())),
            (b"IJ", PrintableChar('x')),
            (b"AB", IntArray(vec![1, 2, 3, 52124])),
        ]
        .into_iter()
        .map(|(a, b)| OptionalField::new(a, b))
        .collect();

        // Ignoring optional fields works

        let segment_1: Option<Segment<()>> = ParseGFA::parse_line(&fields);

        assert_eq!(
            Some(Segment {
                name: name.to_string(),
                sequence: seq.to_string(),
                optional: ()
            }),
            segment_1
        );

        let segment_2: Segment<OptionalFields> =
            ParseGFA::parse_line(&fields).unwrap();

        assert_eq!(segment_2.name, name);
        assert_eq!(segment_2.sequence, seq);
        assert_eq!(segment_2.optional, optional_fields);
    }
}
