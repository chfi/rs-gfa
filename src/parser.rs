use bstr::io::*;
use bstr::{BStr, BString, ByteSlice, ByteVec};
use std::fs::File;
use std::io::prelude::*;
use std::io::{BufReader, Lines};
use std::path::PathBuf;

use lazy_static::lazy_static;
use regex::bytes::Regex;

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

    fn make_filter(&self) -> Box<dyn for<'a> Fn(&'a BStr) -> Option<&'a BStr>> {
        let string = self.filter_chars();
        Box::new(move |s| {
            if string.contains_str(&s[0..1]) {
                Some(s)
            } else {
                None
            }
        })
    }

    fn filter_chars(&self) -> BString {
        let mut result = BString::from("H");
        if self.segments {
            result.push(b'S');
        }
        if self.links {
            result.push(b'L');
        }
        if self.containments {
            result.push(b'C');
        }
        if self.paths {
            result.push(b'P');
        }

        result
    }
}

pub struct GFAParser<T> {
    filter: Box<dyn Fn(&'_ BStr) -> Option<&'_ BStr>>,
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

    pub fn filter_line<'a>(&self, line: &'a BStr) -> Option<&'a BStr> {
        (self.filter)(line)
    }
}

impl<T: OptFields> GFAParser<T> {
    pub fn parse_all<I>(&self, mut input: I) -> GFA<T>
    where
        I: Iterator,
        I::Item: AsRef<BStr>,
    {
        use Line::*;
        let mut gfa = GFA::new();
        for line in input {
            match self.parse_line(line.as_ref()) {
                Some(Segment(s)) => gfa.segments.push(s),
                Some(Link(s)) => gfa.links.push(s),
                Some(Containment(s)) => gfa.containments.push(s),
                Some(Path(s)) => gfa.paths.push(s),
                _ => (),
            }
        }

        gfa
    }

    pub fn parse_line(&self, line: &BStr) -> Option<Line<T>> {
        use Line::*;
        if let Some(line) = self.filter_line(line) {
            let mut fields = line.split_str(b"\t");
            let hdr = fields.next()?;
            match hdr {
                b"H" => ParseGFA::parse_line(fields).map(Header),
                b"S" => ParseGFA::parse_line(fields).map(Segment),
                b"L" => ParseGFA::parse_line(fields).map(Link),
                b"C" => ParseGFA::parse_line(fields).map(Containment),
                b"P" => ParseGFA::parse_line(fields).map(Path),
                _ => None,
            }
        } else {
            None
        }
    }
}

// parse_optionals returns Self, not Option, since, well, they're optional anyway
pub trait OptFields: Sized + Default {
    fn parse<T>(input: T) -> Self
    where
        T: IntoIterator,
        T::Item: AsRef<[u8]>;

    fn get_field(&self, tag: OptTag) -> Option<&OptionalFieldValue>;
}

impl OptFields for () {
    fn parse<T>(_input: T) -> Self
    where
        T: IntoIterator,
        T::Item: AsRef<[u8]>,
    {
        ()
    }

    fn get_field(&self, _: OptTag) -> Option<&OptionalFieldValue> {
        None
    }
}

impl OptFields for Vec<OptionalField> {
    fn parse<T>(input: T) -> Self
    where
        T: IntoIterator,
        T::Item: AsRef<[u8]>,
    {
        input
            .into_iter()
            .filter_map(|f| parse_optional_field(f.as_ref()))
            .collect()
    }

    fn get_field(&self, tag: OptTag) -> Option<&OptionalFieldValue> {
        self.iter().find(|o| o.tag == tag).map(|v| &v.content)
    }
}

trait ParseGFA: Sized + Default {
    fn parse_line<I>(input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>;
}

impl<T: OptFields> ParseGFA for Header<T> {
    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let next = input.next()?;
        let version = parse_optional_field(next.as_ref())?;

        let optional = T::parse(input);
        if let OptionalFieldValue::PrintableString(version) = version.content {
            Some(Header {
                version: Some(version),
                optional,
            })
        } else {
            None
        }
    }
}

fn parse_name<I>(input: &mut I) -> Option<BString>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
{
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?-u)[!-)+-<>-~][!-~]*").unwrap();
    }

    let next = input.next()?;
    RE.find(next.as_ref()).map(|s| BString::from(s.as_bytes()))
}

fn parse_sequence<I>(input: &mut I) -> Option<BString>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
{
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?-u)\*|[A-Za-z=.]+").unwrap();
    }

    let next = input.next()?;
    RE.find(next.as_ref()).map(|s| BString::from(s.as_bytes()))
}

fn parse_orient<I>(input: &mut I) -> Option<Orientation>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
{
    let next = input.next()?;
    match next.as_ref() {
        b"+" => Some(Orientation::Forward),
        b"-" => Some(Orientation::Backward),
        _ => None,
    }
}

impl<T: OptFields> ParseGFA for Segment<T> {
    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let name = parse_name(&mut input)?;
        let sequence = parse_sequence(&mut input)?;
        let optional = T::parse(input);
        Some(Segment {
            name,
            sequence,
            optional,
        })
    }
}

impl<T: OptFields> ParseGFA for Link<T> {
    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let from_segment = parse_name(&mut input)?;
        let from_orient = parse_orient(&mut input)?;
        let to_segment = parse_name(&mut input)?;
        let to_orient = parse_orient(&mut input)?;
        let overlap = input.next().map(|bs| Vec::from_slice(bs.as_ref()))?;

        let optional = T::parse(input);
        Some(Link {
            from_segment,
            from_orient,
            to_segment,
            to_orient,
            overlap,
            optional,
        })
    }
}

impl<T: OptFields> ParseGFA for Containment<T> {
    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let container_name = parse_name(&mut input)?;
        let container_orient = parse_orient(&mut input)?;
        let contained_name = parse_name(&mut input)?;
        let contained_orient = parse_orient(&mut input)?;

        let pos = input.next().and_then(|bs| {
            let st = std::str::from_utf8(bs.as_ref()).unwrap();
            st.parse().ok()
        })?;
        let next = input.next()?;
        let overlap = Vec::from_slice(next.as_ref());

        let optional = T::parse(input);
        Some(Containment {
            container_name,
            container_orient,
            contained_name,
            contained_orient,
            overlap,
            pos,
            optional,
        })
    }
}

impl<T: OptFields> ParseGFA for Path<T> {
    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let path_name = parse_name(&mut input)?;

        let next = input.next()?;
        let segments = next.as_ref().split_str(b",");
        let segment_names = segments.map(|s| BString::from(s)).collect();
        // let segment_names = BString::from(next.as_ref());
        // let segment_names = segments.map(parse_path_segment).collect();

        let next = input.next()?;
        let split = next.as_ref().split_str(b",");
        let overlaps = split.map(|s| Vec::from_slice(s)).collect();

        let optional = T::parse(input);

        Some(Path {
            path_name,
            segment_names,
            overlaps,
            optional,
        })
    }
}

fn parse_optional_tag(input: &[u8]) -> Option<BString> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?-u)[A-Za-z][A-Za-z0-9]").unwrap();
    }
    RE.find(input).map(|s| BString::from(s.as_bytes()))
}

fn parse_optional_char(input: &[u8]) -> Option<u8> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?-u)[!-~]").unwrap();
    }

    RE.find(input).map(|s| s.as_bytes()[0])
}

fn parse_optional_int(input: &[u8]) -> Option<i64> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?-u)[-+]?[0-9]+").unwrap();
    }
    RE.find(input)
        .and_then(|s| std::str::from_utf8(s.as_bytes()).unwrap().parse().ok())
}

fn parse_optional_float(input: &[u8]) -> Option<f32> {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"(?-u)[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?").unwrap();
    }

    RE.find(input)
        .and_then(|s| std::str::from_utf8(s.as_bytes()).unwrap().parse().ok())
}

fn parse_optional_string(input: &[u8]) -> Option<BString> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?-u)[ !-~]+").unwrap();
    }
    RE.find(input).map(|s| BString::from(s.as_bytes()))
}

// TODO I'm not entirely sure if this works as it should; I assume it
// should actually parse pairs of digits
fn parse_optional_bytearray(input: &[u8]) -> Option<Vec<u32>> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?-u)[0-9A-F]+").unwrap();
    }

    RE.find(input).map(|s| {
        std::str::from_utf8(s.as_bytes())
            .unwrap()
            .chars()
            .filter_map(|c| c.to_digit(16))
            .collect()
    })
    // .map(|s| s.as_str().chars().filter_map(|c| c.to_digit(16)).collect())
}

fn parse_optional_array<T>(input: &[u8]) -> Option<Vec<T>>
where
    T: std::str::FromStr,
{
    let iter = input.split_str(b",");
    let result = iter
        .filter_map(|s| std::str::from_utf8(s.as_bytes()).unwrap().parse().ok())
        .collect();
    Some(result)
}

fn parse_optional_field(input: &[u8]) -> Option<OptionalField> {
    use OptionalFieldValue::*;

    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?-u)[AifZJHB]").unwrap();
    }

    let mut fields = input.split_str(b":");

    let field_tag = fields.next()?;
    assert_eq!(field_tag.len(), 2);
    let field_tag = OptTag::from_bytes(field_tag)?;
    // let fields: Vec<_> = input.split_terminator(':').collect();

    // let field_type = RE.find(fields[1]).map(|s| s.as_str())?;
    let field_type = fields.next()?;
    assert_eq!(field_type.len(), 1);
    let field_type = RE.find(field_type).map(|s| s.as_bytes()[0])?;
    // let field_type = RE.find(&input[3..=3]).map(|s| s.as_str())?;
    // let field_tag = parse_optional_tag(&input[0..=1])?;
    // let field_tag = OptTag::from_str(&input[0..=1])?;
    let field_contents = fields.next()?;
    // let field_contents = &input[5..];
    let field_value = match field_type {
        // char
        b'A' => parse_optional_char(field_contents).map(PrintableChar),
        // int
        b'i' => parse_optional_int(field_contents).map(SignedInt),
        // float
        b'f' => parse_optional_float(field_contents).map(Float),
        // string
        b'Z' => parse_optional_string(field_contents).map(PrintableString),
        // JSON string
        b'J' => parse_optional_string(field_contents).map(JSON),
        // bytearray
        b'H' => parse_optional_bytearray(field_contents).map(ByteArray),
        // float or int array
        b'B' => {
            if field_contents.starts_with(b"f") {
                parse_optional_array(&field_contents[1..]).map(FloatArray)
            } else {
                parse_optional_array(&field_contents[1..]).map(IntArray)
            }
        }
        _ => panic!(
            "Tried to parse optional field with unknown type '{}'",
            field_type,
        ),
    }?;

    Some(OptionalField {
        tag: field_tag,
        content: field_value,
    })
}

/*
pub fn parse_gfa_line<T: OptFields>(
    line: &str,
    config: &GFAParsingConfig,
) -> Option<Line<T>> {
    let mut fields = line.split_terminator('\t');
    let ltyp = fields.next()?;
    // let fields: Vec<_> = line.split_terminator('\t').collect();
    match ltyp {
        "H" => {
            let input = fields.next()?;
            let h = parse_header(input)?;
            Some(Line::Header(h))
        }
        "#" => Some(Line::Comment),
        "S" => {
            if config.segments {
                let s: Segment<T> = ParseGFA::parse_line(&mut fields)?;
                Some(Line::Segment(s))
            } else {
                None
            }
        }
        "L" => {
            if config.links {
                let l = ParseGFA::parse_line(&mut fields)?;
                Some(Line::Link(l))
            } else {
                None
            }
        }
        "C" => {
            if config.containments {
                let c = ParseGFA::parse_line(&mut fields)?;
                Some(Line::Containment(c))
            } else {
                None
            }
        }
        "P" => {
            if config.paths {
                let p = ParseGFA::parse_line(&mut fields)?;
                Some(Line::Path(p))
            } else {
                None
            }
        }
        _ => Some(Line::Comment),
    }
}
*/

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

/*

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
*/

#[cfg(test)]
mod tests {
    use super::*;

    /*
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
    */

    #[test]
    fn can_parse_link() {
        let link = "11	+	12	-	4M";
        let link_ = Link {
            from_segment: BString::from("11"),
            from_orient: Orientation::Forward,
            to_segment: BString::from("12"),
            to_orient: Orientation::Backward,
            overlap: b"4M".to_vec(),
            optional: (),
        };
        let fields = link.split_terminator('\t');
        let parsed: Option<Link<()>> = ParseGFA::parse_line(fields);
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

        let fields = cont.split_terminator('\t');
        let parsed: Option<Containment<()>> = ParseGFA::parse_line(fields);
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

        let fields = path.split_terminator('\t');

        let result: Option<Path<()>> = ParseGFA::parse_line(fields);

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
        let fields = seg.split_terminator('\t');

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

        let segment_1: Option<Segment<()>> =
            ParseGFA::parse_line(fields.clone());

        assert_eq!(
            Some(Segment {
                name: name.to_string(),
                sequence: seq.to_string(),
                optional: ()
            }),
            segment_1
        );

        let segment_2: Segment<OptionalFields> =
            ParseGFA::parse_line(fields.clone()).unwrap();

        assert_eq!(segment_2.name, name);
        assert_eq!(segment_2.sequence, seq);
        assert_eq!(segment_2.optional, optional_fields);
    }
}
