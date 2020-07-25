use bstr::{BStr, BString, ByteSlice};
use lazy_static::lazy_static;
use regex::bytes::Regex;

use crate::gfa::*;
use crate::optfields::*;

/// GFAParser encapsulates a parsing configuration
pub struct GFAParser<T: OptFields> {
    filter: Box<dyn Fn(&'_ BStr) -> Option<&'_ BStr>>,
    _optional_fields: std::marker::PhantomData<T>,
}

impl<T: OptFields> GFAParser<T> {
    pub fn new() -> Self {
        Self::with_config(GFAParsingConfig::all())
    }

    pub fn with_config(config: GFAParsingConfig) -> Self {
        let filter = config.make_filter();
        GFAParser {
            filter,
            _optional_fields: std::marker::PhantomData,
        }
    }

    /// Filters a line before parsing, only passing through the lines
    /// enabled in the config used to make this parser
    fn filter_line<'a>(&self, line: &'a BStr) -> Option<&'a BStr> {
        (self.filter)(line)
    }
}

impl<T: OptFields> GFAParser<T> {
    /// Consume a line-by-line iterator of bytestrings to produce a
    /// GFA object
    pub fn parse_all<I>(&self, input: I) -> GFA<BString, T>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
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

    /// Parse a single line into a GFA line
    pub fn parse_line(&self, line: &[u8]) -> Option<Line<BString, T>> {
        use Line::*;
        let line: &BStr = line.as_ref();
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

    pub fn parse_file<'a, P: Into<&'a std::path::Path>>(
        &self,
        path: P,
    ) -> std::io::Result<GFA<BString, T>> {
        use Line::*;
        use {
            bstr::io::BufReadExt,
            std::{fs::File, io::BufReader},
        };

        let file = File::open(path.into())?;
        let lines = BufReader::new(file).byte_lines();

        let mut gfa = GFA::new();

        for line in lines {
            let line = line?;
            match self.parse_line(line.as_ref()) {
                Some(Segment(s)) => gfa.segments.push(s),
                Some(Link(s)) => gfa.links.push(s),
                Some(Containment(s)) => gfa.containments.push(s),
                Some(Path(s)) => gfa.paths.push(s),
                _ => (),
            }
        }

        Ok(gfa)
    }
}

/// Represents the user-facing parser configuration that does not
/// depend on the type of the resulting GFA object; currently limited
/// to filtering which lines to parse and which to ignore
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct GFAParsingConfig {
    pub segments: bool,
    pub links: bool,
    pub containments: bool,
    pub paths: bool,
}

impl std::default::Default for GFAParsingConfig {
    fn default() -> Self {
        Self::all()
    }
}

impl GFAParsingConfig {
    /// Parse no GFA lines, useful if you only want to parse one line type
    pub fn none() -> Self {
        GFAParsingConfig {
            segments: false,
            links: false,
            containments: false,
            paths: false,
        }
    }

    /// Parse all GFA lines
    pub fn all() -> Self {
        GFAParsingConfig {
            segments: true,
            links: true,
            containments: true,
            paths: true,
        }
    }

    fn make_filter(&self) -> Box<dyn for<'a> Fn(&'a BStr) -> Option<&'a BStr>> {
        let mut filter_string = BString::from("H");
        if self.segments {
            filter_string.push(b'S');
        }
        if self.links {
            filter_string.push(b'L');
        }
        if self.containments {
            filter_string.push(b'C');
        }
        if self.paths {
            filter_string.push(b'P');
        }
        Box::new(move |s| {
            if filter_string.contains_str(&s[0..1]) {
                Some(s)
            } else {
                None
            }
        })
    }
}

/// Trait for parsing a single line into one of the GFA line types
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
        let version = OptField::parse(next.as_ref())?;

        let optional = T::parse(input);

        if let OptFieldVal::Z(version) = version.value {
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

impl<T: OptFields> ParseGFA for Segment<BString, T> {
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

impl<T: OptFields> ParseGFA for Link<BString, T> {
    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        use Orientation as O;
        let from_segment = parse_name(&mut input)?;
        let from_orient = input.next().and_then(O::from_bytes)?;
        let to_segment = parse_name(&mut input)?;
        let to_orient = input.next().and_then(O::from_bytes)?;
        let overlap = input.next()?.as_ref().into();

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

impl<T: OptFields> ParseGFA for Containment<BString, T> {
    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        use std::str::from_utf8;
        use Orientation as O;

        let container_name = parse_name(&mut input)?;
        let container_orient = input.next().and_then(O::from_bytes)?;
        let contained_name = parse_name(&mut input)?;
        let contained_orient = input.next().and_then(O::from_bytes)?;

        let pos = input.next()?;
        let pos = from_utf8(pos.as_ref()).ok().and_then(|p| p.parse().ok())?;

        let overlap = input.next()?.as_ref().into();

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

        let segment_names =
            input.next().map(|bs| BString::from(bs.as_ref()))?;

        let overlaps = input
            .next()?
            .as_ref()
            .split_str(b",")
            .map(BString::from)
            .collect();

        let optional = T::parse(input);

        Some(Path {
            path_name,
            segment_names,
            overlaps,
            optional,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_parse_header() {
        let hdr = "VN:Z:1.0";
        let hdr_ = Header {
            version: Some("1.0".into()),
            optional: (),
        };

        let result: Option<Header<()>> = ParseGFA::parse_line([hdr].iter());

        match result {
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
            from_segment: "11".into(),
            from_orient: Orientation::Forward,
            to_segment: "12".into(),
            to_orient: Orientation::Backward,
            overlap: "4M".into(),
            optional: (),
        };
        let fields = link.split_terminator('\t');
        let parsed: Option<Link<BString, ()>> = ParseGFA::parse_line(fields);
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

        let cont_: Containment<BString, _> = Containment {
            container_name: "1".into(),
            container_orient: Orientation::Backward,
            contained_name: "2".into(),
            contained_orient: Orientation::Forward,
            overlap: "100M".into(),
            pos: 110,
            optional: (),
        };

        let fields = cont.split_terminator('\t');
        let parsed: Option<Containment<BString, ()>> =
            ParseGFA::parse_line(fields);
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
            path_name: "14".into(),
            segment_names: "11+,12-,13+".into(),
            overlaps: vec!["4M".into(), "5M".into()],
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

    // #[test]
    // fn parse_gfa_file() {
    // }

    /*
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
    */

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
        use OptFieldVal::*;
        let name = "11";
        let seq = "ACCTT";
        let seg = "11\tACCTT\tLN:i:123\tSH:H:AACCFF05\tRC:i:123\tUR:Z:http://test.com/\tIJ:A:x\tAB:B:I1,2,3,52124";
        let fields = seg.split_terminator('\t');

        let optional_fields: Vec<_> = vec![
            OptField::new(b"LN", Int(123)),
            OptField::new(
                b"SH",
                H(vec![0xA, 0xA, 0xC, 0xC, 0xF, 0xF, 0x0, 0x5]),
            ),
            OptField::new(b"RC", Int(123)),
            OptField::new(b"UR", Z(BString::from("http://test.com/"))),
            OptField::new(b"IJ", A(b'x')),
            OptField::new(b"AB", BInt(vec![1, 2, 3, 52124])),
        ]
        .into_iter()
        // .map(|(a, b)| OptionalField::new(a, b))
        .collect();

        // Ignoring optional fields works

        let segment_1: Option<Segment<BString, ()>> =
            ParseGFA::parse_line(fields.clone());

        assert_eq!(
            Some(Segment {
                name: BString::from(name),
                sequence: BString::from(seq),
                optional: ()
            }),
            segment_1
        );

        let segment_2: Segment<BString, OptionalFields> =
            ParseGFA::parse_line(fields.clone()).unwrap();

        assert_eq!(segment_2.name, name);
        assert_eq!(segment_2.sequence, seq);
        assert_eq!(segment_2.optional, optional_fields);
    }
}
