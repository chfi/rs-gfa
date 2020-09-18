use bstr::{BStr, BString, ByteSlice};
use lazy_static::lazy_static;
use regex::bytes::Regex;

use crate::gfa::*;
use crate::optfields::*;

type GFALineFilter = Box<dyn Fn(&'_ BStr) -> Option<&'_ BStr>>;

/// Builder struct for GFAParsers
pub struct GFAParserBuilder {
    pub segments: bool,
    pub links: bool,
    pub containments: bool,
    pub paths: bool,
}

impl GFAParserBuilder {
    /// Parse no GFA lines, useful if you only want to parse one line type.
    pub fn none() -> Self {
        GFAParserBuilder {
            segments: false,
            links: false,
            containments: false,
            paths: false,
        }
    }

    /// Parse all GFA lines.
    pub fn all() -> Self {
        GFAParserBuilder {
            segments: true,
            links: true,
            containments: true,
            paths: true,
        }
    }

    pub fn build<N: SegmentId, T: OptFields>(self) -> GFAParser<N, T> {
        let filter = self.make_filter();
        GFAParser {
            filter,
            _optional_fields: std::marker::PhantomData,
            _segment_names: std::marker::PhantomData,
        }
    }

    pub fn build_usize_id<T: OptFields>(self) -> GFAParser<usize, T> {
        self.build()
    }

    pub fn build_bstr_id<T: OptFields>(self) -> GFAParser<BString, T> {
        self.build()
    }

    fn make_filter(&self) -> GFALineFilter {
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

pub struct GFAParser<N: SegmentId, T: OptFields> {
    filter: GFALineFilter,
    _optional_fields: std::marker::PhantomData<T>,
    _segment_names: std::marker::PhantomData<N>,
}

impl<N: SegmentId, T: OptFields> GFAParser<N, T> {
    /// Create a new GFAParser that will parse all four GFA line
    /// types, and use the optional fields parser and storage `T`.
    pub fn new() -> Self {
        let config = GFAParserBuilder::all();
        config.build()
    }

    /// Filters a line before parsing, only passing through the lines
    /// enabled in the config used to make this parser. NB: this will
    /// probably change in the future; I was playing around with
    /// storing the line filter as a closure for performance, but
    /// still need to profile that aspect
    fn filter_line<'a>(&self, line: &'a BStr) -> Option<&'a BStr> {
        (self.filter)(line)
    }

    pub fn parse_line_bytes(&self, bytes: &[u8]) -> Option<Line<N, T>> {
        let line: &BStr = bytes.trim().as_ref();
        if let Some(line) = self.filter_line(line) {
            let mut fields = line.split_str(b"\t");
            let hdr = fields.next()?;
            match hdr {
                b"H" => Header::parse_line(fields).map(Header::wrap),
                b"S" => Segment::parse_line(fields).map(Segment::wrap),
                b"L" => Link::parse_line(fields).map(Link::wrap),
                b"C" => Containment::parse_line(fields).map(Containment::wrap),
                b"P" => Path::parse_line(fields).map(Path::wrap),
                _ => None,
            }
        } else {
            None
        }
    }

    /// Convenience function for parsing a .gfa file. Opens the file
    /// at the provided path and reads it line-by-line.
    pub fn parse_file<P: AsRef<std::path::Path>>(
        &self,
        path: P,
    ) -> Result<GFA<N, T>, ParseError> {
        use {
            bstr::io::BufReadExt,
            std::{fs::File, io::BufReader},
        };

        let file = File::open(path)?;
        let lines = BufReader::new(file).byte_lines();

        let mut gfa = GFA::new();

        for line in lines {
            let line = line?;
            if let Some(line) = self.parse_line(line.as_ref()) {
                gfa.insert_line(line);
            }
        }

        Ok(gfa)
    }
}

impl<T: OptFields> Header<T> {
    fn wrap<N: SegmentId>(self) -> Line<N, T> {
        Line::Header(self)
    }

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

impl<N: SegmentId, T: OptFields> Segment<N, T> {
    fn wrap(self) -> Line<N, T> {
        Line::Segment(self)
    }

    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let name = N::parse_next(&mut input)?;
        let sequence = parse_sequence(&mut input)?;
        let optional = T::parse(input);
        Some(Segment {
            name,
            sequence,
            optional,
        })
    }
}

impl<N: SegmentId, T: OptFields> Link<N, T> {
    fn wrap(self) -> Line<N, T> {
        Line::Link(self)
    }

    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        use Orientation as O;
        let from_segment = N::parse_next(&mut input)?;
        let from_orient = input.next().and_then(O::from_bytes_plus_minus)?;
        let to_segment = N::parse_next(&mut input)?;
        let to_orient = input.next().and_then(O::from_bytes_plus_minus)?;
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

impl<N: SegmentId, T: OptFields> Containment<N, T> {
    fn wrap(self) -> Line<N, T> {
        Line::Containment(self)
    }

    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        use std::str::from_utf8;
        use Orientation as O;

        let container_name = N::parse_next(&mut input)?;
        let container_orient =
            input.next().and_then(O::from_bytes_plus_minus)?;
        let contained_name = N::parse_next(&mut input)?;
        let contained_orient =
            input.next().and_then(O::from_bytes_plus_minus)?;

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

impl<N: SegmentId, T: OptFields> Path<N, T> {
    fn wrap(self) -> Line<N, T> {
        Line::Path(self)
    }

    fn parse_line<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        // Use the SegmentId parser for the path name as well; it's
        // just always BString
        let path_name = BString::parse_next(&mut input)?;

        let segment_names =
            input.next().map(|bs| BString::from(bs.as_ref()))?;

        let overlaps = input
            .next()?
            .as_ref()
            .split_str(b",")
            .map(BString::from)
            .collect();

        let optional = T::parse(input);

        Some(Path::new(path_name, segment_names, overlaps, optional))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_parse_header() {
        let hdr = b"VN:Z:1.0";
        let hdr_ = Header {
            version: Some("1.0".into()),
            optional: (),
        };

        let result: Option<Header<()>> = Header::parse_line([hdr].iter());

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
        let link_: Link<BString, ()> = Link {
            from_segment: "11".into(),
            from_orient: Orientation::Forward,
            to_segment: "12".into(),
            to_orient: Orientation::Backward,
            overlap: "4M".into(),
            optional: (),
        };

        let fields = link.split_terminator('\t');
        let result = Link::parse_line(fields);

        match result {
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
        let result = Containment::parse_line(fields);
        match result {
            None => {
                panic!("Error parsing containment");
            }
            Some(c) => assert_eq!(c, cont_),
        }
    }

    #[test]
    fn can_parse_path() {
        let path = "14\t11+,12-,13+\t4M,5M";

        let path_: Path<BString, _> = Path::new(
            "14".into(),
            "11+,12-,13+".into(),
            vec!["4M".into(), "5M".into()],
            (),
        );

        let fields = path.split_terminator('\t');

        let result = Path::parse_line(fields);

        match result {
            None => {
                panic!("Error parsing path");
            }
            Some(p) => assert_eq!(p, path_),
        }
    }

    #[test]
    fn can_parse_gfa_lines() {
        let parser = GFAParser::new();
        let gfa: GFA<BString, ()> = parser.parse_file(&"./lil.gfa").unwrap();

        let num_segs = gfa.segments.len();
        let num_links = gfa.links.len();
        let num_paths = gfa.paths.len();
        let num_conts = gfa.containments.len();

        assert_eq!(num_segs, 15);
        assert_eq!(num_links, 20);
        assert_eq!(num_conts, 0);
        assert_eq!(num_paths, 3);
    }

    #[test]
    fn segment_parser() {
        use OptFieldVal::*;
        let name = "11";
        let seq = "ACCTT";
        let segment_bytes = "11\tACCTT\tLN:i:123\tSH:H:AACCFF05\tRC:i:123\tUR:Z:http://test.com/\tIJ:A:x\tAB:B:I1,2,3,52124";
        let fields = segment_bytes.split_terminator('\t');

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
        .collect();

        let segment_1: Option<Segment<BString, ()>> =
            Segment::parse_line(fields.clone());

        assert_eq!(
            Some(Segment {
                name: BString::from(name),
                sequence: BString::from(seq),
                optional: ()
            }),
            segment_1
        );

        let segment_2: Segment<BString, OptionalFields> =
            Segment::parse_line(fields).unwrap();

        assert_eq!(segment_2.name, name);
        assert_eq!(segment_2.sequence, seq);
        assert_eq!(segment_2.optional, optional_fields);
    }
}
