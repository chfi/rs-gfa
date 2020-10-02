pub mod error;

pub use self::error::{GFAFieldResult, GFAResult, ParseError, ParseFieldError};

use bstr::{BStr, BString, ByteSlice};
use lazy_static::lazy_static;
use regex::bytes::Regex;

use crate::{cigar::CIGAR, gfa::*, optfields::*};

use crate::parser::error::ParserTolerance;

/// Builder struct for GFAParsers
pub struct GFAParserBuilder {
    pub segments: bool,
    pub links: bool,
    pub containments: bool,
    pub paths: bool,
    pub tolerance: ParserTolerance,
}

impl GFAParserBuilder {
    /// Parse no GFA lines, useful if you only want to parse one line type.
    pub fn none() -> Self {
        GFAParserBuilder {
            segments: false,
            links: false,
            containments: false,
            paths: false,
            tolerance: Default::default(),
        }
    }

    /// Parse all GFA lines.
    pub fn all() -> Self {
        GFAParserBuilder {
            segments: true,
            links: true,
            containments: true,
            paths: true,
            tolerance: Default::default(),
        }
    }

    pub fn ignore_errors(mut self) -> Self {
        self.tolerance = ParserTolerance::IgnoreAll;
        self
    }

    pub fn ignore_safe_errors(mut self) -> Self {
        self.tolerance = ParserTolerance::Safe;
        self
    }

    pub fn pedantic_errors(mut self) -> Self {
        self.tolerance = ParserTolerance::Pedantic;
        self
    }

    pub fn build<N: SegmentId, T: OptFields>(self) -> GFAParser<N, T> {
        GFAParser {
            segments: self.segments,
            links: self.links,
            containments: self.containments,
            paths: self.paths,
            tolerance: self.tolerance,
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
}

pub struct GFAParser<N: SegmentId, T: OptFields> {
    segments: bool,
    links: bool,
    containments: bool,
    paths: bool,
    tolerance: ParserTolerance,
    _optional_fields: std::marker::PhantomData<T>,
    _segment_names: std::marker::PhantomData<N>,
}

impl<N: SegmentId, T: OptFields> Default for GFAParser<N, T> {
    fn default() -> Self {
        let config = GFAParserBuilder::all();
        config.build()
    }
}

impl<N: SegmentId, T: OptFields> GFAParser<N, T> {
    /// Create a new GFAParser that will parse all four GFA line
    /// types, and use the optional fields parser and storage `T`.
    pub fn new() -> Self {
        Default::default()
    }

    pub fn parse_gfa_line(&self, bytes: &[u8]) -> GFAResult<Line<N, T>> {
        let line: &BStr = bytes.trim().as_ref();

        let mut fields = line.split_str(b"\t");
        let hdr = fields.next().ok_or(ParseError::EmptyLine)?;

        let invalid_line =
            |e: ParseFieldError| ParseError::invalid_line(e, bytes);

        let line = match hdr {
            b"H" => Header::parse_line(fields).map(Header::wrap),
            b"S" if self.segments => {
                Segment::parse_line(fields).map(Segment::wrap)
            }
            b"L" if self.links => Link::parse_line(fields).map(Link::wrap),
            b"C" if self.containments => {
                Containment::parse_line(fields).map(Containment::wrap)
            }
            b"P" if self.paths => Path::parse_line(fields).map(Path::wrap),
            _ => return Err(ParseError::UnknownLineType),
        }
        .map_err(invalid_line)?;
        Ok(line)
    }

    pub fn parse_lines<I>(&self, lines: I) -> GFAResult<GFA<N, T>>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let mut gfa = GFA::new();

        for line in lines {
            match self.parse_gfa_line(line.as_ref()) {
                Ok(parsed) => gfa.insert_line(parsed),
                Err(err) if err.can_safely_continue(&self.tolerance) => (),
                Err(err) => return Err(err),
            };
        }

        Ok(gfa)
    }

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
            match self.parse_gfa_line(line.as_ref()) {
                Ok(parsed) => gfa.insert_line(parsed),
                Err(err) if err.can_safely_continue(&self.tolerance) => (),
                Err(err) => return Err(err),
            };
        }

        Ok(gfa)
    }
}

fn next_field<I, P>(mut input: I) -> GFAFieldResult<P>
where
    I: Iterator<Item = P>,
    P: AsRef<[u8]>,
{
    input.next().ok_or(ParseFieldError::MissingFields)
}

fn parse_orientation<I>(mut input: I) -> GFAFieldResult<Orientation>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
{
    let next = next_field(&mut input)?;
    let parsed = Orientation::from_bytes_plus_minus(next.as_ref());
    Orientation::parse_error(parsed)
}

impl<T: OptFields> Header<T> {
    #[inline]
    fn wrap<N: SegmentId>(self) -> Line<N, T> {
        Line::Header(self)
    }

    #[inline]
    fn parse_line<I>(mut input: I) -> GFAFieldResult<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let next = next_field(&mut input)?;
        let version = OptField::parse(next.as_ref());
        let version =
            if let Some(OptFieldVal::Z(version)) = version.map(|v| v.value) {
                Some(version)
            } else {
                None
            };

        let optional = T::parse(input);

        Ok(Header { version, optional })
    }
}

fn parse_sequence<I>(input: &mut I) -> GFAFieldResult<BString>
where
    I: Iterator,
    I::Item: AsRef<[u8]>,
{
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?-u)\*|[A-Za-z=.]+").unwrap();
    }

    let next = next_field(input)?;
    RE.find(next.as_ref())
        .map(|s| BString::from(s.as_bytes()))
        .ok_or(ParseFieldError::InvalidField("Sequence"))
}

impl<N: SegmentId, T: OptFields> Segment<N, T> {
    #[inline]
    fn wrap(self) -> Line<N, T> {
        Line::Segment(self)
    }

    #[inline]
    fn parse_line<I>(mut input: I) -> GFAFieldResult<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let name = N::parse_next(&mut input)?;
        let sequence = parse_sequence(&mut input)?;
        let optional = T::parse(input);
        Ok(Segment {
            name,
            sequence,
            optional,
        })
    }
}

impl<N: SegmentId, T: OptFields> Link<N, T> {
    #[inline]
    fn wrap(self) -> Line<N, T> {
        Line::Link(self)
    }

    #[inline]
    fn parse_line<I>(mut input: I) -> GFAFieldResult<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let from_segment = N::parse_next(&mut input)?;
        let from_orient = parse_orientation(&mut input)?;
        let to_segment = N::parse_next(&mut input)?;
        let to_orient = parse_orientation(&mut input)?;

        let overlap = next_field(&mut input)?.as_ref().into();

        let optional = T::parse(input);
        Ok(Link {
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
    #[inline]
    fn wrap(self) -> Line<N, T> {
        Line::Containment(self)
    }

    #[inline]
    fn parse_line<I>(mut input: I) -> GFAFieldResult<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let container_name = N::parse_next(&mut input)?;
        let container_orient = parse_orientation(&mut input)?;

        let contained_name = N::parse_next(&mut input)?;
        let contained_orient = parse_orientation(&mut input)?;

        let pos = next_field(&mut input)?;
        let pos = pos.as_ref().to_str()?.parse()?;

        let overlap = next_field(&mut input)?.as_ref().into();

        let optional = T::parse(input);

        Ok(Containment {
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
    #[inline]
    fn wrap(self) -> Line<N, T> {
        Line::Path(self)
    }

    #[inline]
    fn parse_line<I>(mut input: I) -> GFAFieldResult<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        // Use the SegmentId parser for the path name as well; it's
        // just always BString
        let path_name = BString::parse_next(&mut input)?;

        let segment_names =
            next_field(&mut input).map(|bs| BString::from(bs.as_ref()))?;

        let overlaps = next_field(&mut input)?
            .as_ref()
            .split_str(b",")
            .map(|bs| {
                if bs == b"*" {
                    None
                } else {
                    CIGAR::from_bytestring(bs)
                }
            })
            .collect();

        /*
        // special case for throwing away the *s if all overlaps are *
        // faster but will require some more logic in the Path
        // interface, so I'm sticking to the other one for now

        let overlaps = next_field(&mut input)?;
        let overlaps = overlaps
            .as_ref()
            .split_str(b",")
            // .map(BString::from)
            .map(|bs| {
                if bs == b"*" {
                    None
                } else {
                    CIGAR::from_bytestring(bs)
                }
            });

        let (overlaps, none): (Vec<_>, Vec<_>) =
            overlaps.partition(|x| x.is_some());
        let overlaps = if overlaps.is_empty() { none } else { overlaps };
        */
        // .collect();

        let optional = T::parse(input);

        Ok(Path::new(path_name, segment_names, overlaps, optional))
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

        let result: GFAFieldResult<Header<()>> =
            Header::parse_line([hdr].iter());

        match result {
            Err(_) => {
                panic!("Error parsing header");
            }
            Ok(h) => assert_eq!(h, hdr_),
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
            Err(_) => {
                panic!("Error parsing link");
            }
            Ok(l) => assert_eq!(l, link_),
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
            Err(_) => {
                panic!("Error parsing containment");
            }
            Ok(c) => assert_eq!(c, cont_),
        }
    }

    #[test]
    fn can_parse_path() {
        let path = "14\t11+,12-,13+\t4M,5M";

        let cigars = vec![b"4M", b"5M"]
            .iter()
            .map(|bs| CIGAR::from_bytestring(&bs[..]))
            .collect();

        let path_: Path<BString, _> =
            Path::new("14".into(), "11+,12-,13+".into(), cigars, ());

        let fields = path.split_terminator('\t');

        let result = Path::parse_line(fields);

        match result {
            Err(_) => {
                panic!("Error parsing path");
            }
            Ok(p) => assert_eq!(p, path_),
        }
    }

    #[test]
    fn can_parse_gfa_lines() {
        let parser = GFAParser::new();
        let gfa: GFA<BString, ()> =
            parser.parse_file(&"./test/gfas/lil.gfa").unwrap();

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
    fn gfa_usize_parser_can_fail() {
        let usize_parser: GFAParser<usize, OptionalFields> = GFAParser::new();
        let usize_gfa = usize_parser.parse_file(&"./test/gfas/diatom.gfa");

        assert!(usize_gfa.is_err());

        let err = usize_gfa.unwrap_err();

        assert!(matches!(
            err,
            ParseError::InvalidLine(ParseFieldError::UintIdError, _)
        ));
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

        let segment_1: GFAFieldResult<Segment<BString, ()>> =
            Segment::parse_line(fields.clone());

        assert!(segment_1.is_ok());
        assert_eq!(
            Segment {
                name: BString::from(name),
                sequence: BString::from(seq),
                optional: ()
            },
            segment_1.unwrap(),
        );

        let segment_2: Segment<BString, OptionalFields> =
            Segment::parse_line(fields).unwrap();

        assert_eq!(segment_2.name, name);
        assert_eq!(segment_2.sequence, seq);
        assert_eq!(segment_2.optional, optional_fields);
    }
}
