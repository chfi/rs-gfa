use bstr::{BStr, BString, ByteSlice};
use lazy_static::lazy_static;
use regex::bytes::Regex;

use crate::gfa::*;
use crate::optfields::*;

type GFALineFilter = Box<dyn Fn(&'_ BStr) -> Option<&'_ BStr>>;

#[derive(Debug, Clone)]
pub enum ParseFieldError {
    /// A segment ID couldn't be parsed as a usize. Can only happen
    /// when parsing into a GFA<usize, T>.
    UsizeIdError,
    /// A bytestring couldn't be parsed as a bytestring, can happen
    /// when the contents aren't UTF8.
    BStringUtf8Error,
    /// Attempted to parse an orientation that wasn't + or -.
    OrientationError,
    /// A required field was incorrectly formatted. Includes the field
    /// name as defined by the GFA1 spec.
    InvalidField(&'static str),
    MissingFields,
    Other,
}

/// Type encapsulating different kinds of GFA parsing errors
#[derive(Debug)]
pub enum ParseError {
    /// The line type was something other than 'H', 'S', 'L', 'C', or
    /// 'P'. This is ignored by the file parser rather than a fail
    /// condition.
    UnknownLineType,
    /// Tried to parse an empty line. Can be ignored.
    EmptyLine,
    /// A line couldn't be parsed. Includes the problem line and a
    /// variant describing the error.
    InvalidLine(ParseFieldError, String),
    InvalidField(ParseFieldError),
    /// Wrapper for an IO error.
    IOError(std::io::Error),

    Other,
}

type GFAFieldResult<T> = Result<T, ParseFieldError>;
type GFAResult<T> = Result<T, ParseError>;

impl From<std::io::Error> for ParseError {
    fn from(err: std::io::Error) -> Self {
        Self::IOError(err)
    }
}

impl ParseError {
    pub fn other() -> Self {
        Self::Other
    }

    pub(crate) fn invalid_line(error: ParseFieldError, line: &[u8]) -> Self {
        let s = std::str::from_utf8(line).unwrap();
        Self::InvalidLine(error, s.into())
    }

    pub fn break_if_necessary(self) -> GFAResult<()> {
        if self.can_safely_continue() {
            Ok(())
        } else {
            Err(self)
        }
    }

    pub const fn can_safely_continue(&self) -> bool {
        match self {
            ParseError::EmptyLine => true,
            ParseError::UnknownLineType => true,
            _ => false,
        }
    }
}

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
                b"H" => Header::parse_line(fields).ok().map(Header::wrap),
                b"S" => Segment::parse_line(fields).ok().map(Segment::wrap),
                b"L" => Link::parse_line(fields).ok().map(Link::wrap),
                b"C" => {
                    Containment::parse_line(fields).ok().map(Containment::wrap)
                }
                b"P" => Path::parse_line(fields).ok().map(Path::wrap),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn parse_line_with_error(
        &self,
        bytes: &[u8],
    ) -> Result<Line<N, T>, ParseError> {
        let line: &BStr = bytes.trim().as_ref();

        let mut fields = line.split_str(b"\t");
        let hdr = fields.next().ok_or(ParseError::EmptyLine)?;

        let invalid_line =
            |e: ParseFieldError| ParseError::invalid_line(e, bytes);

        match hdr {
            b"H" => Header::parse_line(fields)
                .map(Header::wrap)
                .map_err(invalid_line),
            b"S" => Segment::parse_line(fields)
                .map(Segment::wrap)
                .map_err(invalid_line),
            b"L" => Link::parse_line(fields)
                .map(Link::wrap)
                .map_err(invalid_line),
            b"C" => Containment::parse_line(fields)
                .map(Containment::wrap)
                .map_err(invalid_line),
            b"P" => Path::parse_line(fields)
                .map(Path::wrap)
                .map_err(invalid_line),
            _ => Err(ParseError::UnknownLineType),
        }
    }

    pub fn parse_lines<I>(&self, lines: I) -> GFAResult<GFA<N, T>>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let mut gfa = GFA::new();

        for line in lines {
            match self.parse_line_with_error(line.as_ref()) {
                Ok(parsed) => gfa.insert_line(parsed),
                Err(err) if err.can_safely_continue() => (),
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
            match self.parse_line_with_error(line.as_ref()) {
                Ok(parsed) => gfa.insert_line(parsed),
                Err(err) if err.can_safely_continue() => (),
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
    fn wrap<N: SegmentId>(self) -> Line<N, T> {
        Line::Header(self)
    }

    fn parse_line<I>(mut input: I) -> GFAFieldResult<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let next = input.next().ok_or(ParseFieldError::MissingFields)?;
        let version = OptField::parse(next.as_ref())
            .ok_or(ParseFieldError::MissingFields)?;
        let optional = T::parse(input);

        if let OptFieldVal::Z(version) = version.value {
            Ok(Header {
                version: Some(version),
                optional,
            })
        } else {
            Err(ParseFieldError::Other)
        }
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

    let next = input.next().ok_or(ParseFieldError::MissingFields)?;
    RE.find(next.as_ref())
        .map(|s| BString::from(s.as_bytes()))
        .ok_or(ParseFieldError::InvalidField("Sequence"))
}

impl<N: SegmentId, T: OptFields> Segment<N, T> {
    fn wrap(self) -> Line<N, T> {
        Line::Segment(self)
    }

    fn parse_line<I>(mut input: I) -> GFAFieldResult<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        // let name = N::parse_next(&mut input).ok_or(?;

        let name = N::parse_next_result(&mut input)?;
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
    fn wrap(self) -> Line<N, T> {
        Line::Link(self)
    }

    fn parse_line<I>(mut input: I) -> GFAFieldResult<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let from_segment = N::parse_next_result(&mut input)?;
        let from_orient = parse_orientation(&mut input)?;
        let to_segment = N::parse_next_result(&mut input)?;
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
    fn wrap(self) -> Line<N, T> {
        Line::Containment(self)
    }

    fn parse_line<I>(mut input: I) -> GFAFieldResult<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        use std::str::from_utf8;

        let container_name = N::parse_next_result(&mut input)?;
        let container_orient = parse_orientation(&mut input)?;
        // input.next().and_then(O::from_bytes_plus_minus)?;
        let contained_name = N::parse_next_result(&mut input)?;
        let contained_orient = parse_orientation(&mut input)?;
        // input.next().and_then(O::from_bytes_plus_minus)?;

        let next = next_field(&mut input)?;
        // input.next()?;
        let pos = from_utf8(next.as_ref()).ok().and_then(|p| p.parse().ok());

        let parsed_pos = pos.ok_or(ParseFieldError::BStringUtf8Error)?;

        let overlap = next_field(&mut input)?.as_ref().into();

        let optional = T::parse(input);
        Ok(Containment {
            container_name,
            container_orient,
            contained_name,
            contained_orient,
            overlap,
            pos: parsed_pos,
            optional,
        })
    }
}

impl<N: SegmentId, T: OptFields> Path<N, T> {
    fn wrap(self) -> Line<N, T> {
        Line::Path(self)
    }

    fn parse_line<I>(mut input: I) -> GFAFieldResult<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        // Use the SegmentId parser for the path name as well; it's
        // just always BString
        // let path_name = BString::parse_next(&mut input)?;
        let path_name = BString::parse_next_result(&mut input)?;

        // let next = next_field(&mut input)?;
        let segment_names = next_field(&mut input)
            // .as_ref()
            .map(|bs| BString::from(bs.as_ref()))?;
        // input.next().map(|bs| BString::from(bs.as_ref()))?;

        let overlaps = next_field(&mut input)?
            .as_ref()
            .split_str(b",")
            .map(BString::from)
            .collect();
        // let overlaps = input
        //     .next()?
        //     .as_ref()
        //     .split_str(b",")
        //     .map(BString::from)
        //     .collect();

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

        let path_: Path<BString, _> = Path::new(
            "14".into(),
            "11+,12-,13+".into(),
            vec!["4M".into(), "5M".into()],
            (),
        );

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
