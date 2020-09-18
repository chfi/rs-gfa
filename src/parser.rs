use bstr::{BStr, BString, ByteSlice};
use lazy_static::lazy_static;
use regex::bytes::Regex;

use crate::gfa::*;
use crate::optfields::*;

type GFALineFilter = Box<dyn Fn(&'_ BStr) -> Option<&'_ BStr>>;

/// Trait for the types that can be parsed and used as segment IDs;
/// will probably only be usize and BString
pub trait SegmentId: Sized {
    fn parse_id<I>(input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>;
}

impl SegmentId for usize {
    fn parse_id<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"(?-u)[0-9]+").unwrap();
        }
        let next = input.next()?;
        RE.find(next.as_ref()).map(|bs| {
            let s = std::str::from_utf8(bs.as_bytes()).unwrap();
            s.parse::<usize>().unwrap()
        })
    }
}

impl SegmentId for BString {
    fn parse_id<I>(mut input: I) -> Option<Self>
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

    pub fn build<N: SegmentId, T: OptFields>(self) -> NewGFAParser<N, T> {
        let filter = self.make_filter();
        NewGFAParser {
            filter,
            _optional_fields: std::marker::PhantomData,
            _segment_names: std::marker::PhantomData,
        }
    }

    pub fn build_usize_id<T: OptFields>(self) -> NewGFAParser<usize, T> {
        self.build()
    }

    pub fn build_bstr_id<T: OptFields>(self) -> NewGFAParser<BString, T> {
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

/// GFAParser encapsulates a parsing configuration, which is currently
/// limited to filtering based on line type, and the choice of
/// optional fields storage (see optfields.rs). Will likely become
/// generic over the `N` type in `GFA<N, T: OptFields>`, but for now
/// the parser only produces GFA lines with BString segment names.
pub struct GFAParser<T: OptFields> {
    filter: GFALineFilter,
    _optional_fields: std::marker::PhantomData<T>,
}

impl<T: OptFields> Default for GFAParser<T> {
    fn default() -> Self {
        Self::with_config(GFAParsingConfig::all())
    }
}

impl<T: OptFields> GFAParser<T> {
    /// Create a new GFAParser that will parse all four GFA line
    /// types, and use the optional fields parser and storage `T`.
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new GFAParser using the provided configuration.
    /// `GFAParsingConfig::all()` and `GFAParsingConfig::none()` can
    /// be used as a basis for more granular choices.
    pub fn with_config(config: GFAParsingConfig) -> Self {
        let filter = config.make_filter();
        GFAParser {
            filter,
            _optional_fields: std::marker::PhantomData,
        }
    }

    /// Filters a line before parsing, only passing through the lines
    /// enabled in the config used to make this parser. NB: this will
    /// probably change in the future; I was playing around with
    /// storing the line filter as a closure for performance, but
    /// still need to profile that aspect
    fn filter_line<'a>(&self, line: &'a BStr) -> Option<&'a BStr> {
        (self.filter)(line)
    }

    /// Consume an iterator of lines of bytes, parsing each as a GFA
    /// line. Returns a GFA object containing all the parsed lines.
    /// Lines that could not be parsed are ignored.
    pub fn parse_all<I>(&self, input: I) -> GFA<BString, T>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let mut gfa = GFA::new();
        for line in input {
            if let Some(line) = self.parse_line(line.as_ref()) {
                gfa.insert_line(line)
            }
        }
        gfa
    }

    /// Parse a single line into a GFA line. The result can be
    /// inserted into a GFA object using the GFA insert_line() method.
    pub fn parse_line(&self, line: &[u8]) -> Option<Line<BString, T>> {
        use Line::*;
        let line: &BStr = line.trim().as_ref();
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

    /// Convenience function for parsing a .gfa file. Opens the file
    /// at the provided path and reads it line-by-line.
    pub fn parse_file<P: AsRef<std::path::Path>>(
        &self,
        path: P,
    ) -> std::io::Result<GFA<BString, T>> {
        use {
            bstr::io::BufReadExt,
            std::{fs::File, io::BufReader},
        };

        let file = File::open(path.as_ref())?;
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

/// Represents the user-facing parser configuration that does not
/// depend on the type of the resulting GFA object; currently limited
/// to filtering which lines to parse and which to ignore.
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
    /// Parse no GFA lines, useful if you only want to parse one line type.
    pub fn none() -> Self {
        GFAParsingConfig {
            segments: false,
            links: false,
            containments: false,
            paths: false,
        }
    }

    /// Parse all GFA lines.
    pub fn all() -> Self {
        GFAParsingConfig {
            segments: true,
            links: true,
            containments: true,
            paths: true,
        }
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

/// Trait for parsing a single line into one of the GFA line types,
/// should only be implemented for the five types in the spec. NB: For
/// now this trait is oblivious to the type of optional fields, but
/// that may change in the future, for example if we want an OptFields
/// type that requires some tags be present.
trait ParseGFA: Sized + Default {
    /// The parsers all work on iterators over fields of byte slices,
    /// assumed to be created by splitting the lines of a GFA file on
    /// tabs.
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

impl<N: SegmentId, T: OptFields> Segment<N, T> {
    fn parse_line_<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let name = N::parse_id(&mut input)?;
        let sequence = parse_sequence(&mut input)?;
        let optional = T::parse(input);
        Some(Segment {
            name,
            sequence,
            optional,
        })
    }
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

impl<N: SegmentId, T: OptFields> Link<N, T> {
    fn parse_line_<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        use Orientation as O;
        let from_segment = N::parse_id(&mut input)?;
        let from_orient = input.next().and_then(O::from_bytes)?;
        let to_segment = N::parse_id(&mut input)?;
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

impl<N: SegmentId, T: OptFields> Containment<N, T> {
    fn parse_line_<I>(mut input: I) -> Option<Self>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        use std::str::from_utf8;
        use Orientation as O;

        let container_name = N::parse_id(&mut input)?;
        let container_orient = input.next().and_then(O::from_bytes)?;
        let contained_name = N::parse_id(&mut input)?;
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

impl<T: OptFields> Path<T> {
    fn parse_line_<I>(mut input: I) -> Option<Self>
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

    #[test]
    fn can_parse_gfa_lines() {
        let parser = GFAParser::new();
        let gfa: GFA<BString, ()> = parser.parse_file("./lil.gfa").unwrap();

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
        .collect();

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
