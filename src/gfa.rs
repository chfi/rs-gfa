use bstr::{BStr, BString, ByteSlice};
use lazy_static::lazy_static;
use regex::bytes::Regex;
use serde::{Deserialize, Serialize};

use crate::optfields::*;
use crate::parser::ParseFieldError;

/// Trait for the types that can be parsed and used as segment IDs;
/// will probably only be usize and BString.
pub trait SegmentId: Sized + Default {
    const ERROR: ParseFieldError;

    fn parse_id(input: &[u8]) -> Option<Self>;

    fn parse_next<I>(mut input: I) -> Result<Self, ParseFieldError>
    where
        I: Iterator,
        I::Item: AsRef<[u8]>,
    {
        let next = input.next().ok_or(ParseFieldError::MissingFields)?;
        Self::parse_id(next.as_ref()).ok_or(Self::ERROR)
    }
}
pub mod name_conversion;

impl SegmentId for usize {
    const ERROR: ParseFieldError = ParseFieldError::UintIdError;

    fn parse_id(input: &[u8]) -> Option<Self> {
        lazy_static! {
            static ref RE: Regex =
                Regex::new(r"(?-u)[!-)+-<>-~][!-~]*").unwrap();
        }
        RE.find(input).and_then(|bs| {
            let s = std::str::from_utf8(bs.as_bytes()).unwrap();
            s.parse::<usize>().ok()
        })
    }
}

impl SegmentId for BString {
    const ERROR: ParseFieldError = ParseFieldError::Utf8Error;

    fn parse_id(input: &[u8]) -> Option<Self> {
        lazy_static! {
            static ref RE: Regex =
                Regex::new(r"(?-u)[!-)+-<>-~][!-~]*").unwrap();
        }
        RE.find(input).map(|s| BString::from(s.as_bytes()))
    }
}

/// This module defines the various GFA line types, the GFA object,
/// and some utility functions and types.

/// Simple representation of a parsed GFA file, using a Vec<T> to
/// store each separate GFA line type.
#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct GFA<N, T: OptFields> {
    pub header: Header<T>,
    pub segments: Vec<Segment<N, T>>,
    pub links: Vec<Link<N, T>>,
    pub containments: Vec<Containment<N, T>>,
    pub paths: Vec<Path<N, T>>,
}

/// Enum containing the different kinds of GFA lines.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Line<N, T: OptFields> {
    Header(Header<T>),
    Segment(Segment<N, T>),
    Link(Link<N, T>),
    Containment(Containment<N, T>),
    Path(Path<N, T>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum LineRef<'a, N, T: OptFields> {
    Header(&'a Header<T>),
    Segment(&'a Segment<N, T>),
    Link(&'a Link<N, T>),
    Containment(&'a Containment<N, T>),
    Path(&'a Path<N, T>),
}

impl<N, T: OptFields> GFA<N, T> {
    /// Insert a GFA line (wrapped in the Line enum) into an existing
    /// GFA. Simply pushes it into the corresponding Vec in the GFA,
    /// or replaces the header, so there's no deduplication or sorting
    /// taking place.
    pub fn insert_line(&mut self, line: Line<N, T>) {
        use Line::*;
        match line {
            Header(h) => self.header = h,
            Segment(s) => self.segments.push(s),
            Link(s) => self.links.push(s),
            Containment(s) => self.containments.push(s),
            Path(s) => self.paths.push(s),
        }
    }

    /// Consume a GFA object to produce an iterator over all the lines
    /// contained within. The iterator first produces all segments, then
    /// links, then containments, and finally paths.
    pub fn lines_into_iter(self) -> impl Iterator<Item = Line<N, T>> {
        use Line::*;
        let segs = self.segments.into_iter().map(Segment);
        let links = self.links.into_iter().map(Link);
        let conts = self.containments.into_iter().map(Containment);
        let paths = self.paths.into_iter().map(Path);

        segs.chain(links).chain(conts).chain(paths)
    }

    /// Return an iterator over references to the lines in the GFA
    pub fn lines_iter<'a>(&'a self) -> impl Iterator<Item = LineRef<'a, N, T>> {
        use LineRef::*;
        let segs = self.segments.iter().map(Segment);
        let links = self.links.iter().map(Link);
        let conts = self.containments.iter().map(Containment);
        let paths = self.paths.iter().map(Path);

        segs.chain(links).chain(conts).chain(paths)
    }
}

impl<N: SegmentId, T: OptFields> GFA<N, T> {
    pub fn new() -> Self {
        Default::default()
    }
}

/// The header line of a GFA graph
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Header<T: OptFields> {
    pub version: Option<BString>,
    pub optional: T,
}

impl<T: OptFields> Default for Header<T> {
    fn default() -> Self {
        Header {
            version: Some("1.0".into()),
            optional: Default::default(),
        }
    }
}

/// A segment in a GFA graph. Generic over the name type, but
/// currently the parser is only defined for N = BString
#[derive(
    Default, Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize, Hash,
)]
pub struct Segment<N, T: OptFields> {
    pub name: N,
    pub sequence: BString,
    pub optional: T,
}

impl<N, T: OptFields> Segment<N, T> {
    pub(crate) fn nameless_clone<M: Default>(&self) -> Segment<M, T> {
        Segment {
            name: Default::default(),
            sequence: self.sequence.clone(),
            optional: self.optional.clone(),
        }
    }
}

impl<N, T: OptFields> Link<N, T> {
    pub(crate) fn nameless_clone<M: Default>(&self) -> Link<M, T> {
        Link {
            from_segment: Default::default(),
            from_orient: self.from_orient,
            to_segment: Default::default(),
            to_orient: self.to_orient,
            overlap: self.overlap.clone(),
            optional: self.optional.clone(),
        }
    }
}

impl<N, T: OptFields> Containment<N, T> {
    pub(crate) fn nameless_clone<M: Default>(&self) -> Containment<M, T> {
        Containment {
            container_name: Default::default(),
            container_orient: self.container_orient,
            contained_name: Default::default(),
            contained_orient: self.contained_orient,
            pos: self.pos,
            overlap: self.overlap.clone(),
            optional: self.optional.clone(),
        }
    }
}

fn parse_usize(bs: &BString) -> Option<usize> {
    let s = std::str::from_utf8(bs.as_ref()).ok()?;
    s.parse::<usize>().ok()
}

impl<T: OptFields> Segment<BString, T> {
    pub fn new(name: &[u8], sequence: &[u8]) -> Self {
        Segment {
            name: BString::from(name),
            sequence: BString::from(sequence),
            optional: Default::default(),
        }
    }
}

#[derive(
    Default, Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize, Hash,
)]
pub struct Link<N, T: OptFields> {
    pub from_segment: N,
    pub from_orient: Orientation,
    pub to_segment: N,
    pub to_orient: Orientation,
    pub overlap: BString,
    pub optional: T,
}

impl<T: OptFields> Link<BString, T> {
    pub fn new(
        from_segment: &[u8],
        from_orient: Orientation,
        to_segment: &[u8],
        to_orient: Orientation,
        overlap: &[u8],
    ) -> Link<BString, T> {
        Link {
            from_segment: from_segment.into(),
            from_orient,
            to_segment: to_segment.into(),
            to_orient,
            overlap: overlap.into(),
            optional: Default::default(),
        }
    }

    pub fn usize_name(&self) -> Option<Link<usize, T>> {
        let from_segment = parse_usize(&self.from_segment)?;
        let to_segment = parse_usize(&self.to_segment)?;
        Some(Link {
            from_segment,
            from_orient: self.from_orient,
            to_segment,
            to_orient: self.to_orient,
            overlap: self.overlap.clone(),
            optional: self.optional.clone(),
        })
    }
}

#[derive(
    Default, Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize, Hash,
)]
pub struct Containment<N, T: OptFields> {
    pub container_name: N,
    pub container_orient: Orientation,
    pub contained_name: N,
    pub contained_orient: Orientation,
    pub pos: usize,
    pub overlap: BString,
    pub optional: T,
}

impl<T: OptFields> Containment<BString, T> {
    pub fn usize_name(&self) -> Option<Containment<usize, T>> {
        let container_name = parse_usize(&self.container_name)?;
        let contained_name = parse_usize(&self.contained_name)?;
        Some(Containment {
            container_name,
            container_orient: self.container_orient,
            contained_name,
            contained_orient: self.contained_orient,
            pos: self.pos,
            overlap: self.overlap.clone(),
            optional: self.optional.clone(),
        })
    }
}

/// The step list that the path actually consists of is an unparsed
/// BString to keep memory down; use path.iter() to get an iterator
/// over the parsed path segments and orientations.
#[derive(
    Default, Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize, Hash,
)]
pub struct Path<N, T: OptFields> {
    pub path_name: BString,
    pub segment_names: BString,
    pub overlaps: Vec<BString>,
    pub optional: T,
    _segment_names: std::marker::PhantomData<N>,
}

impl<N: SegmentId, T: OptFields> Path<N, T> {
    pub fn new(
        path_name: BString,
        segment_names: BString,
        overlaps: Vec<BString>,
        optional: T,
    ) -> Self {
        Path {
            path_name,
            segment_names,
            overlaps,
            optional,
            _segment_names: std::marker::PhantomData,
        }
    }
}

impl<N: SegmentId, T: OptFields> Path<N, T> {
    /// Parses (and copies!) a segment ID in the path segment list
    pub fn parse_segment_id(input: &[u8]) -> Option<(N, Orientation)> {
        use Orientation::*;
        let last = input.len() - 1;
        let orient = match input[last] {
            b'+' => Forward,
            b'-' => Backward,
            _ => panic!("Path segment did not include orientation"),
        };
        let seg = &input[..last];
        let id = N::parse_id(seg)?;
        Some((id, orient))
    }
}

impl<T: OptFields> Path<BString, T> {
    /// `true` if each of the segment names of the path can be parsed to `usize`
    pub fn usize_segments(&self) -> bool {
        self.iter()
            .all(|(seg, _)| seg.iter().all(|b| b.is_ascii_digit()))
    }

    pub fn segment_id_ref(input: &[u8]) -> (&'_ BStr, Orientation) {
        use Orientation::*;
        let last = input.len() - 1;
        let orient = match input[last] {
            b'+' => Forward,
            b'-' => Backward,
            _ => panic!("Path segment did not include orientation"),
        };
        let seg = &input[..last];
        (seg.as_ref(), orient)
    }
}

impl<T: OptFields> Path<BString, T> {
    /// Produces an iterator over the segments of the given path,
    /// parsing the orientation and producing a slice to each segment
    /// name
    pub fn iter(&self) -> impl Iterator<Item = (&'_ BStr, Orientation)> {
        self.segment_names.split_str(b",").map(Self::segment_id_ref)
    }

    pub fn usize_path(&self) -> Option<Path<usize, T>> {
        if self.usize_segments() {
            Some(Path::new(
                self.path_name.clone(),
                self.segment_names.clone(),
                self.overlaps.clone(),
                self.optional.clone(),
            ))
        } else {
            None
        }
    }
}

impl<T: OptFields> Path<usize, T> {
    /// Produces an iterator over the usize segments of the given
    /// path.
    pub fn iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = (usize, Orientation)> + 'a {
        self.segment_names
            .split_str(b",")
            .filter_map(Self::parse_segment_id)
    }
}

/// Represents segment orientation/strand
#[derive(
    Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize, Hash,
)]
pub enum Orientation {
    Forward,
    Backward,
}

impl Orientation {
    /// Parse an orientation from a single-element, where + is
    /// Forward, - is Backward
    pub fn from_bytes_plus_minus<T: AsRef<[u8]>>(bs: T) -> Option<Self> {
        match bs.as_ref() {
            b"+" => Some(Orientation::Forward),
            b"-" => Some(Orientation::Backward),
            _ => None,
        }
    }

    pub fn parse_error(opt: Option<Self>) -> Result<Self, ParseFieldError> {
        opt.ok_or(ParseFieldError::OrientationError)
    }

    pub fn write_plus_minus(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let sym = match self {
            Self::Forward => '+',
            Self::Backward => '-',
        };
        write!(f, "{}", sym)
    }

    pub fn plus_minus_as_byte(&self) -> u8 {
        match self {
            Self::Forward => b'+',
            Self::Backward => b'-',
        }
    }

    /// Parse an orientation from a single-element bytestring, where >
    /// is Forward, < is Backward
    pub fn from_bytes_gt_ln<T: AsRef<[u8]>>(bs: T) -> Option<Self> {
        match bs.as_ref() {
            b">" => Some(Orientation::Forward),
            b"<" => Some(Orientation::Backward),
            _ => None,
        }
    }

    pub fn write_gt_ln(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let sym = match self {
            Self::Forward => '>',
            Self::Backward => '<',
        };
        write!(f, "{}", sym)
    }
}

/// Default orientation is forward
impl Default for Orientation {
    fn default() -> Orientation {
        Orientation::Forward
    }
}

/// Forward is true, backward is false
impl From<Orientation> for bool {
    fn from(o: Orientation) -> bool {
        match o {
            Orientation::Forward => true,
            Orientation::Backward => false,
        }
    }
}

impl Orientation {
    pub fn is_reverse(&self) -> bool {
        !bool::from(*self)
    }
}

/// The default parser uses the GFA spec with + as Forward, - as Backward
impl std::str::FromStr for Orientation {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Orientation::from_bytes_plus_minus(s.as_bytes())
            .ok_or("Could not parse orientation (was not + or -)")
    }
}

/// Display uses the GFA spec, mapping Forward to "+", Backward to "-"
impl std::fmt::Display for Orientation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_plus_minus(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn path_iter() {
        use Orientation::*;
        let path: Path<BString, _> = Path::new(
            "14".into(),
            "11+,12-,13+".into(),
            vec!["4M".into(), "5M".into()],
            (),
        );
        let mut path_iter = path.iter();
        assert_eq!(Some(("11".into(), Forward)), path_iter.next());
        assert_eq!(Some(("12".into(), Backward)), path_iter.next());
        assert_eq!(Some(("13".into(), Forward)), path_iter.next());
        assert_eq!(None, path_iter.next());
    }
}
