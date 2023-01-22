pub mod name_conversion;
pub mod orientation;
pub mod traits;

pub use self::orientation::*;
pub use self::traits::*;

use crate::{cigar::CIGAR, optfields::*};
use std::fmt::{self, Display, Formatter, Write};

use bstr::{BStr, ByteSlice};
#[cfg(feature = "serde1")]
use serde::{Deserialize, Serialize};

fn write_optional_fields<U: OptFields, T: Write>(opts: &U, stream: &mut T) {
    for field in opts.fields() {
        write!(stream, "\t{}", field).unwrap_or_else(|err| {
            panic!(
                "Error writing optional field '{:?}' to stream, {:?}",
                field, err
            )
        })
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

impl<N: SegmentId, T: OptFields> Display for GFA<N, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n", self.header,)
            .expect("Error writing header to GFA stream.");

        for segment in self.segments.iter() {
            write!(f, "{}\n", segment).expect(&format!(
                "Error writing segment {} to GFA stream",
                segment.name.display()
            ));
        }

        for link in self.links.iter() {
            write!(f, "{}\n", link).expect(&format!(
                "Error writing link from {} to {} to GFA stream",
                link.from_segment.display(),
                link.to_segment.display()
            ));
        }

        for containment in self.containments.iter() {
            write!(f, "{}\n", containment).expect(&format!(
                "Error writing containment {} within {} to GFA stream",
                containment.contained_name.display(),
                containment.container_name.display()
            ));
        }

        for path in self.paths.iter() {
            write!(f, "{}\n", path).expect(&format!(
                "Error writing path {} to GFA stream",
                path.path_name.display()
            ));
        }

        Ok(())
    }
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

macro_rules! some_line_fn {
    ($name:ident, $tgt:ty, $variant:path) => {
        impl<N, T: OptFields> Line<N, T> {
            pub fn $name(self) -> Option<$tgt> {
                if let $variant(x) = self {
                    Some(x)
                } else {
                    None
                }
            }
        }
    };
}

some_line_fn!(some_header, Header<T>, Line::Header);
some_line_fn!(some_segment, Segment<N, T>, Line::Segment);
some_line_fn!(some_link, Link<N, T>, Line::Link);
some_line_fn!(some_containment, Containment<N, T>, Line::Containment);
some_line_fn!(some_path, Path<N, T>, Line::Path);

macro_rules! some_line_ref_fn {
    ($name:ident, $tgt:ty, $variant:path) => {
        impl<'a, N, T: OptFields> LineRef<'a, N, T> {
            pub fn $name(self) -> Option<&'a $tgt> {
                if let $variant(x) = self {
                    Some(x)
                } else {
                    None
                }
            }
        }
    };
}

some_line_ref_fn!(some_header, Header<T>, LineRef::Header);
some_line_ref_fn!(some_segment, Segment<N, T>, LineRef::Segment);
some_line_ref_fn!(some_link, Link<N, T>, LineRef::Link);
some_line_ref_fn!(some_containment, Containment<N, T>, LineRef::Containment);
some_line_ref_fn!(some_path, Path<N, T>, LineRef::Path);

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
    #[inline]
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
    pub fn lines_iter(&'_ self) -> impl Iterator<Item = LineRef<'_, N, T>> {
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
    pub version: Option<Vec<u8>>,
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

impl<T: OptFields> Display for Header<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "H",)?;
        if let Some(v) = &self.version {
            write!(f, "\tVN:Z:{}", v.as_bstr())
                .expect("Error writing version number in header line.")
        }
        write_optional_fields(&self.optional, f);
        Ok(())
    }
}

/// A segment in a GFA graph. Generic over the name type, but
/// currently the parser is only defined for N = Vec<u8>
#[derive(Default, Debug, Clone, PartialEq, PartialOrd, Hash)]
#[cfg_attr(feature = "serde1", derive(Serialize, Deserialize))]
pub struct Segment<N, T: OptFields> {
    pub name: N,
    pub sequence: Vec<u8>,
    pub optional: T,
}

impl<T: OptFields> Segment<Vec<u8>, T> {
    #[inline]
    pub fn new(name: &[u8], sequence: &[u8]) -> Self {
        Segment {
            name: Vec::from(name),
            sequence: Vec::from(sequence),
            optional: Default::default(),
        }
    }
}

impl<N: SegmentId, U: OptFields> Display for Segment<N, U> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "S\t{}\t{}", self.name.display(), self.sequence.as_bstr())?;
        write_optional_fields(&self.optional, f);

        Ok(())
    }
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd, Hash)]
#[cfg_attr(feature = "serde1", derive(Serialize, Deserialize))]
pub struct Link<N, T: OptFields> {
    pub from_segment: N,
    pub from_orient: Orientation,
    pub to_segment: N,
    pub to_orient: Orientation,
    pub overlap: Vec<u8>,
    pub optional: T,
}

impl<T: OptFields> Link<Vec<u8>, T> {
    #[inline]
    pub fn new(
        from_segment: &[u8],
        from_orient: Orientation,
        to_segment: &[u8],
        to_orient: Orientation,
        overlap: &[u8],
    ) -> Link<Vec<u8>, T> {
        Link {
            from_segment: from_segment.into(),
            from_orient,
            to_segment: to_segment.into(),
            to_orient,
            overlap: overlap.into(),
            optional: Default::default(),
        }
    }
}

impl<N: SegmentId, U: OptFields> Display for Link<N, U> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "L\t{}\t{}\t{}\t{}\t{}",
            self.from_segment.display(),
            self.from_orient,
            self.to_segment.display(),
            self.to_orient,
            self.overlap.as_bstr(),
        )
        .expect("Error writing link to stream");
        write_optional_fields(&self.optional, f);
        Ok(())
    }
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd, Hash)]
#[cfg_attr(feature = "serde1", derive(Serialize, Deserialize))]
pub struct Containment<N, T: OptFields> {
    pub container_name: N,
    pub container_orient: Orientation,
    pub contained_name: N,
    pub contained_orient: Orientation,
    pub pos: usize,
    pub overlap: Vec<u8>,
    pub optional: T,
}

impl<N: SegmentId, T: OptFields> Display for Containment<N, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "C\t{}\t{}\t{}\t{}\t{}\t{}",
            self.container_name.display(),
            self.container_orient,
            self.contained_name.display(),
            self.contained_orient,
            self.pos,
            self.overlap.as_bstr()
        )
        .expect("Error writing containment to stream");
        write_optional_fields(&self.optional, f);
        Ok(())
    }
}

/// The step list that the path actually consists of is an unparsed
/// Vec<u8> to keep memory down; use path.iter() to get an iterator
/// over the parsed path segments and orientations.
#[derive(Default, Debug, Clone, PartialEq, PartialOrd, Hash)]
#[cfg_attr(feature = "serde1", derive(Serialize, Deserialize))]
pub struct Path<N, T: OptFields> {
    pub path_name: Vec<u8>,
    pub segment_names: Vec<u8>,
    pub overlaps: Vec<Option<CIGAR>>,
    pub optional: T,
    _segment_names: std::marker::PhantomData<N>,
}

impl<N: SegmentId, T: OptFields> Path<N, T> {
    #[inline]
    pub fn new(
        path_name: Vec<u8>,
        segment_names: Vec<u8>,
        overlaps: Vec<Option<CIGAR>>,
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

impl<N, T: OptFields> Display for Path<N, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "P\t{}\t", self.path_name.as_bstr())
            .expect("Error writing path to stream");

        write!(f, "{}\t", self.segment_names.as_bstr()).unwrap();

        self.overlaps.iter().enumerate().for_each(|(i, o)| {
            if i != 0 {
                write!(f, ",").unwrap();
            }
            match o {
                None => write!(f, "*").unwrap(),
                // Some(o) => write!(f, "{}", o.to_str().unwrap()).unwrap(),
                Some(o) => write!(f, "{}", o).unwrap(),
            }
        });

        write_optional_fields(&self.optional, f);
        Ok(())
    }
}

impl<N: SegmentId, T: OptFields> Path<N, T> {
    /// Parses (and copies!) a segment ID in the path segment list
    #[inline]
    fn parse_segment_id(input: &[u8]) -> Option<(N, Orientation)> {
        use Orientation::*;
        let orient = match input.last()? {
            b'+' => Forward,
            b'-' => Backward,
            _ => panic!("Path segment did not include orientation"),
        };
        let seg = &input[..input.len() - 1];
        let id = N::parse_id(seg)?;
        Some((id, orient))
    }
}

impl<T: OptFields> Path<Vec<u8>, T> {
    /// Produces an iterator over the segments of the given path,
    /// parsing the orientation and producing a slice to each segment
    /// name
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&'_ BStr, Orientation)> {
        self.segment_names
            .split_str(b",")
            .filter_map(Self::segment_id_ref)
    }

    fn segment_id_ref(input: &[u8]) -> Option<(&'_ BStr, Orientation)> {
        use Orientation::*;
        let orient = match input.last()? {
            b'+' => Forward,
            b'-' => Backward,
            _ => panic!("Path segment did not include orientation"),
        };
        let seg = &input[..input.len() - 1];
        Some((seg.as_ref(), orient))
    }
}

impl<T: OptFields> Path<usize, T> {
    /// Produces an iterator over the usize segments of the given
    /// path.
    #[inline]
    pub fn iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = (usize, Orientation)> + 'a {
        self.segment_names
            .split_str(b",")
            .filter_map(Self::parse_segment_id)
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Read as IoRead;
    use std::io::Write as IoWrite;
    use tempfile::NamedTempFile;

    #[test]
    fn path_iter() {
        use Orientation::*;

        let cigars = vec![b"4M", b"5M"]
            .iter()
            .map(|bs| CIGAR::from_bytestring(&bs[..]))
            .collect();

        let path: Path<Vec<u8>, _> =
            Path::new("14".into(), "11+,12-,13+".into(), cigars, ());

        let mut path_iter = path.iter();
        assert_eq!(Some(("11".into(), Forward)), path_iter.next());
        assert_eq!(Some(("12".into(), Backward)), path_iter.next());
        assert_eq!(Some(("13".into(), Forward)), path_iter.next());
        assert_eq!(None, path_iter.next());
    }

    #[test]
    fn gfa_line_ref_iter() {
        let parser: crate::parser::GFAParser<usize, ()> =
            crate::parser::GFAParser::new();
        let gfa = parser.parse_file(&"./test/gfas/lil.gfa").unwrap();
        let gfa_lineref = gfa.lines_iter();

        for line in gfa_lineref {
            let seg = line.some_segment();
            println!("{:?}", seg);
        }
    }

    #[test]
    fn write_segment_to_string_buffer() {
        use OptFieldVal::*;
        let mut segment: Segment<Vec<u8>, OptionalFields> =
            Segment::new(b"seg1", b"GCCCTA");
        let opt_ij = OptField::new(b"IJ", A(b'x'));
        let opt_ab = OptField::new(b"AB", BInt(vec![1, 2, 3, 52124]));
        let opt_ur =
            OptField::new(b"UR", Z(Vec::<u8>::from("http://test.com/")));
        let opt_rc = OptField::new(b"RC", Int(123));
        segment.optional = vec![opt_rc, opt_ur, opt_ij, opt_ab];
        let expected = "S\tseg1\tGCCCTA\tRC:i:123\tUR:Z:http://test.com/\tIJ:A:x\tAB:B:I1,2,3,52124";
        let mut string = String::new();
        write!(&mut string, "{}", segment)
            .expect("Error writing to string buffer");
        assert_eq!(string, expected);
    }

    #[test]
    fn write_link_to_string_buffer() {
        let link: Link<Vec<u8>, ()> = Link::new(
            b"13",
            Orientation::Forward,
            b"552",
            Orientation::Backward,
            b"0M",
        );
        let mut string = String::new();
        write!(&mut string, "{}", link)
            .expect("Error writing to string buffer");
        assert_eq!(string, "L\t13\t+\t552\t-\t0M");
    }

    #[test]
    fn write_path_to_string_buffer() {
        use crate::cigar::CIGAR;

        let cigars = vec![b"8M", b"1M", b"3M"]
            .iter()
            .map(|bs| CIGAR::from_bytestring(&bs[..]))
            .collect();

        let path: Path<Vec<u8>, _> =
            Path::new("path1".into(), "13+,51-,241+".into(), cigars, ());

        let mut string = String::new();
        write!(&mut string, "{}", path)
            .expect("Error writing to string buffer");
        assert_eq!(string, "P\tpath1\t13+,51-,241+\t8M,1M,3M");
    }

    #[test]
    fn write_gfa_to_string_buffer() {
        use std::io::Read;
        use std::path::PathBuf;

        let parser = crate::parser::GFAParser::new();
        let in_gfa: GFA<Vec<u8>, ()> =
            parser.parse_file(&"./test/gfas/lil.gfa").unwrap();

        let mut file =
            std::fs::File::open(&PathBuf::from("./test/gfas/lil.gfa")).unwrap();
        let mut file_string = String::new();
        file.read_to_string(&mut file_string).unwrap();

        let mut string = String::new();
        write!(&mut string, "{}", in_gfa)
            .expect("Error writing to string buffer");

        assert_eq!(string, file_string);
    }

    #[test]
    fn write_segment_to_file_buffer() {
        use OptFieldVal::*;
        let mut segment: Segment<Vec<u8>, OptionalFields> =
            Segment::new(b"seg1", b"GCCCTA");
        let opt_ij = OptField::new(b"IJ", A(b'x'));
        let opt_ab = OptField::new(b"AB", BInt(vec![1, 2, 3, 52124]));
        let opt_ur =
            OptField::new(b"UR", Z(Vec::<u8>::from("http://test.com/")));
        let opt_rc = OptField::new(b"RC", Int(123));
        segment.optional = vec![opt_rc, opt_ur, opt_ij, opt_ab];
        let expected = "S\tseg1\tGCCCTA\tRC:i:123\tUR:Z:http://test.com/\tIJ:A:x\tAB:B:I1,2,3,52124";

        let mut tempfile =
            NamedTempFile::new().expect("Error creating temp file");
        tempfile
            .write_all(format!("{}", segment).as_bytes())
            .expect("Error writing to file buffer.");

        let mut tempfile_reader =
            tempfile.reopen().expect("error re-opening temp file.");

        let mut string = String::new();
        tempfile_reader
            .read_to_string(&mut string)
            .expect("Error parsing file");
        assert_eq!(string, expected);
    }

    #[test]
    fn write_link_to_file_buffer() {
        let link: Link<Vec<u8>, ()> = Link::new(
            b"13",
            Orientation::Forward,
            b"552",
            Orientation::Backward,
            b"0M",
        );
        let mut tempfile =
            NamedTempFile::new().expect("Error creating temp file");
        tempfile
            .write_all(format!("{}", link).as_bytes())
            .expect("Error writing to file buffer");

        let mut tempfile_reader =
            tempfile.reopen().expect("error re-opening temp file.");

        let mut string = String::new();
        tempfile_reader
            .read_to_string(&mut string)
            .expect("Error parsing file");
        assert_eq!(string, "L\t13\t+\t552\t-\t0M");
    }

    #[test]
    fn write_path_to_file_buffer() {
        use crate::cigar::CIGAR;

        let cigars = vec![b"8M", b"1M", b"3M"]
            .iter()
            .map(|bs| CIGAR::from_bytestring(&bs[..]))
            .collect();

        let path: Path<Vec<u8>, _> =
            Path::new("path1".into(), "13+,51-,241+".into(), cigars, ());

        let mut tempfile =
            NamedTempFile::new().expect("Error creating temp file");

        tempfile
            .write_all(format!("{}", path).as_bytes())
            .expect("Error writing to file buffer");

        let mut tempfile_reader =
            tempfile.reopen().expect("error re-opening temp file.");

        let mut string = String::new();
        tempfile_reader
            .read_to_string(&mut string)
            .expect("Error parsing file");

        assert_eq!(string, "P\tpath1\t13+,51-,241+\t8M,1M,3M");
    }

    #[test]
    fn write_gfa_to_file_buffer() {
        use std::path::PathBuf;

        let parser = crate::parser::GFAParser::new();
        let in_gfa: GFA<Vec<u8>, ()> =
            parser.parse_file(&"./test/gfas/lil.gfa").unwrap();

        let mut file =
            std::fs::File::open(&PathBuf::from("./test/gfas/lil.gfa")).unwrap();
        let mut file_string = String::new();
        file.read_to_string(&mut file_string).unwrap();

        let mut tempfile =
            NamedTempFile::new().expect("Error creating temp file");
        write!(&mut tempfile, "{}", in_gfa)
            .expect("Error writing to file buffer");

        let mut tempfile_reader =
            tempfile.reopen().expect("error re-opening temp file.");

        let mut string = String::new();
        tempfile_reader
            .read_to_string(&mut string)
            .expect("Error parsing file");

        assert_eq!(string, file_string);
    }
}
