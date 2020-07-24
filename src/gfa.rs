use bstr::{BStr, BString, ByteSlice, ByteVec};

use crate::optfields::*;

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct Header<T: OptFields> {
    pub version: Option<BString>,
    pub optional: T,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct Segment<N, T: OptFields> {
    pub name: N,
    pub sequence: BString,
    pub optional: T,
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

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Orientation {
    Forward,
    Backward,
}

// It makes sense for forward to be the default
impl std::default::Default for Orientation {
    fn default() -> Orientation {
        Orientation::Forward
    }
}

impl Orientation {
    pub fn is_reverse(&self) -> bool {
        match self {
            Self::Forward => false,
            Self::Backward => true,
        }
    }

    pub fn from_bytes<T: AsRef<[u8]>>(bs: T) -> Option<Self> {
        match bs.as_ref() {
            b"+" => Some(Orientation::Forward),
            b"-" => Some(Orientation::Backward),
            _ => None,
        }
    }

    pub fn from_u8(u: u8) -> Option<Self> {
        match u {
            b'+' => Some(Self::Forward),
            b'-' => Some(Self::Backward),
            _ => None,
        }
    }
}

impl std::str::FromStr for Orientation {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Self::Forward),
            "-" => Ok(Self::Backward),
            _ => Err("Could not parse orientation (was not + or -)"),
        }
    }
}

impl std::fmt::Display for Orientation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sym = match self {
            Self::Forward => '+',
            Self::Backward => '-',
        };
        write!(f, "{}", sym)
    }
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
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
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct Containment<N, T: OptFields> {
    pub container_name: N,
    pub container_orient: Orientation,
    pub contained_name: N,
    pub contained_orient: Orientation,
    pub pos: usize,
    pub overlap: BString,
    pub optional: T,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct Path<T: OptFields> {
    pub path_name: BString,
    pub segment_names: BString,
    pub overlaps: Vec<BString>,
    pub optional: T,
}

fn parse_path_segment<'a>(input: &'a [u8]) -> (&'a BStr, Orientation) {
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

impl<T: OptFields> Path<T> {
    /// A parsing iterator over the path's segments
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a BStr, Orientation)> {
        self.segment_names.split_str(b",").map(parse_path_segment)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Line<N, T: OptFields> {
    Header(Header<T>),
    Segment(Segment<N, T>),
    Link(Link<N, T>),
    Containment(Containment<N, T>),
    Path(Path<T>),
    Comment,
}

pub fn gfa_into_iter<N, T: OptFields>(
    gfa: GFA<N, T>,
) -> impl Iterator<Item = Line<N, T>> {
    use Line::*;
    let segs = gfa.segments.into_iter().map(Segment);
    let links = gfa.links.into_iter().map(Link);
    let conts = gfa.containments.into_iter().map(Containment);
    let paths = gfa.paths.into_iter().map(Path);

    segs.chain(links).chain(conts).chain(paths)
}

/// Simple representation of a parsed GFA file
#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct GFA<N, T: OptFields> {
    pub version: Option<String>,
    pub segments: Vec<Segment<N, T>>,
    pub links: Vec<Link<N, T>>,
    pub containments: Vec<Containment<N, T>>,
    pub paths: Vec<Path<T>>,
}

impl<N: Default, T: OptFields> GFA<N, T> {
    pub fn new() -> Self {
        Default::default()
    }
}

/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_path() {
        let name = "path1";
        let seg_names = vec!["1+", "2-", "13-", "60+"];
        let overlaps: Vec<_> = vec!["8M", "10M", "0M", "2M"]
            .into_iter()
            .map(|s| s.bytes().collect())
            .collect();

        let path_expected = Path {
            path_name: name.to_string(),
            segment_names: vec![
                (b"1".to_string(), Orientation::Forward),
                (b"2".to_string(), Orientation::Backward),
                (b"13".to_string(), Orientation::Backward),
                (b"60".to_string(), Orientation::Forward),
            ],
            overlaps: overlaps.clone(),
            optional: (),
        };

        let path = Path::new(name, seg_names, overlaps);

        assert_eq!(path, path_expected);
    }
}

*/
