use bstr::{BStr, BString, ByteSlice, ByteVec};
use std::collections::HashMap;

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct Header<T> {
    pub version: Option<BString>,
    pub optional: T,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum OptionalFieldValue {
    PrintableChar(u8),
    SignedInt(i64),
    Float(f32),
    PrintableString(BString),
    JSON(BString),
    ByteArray(Vec<u32>),
    IntArray(Vec<i64>),
    FloatArray(Vec<f32>),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct OptTag([u8; 2]);

impl OptTag {
    pub fn from_bytes(input: &[u8]) -> Option<Self> {
        if input.len() > 2 {
            panic!("tried to parse optional tag with more than two chars");
        } else if input.len() > 1 {
            if input[0..=1].iter().all(|x| x.is_ascii_alphabetic()) {
                Some(OptTag([input[0], input[1]]))
            } else {
                None
            }
        } else {
            None
        }
    }
    pub fn from_str(input: &str) -> Option<Self> {
        if input.len() > 1 {
            let mut fs = input.bytes();
            let a = fs.next().filter(|x| x.is_ascii_alphabetic())?;
            let b = fs.next().filter(|x| x.is_ascii_alphabetic())?;

            Some(OptTag([a, b]))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct OptionalField {
    pub tag: OptTag,
    pub content: OptionalFieldValue,
}

impl OptionalField {
    pub fn new(tag: &[u8], content: OptionalFieldValue) -> Self {
        OptionalField {
            tag: OptTag([tag[0], tag[1]]),
            content,
        }
    }
}

impl std::fmt::Display for OptTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}{:x}", self.0[0], self.0[1])
    }
}

pub type OptionalFields = Vec<OptionalField>;

impl std::fmt::Display for OptionalField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OptionalFieldValue::*;
        write!(f, "{}:", self.tag)?;
        match &self.content {
            PrintableChar(c) => write!(f, "A:{}", c),
            SignedInt(i) => write!(f, "i:{}", i),
            Float(d) => write!(f, "f:{}", d),
            PrintableString(s) => write!(f, "Z:{}", s),
            JSON(s) => write!(f, "J:{}", s),
            ByteArray(a) => {
                let mut array_str = String::new();
                for x in a {
                    array_str.push(std::char::from_digit(*x, 16).unwrap())
                }
                write!(f, "H:{}", array_str)
            }
            IntArray(a) => {
                let mut array_str = String::new();
                for (i, x) in a.into_iter().enumerate() {
                    if i > 0 {
                        array_str.push_str(",");
                    }
                    array_str.push_str(&x.to_string());
                }
                write!(f, "B:I{}", array_str)
            }
            FloatArray(a) => {
                let mut array_str = String::new();
                for (i, x) in a.into_iter().enumerate() {
                    if i > 0 {
                        array_str.push_str(",");
                    }
                    array_str.push_str(&x.to_string());
                }
                write!(f, "B:f{}", array_str)
            }
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct Segment<T> {
    pub name: BString,
    pub sequence: BString,
    pub optional: T,
}

impl<T: Default> Segment<T> {
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

impl Orientation {
    pub fn is_reverse(&self) -> bool {
        match self {
            Self::Forward => false,
            Self::Backward => true,
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
pub struct Link<T> {
    pub from_segment: BString,
    pub from_orient: Orientation,
    pub to_segment: BString,
    pub to_orient: Orientation,
    pub overlap: Vec<u8>,
    pub optional: T,
}

impl<T: Default> Link<T> {
    pub fn new(
        from_segment: &[u8],
        from_orient: Orientation,
        to_segment: &[u8],
        to_orient: Orientation,
        overlap: &[u8],
    ) -> Link<T> {
        Link {
            from_segment: from_segment.into(),
            from_orient,
            to_segment: to_segment.into(),
            to_orient,
            overlap: Vec::from_slice(overlap),
            optional: Default::default(),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct Containment<T> {
    pub container_name: BString,
    pub container_orient: Orientation,
    pub contained_name: BString,
    pub contained_orient: Orientation,
    pub pos: usize,
    pub overlap: Vec<u8>,
    pub optional: T,
}

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct Path<T> {
    pub path_name: BString,
    pub segment_names: BString,
    pub overlaps: Vec<Vec<u8>>,
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

impl<T> Path<T> {
    /// A parsing iterator over the path's segments
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a BStr, Orientation)> {
        self.segment_names.split_str(b",").map(parse_path_segment)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Line<T> {
    Header(Header<T>),
    Segment(Segment<T>),
    Link(Link<T>),
    Containment(Containment<T>),
    Path(Path<T>),
    Comment,
}

// struct to hold the results of parsing a file; not actually a graph
#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub struct GFA<T> {
    pub version: Option<String>,
    pub segments: Vec<Segment<T>>,
    pub links: Vec<Link<T>>,
    pub containments: Vec<Containment<T>>,
    pub paths: Vec<Path<T>>,
}

impl<T: Default> GFA<T> {
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
