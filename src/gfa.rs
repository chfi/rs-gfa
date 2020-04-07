#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Header {
    pub version: String,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum OptionalFieldValue {
    PrintableChar(char),
    SignedInt(i64),
    Float(f32),
    PrintableString(String),
    JSON(String),
    ByteArray(Vec<u8>),
    IntArray(Vec<i32>),
    FloatArray(Vec<f32>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct OptionalField {
    pub tag: String,
    pub content: OptionalFieldValue,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Segment {
    pub name: String,
    pub sequence: String,
    pub read_count: Option<i64>,
    pub fragment_count: Option<i64>,
    pub kmer_count: Option<i64>,
    pub uri: Option<String>,
}

impl Segment {
    pub fn new(name: &str, sequence: &str) -> Self {
        Segment {
            name: name.to_string(),
            sequence: sequence.to_string(),
            read_count: None,
            fragment_count: None,
            kmer_count: None,
            uri: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Orientation {
    Forward,
    Backward,
}

impl Orientation {
    pub fn as_bool(&self) -> bool {
        match self {
            Self::Forward => true,
            Self::Backward => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Link {
    pub from_segment: String,
    pub from_orient: Orientation,
    pub to_segment: String,
    pub to_orient: Orientation,
    pub overlap: String,
    pub map_quality: Option<i64>,
    pub num_mismatches: Option<i64>,
    pub read_count: Option<i64>,
    pub fragment_count: Option<i64>,
    pub kmer_count: Option<i64>,
    pub edge_id: Option<String>,
}

impl Link {
    pub fn new(
        from_segment: &str,
        from_orient: Orientation,
        to_segment: &str,
        to_orient: Orientation,
        overlap: &str,
    ) -> Link {
        Link {
            from_segment: from_segment.to_string(),
            from_orient: from_orient,
            to_segment: to_segment.to_string(),
            to_orient: to_orient,
            overlap: overlap.to_string(),
            map_quality: None,
            num_mismatches: None,
            read_count: None,
            fragment_count: None,
            kmer_count: None,
            edge_id: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Containment {
    pub container_name: String,
    pub container_orient: Orientation,
    pub contained_name: String,
    pub contained_orient: Orientation,
    pub pos: usize,
    pub overlap: String,
    pub read_coverage: Option<i64>,
    pub num_mismatches: Option<i64>,
    pub edge_id: Option<String>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Path {
    pub path_name: String,
    pub segment_names: Vec<String>,
    pub overlaps: Vec<String>,
}

impl Path {
    pub fn new(path_name: &str, seg_names: Vec<&str>, overlaps: Vec<&str>) -> Path {
        let segment_names = seg_names.iter().map(|s| s.to_string()).collect();
        let overlaps = overlaps.iter().map(|s| s.to_string()).collect();
        Path {
            path_name: path_name.to_string(),
            segment_names,
            overlaps,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Line {
    Header(Header),
    Segment(Segment),
    Link(Link),
    Containment(Containment),
    Path(Path),
    Comment,
}

// struct to hold the results of parsing a file; not actually a graph
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct GFA {
    pub segments: Vec<Segment>,
    pub links: Vec<Link>,
    pub containments: Vec<Containment>,
    pub paths: Vec<Path>,
}

impl GFA {
    pub fn new() -> Self {
        GFA {
            segments: vec![],
            links: vec![],
            containments: vec![],
            paths: vec![],
        }
    }
}
