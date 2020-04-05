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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Orientation {
    Forward,
    Backward,
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
