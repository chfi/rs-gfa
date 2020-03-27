pub struct Segment {
    pub name: String,
    pub sequence: String,
    pub read_count: Option<i64>,
    pub fragment_count: Option<i64>,
    pub kmer_count: Option<i64>,
    pub uri: Option<String>,
}

pub enum Orientation {
    Forward,
    Backward,
}

pub struct Line {
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

pub struct Path {
    pub path_name: String,
    pub segment_names: Vec<String>,
    pub overlaps: Vec<String>,
}
