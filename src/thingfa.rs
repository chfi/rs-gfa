use std::collections::BTreeMap;
use std::collections::HashMap;

use bstr::ByteSlice;
use bstr::ByteVec;
use bstr::{BStr, BString};

use crate::gfa::Orientation;
use Orientation::*;

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Oriented<T>(T, Orientation);

impl<T> Oriented<T> {
    pub fn forward(t: T) -> Self {
        Oriented(t, Forward)
    }

    pub fn backward(t: T) -> Self {
        Oriented(t, Backward)
    }

    // pub fn in_order(self, other: Self) -> (Self, Self) {
    /*
    ++
    j
    */
    // }

    pub fn content(&self) -> &T {
        &self.0
    }

    pub fn orient(&self) -> Orientation {
        self.1
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Oriented<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        write!(f, "{}", self.1)
    }
}

// use std::str;

// impl<T: str::FromStr> str::FromStr for Oriented<T> {
//     type Err = &'static str;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         if s.strip_suffix("-")
//     }
// }

#[derive(Default, Clone, PartialEq)]
struct NameMap {
    next_id: usize,
    map: HashMap<BString, usize>,
    inv_map: Vec<BString>,
}

impl NameMap {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn increment_index(&mut self) -> usize {
        let old = self.next_id;
        self.next_id += 1;
        old
    }

    pub fn increment_with<F: FnOnce(usize)>(&mut self, f: F) {
        let old = self.next_id;
        self.next_id += 1;
        f(old)
    }

    pub fn index_with<F>(&mut self, s: &BStr, f: F) -> usize
    where
        F: FnOnce(&BStr, usize),
    {
        if let Some(ix) = self.map.get(s) {
            *ix
        } else {
            let ix = self.increment_index();
            self.map.insert(BString::from(s), ix);
            self.inv_map.push(BString::from(s));
            f(s, ix);
            ix
        }
    }

    pub fn index(&mut self, s: &BStr) -> usize {
        if let Some(ix) = self.map.get(s) {
            *ix
        } else {
            let ix = self.increment_index();
            self.map.insert(BString::from(s), ix);
            self.inv_map.push(BString::from(s));
            ix
        }
    }

    pub fn inv_map(self) -> Vec<BString> {
        self.inv_map
    }

    /// Returns the index if it exists
    pub fn get_index(&self, s: &BStr) -> Option<usize> {
        self.map.get(s).cloned()
    }

    pub fn get_name(&self, i: usize) -> Option<&BStr> {
        self.inv_map.get(i).map(|bs| bs.as_bstr())
    }

    /// Returns the new index only if the given name doesn't already
    /// exist
    pub fn get_new_index(&mut self, s: &BStr) -> Option<usize> {
        if let None = self.map.get(s) {
            Some(self.increment_index())
        } else {
            None
        }
    }

    pub fn get_index_unsafe(&self, s: &BStr) -> usize {
        self.map[s]
    }
}

#[derive(Default, Clone, PartialEq)]
pub struct GFA {
    min_id: usize,
    next_id: usize,
    name_map: NameMap,
    segments: Vec<Segment>,
    links: Vec<Link>,
    // containments: Vec<Containment>,
    paths: Vec<Path>,
}

impl GFA {
    pub fn new() -> GFA {
        GFA {
            min_id: usize::min_value(),
            next_id: usize::min_value(),
            ..Default::default()
        }
    }

    /*
    pub fn index(&mut self, s: &str) -> usize {
        if let Some(ix) = self.name_map.get(s) {
            *ix
        } else {
            let ix = self.next_id;
            self.name_map.insert(s.to_string(), ix);
            self.next_id += 1;
            ix
        }
    }

    pub fn get_index(&self, s: &str) -> Option<usize> {
        self.name_map.get(s).cloned()
    }
    */

    pub fn get_segment(&self, s: &BStr) -> Option<&Segment> {
        self.name_map.get_index(s).map(|i| &self.segments[i])
    }

    // If the segment name already exists in the name map, it's likely
    // due to a name segment added by another line type. If that is
    // the case, we just update the corresponding segment with the new
    // sequence. Otherwise, add the segment name to the name map,
    // and
    pub fn insert_segment(&mut self, name: &BStr, seq: &BStr) {
        let ix = self.name_segment(name);
        self.segments[ix].sequence = BString::from(seq);
    }

    // given a segment name, returns the index to the corresponding
    // segment in the segments list. if the segment doesn't exist, the
    // segments list is extended with a placeholder segment
    pub fn name_segment(&mut self, name: &BStr) -> usize {
        if let Some(ix) = self.name_map.get_index(name) {
            ix
        } else {
            let ix = self.name_map.get_new_index(name).unwrap();
            self.segments.push(Segment {
                id: ix,
                sequence: BString::from(vec![]),
            });
            ix
        }
    }

    // insert the given link into the map, adding the from and to
    // segments to the name map if needed
    pub fn insert_link(
        &mut self,
        from: &Oriented<&BStr>,
        to: &Oriented<&BStr>,
    ) {
        let from_ix = self.name_segment(from.0);
        let to_ix = self.name_segment(from.0);

        self.links.push(Link {
            from: Oriented(from_ix, from.1),
            to: Oriented(to_ix, to.1),
        });
    }

    pub fn parse_path(&mut self, line: &BStr) {
        let mut fields = line.split_str("\t");

        let path_name: &BStr = fields.next().map(|n| n.as_bstr()).unwrap();
        let path_name = BString::from(path_name);

        let segs = fields.next().unwrap();

        let segments: Vec<Oriented<usize>> = segs
            .split_str(",")
            .map(|s| {
                let orient = match s.last_byte() {
                    Some(b'+') => Orientation::Forward,
                    Some(b'-') => Orientation::Backward,
                    _ => panic!("Path segment did not include orientation"),
                };
                let seg = &s[0..s.len() - 1];
                let ix = self.name_segment(seg.as_bstr());
                Oriented(ix, orient)
            })
            .collect();

        let path = Path {
            name: path_name,
            segments,
        };

        self.paths.push(path);
    }
    // pub fn insert_path(&mut self,

    // pub fn
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Segment {
    pub id: usize,
    pub sequence: BString,
}

// impl Segment {
//     pub fn parse_line(s: &str) -> Segment {}
// }

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Link {
    pub from: Oriented<usize>,
    pub to: Oriented<usize>,
    // pub overlap: String
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Containment {
    pub container: Oriented<usize>,
    pub contained: Oriented<usize>,
    // pub overlap: String
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Path {
    pub name: BString,
    pub segments: Vec<Oriented<usize>>,
    // pub overlaps: Vec<String>,
}

use std::str;

pub struct Cigar(Vec<u8>);

impl Cigar {
    pub fn parse_byte(x: u8) -> Option<u8> {
        if x.is_ascii_digit() || b"MIDNSHP=".contains(&x) {
            Some(x)
        } else {
            None
        }
    }

    pub fn parse_byte_panic(x: u8) -> u8 {
        if x.is_ascii_digit() || b"MIDNSHP=".contains(&x) {
            x
        } else {
            panic!("Failed to parse CIGAR: {:?}", x);
        }
    }

    pub fn new() -> Self {
        Cigar(Vec::new())
    }

    pub fn from_bytes(bs: Vec<u8>) -> Self {
        if bs.iter().all(|x| b"MIDNSHP=0123456789".contains(x)) {
            Cigar(bs)
        } else {
            panic!("Failed to parse CIGAR: {:?}", bs);
        }
    }
    // pub fn new

    pub fn insert_bytes(&mut self, bs: &[u8]) {
        let mut cig = bs
            .into_iter()
            .map(|b| Cigar::parse_byte_panic(*b))
            .collect();
        self.0.append(&mut cig);
    }
    // pub fn parse_bytes(x: &[u8]) -> Option<Cigar<u8>> {
    // }
    // pub fn parse_pair((x, y): (u8, u8)) -> Option
}

// impl<T: str::FromStr> str::FromStr for Cigar {
//     type Err = &'static str;
// }
