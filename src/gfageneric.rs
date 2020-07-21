use std::collections::BTreeMap;
use std::collections::HashMap;
use std::hash::Hash;

use bstr::ByteSlice;
use bstr::ByteVec;
use bstr::{BStr, BString};

use crate::gfa::{OptionalField, Orientation};
use Orientation::*;

pub type NoFields = ();
pub type VecFields = Vec<OptionalField>;

trait Indexable: Sized {
    type Name: Ord + Hash;
    fn insert(self, map: &mut NameMap<Self::Name>, name: Self::Name) -> usize {
        if let None = map.get(&name) {
            let ix = map.len();
            map.insert(name, ix);
            ix
        } else {
            panic!("Tried to insert name into map twice");
        }
    }

    fn get(
        &self,
        map: &NameMap<Self::Name>,
        name: &Self::Name,
    ) -> Option<usize> {
        map.get(&name).cloned()
    }
}

pub struct Segment<N, T> {
    name: N,
    sequence: String,
    optional: T,
}

pub type NameMap<N> = HashMap<N, usize>;

pub fn insert_name(map: &mut NameMap<String>, name: String) -> usize {
    if let None = map.get(&name) {
        let ix = map.len();
        map.insert(name, ix);
        ix
    } else {
        panic!("Tried to insert name into map twice");
    }
}

// pub fn insert_name<N: Ord + Hash>(map: &mut NameMap<N>, name: N) -> usize {
// }

impl<N: Ord + Hash, T> Segment<N, T> {
    pub fn to_index(self, map: &mut HashMap<N, usize>) -> Segment<usize, T> {
        if let Some(_) = map.get(&self.name) {
            panic!("Tried to index segment twice");
        } else {
            let ix = map.len();
            let sequence = self.sequence;
            let name = self.name;
            let optional = self.optional;
            map.insert(name, ix);
            Segment {
                name: ix,
                sequence,
                optional,
            }
        }
    }
}

impl<T> Segment<usize, T> {
    // pub fn with_name_map<N: Ord + Hash>(
    //     map: &mut HashMap<N, usize>,
    //     name: N,
    //     seq: String,
    //     opts: T,
    // ) {
    //     if let Some(ix) = map.get(&name) {
    //     } else {
    //         let ix = map.len();
    //         map.insert(name, ix);
    //     }
    // }
}

pub struct Link<N, T> {
    from_seg: N,
    from_orient: Orientation,
    to_seg: N,
    to_orient: Orientation,
    optional: T,
    // overlap: Vec<u8>,
}

pub struct GFA<N, T> {
    segments: Vec<Segment<N, T>>,
    links: Vec<Segment<N, T>>,
}

// pub struct
