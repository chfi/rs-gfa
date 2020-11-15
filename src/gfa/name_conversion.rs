use crate::{
    gfa::{Containment, Link, Path, Segment, GFA},
    optfields::*,
};

use bstr::{ByteSlice, ByteVec};

use fnv::FnvHashMap;

use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

#[cfg(feature = "serde1")]
use serde::{Deserialize, Serialize};

fn hash_gfa<T: OptFields>(gfa: &GFA<Vec<u8>, T>) -> u64 {
    let mut hasher = DefaultHasher::new();

    for seg in gfa.segments.iter() {
        seg.name.hash(&mut hasher);
        seg.sequence.hash(&mut hasher);
    }
    for link in gfa.links.iter() {
        link.from_segment.hash(&mut hasher);
        link.from_orient.hash(&mut hasher);
        link.to_segment.hash(&mut hasher);
        link.to_orient.hash(&mut hasher);
        link.overlap.hash(&mut hasher);
    }
    for cont in gfa.containments.iter() {
        cont.container_name.hash(&mut hasher);
        cont.container_orient.hash(&mut hasher);
        cont.contained_name.hash(&mut hasher);
        cont.contained_orient.hash(&mut hasher);
        cont.pos.hash(&mut hasher);
        cont.overlap.hash(&mut hasher);
    }

    for path in gfa.paths.iter() {
        path.path_name.hash(&mut hasher);
        path.segment_names.hash(&mut hasher);
    }

    hasher.finish()
}

/// This is a helper struct for handling serialization/deserialization
/// of NameMaps to text-based formats such as ASCII
#[cfg(feature = "serde1")]
#[derive(Serialize, Deserialize)]
pub struct NameMapString {
    pub(crate) name_map: FnvHashMap<String, usize>,
    pub(crate) inverse_map: Vec<String>,
    pub(crate) hash: u64,
}

#[cfg(feature = "serde1")]
impl NameMapString {
    fn from_name_map(map: &NameMap) -> Self {
        let name_map: FnvHashMap<String, usize> = map
            .name_map
            .iter()
            .map(|(k, v)| (k.to_str().unwrap().into(), *v))
            .collect();

        let inverse_map: Vec<String> = map
            .inverse_map
            .iter()
            .map(|k| k.to_str().unwrap().into())
            .collect();

        NameMapString {
            name_map,
            inverse_map,
            hash: map.hash,
        }
    }

    fn into_name_map(self) -> NameMap {
        let name_map: FnvHashMap<Vec<u8>, usize> = self
            .name_map
            .iter()
            .map(|(k, v)| {
                let k: Vec<u8> = Vec::from(k.as_bytes());
                (k, *v)
            })
            .collect();

        let inverse_map: Vec<Vec<u8>> = self
            .inverse_map
            .iter()
            .map(|k| k.as_bytes().into())
            .collect();

        NameMap {
            name_map,
            inverse_map,
            hash: self.hash,
        }
    }
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serde1", derive(Serialize, Deserialize))]
pub struct NameMap {
    pub(crate) name_map: FnvHashMap<Vec<u8>, usize>,
    pub(crate) inverse_map: Vec<Vec<u8>>,
    /// The hash is calculated on the GFA<Vec<u8>, _> value
    pub(crate) hash: u64,
}

impl NameMap {
    /// Save the NameMap to a JSON file.
    #[cfg(feature = "serde1")]
    pub fn save_json<P: AsRef<std::path::Path>>(
        &self,
        path: P,
    ) -> std::io::Result<()> {
        use std::{fs::File, io::BufWriter};
        let file = File::create(path.as_ref())?;
        let writer = BufWriter::new(file);
        let name_map = NameMapString::from_name_map(self);
        serde_json::to_writer(writer, &name_map)?;
        Ok(())
    }

    /// Load a NameMap from a JSON file.
    #[cfg(feature = "serde1")]
    pub fn load_json<P: AsRef<std::path::Path>>(
        path: P,
    ) -> std::io::Result<Self> {
        use std::{fs::File, io::BufReader};
        let file = File::open(path.as_ref())?;
        let reader = BufReader::new(file);
        let name_map: NameMapString = serde_json::from_reader(reader)?;
        Ok(name_map.into_name_map())
    }

    pub fn map_name<N: AsRef<[u8]>>(&self, name: N) -> Option<usize> {
        self.name_map.get(name.as_ref()).copied()
    }

    pub fn inverse_map_name(&self, id: usize) -> Option<&'_ [u8]> {
        self.inverse_map.get(id).map(|bs| bs.as_ref())
    }

    fn map_path_segments<T: OptFields>(
        &self,
        path: &Path<Vec<u8>, T>,
    ) -> Option<Path<usize, T>> {
        let mut misses = 0;
        let new_segs: Vec<u8> = path
            .iter()
            .filter_map(|(seg, o)| {
                let n = self.map_name(seg).map(|s| (s, o));
                if n.is_none() {
                    misses += 1;
                }
                n
            })
            .enumerate()
            .flat_map(|(i, (seg, o))| {
                let s = if i == 0 {
                    format!("{}{}", seg, o)
                } else {
                    format!(",{}{}", seg, o)
                };
                Vec::from(s.as_bytes())
            })
            .collect();

        if misses > 0 {
            return None;
        }
        let new_path = Path::new(
            path.path_name.clone(),
            new_segs,
            path.overlaps.clone(),
            path.optional.clone(),
        );
        Some(new_path)
    }

    fn inverse_map_path_segments<T: OptFields>(
        &self,
        path: &Path<usize, T>,
    ) -> Option<Path<Vec<u8>, T>> {
        let mut misses = 0;
        let new_segs: Vec<u8> = path
            .iter()
            .filter_map(|(seg, o)| {
                let n = self.inverse_map.get(seg).map(|s| (s, o));
                if n.is_none() {
                    misses += 1;
                }
                n
            })
            .enumerate()
            .flat_map(|(i, (seg, o))| {
                let s = if i == 0 {
                    format!("{}{}", seg.as_bstr(), o)
                } else {
                    format!(",{}{}", seg.as_bstr(), o)
                };
                Vec::from(s.as_bytes())
            })
            .collect();

        if misses > 0 {
            return None;
        }

        let new_path = Path::new(
            path.path_name.clone(),
            new_segs,
            path.overlaps.clone(),
            path.optional.clone(),
        );
        Some(new_path)
    }

    pub fn gfa_bytestring_to_usize<T: OptFields>(
        &self,
        gfa: &GFA<Vec<u8>, T>,
        check_hash: bool,
    ) -> Option<GFA<usize, T>> {
        #[allow(clippy::collapsible_if)]
        if check_hash {
            if hash_gfa(gfa) != self.hash {
                return None;
            }
        }

        let mut segments = Vec::with_capacity(gfa.segments.len());
        let mut links = Vec::with_capacity(gfa.links.len());
        let mut containments = Vec::with_capacity(gfa.containments.len());
        let mut paths = Vec::with_capacity(gfa.paths.len());

        for seg in gfa.segments.iter() {
            let name = self.map_name(&seg.name)?;
            let mut new_seg: Segment<usize, T> = seg.nameless_clone();
            new_seg.name = name;
            segments.push(new_seg);
        }

        for link in gfa.links.iter() {
            let from_name = self.map_name(&link.from_segment)?;
            let to_name = self.map_name(&link.to_segment)?;
            let mut new_link: Link<usize, T> = link.nameless_clone();
            new_link.from_segment = from_name;
            new_link.to_segment = to_name;
            links.push(new_link);
        }

        for cont in gfa.containments.iter() {
            let container_name = self.map_name(&cont.container_name)?;
            let contained_name = self.map_name(&cont.contained_name)?;
            let mut new_cont: Containment<usize, T> = cont.nameless_clone();
            new_cont.container_name = container_name;
            new_cont.contained_name = contained_name;
            containments.push(new_cont);
        }

        for path in gfa.paths.iter() {
            let new_path = self.map_path_segments(path)?;
            paths.push(new_path);
        }

        Some(GFA {
            header: gfa.header.clone(),
            segments,
            links,
            containments,
            paths,
        })
    }

    pub fn gfa_usize_to_bytestring<T: OptFields>(
        &self,
        gfa: &GFA<usize, T>,
    ) -> Option<GFA<Vec<u8>, T>> {
        let mut segments = Vec::with_capacity(gfa.segments.len());
        let mut links = Vec::with_capacity(gfa.links.len());
        let mut containments = Vec::with_capacity(gfa.containments.len());
        let mut paths = Vec::with_capacity(gfa.paths.len());

        for seg in gfa.segments.iter() {
            let name = self.inverse_map_name(seg.name)?;
            let mut new_seg: Segment<Vec<u8>, T> = seg.nameless_clone();
            // new_seg.name = name.into();
            new_seg.name = Vec::from_slice(name);
            segments.push(new_seg);
        }

        for link in gfa.links.iter() {
            let from_name = self.inverse_map_name(link.from_segment)?;
            let to_name = self.inverse_map_name(link.to_segment)?;
            let mut new_link: Link<Vec<u8>, T> = link.nameless_clone();
            new_link.from_segment = Vec::from_slice(from_name);
            new_link.to_segment = Vec::from_slice(to_name);
            links.push(new_link);
        }

        for cont in gfa.containments.iter() {
            let container_name = self.inverse_map_name(cont.container_name)?;
            let contained_name = self.inverse_map_name(cont.contained_name)?;
            let mut new_cont: Containment<Vec<u8>, T> = cont.nameless_clone();
            new_cont.container_name = Vec::from_slice(container_name);
            new_cont.contained_name = Vec::from_slice(contained_name);
            containments.push(new_cont);
        }

        for path in gfa.paths.iter() {
            let new_path = self.inverse_map_path_segments(path)?;
            paths.push(new_path);
        }

        Some(GFA {
            header: gfa.header.clone(),
            segments,
            links,
            containments,
            paths,
        })
    }

    pub fn build_from_gfa<T: OptFields>(gfa: &GFA<Vec<u8>, T>) -> Self {
        let mut name_map = FnvHashMap::default();
        let mut inverse_map = Vec::with_capacity(gfa.segments.len());

        let mut get_ix = |name: &[u8]| {
            let name: Vec<u8> = Vec::from_slice(name);
            if let Some(ix) = name_map.get(&name) {
                *ix
            } else {
                let ix = name_map.len();
                name_map.insert(name.clone(), ix);
                inverse_map.push(name);
                ix
            }
        };

        let hash = hash_gfa(gfa);

        for seg in gfa.segments.iter() {
            get_ix(seg.name.as_ref());
        }
        for link in gfa.links.iter() {
            get_ix(link.from_segment.as_ref());
            get_ix(link.from_segment.as_ref());
        }
        for cont in gfa.containments.iter() {
            get_ix(cont.container_name.as_ref());
            get_ix(cont.contained_name.as_ref());
        }

        NameMap {
            name_map,
            inverse_map,
            hash,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::GFAParser;

    fn diatom_name_map_path() -> &'static str {
        "./test/gfas/diatom_map.json"
    }

    fn lil_name_map_path() -> &'static str {
        "./test/gfas/lil_map.json"
    }

    fn load_diatom_gfa() -> GFA<Vec<u8>, OptionalFields> {
        let parser = GFAParser::new();
        let gfa: GFA<Vec<u8>, OptionalFields> =
            parser.parse_file(&"./test/gfas/diatom.gfa").unwrap();
        gfa
    }

    fn load_lil_gfa() -> GFA<Vec<u8>, OptionalFields> {
        let parser = GFAParser::new();
        let gfa: GFA<Vec<u8>, OptionalFields> =
            parser.parse_file(&"./test/gfas/lil.gfa").unwrap();
        gfa
    }

    fn test_isomorphism(original_gfa: &GFA<Vec<u8>, OptionalFields>) {
        let name_map = NameMap::build_from_gfa(original_gfa);

        let usize_gfa = name_map
            .gfa_bytestring_to_usize(&original_gfa, false)
            .unwrap();

        assert_eq!(original_gfa.segments.len(), usize_gfa.segments.len());
        assert_eq!(original_gfa.links.len(), usize_gfa.links.len());
        assert_eq!(
            original_gfa.containments.len(),
            usize_gfa.containments.len()
        );
        assert_eq!(original_gfa.paths.len(), usize_gfa.paths.len());

        let inverted_gfa =
            name_map.gfa_usize_to_bytestring(&usize_gfa).unwrap();

        assert_eq!(original_gfa, &inverted_gfa);
    }

    #[test]
    fn lil_name_map_isomorphism() {
        let original_gfa = load_lil_gfa();
        test_isomorphism(&original_gfa);
    }

    #[test]
    fn diatom_name_map_isomorphism() {
        let original_gfa = load_diatom_gfa();
        test_isomorphism(&original_gfa);
    }

    #[test]
    #[cfg(feature = "serde1")]
    fn lil_name_map_serde() {
        let gfa = load_lil_gfa();
        let name_map = NameMap::build_from_gfa(&gfa);

        let _ = std::fs::remove_file(lil_name_map_path());
        name_map.save_json(lil_name_map_path()).unwrap();
        let loaded_map = NameMap::load_json(lil_name_map_path()).unwrap();

        assert_eq!(name_map, loaded_map);
    }

    #[test]
    #[cfg(feature = "serde1")]
    fn diatom_name_map_serde() {
        let gfa = load_diatom_gfa();
        let name_map = NameMap::build_from_gfa(&gfa);

        let new_gfa = name_map.gfa_bytestring_to_usize(&gfa, false).unwrap();

        let _ = std::fs::remove_file(diatom_name_map_path());
        name_map.save_json(diatom_name_map_path()).unwrap();
        let loaded_map = NameMap::load_json(diatom_name_map_path()).unwrap();

        assert_eq!(name_map, loaded_map);

        let inverted_gfa =
            loaded_map.gfa_usize_to_bytestring(&new_gfa).unwrap();

        assert_eq!(gfa, inverted_gfa);
    }
}
