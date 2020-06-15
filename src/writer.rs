use crate::gfa::{Link, Orientation, Path, Segment};

// TODO none of these functions use the optional fields yet

// Write header

// Write segment
pub fn segment_string(seg: &Segment) -> String {
    format!("S\t{}\t{}", seg.name, seg.sequence)
}

// Write link
pub fn link_string(link: &Link) -> String {
    format!(
        "L\t{}\t{}\t{}\t{}\t{}",
        link.from_segment, link.from_orient, link.to_segment, link.to_orient, link.overlap
    )
}

// Write path
pub fn path_string(path: &Path) -> String {
    let mut result = format!("P\t{}\t", path.path_name);
    path.segment_names
        .iter()
        .enumerate()
        .for_each(|(i, (n, o))| {
            if i != 0 {
                result.push_str(",");
            }
            result.push_str(&format!("{}{}", n, o));
        });
    result.push_str("\t");
    path.overlaps.iter().enumerate().for_each(|(i, o)| {
        if i != 0 {
            result.push_str(",");
        }
        result.push_str(&o);
    });
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn print_segment() {
        let segment = Segment::new("seg1", "GCCCTA");
        let string = segment_string(&segment);
        assert_eq!(string, "S\tseg1\tGCCCTA");
    }

    #[test]
    fn print_link() {
        let link = Link::new(
            "13",
            Orientation::Forward,
            "552",
            Orientation::Backward,
            "0M",
        );
        let string = link_string(&link);
        assert_eq!(string, "L\t13\t+\t552\t-\t0M");
    }

    #[test]
    fn print_path() {
        let path = Path::new(
            "path1",
            vec!["13+", "51-", "241+"],
            vec!["8M", "1M", "3M"]
                .into_iter()
                .map(String::from)
                .collect(),
        );

        let string = path_string(&path);
        assert_eq!(string, "P\tpath1\t13+,51-,241+\t8M,1M,3M");
    }
}
