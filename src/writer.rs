use crate::gfa::{Link, Orientation, Path, Segment, GFA};
use std::fmt::Write;

// TODO none of these functions use the optional fields yet

// Write header
pub fn header_string() -> String {
    "H\tVN:Z:1.0".to_string()
}

// Write segment
pub fn write_segment<T: Write>(seg: &Segment, stream: &mut T) {
    write!(stream, "S\t{}\t{}", seg.name, seg.sequence).expect("Error writing segment to stream");
}

pub fn segment_string(seg: &Segment) -> String {
    let mut result = String::new();
    write_segment(seg, &mut result);
    result
}

// Write link
pub fn write_link<T: Write>(link: &Link, stream: &mut T) {
    write!(
        stream,
        "L\t{}\t{}\t{}\t{}\t{}",
        link.from_segment, link.from_orient, link.to_segment, link.to_orient, link.overlap
    )
    .expect("Error writing link to stream");
}

pub fn link_string(link: &Link) -> String {
    let mut result = String::new();
    write_link(link, &mut result);
    result
}

// Write path
pub fn write_path<T: Write>(path: &Path, stream: &mut T) {
    write!(stream, "P\t{}\t", path.path_name).expect("Error writing path to stream");
    path.segment_names
        .iter()
        .enumerate()
        .for_each(|(i, (n, o))| {
            if i != 0 {
                write!(stream, ",").unwrap();
            }
            write!(stream, "{}{}", n, o).unwrap();
        });
    write!(stream, "\t").unwrap();
    path.overlaps.iter().enumerate().for_each(|(i, o)| {
        if i != 0 {
            write!(stream, ",").unwrap();
        }
        write!(stream, "{}", o).unwrap();
    });
}

pub fn path_string(path: &Path) -> String {
    let mut result = String::new();
    write_path(path, &mut result);
    result
}
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
