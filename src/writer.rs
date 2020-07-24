use crate::gfa::*;
use crate::parser::OptFields;
use bstr::{BStr, BString, ByteSlice, ByteVec};
use std::fmt::Display;
use std::fmt::Write;

pub fn write_optional_fields<U: OptFields, T: Write>(opts: &U, stream: &mut T) {
    for field in opts.fields() {
        write!(stream, "\t{}", field).unwrap_or_else(|err| {
            panic!(
                "Error writing optional field '{:?}' to stream, {:?}",
                field, err
            )
        })
    }
}

pub fn write_header<T: Write>(version: &Option<String>, stream: &mut T) {
    if let Some(v) = version {
        write!(stream, "H\tVN:Z:{}", v).unwrap();
    } else {
        write!(stream, "H").unwrap();
    }
}

// Write segment
pub fn write_segment<N: Display, T: Write>(
    seg: &Segment<N, OptionalFields>,
    stream: &mut T,
) {
    write!(stream, "S\t{}\t{}", seg.name, seg.sequence)
        .expect("Error writing segment to stream");

    write_optional_fields(&seg.optional, stream);
}

pub fn segment_string<N: Display>(seg: &Segment<N, OptionalFields>) -> String {
    let mut result = String::new();
    write_segment(seg, &mut result);
    result
}

// Write link
pub fn write_link<N: Display, T: Write>(
    link: &Link<N, OptionalFields>,
    stream: &mut T,
) {
    write!(
        stream,
        "L\t{}\t{}\t{}\t{}",
        link.from_segment,
        link.from_orient,
        link.to_segment,
        link.to_orient // link.overlap
                       // TODO fix overlap!
    )
    .expect("Error writing link to stream");

    let link = link.clone();
    write_optional_fields(&link.optional, stream);
}

pub fn link_string<N: Display>(link: &Link<N, OptionalFields>) -> String {
    let mut result = String::new();
    write_link(link, &mut result);
    result
}

// Write path
pub fn write_path<U: OptFields, T: Write>(path: &Path<U>, stream: &mut T) {
    write!(stream, "P\t{}\t", path.path_name)
        .expect("Error writing path to stream");

    write!(stream, "{}\t", path.segment_names).unwrap();

    path.overlaps.iter().enumerate().for_each(|(i, o)| {
        if i != 0 {
            write!(stream, ",").unwrap();
        }
        write!(stream, "{}", String::from_utf8(o.to_vec()).unwrap()).unwrap();
    });

    write_optional_fields(&path.optional, stream);
}

pub fn path_string<T: OptFields>(path: &Path<T>) -> String {
    let mut result = String::new();
    write_path(path, &mut result);
    result
}

// Write GFA
pub fn write_gfa<N: Display, T: Write>(
    gfa: &GFA<N, OptionalFields>,
    stream: &mut T,
) {
    write_header(&gfa.version, stream);
    writeln!(stream).unwrap();
    gfa.segments.iter().for_each(|s| {
        write_segment(s, stream);
        writeln!(stream).unwrap();
    });

    gfa.paths.iter().for_each(|p| {
        write_path(p, stream);
        writeln!(stream).unwrap();
    });

    gfa.links.iter().for_each(|l| {
        write_link(l, stream);
        writeln!(stream).unwrap();
    });
}

pub fn gfa_string(gfa: &GFA<BString, OptionalFields>) -> String {
    let mut result = String::new();
    write_gfa(gfa, &mut result);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gfa::Orientation;

    #[test]
    fn print_segment() {
        let mut segment = Segment::new(b"seg1", b"GCCCTA");
        let opt1 =
            OptionalField::new(b"IJ", OptionalFieldValue::PrintableChar(b'x'));
        let opt2 = OptionalField::new(
            b"AB",
            OptionalFieldValue::IntArray(vec![1, 2, 3, 52124]),
        );
        segment.optional = vec![opt1, opt2];
        let expected = "S\tseg1\tGCCCTA\tRC:i:123\tUR:Z:http://test.com/\tIJ:A:x\tAB:B:I1,2,3,52124";
        let string = segment_string(&segment);
        assert_eq!(string, expected);
    }

    #[test]
    fn print_link() {
        let link = Link::new(
            b"13",
            Orientation::Forward,
            b"552",
            Orientation::Backward,
            b"0M",
        );
        let string = link_string(&link);
        assert_eq!(string, "L\t13\t+\t552\t-\t0M");
    }

    #[test]
    fn print_path() {
        let path = Path {
            path_name: "path1".into(),
            segment_names: "13+,51-,241+".into(),
            overlaps: vec![b"8M".to_vec(), b"1M".to_vec(), b"3M".to_vec()],
            optional: (),
        };

        let string = path_string(&path);
        assert_eq!(string, "P\tpath1\t13+,51-,241+\t8M,1M,3M");
    }

    use std::io::Read;
    use std::path::PathBuf;

    /*
    #[test]
    fn print_gfa() {
        let in_gfa =
            crate::parser::parse_gfa(&PathBuf::from("./lil.gfa")).unwrap();
        let mut file =
            std::fs::File::open(&PathBuf::from("./lil.gfa")).unwrap();
        let mut file_string = String::new();
        file.read_to_string(&mut file_string).unwrap();

        let string = gfa_string(&in_gfa);

        assert_eq!(string, file_string);
    }
    */
}
