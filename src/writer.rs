use crate::gfa::*;
use crate::optfields::*;
use bstr::BString;
use std::fmt::Display;
use std::fmt::Write;

/// This entire module will probably be removed, with the functions
/// replaced by Display implementations on GFA and the GFA line types,
/// but I haven't gotten around to it yet

fn write_optional_fields<U: OptFields, T: Write>(opts: &U, stream: &mut T) {
    for field in opts.fields() {
        write!(stream, "\t{}", field).unwrap_or_else(|err| {
            panic!(
                "Error writing optional field '{:?}' to stream, {:?}",
                field, err
            )
        })
    }
}

fn write_header<U: OptFields, T: Write>(header: &Header<U>, stream: &mut T) {
    write!(stream, "H").unwrap();
    if let Some(v) = &header.version {
        write!(stream, "\tVN:Z:{}", v).unwrap();
    }
    write_optional_fields(&header.optional, stream);
}

// Write segment
fn write_segment<N: Display, T: Write, U: OptFields>(
    seg: &Segment<N, U>,
    stream: &mut T,
) {
    write!(stream, "S\t{}\t{}", seg.name, seg.sequence)
        .expect("Error writing segment to stream");

    write_optional_fields(&seg.optional, stream);
}

// Write link
fn write_link<N: Display, T: Write, U: OptFields>(
    link: &Link<N, U>,
    stream: &mut T,
) {
    write!(
        stream,
        "L\t{}\t{}\t{}\t{}\t{}",
        link.from_segment,
        link.from_orient,
        link.to_segment,
        link.to_orient,
        link.overlap,
    )
    .expect("Error writing link to stream");

    write_optional_fields(&link.optional, stream);
}

// Write path
fn write_path<N, U: OptFields, T: Write>(path: &Path<N, U>, stream: &mut T) {
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

// Write GFA
pub fn write_gfa<N: Display, T: Write, U: OptFields>(
    gfa: &GFA<N, U>,
    stream: &mut T,
) {
    write_header(&gfa.header, stream);
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
        use OptFieldVal::*;
        let mut segment: Segment<BString, OptionalFields> =
            Segment::new(b"seg1", b"GCCCTA");
        let opt_ij = OptField::new(b"IJ", A(b'x'));
        let opt_ab = OptField::new(b"AB", BInt(vec![1, 2, 3, 52124]));
        let opt_ur = OptField::new(b"UR", Z(BString::from("http://test.com/")));
        let opt_rc = OptField::new(b"RC", Int(123));
        segment.optional = vec![opt_rc, opt_ur, opt_ij, opt_ab];
        let expected = "S\tseg1\tGCCCTA\tRC:i:123\tUR:Z:http://test.com/\tIJ:A:x\tAB:B:I1,2,3,52124";
        let mut string = String::new();
        write_segment(&segment, &mut string);
        assert_eq!(string, expected);
    }

    #[test]
    fn print_link() {
        let link: Link<BString, ()> = Link::new(
            b"13",
            Orientation::Forward,
            b"552",
            Orientation::Backward,
            b"0M",
        );
        let mut string = String::new();
        write_link(&link, &mut string);
        assert_eq!(string, "L\t13\t+\t552\t-\t0M");
    }

    #[test]
    fn print_path() {
        let path: Path<BString, _> = Path::new(
            "path1".into(),
            "13+,51-,241+".into(),
            vec!["8M".into(), "1M".into(), "3M".into()],
            (),
        );

        let mut string = String::new();
        write_path(&path, &mut string);
        assert_eq!(string, "P\tpath1\t13+,51-,241+\t8M,1M,3M");
    }

    #[test]
    fn print_gfa() {
        use std::io::Read;
        use std::path::PathBuf;

        let parser = crate::parser::GFAParser::new();
        let in_gfa: GFA<BString, ()> = parser.parse_file(&"./lil.gfa").unwrap();

        let mut file =
            std::fs::File::open(&PathBuf::from("./lil.gfa")).unwrap();
        let mut file_string = String::new();
        file.read_to_string(&mut file_string).unwrap();

        let mut string = String::new();
        write_gfa(&in_gfa, &mut string);

        assert_eq!(string, file_string);
    }
}
