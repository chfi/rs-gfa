use std::fs::File;
use std::io;
use std::io::BufReader;
use std::path::PathBuf;

use bstr::io::*;
use bstr::BString;

use gfa::gfa::*;
use gfa::optfields::*;
use gfa::parser::*;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

fn load_lines(path: &PathBuf) -> io::Result<Vec<Vec<u8>>> {
    let file = File::open(path)?;
    let lines = BufReader::new(file).byte_lines();
    let result = lines.map(|l| l.unwrap()).collect();
    Ok(result)
}

fn parse_lines<T: OptFields>(input: &[Vec<u8>]) -> GFA<BString, T> {
    let parser: GFAParser<T> = GFAParser::new();

    let mut gfa: GFA<BString, T> = GFA::new();

    for line in input.iter() {
        match parser.parse_line(line[..].as_ref()) {
            Some(Line::Segment(s)) => gfa.segments.push(s),
            Some(Line::Link(l)) => gfa.links.push(l),
            Some(Line::Containment(c)) => gfa.containments.push(c),
            Some(Line::Path(p)) => gfa.paths.push(p),
            _ => (),
        }
    }

    gfa
}

fn parse_lines_noopt(input: &[Vec<u8>]) -> GFA<BString, ()> {
    parse_lines(input)
}

fn parse_lines_withopt(input: &[Vec<u8>]) -> GFA<BString, OptionalFields> {
    parse_lines(input)
}

static GFAPATH: &str = "./test/gfas/";

macro_rules! bench_gfa {
    ($parser:ident, $id:literal, $name:ident, $gfa:literal) => {
        fn $name(c: &mut Criterion) {
            let mut path = PathBuf::from(GFAPATH);
            path.push($gfa);
            let lines: Vec<Vec<u8>> = load_lines(&path).unwrap();
            c.bench_with_input(BenchmarkId::new($id, $gfa), &lines, |b, l| {
                b.iter(|| $parser(&l));
            });
        }
    };
}

macro_rules! bench_gfa_noopt {
    ($name:ident, $gfa:literal) => {
        bench_gfa!(parse_lines_noopt, "excluding_optionals", $name, $gfa);
    };
}

macro_rules! bench_gfa_withopt {
    ($name:ident, $gfa:literal) => {
        bench_gfa!(parse_lines_withopt, "including_optionals", $name, $gfa);
    };
}

bench_gfa_noopt!(cov_noopt, "relabeledSeqs.gfa");
bench_gfa_noopt!(a3105_noopt, "A-3105.gfa");
bench_gfa_noopt!(a3105_sort_noopt, "A-3105.sort.gfa");
bench_gfa_noopt!(drb1_noopt, "DRB1-3123.gfa");
bench_gfa_noopt!(drb1_sort_noopt, "DRB1-3123.sort.gfa");

bench_gfa_withopt!(cov_withopt, "relabeledSeqs.gfa");
bench_gfa_withopt!(a3105_withopt, "A-3105.gfa");
bench_gfa_withopt!(a3105_sort_withopt, "A-3105.sort.gfa");
bench_gfa_withopt!(drb1_withopt, "DRB1-3123.gfa");
bench_gfa_withopt!(drb1_sort_withopt, "DRB1-3123.sort.gfa");

criterion_group!(
    name = no_opt_benches;
    config = Criterion::default().sample_size(25);
    targets = cov_noopt, a3105_noopt, a3105_sort_noopt, drb1_noopt, drb1_sort_noopt
);

criterion_group!(
    name = with_opt_benches;
    config = Criterion::default().sample_size(25);
    targets = cov_withopt, a3105_withopt, a3105_sort_withopt, drb1_withopt, drb1_sort_withopt
);

criterion_main!(no_opt_benches, with_opt_benches);
