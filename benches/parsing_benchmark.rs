use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::{BufReader, BufWriter, Write};
use std::path::PathBuf;

use lazy_static::lazy_static;

use gfa::gfa::*;
use gfa::parser::*;

use criterion::{
    black_box, criterion_group, criterion_main, BenchmarkId, Criterion,
};

struct GFARaw {
    segments: Vec<String>,
    links: Vec<String>,
    paths: Vec<String>,
}

fn read_raw(path: &PathBuf) -> io::Result<Vec<String>> {
    let file = File::open(path)?;
    let lines = &mut BufReader::new(file).lines();

    let result = lines.map(|l| l.unwrap()).collect();

    Ok(result)
}

fn split_lines(lines: Vec<String>) -> GFARaw {
    let mut gfa = GFARaw {
        segments: Vec::new(),
        links: Vec::new(),
        paths: Vec::new(),
    };

    for l in lines {
        let l: String = l;
        match l.get(0..=1) {
            Some("S") => gfa.segments.push(l),
            Some("L") => gfa.links.push(l),
            Some("P") => gfa.paths.push(l),
            _ => (),
        }
    }

    gfa
}

fn parse_lines<T: HasOptFields>(input: &[String]) -> GFA<T> {
    // let file = File::open(path)?;
    // let lines = &mut BufReader::new(file).lines();
    let mut lines = input.iter();
    let conf = GFAParsingConfig::all();
    let parse = |l| parse_gfa_line(l, &conf);

    let mut gfa: GFA<T> = GFA::new();

    while let Some(l) = lines.next() {
        match parse(l) {
            Some(Line::Segment(s)) => gfa.segments.push(s),
            Some(Line::Link(l)) => gfa.links.push(l),
            Some(Line::Containment(c)) => gfa.containments.push(c),
            Some(Line::Path(p)) => gfa.paths.push(p),
            _ => (),
        }
    }
    gfa
}

fn parse_lines_noopt(input: &[String]) -> GFA<()> {
    parse_lines(input)
}

fn parse_lines_withopt(input: &[String]) -> GFA<OptionalFields> {
    parse_lines(input)
}

static GFAPATH: &str = "./test/gfas/";

macro_rules! bench_gfa {
    ($parser:ident, $id:literal, $name:ident, $gfa:literal) => {
        fn $name(c: &mut Criterion) {
            let mut path = PathBuf::from(GFAPATH);
            path.push($gfa);
            let lines: Vec<String> = read_raw(&path).unwrap();
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
