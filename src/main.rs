use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::{BufReader, BufWriter, Write};
use std::path::PathBuf;

use gfa::gfa::*;
use gfa::parser::*;

fn parse_lines<'a, T: OptFields>(path: &PathBuf) -> GFA<T> {
    let file = File::open(path).unwrap();
    let lines = &mut BufReader::new(file).lines();
    let conf = GFAParsingConfig::all();
    let parser: GFAParser<T> = GFAParser::new(conf);

    let mut gfa_lines =
        lines.filter_map(move |l| parser.parse_line(&l.unwrap()));

    let mut gfa: GFA<T> = GFA::new();

    while let Some(l) = gfa_lines.next() {
        match l {
            Line::Segment(s) => gfa.segments.push(s),
            Line::Link(l) => gfa.links.push(l),
            Line::Containment(c) => gfa.containments.push(c),
            Line::Path(p) => gfa.paths.push(p),
            _ => (),
        }
    }
    gfa
}

type NoOpts = GFA<()>;

type WithOpts = GFA<OptionalFields>;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let path = PathBuf::from(&args[1]);
    // let file = File::open(&path).unwrap();
    // let lines = &mut BufReader::new(file).lines();

    let gfa: NoOpts = parse_lines(&path);
    println!("# segments: {}", gfa.segments.len());
    println!("# links: {}", gfa.links.len());
    println!("# containments: {}", gfa.containments.len());
    println!("# paths: {}", gfa.paths.len());

    // let conf = GFAParsingConfig::all();
    // let parser: GFAParser<()> = GFAParser::new(conf);

    // let filter = parser.filter_line;

    // log_lines(&path);

    // let file = File::open(
    // println!("Hello, world!");
}
