// use nom::{
//     IResult,
//     bytes::complete::{tag, take_while_m_n},

// use std::collections::BTreeMap;
// use std::collections::HashMap;

use gfa::gfa::{OptionalField, OptionalFieldValue};
use gfa::gfageneric::*;
// use gfa::gfageneric::*;

type MinSegment<'a> = Segment<&'a [u8], ()>;
type OptSegment<'a> = Segment<&'a [u8], Vec<OptionalField>>;

type MinNamedSeg = Segment<String, ()>;
type OptNamedSeg = Segment<String, Vec<OptionalField>>;

type MinIndexSeg = Segment<usize, ()>;
type OptIndexSeg = Segment<usize, Vec<OptionalField>>;

type MinLink<'a> = Link<&'a [u8], ()>;
type OptLink<'a> = Link<&'a [u8], Vec<OptionalField>>;

type MinNamedLink = Link<String, ()>;
type OptNamedLink = Link<String, Vec<OptionalField>>;

type MinIndexLink = Link<usize, ()>;
type OptIndexLink = Link<usize, Vec<OptionalField>>;

use std::mem::{size_of, size_of_val};

fn main() {
    println!("Hello, world!");

    println!("segments");
    println!("min: {}", size_of::<MinSegment<'_>>());
    println!("opt: {}", size_of::<OptSegment<'_>>());
    println!("min: {}", size_of::<MinNamedSeg>());
    println!("opt: {}", size_of::<OptNamedSeg>());
    println!("min: {}", size_of::<MinIndexSeg>());
    println!("opt: {}", size_of::<OptIndexSeg>());

    println!("links");
    println!("min: {}", size_of::<MinLink<'_>>());
    println!("opt: {}", size_of::<OptLink<'_>>());

    println!("min: {}", size_of::<MinNamedLink>());
    println!("opt: {}", size_of::<OptNamedLink>());

    println!("min: {}", size_of::<MinIndexLink>());
    println!("opt: {}", size_of::<OptIndexLink>());
}
