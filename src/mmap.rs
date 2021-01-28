use crate::{
    gfa::{Line, Link, Path, Segment},
    parser::GFAParser,
};

use anyhow::{bail, Result};

use memmap::Mmap;

use std::fs::File;
use std::io::prelude::*;

use bstr::ByteSlice;

#[derive(Debug)]
pub struct MmapGFA {
    pub cursor: std::io::Cursor<Mmap>,
    pub line_buf: Vec<u8>,
    pub current_line_len: usize,
    pub last_buf_offset: usize,
    pub parser: GFAParser<usize, ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineType {
    Segment,
    Link,
    Path,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineIndices {
    pub segments: Vec<(usize, usize)>,
    pub links: Vec<usize>,
    pub paths: Vec<usize>,
}

#[derive(Debug)]
pub struct SegmentIter<'a> {
    mmap: &'a mut MmapGFA,
    parser: GFAParser<usize, ()>,
}

impl<'a> Iterator for SegmentIter<'a> {
    type Item = Segment<usize, ()>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Ok(line) = self.mmap.next_line() {
            if let Some(b'S') = line.first() {
                if let Some(Line::Segment(s)) =
                    self.parser.parse_gfa_line(line).ok()
                {
                    return Some(s);
                }
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct LinkIter<'a> {
    mmap: &'a mut MmapGFA,
    parser: GFAParser<usize, ()>,
}

impl<'a> Iterator for LinkIter<'a> {
    type Item = Link<usize, ()>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Ok(line) = self.mmap.next_line() {
            if let Some(b'S') = line.first() {
                if let Some(Line::Link(s)) =
                    self.parser.parse_gfa_line(line).ok()
                {
                    return Some(s);
                }
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct PathIter<'a> {
    mmap: &'a mut MmapGFA,
    parser: GFAParser<usize, ()>,
}

impl<'a> Iterator for PathIter<'a> {
    type Item = Path<usize, ()>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Ok(line) = self.mmap.next_line() {
            if let Some(b'S') = line.first() {
                if let Some(Line::Path(s)) =
                    self.parser.parse_gfa_line(line).ok()
                {
                    return Some(s);
                }
            }
        }
        None
    }
}

impl MmapGFA {
    pub fn new(path: &str) -> Result<Self> {
        let file = File::open(path)?;
        let mmap = unsafe { Mmap::map(&file)? };

        let cursor = std::io::Cursor::new(mmap);
        let line_buf = Vec::with_capacity(1024);
        let current_line_len = 0;
        let last_buf_offset = 0;

        let parser = GFAParser::new();

        Ok(Self {
            cursor,
            line_buf,
            current_line_len,
            last_buf_offset,
            parser,
        })
    }

    pub fn reset_position(&mut self) -> u64 {
        let cur_pos = self.cursor.position();
        self.cursor.set_position(0);
        cur_pos
    }

    pub fn set_position(&mut self, new_pos: u64) -> u64 {
        let cur_pos = self.cursor.position();
        self.cursor.set_position(new_pos);
        cur_pos
    }
    pub fn get_ref(&self) -> &[u8] {
        self.cursor.get_ref().as_ref()
    }

    pub fn get_parser(&self) -> &GFAParser<usize, ()> {
        &self.parser
    }

    pub fn next_line(&mut self) -> Result<&[u8]> {
        self.line_buf.clear();

        self.last_buf_offset = self.cursor.position() as usize;

        let n_read = self.cursor.read_until(b'\n', &mut self.line_buf)?;

        self.current_line_len = n_read;

        Ok(&self.line_buf[..n_read])
    }

    pub fn read_line_at(&mut self, offset: usize) -> Result<&[u8]> {
        self.cursor.set_position(offset as u64);
        self.next_line()
    }

    pub fn build_index(&mut self) -> Result<LineIndices> {
        let start_position = self.cursor.position();
        let current_line_len = self.current_line_len;
        let last_buf_offset = self.last_buf_offset;

        let mut segments = Vec::new();
        let mut links = Vec::new();
        let mut paths = Vec::new();

        self.cursor.set_position(0);

        let mut line_start = 0;

        loop {
            let line = self.next_line()?;
            let length = line.len();

            if let Some(ref byte) = line.first() {
                match byte {
                    b'S' => {
                        segments.push((line_start, length));
                    }
                    b'L' => {
                        links.push(line_start);
                    }
                    b'P' => {
                        paths.push(line_start);
                    }
                    _ => (),
                };

                line_start += line.len();
            } else {
                break;
            }
        }

        self.cursor.set_position(start_position);
        self.current_line_len = current_line_len;
        self.last_buf_offset = last_buf_offset;

        let res = LineIndices {
            segments,
            links,
            paths,
        };

        Ok(res)
    }

    pub fn current_line(&self) -> &[u8] {
        &self.line_buf[..self.current_line_len]
    }

    pub fn current_line_name(&self) -> Option<&[u8]> {
        let mut iter = self.line_buf.split_str("\t");
        let _lt = iter.next()?;
        let name = iter.next()?;
        Some(name)
    }

    pub fn parse_current_line(&self) -> Result<Line<usize, ()>> {
        let line = self.current_line();
        if line.is_empty() {
            bail!("Line at offset {} is empty", self.last_buf_offset);
        }

        let gfa_line = self.parser.parse_gfa_line(line)?;
        Ok(gfa_line)
    }

    pub fn iter_segments(&mut self, from_start: bool) -> SegmentIter<'_> {
        if from_start {
            self.cursor.set_position(0);
        }
        let parser = self.parser.clone();
        SegmentIter { mmap: self, parser }
    }

    pub fn iter_links(&mut self, from_start: bool) -> LinkIter<'_> {
        if from_start {
            self.cursor.set_position(0);
        }
        let parser = self.parser.clone();
        LinkIter { mmap: self, parser }
    }

    pub fn iter_paths(&mut self, from_start: bool) -> PathIter<'_> {
        if from_start {
            self.cursor.set_position(0);
        }
        let parser = self.parser.clone();
        PathIter { mmap: self, parser }
    }
}
