use serde::{Deserialize, Serialize};

use crate::parser::ParseFieldError;

/// Represents segment orientation/strand
#[derive(
    Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize, Hash,
)]
pub enum Orientation {
    Forward,
    Backward,
}

impl Orientation {
    /// Parse an orientation from a single-element, where + is
    /// Forward, - is Backward
    pub fn from_bytes_plus_minus<T: AsRef<[u8]>>(bs: T) -> Option<Self> {
        match bs.as_ref() {
            b"+" => Some(Orientation::Forward),
            b"-" => Some(Orientation::Backward),
            _ => None,
        }
    }

    pub fn parse_error(opt: Option<Self>) -> Result<Self, ParseFieldError> {
        opt.ok_or(ParseFieldError::OrientationError)
    }

    pub fn write_plus_minus(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let sym = match self {
            Self::Forward => '+',
            Self::Backward => '-',
        };
        write!(f, "{}", sym)
    }

    pub fn plus_minus_as_byte(&self) -> u8 {
        match self {
            Self::Forward => b'+',
            Self::Backward => b'-',
        }
    }

    /// Parse an orientation from a single-element bytestring, where >
    /// is Forward, < is Backward
    pub fn from_bytes_gt_ln<T: AsRef<[u8]>>(bs: T) -> Option<Self> {
        match bs.as_ref() {
            b">" => Some(Orientation::Forward),
            b"<" => Some(Orientation::Backward),
            _ => None,
        }
    }

    pub fn write_gt_ln(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let sym = match self {
            Self::Forward => '>',
            Self::Backward => '<',
        };
        write!(f, "{}", sym)
    }
}

/// Default orientation is forward
impl Default for Orientation {
    fn default() -> Orientation {
        Orientation::Forward
    }
}

/// Forward is true, backward is false
impl From<Orientation> for bool {
    fn from(o: Orientation) -> bool {
        match o {
            Orientation::Forward => true,
            Orientation::Backward => false,
        }
    }
}

impl Orientation {
    pub fn is_reverse(&self) -> bool {
        !bool::from(*self)
    }
}

/// The default parser uses the GFA spec with + as Forward, - as Backward
impl std::str::FromStr for Orientation {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Orientation::from_bytes_plus_minus(s.as_bytes())
            .ok_or("Could not parse orientation (was not + or -)")
    }
}

/// Display uses the GFA spec, mapping Forward to "+", Backward to "-"
impl std::fmt::Display for Orientation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_plus_minus(f)
    }
}
