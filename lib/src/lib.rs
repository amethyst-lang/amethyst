#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
pub use no_std_compat as std;

#[cfg(not(feature = "std"))]
pub use std::prelude::v1::*;

#[cfg(feature = "std")]
pub use std::prelude::v1::{Box, Vec};

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

pub mod frontend;
