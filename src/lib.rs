#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

pub mod backend;
pub mod frontend;
