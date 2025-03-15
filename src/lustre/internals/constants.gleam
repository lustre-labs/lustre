//// Gleam's runtime representation of types in JavaScript is not quite clever
//// enough to notice that empty containers or nullary constructors can be
//// initialised once and reused every time they are needed.
////
//// This might sound like a micro-optimisation, but things like empty lists and
//// dicts are *incredibly* common just in Lustre's internals, so having constant
//// references to them makes a huge difference over the lifetime of a program.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/option
import gleam/set.{type Set}

// CONSTRUCTORS ----------------------------------------------------------------

pub const empty_list = []

@external(javascript, "./constants.ffi.mjs", "empty_dict")
pub fn empty_dict() -> Dict(k, v) {
  dict.new()
}

@external(javascript, "./constants.ffi.mjs", "empty_set")
pub fn empty_set() -> Set(a) {
  set.new()
}

pub const option_none = option.None

pub const error_nil = Error(Nil)
