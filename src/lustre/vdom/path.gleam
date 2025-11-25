// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/string

// CONSTANTS -------------------------------------------------------------------

///
///
pub const root: Path = Root

///
///
pub const separator_element: String = "\t"

///
///
pub const separator_memo: String = "\r"

///
///
pub const separator_event: String = "\n"

// TYPES -----------------------------------------------------------------------

///
///
pub opaque type Path {
  Root
  Key(key: String, parent: Path)
  Index(index: Int, parent: Path)
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn matches(path: Path, any candidates: List(String)) -> Bool {
  case candidates {
    [] -> False
    _ -> do_matches(to_string(path), candidates)
  }
}

fn do_matches(path: String, candidates: List(String)) -> Bool {
  case candidates {
    [] -> False
    [candidate, ..rest] ->
      case string.starts_with(path, candidate) {
        True -> True
        False -> do_matches(path, rest)
      }
  }
}

pub fn split_memo(path: String) -> List(String) {
  string.split(path, on: separator_memo)
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn add(parent: Path, index: Int, key: String) -> Path {
  case key {
    "" -> Index(index:, parent:)
    _ -> Key(key:, parent:)
  }
}

// CONVERSIONS -----------------------------------------------------------------

/// Convert a path to a resolved string with an event name appended to it.
///
pub fn event(path: Path, event: String) -> String {
  do_to_string(path, [separator_event, event])
}

///
///
pub fn to_string(path: Path) -> String {
  do_to_string(path, [])
}

fn do_to_string(path, acc) {
  case path {
    Root ->
      case acc {
        [] -> ""
        [_sep, ..segments] -> string.concat(segments)
      }

    Key(key:, parent:) -> do_to_string(parent, [separator_element, key, ..acc])

    Index(index:, parent:) ->
      do_to_string(parent, [separator_element, int.to_string(index), ..acc])
  }
}
