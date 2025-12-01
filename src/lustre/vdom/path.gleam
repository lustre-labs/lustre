// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/string
import lustre/internals/constants

// CONSTANTS -------------------------------------------------------------------

///
///
pub const root: Path = Root

///
///
pub const separator_element: String = "\t"

///
///
pub const separator_subtree: String = "\r"

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
  Subtree(parent: Path)
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

pub fn split_subtree_path(path: String) -> List(String) {
  string.split(path, on: separator_subtree)
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

pub fn subtree(path: Path) -> Path {
  Subtree(parent: path)
}

// CONVERSIONS -----------------------------------------------------------------

/// Convert a path to a resolved string with an event name appended to it.
/// This returns a partial path, up to the closest Memo barrier.
///
pub fn event(path: Path, event: String) -> String {
  do_to_string(False, path, [separator_event, event, ..constants.empty_list])
}

/// Convert a path to a child tree to a resolved string.
///
pub fn child(path: Path) -> String {
  do_to_string(False, path, constants.empty_list)
}

/// Convert a path to a full resolved string, including all memo barriers.
///
pub fn to_string(path: Path) -> String {
  do_to_string(True, path, constants.empty_list)
}

fn do_to_string(full, path, acc) {
  case path {
    Root -> finish_to_string(acc)

    Key(key:, parent:) ->
      do_to_string(full, parent, [separator_element, key, ..acc])

    Index(index:, parent:) -> {
      let acc = [separator_element, int.to_string(index), ..acc]
      do_to_string(full, parent, acc)
    }

    Subtree(_) if !full -> finish_to_string(acc)
    Subtree(parent:) ->
      case acc {
        [] -> do_to_string(full, parent, acc)
        [_sep, ..acc] -> do_to_string(full, parent, [separator_subtree, ..acc])
      }
  }
}

fn finish_to_string(acc) {
  case acc {
    [] -> ""
    [_sep, ..segments] -> string.concat(segments)
  }
}
