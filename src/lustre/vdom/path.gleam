import gleam/int
import gleam/list
import gleam/string

pub opaque type Path {
  Root
  Key(key: String, parent: Path)
  Index(index: Int, parent: Path)
}

const separator_index = "\n"

const separator_key = "\r"

pub fn new() -> Path {
  Root
}

pub fn to(parent: Path, index: Int, key: String) -> Path {
  case key {
    "" -> Index(index:, parent:)
    _ -> Key(key:, parent:)
  }
}

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
    Key(key:, parent:) -> do_to_string(parent, [separator_key, key, ..acc])
    Index(index:, parent:) ->
      do_to_string(parent, [separator_index, int.to_string(index), ..acc])
  }
}

pub fn matches_any(path: Path, candidates: List(String)) -> Bool {
  case candidates {
    [] -> False
    _ -> {
      let path = to_string(path)
      list.any(candidates, string.starts_with(path, _))
    }
  }
}
