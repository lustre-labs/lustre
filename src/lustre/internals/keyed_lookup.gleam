// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/list
import gleam/set.{type Set}

// TYPES -----------------------------------------------------------------------

pub type KeyedLookup(v) {
  KeyedLookup(values: Dict(String, v), visited: Set(String))
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn new() -> KeyedLookup(v) {
  KeyedLookup(dict.new(), set.new())
}

pub fn from_values(
  values: List(a),
  to_entry: fn(a, Int) -> #(String, v),
) -> KeyedLookup(v) {
  let dict = {
    use dict, value, index <- list.index_fold(values, dict.new())
    let #(key, value) = to_entry(value, index)

    dict.insert(dict, key, value)
  }

  KeyedLookup(dict, set.new())
}

// QUERIES ---------------------------------------------------------------------

pub fn has(lookup: KeyedLookup(v), key: String) -> Bool {
  key != ""
  && !set.contains(lookup.visited, key)
  && dict.has_key(lookup.values, key)
}

pub fn visited(lookup: KeyedLookup(v), key: String) -> Bool {
  key != "" && set.contains(lookup.visited, key)
}

pub fn is_empty(lookup: KeyedLookup(v)) -> Bool {
  dict.size(lookup.values) <= set.size(lookup.visited)
}

// MANIPULATIONS ---------------------------------------------------------------

pub fn set(lookup: KeyedLookup(v), key: String, value: v) -> KeyedLookup(v) {
  case key == "" {
    True -> lookup
    False -> {
      let KeyedLookup(values:, visited:) = lookup
      KeyedLookup(values: dict.insert(values, key, value), visited:)
    }
  }
}

pub fn set_from_values(
  lookup: KeyedLookup(v),
  values: List(a),
  to_entry: fn(a, Int) -> #(String, v),
) -> KeyedLookup(v) {
  let values = {
    use dict, value, index <- list.index_fold(values, lookup.values)
    let #(key, value) = to_entry(value, index)
    case key == "" {
      True -> dict
      False -> dict.insert(dict, key, value)
    }
  }

  KeyedLookup(..lookup, values:)
}

pub fn delete(lookup: KeyedLookup(v), key: String) -> KeyedLookup(v) {
  case key == "" {
    True -> lookup
    False -> {
      let KeyedLookup(values:, visited:) = lookup
      KeyedLookup(values:, visited: set.insert(visited, key))
    }
  }
}

pub fn pop(
  lookup: KeyedLookup(v),
  key: String,
) -> Result(#(v, KeyedLookup(v)), Nil) {
  let KeyedLookup(values:, visited:) = lookup
  case set.contains(visited, key) {
    False ->
      case dict.get(values, key) {
        Ok(value) ->
          Ok(#(value, KeyedLookup(values:, visited: set.insert(visited, key))))
        Error(_) -> Error(Nil)
      }
    True -> Error(Nil)
  }
}

pub fn get(lookup: KeyedLookup(v), key: String) {
  // case set.contains(lookup.visited, key) {
  //   False -> dict.get(lookup.values, key)
  //   True -> Error(Nil)
  // }
  dict.get(lookup.values, key)
}

// CONVERSIONS -----------------------------------------------------------------

pub fn fold_remaining_keys(
  over lookup: KeyedLookup(v),
  from state: state,
  with fun: fn(state, String) -> state,
) -> state {
  let KeyedLookup(visited:, values:) = lookup
  use state, key, _ <- dict.fold(values, state)
  case set.contains(visited, key) {
    True -> state
    False -> fun(state, key)
  }
}
