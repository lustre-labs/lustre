// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/list
import gleam/set.{type Set}

// TYPES -----------------------------------------------------------------------

pub type KeyedLookup(k, v) {
  KeyedLookup(values: Dict(k, v), visited: Set(k))
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn new() -> KeyedLookup(k, v) {
  KeyedLookup(dict.new(), set.new())
}

pub fn from_values(
  values: List(a),
  to_entry: fn(a, Int) -> #(k, v),
) -> KeyedLookup(k, v) {
  let dict = {
    use dict, value, index <- list.index_fold(values, dict.new())
    let #(key, value) = to_entry(value, index)

    dict.insert(dict, key, value)
  }

  KeyedLookup(dict, set.new())
}

// QUERIES ---------------------------------------------------------------------

pub fn has(lookup: KeyedLookup(k, v), key: k) -> Bool {
  !set.contains(lookup.visited, key) && dict.has_key(lookup.values, key)
}

pub fn visited(lookup: KeyedLookup(k, v), key: k) -> Bool {
  set.contains(lookup.visited, key)
}

pub fn is_empty(lookup: KeyedLookup(k, v)) -> Bool {
  dict.size(lookup.values) <= set.size(lookup.visited)
}

// MANIPULATIONS ---------------------------------------------------------------

pub fn set(lookup: KeyedLookup(k, v), key: k, value: v) -> KeyedLookup(k, v) {
  let KeyedLookup(values:, visited:) = lookup
  KeyedLookup(values: dict.insert(values, key, value), visited:)
}

pub fn delete(lookup: KeyedLookup(k, v), key: k) -> KeyedLookup(k, v) {
  let KeyedLookup(values:, visited:) = lookup
  KeyedLookup(values:, visited: set.insert(visited, key))
}

pub fn pop(
  lookup: KeyedLookup(k, v),
  key: k,
) -> Result(#(v, KeyedLookup(k, v)), Nil) {
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

// CONVERSIONS -----------------------------------------------------------------

pub fn remaining_keys(lookup: KeyedLookup(k, v)) -> List(k) {
  let KeyedLookup(visited:, values:) = lookup
  use result, key, _ <- dict.fold(values, [])
  case set.contains(visited, key) {
    True -> result
    False -> [key, ..result]
  }
}
