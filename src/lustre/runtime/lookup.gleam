// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/list
import gleam/set.{type Set}

// TYPES -----------------------------------------------------------------------

pub type Lookup(k, v) =
  #(Dict(k, v), Set(k))

//

pub fn new() -> Lookup(k, v) {
  #(dict.new(), set.new())
}

//

pub fn from_values(
  values: List(a),
  to_entry: fn(a, Int) -> #(k, v),
) -> Lookup(k, v) {
  let dict = {
    use dict, value, index <- list.index_fold(values, dict.new())
    let #(key, value) = to_entry(value, index)

    dict.insert(dict, key, value)
  }

  #(dict, set.new())
}

pub fn has(lookup: Lookup(k, v), key: k) -> Bool {
  dict.has_key(lookup.0, key)
}

pub fn set(lookup: Lookup(k, v), key: k, value: v) -> Lookup(k, v) {
  #(dict.insert(lookup.0, key, value), lookup.1)
}

pub fn delete(lookup: Lookup(k, v), key: k) -> Lookup(k, v) {
  #(dict.delete(lookup.0, key), set.insert(lookup.1, key))
}

pub fn pop(lookup: Lookup(k, v), key: k) -> Result(#(v, Lookup(k, v)), Nil) {
  case dict.get(lookup.0, key) {
    Ok(value) ->
      Ok(#(value, #(dict.delete(lookup.0, key), set.insert(lookup.1, key))))
    Error(_) -> Error(Nil)
  }
}

pub fn remaining_keys(lookup: Lookup(k, v)) -> List(k) {
  dict.keys(lookup.0)
}

pub fn visited(lookup: Lookup(k, v), key: k) -> Bool {
  set.contains(lookup.1, key)
}

pub fn is_empty(lookup: Lookup(k, v)) -> Bool {
  dict.is_empty(lookup.0) && set.is_empty(lookup.1)
}
