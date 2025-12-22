/// A `MutableMap` functions like Gleam's `Dict` and exposes a subset of the same
/// api. As the name implies, there are some important things to know:
///
/// - On the JavaScript target, updates to the map are performed **in-place** and
///   will mutate the map directly.
///
/// - On the JavaScript target, keys in the map are compared by reference, not by
///   hash or value.
///
/// Gleam has neither mutable data structures nor reference equality, so it's
/// incredibly important to use this module with care. It is primarily intended
/// to be used durring diffing to avoid the expensive overhead of (re)constructing
/// event and keyed node lookups.
///
pub type MutableMap(key, value)

///
///
@external(erlang, "maps", "new")
@external(javascript, "./mutable_map.ffi.mjs", "empty")
pub fn new() -> MutableMap(key, value)

///
///
@external(javascript, "./mutable_map.ffi.mjs", "get")
pub fn unsafe_get(map: MutableMap(key, value), key: key) -> value {
  do_get(key, map)
}

@external(erlang, "maps", "get")
fn do_get(key: key, map: MutableMap(key, value)) -> value

@external(javascript, "./mutable_map.ffi.mjs", "get_or_compute")
pub fn get_or_compute(
  map: MutableMap(key, value),
  key: key,
  compute: fn() -> value,
) -> value {
  case do_find(map, key) {
    Ok(value) -> value
    _ -> compute()
  }
}

@external(erlang, "gleam@dict", "get")
fn do_find(map: MutableMap(key, value), key: key) -> Result(value, Nil)

///
///
@external(javascript, "./mutable_map.ffi.mjs", "has_key")
pub fn has_key(map: MutableMap(key, value), key: key) -> Bool {
  do_has_key(key, map)
}

@external(erlang, "maps", "is_key")
fn do_has_key(key: key, dict: MutableMap(key, value)) -> Bool

///
///
@external(javascript, "./mutable_map.ffi.mjs", "insert")
pub fn insert(
  map: MutableMap(key, value),
  key: key,
  value: value,
) -> MutableMap(key, value) {
  do_put(key, value, map)
}

@external(erlang, "maps", "put")
fn do_put(
  key: key,
  value: value,
  map: MutableMap(key, value),
) -> MutableMap(key, value)

///
///
@external(javascript, "./mutable_map.ffi.mjs", "remove")
pub fn delete(map: MutableMap(key, value), key: key) -> MutableMap(key, value) {
  do_remove(key, map)
}

@external(erlang, "maps", "remove")
fn do_remove(key: key, map: MutableMap(key, value)) -> MutableMap(key, value)

///
///
@external(erlang, "maps", "size")
@external(javascript, "./mutable_map.ffi.mjs", "size")
pub fn size(map: MutableMap(key, value)) -> Int

///
///
pub fn is_empty(map: MutableMap(key, value)) -> Bool {
  size(map) == 0
}
