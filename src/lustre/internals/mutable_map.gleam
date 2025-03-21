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
@external(erlang, "gleam@dict", "new")
@external(javascript, "./mutable_map.ffi.mjs", "empty")
pub fn new() -> MutableMap(key, value)

///
///
@external(erlang, "gleam@dict", "get")
@external(javascript, "./mutable_map.ffi.mjs", "get")
pub fn get(map: MutableMap(key, value), key: key) -> Result(value, Nil)

///
///
@external(erlang, "gleam@dict", "insert")
@external(javascript, "./mutable_map.ffi.mjs", "insert")
pub fn insert(
  map: MutableMap(key, value),
  key: key,
  value: value,
) -> MutableMap(key, value)

///
///
@external(erlang, "gleam@dict", "size")
@external(javascript, "./mutable_map.ffi.mjs", "size")
pub fn size(map: MutableMap(key, value)) -> Int

///
///
pub fn is_empty(map: MutableMap(key, value)) -> Bool {
  size(map) == 0
}
