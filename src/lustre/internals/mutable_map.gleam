///
///
pub type MutableMap(key, value)

///
///
@external(erlang, "gleam@dict", "new")
@external(javascript, "./mutable_map.ffi.mjs", "make")
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
