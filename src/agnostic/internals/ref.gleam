///
pub type Ref

///
@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
pub fn from(value: a) -> Ref

///
@external(javascript, "./ref.ffi.mjs", "sameValueZero")
pub fn equal(a: Ref, b: Ref) -> Bool {
  a == b
}

///
pub fn equal_lists(xs: List(Ref), ys: List(Ref)) -> Bool {
  case xs, ys {
    [], [] -> True
    [], _ | _, [] -> False
    [x, ..xs], [y, ..ys] ->
      case equal(x, y) {
        True -> equal_lists(xs, ys)
        False -> False
      }
  }
}
