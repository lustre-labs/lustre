@external(erlang, "cli_ffi", "exec")
pub fn exec(
  run command: String,
  with args: List(String),
  in in: String,
) -> Result(String, #(Int, String))

// CHAINING RESULTS ------------------------------------------------------------

pub fn try(
  result: Result(a, x),
  on_error strategy: ErrorStrategy(x, e),
  then f: fn(a) -> Result(b, e),
) -> Result(b, e) {
  case result {
    Ok(value) -> f(value)
    Error(x) -> Error(strategy(x))
  }
}

pub type ErrorStrategy(x, e) =
  fn(x) -> e

pub fn replace(with error: e) -> ErrorStrategy(x, e) {
  fn(_) { error }
}

pub fn map(with f: fn(x) -> e) -> ErrorStrategy(x, e) {
  f
}

pub fn keep(err: e) -> e {
  err
}

// BOOLEAN GUARDS --------------------------------------------------------------

pub fn guard(
  condition: Bool,
  consequence: e,
  then: fn() -> Result(a, e),
) -> Result(a, e) {
  case condition {
    True -> Error(consequence)
    False -> then()
  }
}

pub fn when(
  condition: Bool,
  consequence: a,
  then: fn() -> Result(a, e),
) -> Result(a, e) {
  case condition {
    True -> Ok(consequence)
    False -> then()
  }
}
