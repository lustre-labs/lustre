//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre/effect](https://lustre.build/api/lustre/effect)

// IMPORTS ---------------------------------------------------------------------

import gleam/list

// TYPES -----------------------------------------------------------------------

///
pub opaque type Effect(msg) {
  Effect(List(fn(fn(msg) -> Nil) -> Nil))
}

// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn from(effect: fn(fn(msg) -> Nil) -> Nil) -> Effect(msg) {
  Effect([effect])
}

/// Typically our app's `update` function needs to return a tuple of
/// `#(model, Effect(msg))`. When we don't need to perform any side effects we
/// can just return `none()`!
///
pub fn none() -> Effect(msg) {
  Effect([])
}

// MANIPULATIONS ---------------------------------------------------------------

///
pub fn batch(effects: List(Effect(msg))) -> Effect(msg) {
  Effect({
    use b, Effect(a) <- list.fold(effects, [])
    list.append(b, a)
  })
}

///
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b) {
  let Effect(l) = effect
  Effect(list.map(
    l,
    fn(effect) { fn(dispatch) { effect(fn(a) { dispatch(f(a)) }) } },
  ))
}
