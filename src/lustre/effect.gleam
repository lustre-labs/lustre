//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre/effect](https://lustre.build/api/lustre/effect)

// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/function

// TYPES -----------------------------------------------------------------------

///
pub opaque type Effect(msg) {
  Effect(all: List(fn(fn(msg) -> Nil, fn(msg) -> Nil) -> Nil))
}

// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn from(effect: fn(fn(msg) -> Nil) -> Nil) -> Effect(msg) {
  // Effects constructed with `effect.from` only get told about the `dispatch`
  // function. If users want to emit events from a component they should use
  // `event.emit` instead!
  Effect([fn(dispatch, _) { effect(dispatch) }])
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
  Effect({
    use effect <- list.map(effect.all)

    fn(dispatch, emit) {
      let dispatch = function.compose(f, dispatch)
      let emit = function.compose(f, emit)

      effect(dispatch, emit)
    }
  })
}
