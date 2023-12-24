//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre/effect](https://lustre.build/api/lustre/effect)

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/function

// TYPES -----------------------------------------------------------------------

///
pub opaque type Effect(msg) {
  Effect(all: List(fn(fn(msg) -> Nil, fn(String, Dynamic) -> Nil) -> Nil))
}

// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn from(effect: fn(fn(msg) -> Nil) -> Nil) -> Effect(msg) {
  // Effects constructed with `effect.from` only get told about the `dispatch`
  // function. If users want to emit events from a component they should use
  // `event.emit` instead!
  Effect([fn(dispatch, _) { effect(dispatch) }])
}

/// Emit a custom event from a component as an effect. Parents can listen to these
/// events in their `view` function like any other HTML event.
/// 
/// You *probably* don't need to use this type of effect if you're not making use
/// of Lustre's components, but in rare cases it may be useful to emit custom
/// events from the DOM node that your Lustre application is mounted to.
/// 
pub fn event(name: String, data: data) -> Effect(msg) {
  Effect([fn(_, emit) { emit(name, dynamic.from(data)) }])
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
/// 
pub fn batch(effects: List(Effect(msg))) -> Effect(msg) {
  Effect({
    use b, Effect(a) <- list.fold(effects, [])
    list.append(b, a)
  })
}

///
/// 
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b) {
  Effect({
    use effect <- list.map(effect.all)

    fn(dispatch, emit) {
      let dispatch = function.compose(f, dispatch)

      effect(dispatch, emit)
    }
  })
}

/// Perform a side effect by supplying your own `dispatch` function. This is
/// primarily used internally by the server runtime, but it is also useful for
/// testing.
/// 
pub fn perform(effect: Effect(a), dispatch: fn(a) -> Nil) -> Nil {
  list.each(effect.all, fn(eff) { eff(dispatch, fn(_, _) { Nil }) })
}
