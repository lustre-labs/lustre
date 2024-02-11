//// In other frameworks it's common for components to perform side effects
//// whenever the need them. An event handler might make an HTTP request, or a
//// component might reach into the DOM to focus an input.
////
//// In Lustre we try to keep side effects separate from our main program loop.
//// This comes with a whole bunch of benefits like making it easier to test and
//// reason about our code, making it possible to implement time-travel debugging,
//// or even to run our app on the server using Lustre's server components. This
//// is great but we still need to perform side effects at some point, so how do
//// we do that?
////
//// The answer is through the `Effect` type. An application's `init` and `update`
//// functions typically return a tuple of `#(model, Effect(msg))`. The `Effect`
//// type is a way of describing to the runtime some side effects that should be
//// performed.
////
//// By going through this abstraction we discourage side effects from being
//// performed in the middle of our program. Furthermore they provide a mechanism
//// for effects to send messages *back* to the main program loop so you can, for
//// example, fire off an HTTP request and turn the response into a message that
//// can be handled by your `update` function.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/json.{type Json}
import gleam/list
import gleam/function

// TYPES -----------------------------------------------------------------------

///
pub opaque type Effect(msg) {
  Effect(all: List(fn(fn(msg) -> Nil, fn(String, Json) -> Nil) -> Nil))
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
pub fn event(name: String, data: Json) -> Effect(msg) {
  Effect([fn(_, emit) { emit(name, data) }])
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
    list.map(effect.all, fn(effect) {
      fn(dispatch, emit) {
        let dispatch = function.compose(f, dispatch)

        effect(dispatch, emit)
      }
    })
  })
}

/// Perform a side effect by supplying your own `dispatch` function. This is
/// primarily used internally by the server runtime, but it is also useful for
/// testing.
///
pub fn perform(
  effect: Effect(a),
  dispatch: fn(a) -> Nil,
  emit: fn(String, Json) -> Nil,
) -> Nil {
  use eff <- list.each(effect.all)

  eff(dispatch, emit)
}
