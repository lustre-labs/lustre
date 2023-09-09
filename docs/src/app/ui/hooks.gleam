// 🚨 This module makes quite judicious use of `dynamic.unsafe_coerce` to wire 
// things up. As things are, this is sound because we control things in such a
// way that it's impossible to pass in things that don't match up to the expected
// types.
//
// If you're defining a new hook to export for this module, pay extra attention
// to make sure you aren't introducing any soundness issues! 🚨

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{Dynamic, dynamic}
import gleam/function
import gleam/map.{Map}
import gleam/result
import lustre.{Error}
import lustre/attribute.{Attribute, property}
import lustre/effect.{Effect}
import lustre/element.{Element, element}
import lustre/event

// HOOKS: STATE ----------------------------------------------------------------

///
/// 
pub fn use_state(
  init: state,
  view: fn(state, fn(state) -> Msg, fn(msg) -> Msg) -> Element(Msg),
) -> Element(msg) {
  let attrs = [property("state", init), property("view", view), on_dispatch()]
  let assert Ok(_) = register_hook("use-state")

  element("use-state", attrs, [])
}

// HOOKS: REDUCER --------------------------------------------------------------

///
/// 
pub fn use_reducer(
  init: state,
  update: fn(state, action) -> state,
  view: fn(state, fn(action) -> Msg, fn(action) -> Msg) -> Element(Msg),
) -> Element(msg) {
  // The `use_reducer` hook is actually just the `use_state` hook under the hood
  // with a wrapper around the `set_state` callback. We could just call out to
  // `use_state` directly but we're doing it like this so that the DOM renders
  // a separate `use-reducer` element, which I think is nicer.
  let view = fn(state, set_state, emit) {
    view(state, function.compose(update(state, _), set_state), emit)
  }
  let attrs = [property("state", init), property("view", view), on_dispatch()]
  let assert Ok(_) = register_hook("use-reducer")

  element("use-reducer", attrs, [])
}

// HOOKS: INTERNAL COMPONENT ---------------------------------------------------

fn register_hook(name: String) -> Result(Nil, Error) {
  // If a component is already registered we will just assume it's because this
  // hook as already been used before. This isn't really an error state so we'll
  // just return `Ok(Nil)` and let our hooks continue.
  case lustre.is_registered(name) {
    True -> Ok(Nil)
    False ->
      lustre.component(
        name,
        init_hook,
        update_hook,
        view_hook,
        map.from_list([
          #("state", dynamic.decode1(Set("state", _), Ok)),
          #("view", dynamic.decode1(Set("view", _), Ok)),
        ]),
      )
  }
}

type Model =
  Map(String, Dynamic)

fn init_hook() -> #(Model, Effect(msg)) {
  #(map.new(), effect.none())
}

/// The type for messages handled internally by the different hooks. You typially
/// won't need to import or refer to this type directly.
///
pub opaque type Msg {
  Set(String, Dynamic)
  Emit(Dynamic)
}

fn update_hook(model: Model, msg: Msg) -> #(Model, Effect(msg)) {
  case msg {
    Set(key, val) -> #(map.insert(model, key, val), effect.none())
    Emit(msg) -> #(model, event.emit("dispatch", msg))
  }
}

fn view_hook(model: Model) -> Element(Msg) {
  case map.get(model, "state"), map.get(model, "view") {
    Ok(state), Ok(view) -> {
      let state = dynamic.unsafe_coerce(state)
      let view = dynamic.unsafe_coerce(view)

      view(state, Set("state", _), Emit)
    }
    _, _ -> element.text("???")
  }
}

// EVENTS ----------------------------------------------------------------------

fn on_dispatch() -> Attribute(msg) {
  use event <- event.on("dispatch")
  event
  |> dynamic.field("detail", dynamic)
  |> result.map(dynamic.unsafe_coerce)
}
