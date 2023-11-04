//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre](https://lustre.build/api/lustre)

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Decoder}
import gleam/map.{type Map}
import lustre/effect.{type Effect}
import lustre/element.{type Element}

// TYPES -----------------------------------------------------------------------

@target(javascript)
///
pub type App(flags, model, msg)

@target(erlang)
///
pub opaque type App(flags, model, msg) {
  App
}

pub type Error {
  AppAlreadyStarted
  AppNotYetStarted
  BadComponentName
  ComponentAlreadyRegistered
  ElementNotFound
  NotABrowser
}

// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn element(element: Element(msg)) -> App(Nil, Nil, msg) {
  let init = fn(_) { #(Nil, effect.none()) }
  let update = fn(_, _) { #(Nil, effect.none()) }
  let view = fn(_) { element }

  application(init, update, view)
}

///
pub fn simple(
  init: fn(flags) -> model,
  update: fn(model, msg) -> model,
  view: fn(model) -> Element(msg),
) -> App(flags, model, msg) {
  let init = fn(flags) { #(init(flags), effect.none()) }
  let update = fn(model, msg) { #(update(model, msg), effect.none()) }

  application(init, update, view)
}

///
@external(javascript, "./lustre.ffi.mjs", "setup")
pub fn application(
  _init: fn(flags) -> #(model, Effect(msg)),
  _update: fn(model, msg) -> #(model, Effect(msg)),
  _view: fn(model) -> Element(msg),
) -> App(flags, model, msg) {
  // Applications are not usable on the erlang target. For those users, `App`
  // is an opaque type (aka they can't see its structure) and functions like
  // `start` and `destroy` are no-ops.
  //
  // Because the constructor is marked as `@target(erlang)` for some reason we
  // can't simply refer to it here even though the compiler should know that the
  // body of this function can only be entered from erlang (because we have an
  // external def for javascript) but alas, it does not.
  //
  // So instead, we must do this awful hack and cast a `Nil` to the `App` type
  // to make everything happy. Theoeretically this is not going to be a problem
  // unless someone starts poking around with their own ffi and at that point
  // they deserve it.
  dynamic.unsafe_coerce(dynamic.from(Nil))
}

@external(javascript, "./lustre.ffi.mjs", "setup_component")
pub fn component(
  _name: String,
  _init: fn() -> #(model, Effect(msg)),
  _update: fn(model, msg) -> #(model, Effect(msg)),
  _view: fn(model) -> Element(msg),
  _on_attribute_change: Map(String, Decoder(msg)),
) -> Result(Nil, Error) {
  Ok(Nil)
}

// EFFECTS ---------------------------------------------------------------------

///
@external(javascript, "./lustre.ffi.mjs", "start")
pub fn start(
  _app: App(flags, model, msg),
  _selector: String,
  _flags: flags,
) -> Result(fn(msg) -> Nil, Error) {
  Error(NotABrowser)
}

///
@external(javascript, "./lustre.ffi.mjs", "destroy")
pub fn destroy(_app: App(flags, model, msg)) -> Result(Nil, Error) {
  Ok(Nil)
}

// UTILS -----------------------------------------------------------------------

///
@external(javascript, "./lustre.ffi.mjs", "is_browser")
pub fn is_browser() -> Bool {
  False
}

///
@external(javascript, "./lustre.ffi.mjs", "is_registered")
pub fn is_registered(_name: String) -> Bool {
  False
}
