//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre](https://lustre.build/api/lustre)

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{Decoder}
import gleam/map.{Map}
import lustre/effect.{Effect}
import lustre/element.{Element}

// TYPES -----------------------------------------------------------------------

@target(javascript)
///
pub type App(flags, model, msg)

@target(erlang)
///
pub opaque type App(flags, model, msg) {
  App(
    init: fn(flags) -> #(model, Effect(msg)),
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
  )
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
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> App(flags, model, msg) {
  App(init, update, view)
}

@external(javascript, "./lustre.ffi.mjs", "setup_component")
pub fn component(
  name: String,
  init: fn() -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  on_attribute_change: Map(String, Decoder(msg)),
) -> Result(Nil, Error) {
  Ok(Nil)
}

// EFFECTS ---------------------------------------------------------------------

///
@external(javascript, "./lustre.ffi.mjs", "start")
pub fn start(
  app: App(flags, model, msg),
  selector: String,
  flags: flags,
) -> Result(fn(msg) -> Nil, Error) {
  Error(NotABrowser)
}

///
@external(javascript, "./lustre.ffi.mjs", "destroy")
pub fn destroy(app: App(flags, model, msg)) -> Result(Nil, Error) {
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
