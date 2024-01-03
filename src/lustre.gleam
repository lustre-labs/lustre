//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre](https://lustre.build/api/lustre)

// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor.{type StartError}
import gleam/result
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/server/runtime.{type Message}

// TYPES -----------------------------------------------------------------------

pub opaque type App(flags, model, msg) {
  App(
    init: fn(flags) -> #(model, Effect(msg)),
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
  )
}

pub type Error {
  ActorError(StartError)
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

/// Construct a Lustre [`App`](#App) that can perform side effects.
/// 
/// 
/// 
/// 
@external(javascript, "./lustre.ffi.mjs", "setup")
pub fn application(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> App(flags, model, msg) {
  component(init, update, view)
}

@target(javascript)
///
/// 
@external(javascript, "./lustre.ffi.mjs", "setup_component")
pub fn component(
  _name: String,
  _init: fn() -> #(model, Effect(msg)),
  _update: fn(model, msg) -> #(model, Effect(msg)),
  _view: fn(model) -> Element(msg),
  _on_attribute_change: Dict(String, Decoder(msg)),
) -> Result(Nil, Error) {
  Ok(Nil)
}

@target(erlang)
///
/// 
pub fn component(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> App(flags, model, msg) {
  App(init, update, view)
}

// EFFECTS ---------------------------------------------------------------------

@target(javascript)
///
@external(javascript, "./lustre.ffi.mjs", "start")
pub fn start(
  _app: App(flags, model, msg),
  _selector: String,
  _flags: flags,
) -> Result(fn(msg) -> Nil, Error) {
  Error(NotABrowser)
}

@target(erlang)
///
pub fn start(
  app: App(flags, model, msg),
  flags: flags,
) -> Result(Subject(Message(msg)), Error) {
  app.init(flags)
  |> runtime.start(app.update, app.view)
  |> result.map_error(ActorError)
}

@target(javascript)
///
/// 
@external(javascript, "./lustre.ffi.mjs", "destroy")
pub fn destroy(_app: App(flags, model, msg)) -> Result(Nil, Error) {
  Error(NotABrowser)
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
