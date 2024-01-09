//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre](https://lustre.build/api/lustre)

// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor.{type StartError}
import gleam/result
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/runtime

// TYPES -----------------------------------------------------------------------

pub opaque type App(runtime, flags, model, msg) {
  App(
    init: fn(flags) -> #(model, Effect(msg)),
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    on_attribute_change: Dict(String, Decoder(msg)),
  )
}

pub type ServerComponent

pub type Browser

pub type WebComponent

pub type Action(runtime, msg) =
  runtime.Action(runtime, msg)

pub type Error {
  ActorError(StartError)
  BadComponentName
  ComponentAlreadyRegistered
  ElementNotFound
  NotABrowser
  NotErlang
}

// CONSTRUCTORS ----------------------------------------------------------------

///
/// 
pub fn element(element: Element(msg)) -> App(Browser, Nil, Nil, msg) {
  let init = fn(_) { #(Nil, effect.none()) }
  let update = fn(_, _) { #(Nil, effect.none()) }
  let view = fn(_) { element }

  application(init, update, view)
}

///
/// 
pub fn simple(
  init: fn(flags) -> model,
  update: fn(model, msg) -> model,
  view: fn(model) -> Element(msg),
) -> App(Browser, flags, model, msg) {
  let init = fn(flags) { #(init(flags), effect.none()) }
  let update = fn(model, msg) { #(update(model, msg), effect.none()) }

  application(init, update, view)
}

///
/// 
pub fn application(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> App(Browser, flags, model, msg) {
  App(init, update, view, dict.new())
}

///
/// 
pub fn component(
  init: fn() -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  on_attribute_change: Dict(String, Decoder(msg)),
) -> App(WebComponent, Nil, model, msg) {
  App(fn(_) { init() }, update, view, on_attribute_change)
}

///
/// 
pub fn server_component(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> App(ServerComponent, flags, model, msg) {
  App(init, update, view, dict.new())
}

// EFFECTS ---------------------------------------------------------------------

///
/// 
pub fn start(
  app: App(Browser, flags, model, msg),
  selector: String,
  flags: flags,
) -> Result(fn(Action(Browser, msg)) -> Nil, Error) {
  use <- bool.guard(!is_browser(), Error(NotABrowser))
  do_start(app, selector, flags)
}

@external(javascript, "./client-runtime.ffi.mjs", "start")
fn do_start(
  _app: App(Browser, flags, model, msg),
  _selector: String,
  _flags: flags,
) -> Result(fn(Action(Browser, msg)) -> Nil, Error) {
  panic
}

///
///
@external(javascript, "./server-runtime.ffi.mjs", "start")
pub fn start_server_component(
  app: App(ServerComponent, flags, model, msg),
  flags: flags,
) -> Result(fn(Action(ServerComponent, msg)) -> Nil, Error) {
  use runtime <- result.map(start_actor(app, flags))
  actor.send(runtime, _)
}

///
/// 
pub fn start_actor(
  app: App(ServerComponent, flags, model, msg),
  flags: flags,
) -> Result(Subject(Action(ServerComponent, msg)), Error) {
  do_start_actor(app, flags)
}

@target(javascript)
fn do_start_actor(_, _) {
  Error(NotErlang)
}

@target(erlang)
fn do_start_actor(
  app: App(ServerComponent, flags, model, msg),
  flags: flags,
) -> Result(Subject(Action(ServerComponent, msg)), Error) {
  app.init(flags)
  |> runtime.start(app.update, app.view)
  |> result.map_error(ActorError)
}

///
/// 
@external(javascript, "./client-component.ffi.mjs", "register")
pub fn register(
  _app: App(WebComponent, Nil, model, msg),
  _name: String,
) -> Result(Nil, Error) {
  Error(NotABrowser)
}

// ACTIONS ---------------------------------------------------------------------

pub fn add_renderer(
  renderer: fn(Element(msg)) -> Nil,
) -> Action(ServerComponent, msg) {
  runtime.AddRenderer(renderer)
}

pub fn dispatch(msg: msg) -> Action(runtime, msg) {
  runtime.Dispatch(msg)
}

pub fn event(name: String, data: Dynamic) -> Action(ServerComponent, msg) {
  runtime.Event(name, data)
}

pub fn remove_renderer(
  renderer: fn(Element(msg)) -> Nil,
) -> Action(ServerComponent, msg) {
  runtime.RemoveRenderer(renderer)
}

pub fn shutdown() -> Action(runtime, msg) {
  runtime.Shutdown
}

// UTILS -----------------------------------------------------------------------

///
@external(javascript, "./client-runtime.ffi.mjs", "is_browser")
pub fn is_browser() -> Bool {
  False
}

///
@external(javascript, "./client-runtime.ffi.mjs", "is_registered")
pub fn is_registered(_name: String) -> Bool {
  False
}
