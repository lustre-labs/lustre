//// Lustre is a framework for rendering Web applications and components using
//// Gleam. This module contains the core API for constructing and communicating
//// with the different kinds of Lustre application.
//// 
//// Lustre currently has three kinds of application:
//// 
//// 1. A `Browser` single-page application: think Elm or React or Vue. These
////    are applications that run in the client's browser and are responsible for
////    rendering the entire page.
//// 
//// 2. A `WebComponent`: an encapsulated Lustre application that can be embedded
////    in a larger Lustre application or bundled and used as a standard Custom
////    Element in any other Web application.
//// 
//// 3. A `ServerComponent`: A Lustre application that runs on the server and
////    computes diffs that can be sent to a browser to be rendered. This is
////    most-similar to Phoenix LiveView.
//// 
//// No matter where a Lustre application runs, it will always follow the same
//// Model-View-Update architecture. Popularised by Elm (where it is known as The
//// Elm Architecture), this pattern has since made its way into many other
//// languages and frameworks and has proven to be a robust and reliable way to
//// build complex user interfaces.
//// 
//// There are three main building blocks to the Model-View-Update architecture:
//// 
//// - A `Model` that represents your application's state and an `init` function
////   to create it.
//// 
//// - A `Msg` type that represents all the different ways the outside world can
////   communicate with your application and an `update` function that modifies
////   your model in response to those messages.
//// 
//// - A `view` function that renders your model to HTML, represented as an
////   `Element`.
//// 
//// To see how those pieces fit together, here's a little diagram:
//// 
//// ```text
////                                          +--------+
////                                          |        |
////                                          | update |
////                                          |        |
////                                          +--------+
////                                            ^    |
////                                            |    |
////                                        Msg |    | #(Model, Effect(Msg))
////                                            |    |
////                                            |    v
//// +------+                         +------------------------+
//// |      |  #(Model, Effect(Msg))  |                        |
//// | init |------------------------>|     Lustre Runtime     |
//// |      |                         |                        |
//// +------+                         +------------------------+
////                                            ^    |
////                                            |    |
////                                        Msg |    | Model
////                                            |    |
////                                            |    v
////                                          +--------+
////                                          |        |
////                                          |  view  |
////                                          |        |
////                                          +--------+
//// ```
//// 
//// ❓ Wondering what that [`Effect`](./effect#effect-type) is all about? Check
////    out the documentation for that over in the [`effect`](./effect) module.
//// 
//// For many types of program, you can easily take these building blocks and
//// construct any of Lustre's application types with little-to-no modification.
//// We like to describe Lustre as a **universal framework**. 
//// 
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
import lustre/element.{type Element, type Patch}
import lustre/runtime

// TYPES -----------------------------------------------------------------------

/// Represents a constructed Lustre application that is ready to be started.
/// Depending on the kind of application you've constructed you have a few 
/// options:
/// 
/// - Use [`start`](#start) to start a `Browser` application on the client.
/// 
/// - Use [`start_server_component`](#start_server_component) to start a 
///   `ServerComponent` anywhere: Erlang, Node, Deno, in the browser.
/// 
/// - Use [`start_actor`](#start_actor) to start a `ServerComponent` specifically
///   for the Erlang target. You'll want to use this one to properly make use of
///   OTP features.
/// 
/// - Use [`register`](#register) to register a `WebComponent` in the browser to
///   be used as a Custom Element.
/// 
/// 💡 That `runtime` type parameter tells you what sort of application this is.
///    It's actually a [phantom type](https://hayleigh-dot-dev.github.io/blog/phantom-types-in-gleam/)
///    that doesn't exist when your program is running. It's used to make sure you
///    don't try and use a `Browser` application on the server. 
/// 
pub opaque type App(runtime, flags, model, msg) {
  App(
    init: fn(flags) -> #(model, Effect(msg)),
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    on_attribute_change: Dict(String, Decoder(msg)),
  )
}

/// A `ServerComponent` is a type of Lustre application that does not directly
/// render anything to the DOM. Instead, it can run anywhere Gleam runs and 
/// operates in a "headless" mode where it computes diffs between renders and
/// sends them to any number of connected listeners.
/// 
/// Lustre Server Components are not tied to any particular transport or network
/// protocol, but they are most commonly used with WebSockets in a fashion similar
/// to Phoenix LiveView.
/// 
pub type ServerComponent

/// The `Browser` runtime is the most typical kind of Lustre application: it's
/// a single-page application that runs in the browser similar to React or Vue.
/// 
pub type Browser

/// A `WebComponent` is an encapsulated Lustre application that runs inside a
/// custom HTML element. These runtimes are excellent for complex stateful widgets
/// that you don't want to manage from a `Browser` application's main update loop.
/// 
/// The Lustre CLI also provides a way for you to bundle a `WebComponent` (or
/// multiple components) into a single JavaScript file that you can drop into any
/// other Web application to use your component as a standard Custom Element.
/// 
pub type WebComponent

/// An action represents a message that can be sent to (some types of) a running
/// Lustre application. Like the [`App`](#App) type, the `runtime` type parameter
/// can be used to determine what kinds of application a particular action can be
/// sent to.
/// 
/// 
/// 
pub type Action(runtime, msg) =
  runtime.Action(runtime, msg)

/// Starting a Lustre application might fail for a number of reasons. This error
/// type enumerates all those reasons, even though some of them are only possible
/// on certain targets.
/// 
/// This generally makes error handling simpler than having to worry about a bunch
/// of different error types and potentially unifying them yourself.
/// 
pub type Error {
  ActorError(StartError)
  BadComponentName
  ComponentAlreadyRegistered
  ElementNotFound
  NotABrowser
  NotErlang
}

// CONSTRUCTORS ----------------------------------------------------------------

/// An element is the simplest type of Lustre application. It renders its contents
/// once and does not handle any messages or effects. Often this type of application
/// is used for folks just getting started with Lustre on the frontend and want a
/// quick way to get something on the screen.
/// 
/// Take a look at the [`simple`](#simple) application constructor if you want to
/// build something interactive.
/// 
/// 💡 Just because an element doesn't have its own update loop, doesn't mean its
///    content is always static! An element application may render a component or
///    server component that has its own encapsulated update loop!
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
  on_attribute_change: Dict(String, Decoder(msg)),
) -> App(ServerComponent, flags, model, msg) {
  App(init, update, view, on_attribute_change)
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
fn do_start(_app: App(Browser, flags, model, msg), _selector: String, _flags: flags) -> Result(
  fn(Action(Browser, msg)) -> Nil,
  Error,
) {
  // It should never be possible for the body of this function to execute on the
  // Erlang target because the `is_browser` guard will prevent it. Instead of
  // a panic, we still return a well-typed `Error` here in the case where someone
  // mistakenly uses this function internally.
  Error(NotABrowser)
}

///
///
@external(javascript, "./server-runtime.ffi.mjs", "start")
pub fn start_server_component(app: App(ServerComponent, flags, model, msg), flags: flags) -> Result(
  fn(Action(ServerComponent, msg)) -> Nil,
  Error,
) {
  use runtime <- result.map(start_actor(app, flags))
  actor.send(runtime, _)
}

///
/// 
/// 🚨 This function is only meaningful on the Erlang target. Attempts to call
/// it on the JavaScript will result in the `NotErlang` error. If you're running
/// a Lustre Server Component on Node or Deno, use 
/// [`start_server_component`](#start_server_component) instead.
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
  |> runtime.start(app.update, app.view, app.on_attribute_change)
  |> result.map_error(ActorError)
}

///
/// 
@external(javascript, "./client-component.ffi.mjs", "register")
pub fn register(_app: App(WebComponent, Nil, model, msg), _name: String) -> Result(
  Nil,
  Error,
) {
  Error(NotABrowser)
}

// ACTIONS ---------------------------------------------------------------------

pub fn add_renderer(
  id: any,
  renderer: fn(Patch(msg)) -> Nil,
) -> Action(ServerComponent, msg) {
  runtime.AddRenderer(dynamic.from(id), renderer)
}

pub fn dispatch(msg: msg) -> Action(runtime, msg) {
  runtime.Dispatch(msg)
}

pub fn event(name: String, data: Dynamic) -> Action(ServerComponent, msg) {
  runtime.Event(name, data)
}

pub fn remove_renderer(id: any) -> Action(ServerComponent, msg) {
  runtime.RemoveRenderer(dynamic.from(id))
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
