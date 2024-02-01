//// Lustre is a framework for rendering Web applications and components using
//// Gleam. This module contains the core API for constructing and communicating
//// with the different kinds of Lustre application.
////
//// Lustre currently has two kinds of application:
////
//// 1. A client-side single-page application: think Elm or React or Vue. These
////    are applications that run in the client's browser and are responsible for
////    rendering the entire page.
////
//// 2. A client-side component: an encapsulated Lustre application that can be
////    rendered inside another Lustre application as a Web Component. Communication
////    happens via attributes and event listeners, like any other encapsulated
////    HTML element.
////
//// 3. A Lustre Server Component. These are applications that run anywhere Gleam
////    runs and communicate with any number of connected clients by sending them
////    patches to apply to their DOM.
////
////    On the server, these applications can be communicated with by sending them
////    messages directly. On the client communication happens the same way as
////    client-side components: through attributes and event listeners.
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
//// For many kinds of app, you can take these three building blocks and put
//// together a Lustre application capable of running *anywhere*. We like to
//// describe Lustre as a **universal framework**.
////
//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre](https://lustre.build/api/lustre)

// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import argv
import glint
import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor.{type StartError}
import gleam/result
import lustre/effect.{type Effect}
import lustre/element.{type Element, type Patch}
import lustre/internals/runtime
import lustre/cli/add
import lustre/cli/build
import lustre/cli/try

// MAIN ------------------------------------------------------------------------

/// This function exists so you can run helpful Lustre utilities from the command
/// line using `gleam run -m lustre`. For a proper help message run:
///
/// ```sh
/// gleam run -m lustre -- --help
/// ```
///
/// 🚨 If you're just using Lustre as a library, *you can ignore this function*.
///
pub fn main() {
  let args = argv.load().arguments

  glint.new()
  |> glint.as_gleam_module
  |> glint.with_name("lustre")
  |> glint.add(at: ["add", "esbuild"], do: add.esbuild())
  |> glint.add(at: ["build", "app"], do: build.app())
  |> glint.add(at: ["build", "component"], do: build.component())
  |> glint.add(at: ["try"], do: try.run())
  |> glint.run(args)
}

// TYPES -----------------------------------------------------------------------

/// Represents a constructed Lustre application that is ready to be started.
/// Depending on the kind of application you've constructed you have a few
/// options:
///
/// - Use [`start`](#start) to start a single-page-application in the browser.
///
/// - Use [`start_server_component`](#start_server_component) to start a Lustre
///   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the
///   browser.
///
/// - Use [`start_actor`](#start_actor) to start a Lustre Server Component only
///   for the Erlang target. BEAM users should always prefer this over
///   `start_server_component` so they can take advantage of OTP features.
///
/// - Use [`register`](#register) to register a component in the browser to be
///   used as a Custom Element. This is useful even if you're not using Lustre
///   to build a SPA.
///
pub opaque type App(flags, model, msg) {
  App(
    init: fn(flags) -> #(model, Effect(msg)),
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    on_attribute_change: Dict(String, Decoder(msg)),
  )
}

/// The `Browser` runtime is the most typical kind of Lustre application: it's
/// a single-page application that runs in the browser similar to React or Vue.
///
pub type ClientSpa

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
pub type Error {
  ActorError(StartError)
  BadComponentName(name: String)
  ComponentAlreadyRegistered(name: String)
  ElementNotFound(selector: String)
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
pub fn element(html: Element(msg)) -> App(Nil, Nil, msg) {
  let init = fn(_) { #(Nil, effect.none()) }
  let update = fn(_, _) { #(Nil, effect.none()) }
  let view = fn(_) { html }

  application(init, update, view)
}

///
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
///
pub fn application(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> App(flags, model, msg) {
  App(init, update, view, dict.new())
}

///
///
pub fn component(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  on_attribute_change: Dict(String, Decoder(msg)),
) -> App(flags, model, msg) {
  App(init, update, view, on_attribute_change)
}

// EFFECTS ---------------------------------------------------------------------

///
///
pub fn start(
  app: App(flags, model, msg),
  onto selector: String,
  with flags: flags,
) -> Result(fn(Action(ClientSpa, msg)) -> Nil, Error) {
  use <- bool.guard(!is_browser(), Error(NotABrowser))
  do_start(app, selector, flags)
}

@external(javascript, "./client-runtime.ffi.mjs", "start")
fn do_start(
  _app: App(flags, model, msg),
  _selector: String,
  _flags: flags,
) -> Result(fn(Action(ClientSpa, msg)) -> Nil, Error) {
  // It should never be possible for the body of this function to execute on the
  // Erlang target because the `is_browser` guard will prevent it. Instead of
  // a panic, we still return a well-typed `Error` here in the case where someone
  // mistakenly uses this function internally.
  Error(NotABrowser)
}

///
///
@external(javascript, "./server-runtime.ffi.mjs", "start")
pub fn start_server_component(
  app: App(flags, model, msg),
  with flags: flags,
) -> Result(fn(Action(ServerComponent, msg)) -> Nil, Error) {
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
  app: App(flags, model, msg),
  with flags: flags,
) -> Result(Subject(Action(ServerComponent, msg)), Error) {
  do_start_actor(app, flags)
}

@target(javascript)
fn do_start_actor(_, _) {
  Error(NotErlang)
}

@target(erlang)
fn do_start_actor(
  app: App(flags, model, msg),
  flags: flags,
) -> Result(Subject(Action(ServerComponent, msg)), Error) {
  app.init(flags)
  |> runtime.start(app.update, app.view, app.on_attribute_change)
  |> result.map_error(ActorError)
}

/// Register a Lustre application as a Web Component. This lets you render that
/// application in another Lustre application's view or use it as a Custom Element
/// outside of Lustre entirely.
///
/// 💡 The provided application can only have `Nil` flags, because there is no way
/// to specify flags when the component is first rendered.
///
/// 💡 There are [some rules](https://developer.mozilla.org/en-US/docs/Web/API/CustomElementRegistry/define#valid_custom_element_names)
/// for what names are valid for a Custom Element. The most important one is that
/// the name *must* contain a hypen so that it can be distinguished from standard
/// HTML elements.
///
/// 🚨 This function is only meaningful when running in the browser. For server
/// contexts, you can render a Lustre Server Component using `start_server_component`
/// or `start_actor` instead.
///
@external(javascript, "./client-component.ffi.mjs", "register")
pub fn register(app: App(Nil, model, msg), name: String) -> Result(Nil, Error) {
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

/// Gleam's conditional compilation makes it possible to have different implementations
/// of a function for different targets, but it's not possible to know what runtime
/// you're targetting at compile-time.
///
/// This is problematic if you're using Lustre Server Components with a JavaScript
/// backend because you'll want to know whether you're currently running on your
/// server or in the browser: this function tells you that!
///
@external(javascript, "./client-runtime.ffi.mjs", "is_browser")
pub fn is_browser() -> Bool {
  False
}

/// Check if the given component name has already been registered as a Custom
/// Element. This is particularly useful in contexts where _other web components_
/// may have been registered and you must avoid collisions.
///
@external(javascript, "./client-runtime.ffi.mjs", "is_registered")
pub fn is_registered(name: String) -> Bool {
  False
}
