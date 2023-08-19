//// Lustre is a declarative framework for building Web apps in Gleam. 

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{Decoder}
import gleam/map.{Map}
import lustre/effect.{Effect}
import lustre/element.{Element}

// TYPES -----------------------------------------------------------------------

/// An `App` describes a Lustre application: what state it holds and what kind
/// of actions get dispatched to update that model. The only useful thing you can
/// do with an `App` is pass it to [`start`](#start).
///
/// You can construct an `App` from the two constructors exposed in this module:
/// [`basic`](#basic) and [`application`](#application). Although you can't do
/// anything but [`start`](#start) them, the constructors are separated in case
/// you want to set up an application but defer starting it until some later point
/// in time.
///
/// ```text
///                                          +--------+
///                                          |        |
///                                          | update |
///                                          |        |
///                                          +--------+
///                                            ^    |
///                                            |    | 
///                                        Msg |    | #(Model, Effect(Msg))
///                                            |    |
///                                            |    v
/// +------+                         +------------------------+
/// |      |  #(Model, Effect(Msg))  |                        |
/// | init |------------------------>|     Lustre Runtime     |
/// |      |                         |                        |
/// +------+                         +------------------------+
///                                            ^    |
///                                            |    |
///                                        Msg |    | Model
///                                            |    |
///                                            |    v
///                                          +--------+
///                                          |        |
///                                          | view |
///                                          |        |
///                                          +--------+
/// ```
///
pub type App(flags, model, msg)

pub type Error {
  AppAlreadyStarted
  AppNotYetStarted
  ComponentAlreadyRegistered
  ElementNotFound
  NotABrowser
}

// CONSTRUCTORS ----------------------------------------------------------------

@target(javascript)
/// Create a basic lustre app that just views some element on the page.
/// Note that this doesn't mean the content is static! With `element.stateful`
/// you can still create components with local state.
///
/// Basic lustre apps don't have any *global* application state and so the
/// plumbing is a lot simpler. If you find yourself passing lots of state around,
/// you might want to consider using [`simple`](#simple) or [`application`](#application)
/// instead.
///
/// ```gleam
/// import lustre
/// import lustre/element
/// 
/// pub fn main () {
///   let app = lustre.element(
///     element.h1([], [
///       element.text("Hello, world!") 
///     ])
///   )
/// 
///   assert Ok(_) = lustre.start(app, "#root")
/// }
/// ```
///
pub fn element(element: Element(msg)) -> App(Nil, Nil, msg) {
  let init = fn(_) { #(Nil, effect.none()) }
  let update = fn(_, _) { #(Nil, effect.none()) }
  let view = fn(_) { element }

  application(init, update, view)
}

@target(javascript)
/// If you start off with a simple `[element`](#element) app, you may find
/// yourself leaning on [`stateful`](./lustrel/element.html#stateful) elements
/// to manage model used throughout your app. If that's the case or if you know
/// you need some global model from the get-go, you might want to construct a 
/// [`simple`](#simple) app instead.
///
/// This is one app constructor that allows your HTML elements to dispatch actions
/// to update your program model. 
///
/// ```gleam
/// import gleam/int
/// import lustre
/// import lustre/element
/// import lustre/event
/// 
/// type Msg {
///   Decr
///   Incr
/// }
/// 
/// pub fn main () {
///   let init = 0
///
///   let update = fn (model, msg) {
///     case msg {
///       Decr -> model - 1
///       Incr -> model + 1
///     }
///   }
///
///   let view = fn (model) {
///     element.div([], [
///       element.button([ event.on_click(Decr) ], [
///         element.text("-")
///       ]),
///
///       element.text(int.to_string(model)),
///
///       element.button([ event.on_click(Incr) ], [
///         element.text("+")
///       ])
///     ])
///   }
/// 
///   let app = lustre.simple(init, update, view)
///   assert Ok(_) = lustre.start(app, "#root")
/// }
/// ```
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

@target(javascript)
/// An evolution of a [`simple`](#simple) app that allows you to return a
/// [`Effect`](./lustre/effect.html#Effect) from your `init` and `update`s. Commands give
/// us a way to perform side effects like sending an HTTP request or running a
/// timer and then dispatch actions back to the runtime to trigger an `update`.
///
///```
/// import lustre
/// import lustre/effect
/// import lustre/element
/// 
/// pub fn main () {
///   let init = #(0, tick())
///
///   let update = fn (model, msg) {
///     case msg {
///       Tick -> #(model + 1, tick())
///     }
///   }
///
///   let view = fn (model) {
///     element.div([], [
///       element.text("Time elapsed: ")
///       element.text(int.to_string(model))
///     ])
///   }
///   
///   let app = lustre.simple(init, update, view)
///   assert Ok(_) = lustre.start(app, "#root")   
/// }
/// 
/// fn tick () -> Effect(Msg) {
///   effect.from(fn (dispatch) {
///     setInterval(fn () {
///       dispatch(Tick)
///     }, 1000)
///   })
/// }
/// 
/// external fn set_timeout (f: fn () -> a, delay: Int) -> Nil 
///   = "" "window.setTimeout"
///```
///
@external(javascript, "./lustre.ffi.mjs", "setup")
pub fn application(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> App(flags, model, msg)

@target(javascript)
@external(javascript, "./lustre.ffi.mjs", "setup_component")
pub fn component(
  name: String,
  init: fn() -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  on_attribute_change: Map(String, Decoder(msg)),
) -> Result(Nil, Error)

// EFFECTS ---------------------------------------------------------------------

@target(javascript)
/// Once you have created a app with either `basic` or `application`, you 
/// need to actually start it! This function will mount your app to the DOM
/// node that matches the query selector you provide.
///
/// If everything mounted OK, we'll get back a dispatch function that you can 
/// call to send actions to your app and trigger an update. 
///
///```
/// import lustre
///
/// pub fn main () {
///   let app = lustre.appliation(init, update, view)
///   assert Ok(dispatch) = lustre.start(app, "#root")
/// 
///   dispatch(Incr)
///   dispatch(Incr)
///   dispatch(Incr)
/// }
///```
///
/// This may not seem super useful at first, but by returning this dispatch
/// function from your `main` (or elsewhere) you can get events into your Lustre
/// app from the outside world.
///
@external(javascript, "./lustre.ffi.mjs", "start")
pub fn start(
  app: App(flags, model, msg),
  selector: String,
  flags: flags,
) -> Result(fn(msg) -> Nil, Error)

// UTILS -----------------------------------------------------------------------

@external(javascript, "./lustre.ffi.mjs", "is_browser")
pub fn is_browser() -> Bool {
  False
}
