//// Lustre is a framework for rendering Web applications and components using
//// Gleam. This module contains the core API for constructing and communicating
//// with Lustre applications. If you're new to Lustre or frontend development in
//// general, make sure you check out the [examples](https://github.com/lustre-labs/lustre/tree/main/examples)
//// or the [quickstart guide]() to get up to speed!
////
//// Lustre currently has three kinds of application:
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
//// 3. A server component. These are applications that run anywhere Gleam runs
////    and communicate with any number of connected clients by sending them
////    patches to apply to their DOM.
////
////    There are two pieces to a server component: the main server component
////    runtime that contains your application logic, and a client-side runtime
////    that listens for patches over a WebSocket and applies them to the DOM.
////
////    The server component runtime can run anywhere Gleam does, but the
////    client-side runtime must be run in a browser. To use it either render the
////    [provided script element](./lustre/server_component.html#script) or use the script files
////    from Lustre's `priv/` directory directly.
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
//// The `Effect` type here encompasses things like HTTP requests and other kinds
//// of communication with the "outside world". You can read more about effects
//// and their purpose in the [`effect`](./effect.html) module.
////
//// For many kinds of app, you can take these three building blocks and put
//// together a Lustre application capable of running *anywhere*. Beacuse of that,
//// we like to describe Lustre as a **universal framework**.
////
//// ## Guides
////
//// A number of guides have been written to teach you how to use Lustre to build
//// different kinds of applications. If you're just getting started with Lustre
//// or frontend development, we recommend reading through them in order:
////
//// - [`01-quickstart`](/guide/01-quickstart.html)
////
//// This list of guides is likely to grow over time, so be sure to check back
//// every now and then to see what's new!
////
//// ## Examples
////
//// If you prefer to learn by seeing and adapting existing code, there are also
//// a number of examples in the [Lustre GitHub repository](https://github.com/lustre-labs/lustre)
//// that each demonstrate a different concept or idea:
////
//// - [`01-hello-world`](https://github.com/lustre-labs/lustre/tree/main/examples/01-hello-world)
//// - [`02-interactivity`](https://github.com/lustre-labs/lustre/tree/main/examples/02-interactivity)
//// - [`03-controlled-inputs`](https://github.com/lustre-labs/lustre/tree/main/examples/03-controlled-inputs)
//// - [`04-custom-event-handlers`](https://github.com/lustre-labs/lustre/tree/main/examples/04-custom-event-handlers)
//// - [`05-http-requests`](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests)
//// - [`06-custom-effects`](https://github.com/lustre-labs/lustre/tree/main/examples/06-custom-effects)
////
//// This list of examples is likely to grow over time, so be sure to check back
//// every now and then to see what's new!
////
//// ## Companion libraries
////
//// While this package contains the runtime and API necessary for building and
//// rendering applications, there is also a small collection of companion libraries
//// built to make building Lustre applications easier:
////
//// - [lustre/ui](https://github.com/lustre-labs/ui) is a collection of pre-designed
////   elements and design tokens for building user interfaces with Lustre.
////
//// - [lustre/ssg](https://github.com/lustre-labs/ssg) is a simple static site
////   generator that you can use to produce static HTML documents from your Lustre
////   applications.
////
//// Both of these packages are heavy works in progress: any feedback or contributions
//// are very welcome!
////
////
//// ## Getting help
////
//// If you're having trouble with Lustre or not sure what the right way to do
//// something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
//// You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
////
//// While our docs are still a work in progress, the official [Elm guide](https://guide.elm-lang.org)
//// is also a great resource for learning about the Model-View-Update architecture
//// and the kinds of patterns that Lustre is built around.
////
//// ## Contributing
////
//// The best way to contribute to Lustre is by building things! If you've built
//// something cool with Lustre you want to share then please share it on the
//// `#sharing` channel in the  [Gleam Discord server](https://discord.gg/Fm8Pwmy).
//// You can also tag Hayleigh on Twitter [@hayleigh-dot-dev](https://twitter.com/hayleighdotdev)
//// or on BlueSky [@hayleigh.dev](https://bsky.app/profile/hayleigh.dev).
////
//// If you run into any issues or have ideas for how to improve Lustre, please
//// open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
//// Fixes and improvements to the documentation are also very welcome!
////
//// Finally, if you'd like, you can support the project through
//// [GitHub Sponsors](https://github.com/sponsors/hayleigh-dot-dev). Sponsorship
//// helps fund the copious amounts of coffee that goes into building and maintaining
//// Lustre, and is very much appreciated!
////

// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{type StartError}
import gleam/result
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/internals/patch
import lustre/internals/runtime

// TYPES -----------------------------------------------------------------------

/// Represents a constructed Lustre application that is ready to be started.
/// Depending on where you want the application to run, you have a few options:
///
/// - Use [`start`](#start) to start a single-page-application in the browser.
///
///   This is the most common way to start a Lustre application. If you're new to
///   Lustre or frontend development in general, make sure you check out the
///   [examples](https://github.com/lustre-labs/lustre/tree/main/examples) or the
///   [quickstart guide]()
///
/// - Use [`start_server_component`](#start_server_component) to start a Lustre
///   Server Component anywhere Gleam will run: Erlang, Node, Deno, or in the
///   browser. If you're running on the BEAM though, you should...
///
/// - Use [`start_actor`](#start_actor) to start a Lustre Server Component only
///   for the Erlang target. BEAM users should always prefer this over
///   `start_server_component` so they can take advantage of OTP features.
///
/// - Use [`register`](#register) to register a component in the browser to be
///   used as a Custom Element. This is useful even if you're not using Lustre
///   to build a SPA.
///
/// If you're only interested in using Lustre as a HTML templating engine, you
/// don't need an `App` at all! You can render an element directly using the
/// [`element.to_string`](./lustre/element.html#to_string) function.
///
pub opaque type App(flags, model, msg) {
  App(
    init: fn(flags) -> #(model, Effect(msg)),
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    // The `dict.mjs` module in the standard library is huge (20+kb!). For folks
    // that don't ever build components and don't use a dictionary in any of their
    // code we'd rather not thrust that increase in bundle size on them just to
    // call `dict.new()`.
    //
    // Using `Option` here at least lets us say `None` for the empty case in the
    // `application` constructor.
    //
    on_attribute_change: Option(Dict(String, Decoder(msg))),
  )
}

/// The `ClientSpa` runtime is the most typical kind of Lustre application: it's
/// a single-page application that runs in the browser similar to React or Elm.
///
/// This type is used to tag the [`Action`](#Action) type to stop you accidentally
/// sending actions to the wrong kind of runtime.
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
/// This type is used to tag the [`Action`](#Action) type to stop you accidentally
/// sending actions to the wrong kind of runtime.
///
pub type ServerComponent

/// An action represents a message that can be sent to a running Lustre application.
/// Code that is orchestrating an application where Lustre is only one part of the
/// system will likely want to send actions to the Lustre runtime. For most kinds of
/// application, you can usually ignore actions entirely.
///
/// The `msg` type parameter is the kind of messages you can send to the runtime's
/// `update` function through the [`dispatch`](#dispatch) action.
///
/// The `runtime` type parameter represents the type of Lustre application that
/// can receive this action. If we [`start`](#start) a typical Lustre SPA, we
/// get back the type `Result(fn(Action(msg, ClientSpa)) -> Nil, Error)`. This
/// means we can only send actions suitable for the [`ClientSpa`](#ClientSpa)
/// runtime, and trying to send actions like [`add_renderer`](#add_renderer) would
/// result in a type error.
///
pub type Action(msg, runtime) =
  runtime.Action(msg, runtime)

/// Patches are sent by server components to any connected renderers. Because
/// server components are not opinionated about your network layer or how your
/// wider application is organised, it is your responsibility to make sure a `Patch`
/// makes its way to the server component client runtime.
///
pub type Patch(msg) =
  patch.Patch(msg)

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
/// > **Note**: Just because an element doesn't have its own update loop, doesn't
/// > mean its content is always static! An element application may render a client
/// > or server component that has its own encapsulated update loop!
///
pub fn element(html: Element(msg)) -> App(Nil, Nil, msg) {
  let init = fn(_) { #(Nil, effect.none()) }
  let update = fn(_, _) { #(Nil, effect.none()) }
  let view = fn(_) { html }

  application(init, update, view)
}

/// A `simple` application has the basic Model-View-Update building blocks present
/// in all Lustre applications, but it cannot handle effects. This is a great way
/// to learn the basics of Lustre and its architecture.
///
/// Once you're comfortable with the Model-View-Update loop and want to start
/// building more complex applications that can communicate with the outside world,
/// you'll want to use the [`application`](#application) constructor instead.
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

/// A complete Lustre application that follows the Model-View-Update architecture
/// and can handle side effects like HTTP requests or querying the DOM. Most real
/// Lustre applications will use this constructor.
///
/// To learn more about effects and their purpose, take a look at the
/// [`effect`](./lustre/effect.html) module or the
/// [HTTP requests example](https://github.com/lustre-labs/lustre/tree/main/examples/05-http-requests).
///
pub fn application(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
) -> App(flags, model, msg) {
  App(init, update, view, None)
}

/// A `component` is a type of Lustre application designed to be embedded within
/// another application and has its own encapsulated update loop. This constructor
/// is almost identical to the [`application`](#application) constructor, but it
/// also allows you to specify a dictionary of attribute names and decoders.
///
/// When a component is rendered in a parent application, it can receive data from
/// the parent application through HTML attributes and properties just like any
/// other HTML element. This dictionary of decoders allows you to specify how to
/// decode those attributes into messages your component's update loop can handle.
///
/// **Note**: Lustre components are conceptually a lot "heavier" than components
/// in frameworks like React. They should be used for more complex UI widgets
/// like a combobox with complex keyboard interactions rather than simple things
/// like buttons or text inputs. Where possible try to think about how to build
/// your UI with simple view functions (functions that return [Elements](./lustre/element.html#Element))
/// and only reach for components when you really need to encapsulate that update
/// loop.
///
pub fn component(
  init: fn(flags) -> #(model, Effect(msg)),
  update: fn(model, msg) -> #(model, Effect(msg)),
  view: fn(model) -> Element(msg),
  on_attribute_change: Dict(String, Decoder(msg)),
) -> App(flags, model, msg) {
  App(init, update, view, Some(on_attribute_change))
}

// EFFECTS ---------------------------------------------------------------------

/// Start a constructed application as a client-side single-page application (SPA).
/// This is the most typical way to start a Lustre application and will *only* work
/// in the browser
///
/// The second argument is a [CSS selector](https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector)
/// used to locate the DOM element where the application will be mounted on to.
/// The most common selectors are `"#app"` to target an element with an id of `app`
/// or `[data-lustre-app]` to target an element with a `data-lustre-app` attribute.
///
/// The third argument is the starting data for the application. This is passed
/// to the application's `init` function.
///
pub fn start(
  app: App(flags, model, msg),
  onto selector: String,
  with flags: flags,
) -> Result(fn(Action(msg, ClientSpa)) -> Nil, Error) {
  use <- bool.guard(!is_browser(), Error(NotABrowser))
  do_start(app, selector, flags)
}

@external(javascript, "./client-runtime.ffi.mjs", "start")
fn do_start(
  _app: App(flags, model, msg),
  _selector: String,
  _flags: flags,
) -> Result(fn(Action(msg, ClientSpa)) -> Nil, Error) {
  // It should never be possible for the body of this function to execute on the
  // Erlang target because the `is_browser` guard will prevent it. Instead of
  // a panic, we still return a well-typed `Error` here in the case where someone
  // mistakenly uses this function internally.
  Error(NotABrowser)
}

/// Start an application as a server component. This runs in a headless mode and
/// doesn't render anything to the DOM. Instead, multiple clients can be attached
/// using the [`add_renderer`](#add_renderer) action.
///
/// If a server component starts successfully, this function will return a callback
/// that can be used to send actions to the component runtime.
///
/// A server component will keep running until the program is terminated or the
/// [`shutdown`](#shutdown) action is sent to it.
///
/// **Note**: Users running their application on the BEAM should use [`start_actor`](#start_actor)
/// instead to make use of Gleam's OTP abstractions.
///
@external(javascript, "./server-runtime.ffi.mjs", "start")
pub fn start_server_component(
  app: App(flags, model, msg),
  with flags: flags,
) -> Result(fn(Action(msg, ServerComponent)) -> Nil, Error) {
  use runtime <- result.map(start_actor(app, flags))
  actor.send(runtime, _)
}

/// Start an application as a server component specifically for the Erlang target.
/// Instead of receiving a callback on successful start, this function returns
/// a [`Subject`](https://hexdocs.pm/gleam_erlang/gleam/erlang/process.html#Subject)
///
///
/// **Note**: This function is only meaningful on the Erlang target. Attempts to
/// call it on the JavaScript will result in the `NotErlang` error. If you're running
/// a Lustre server component on Node or Deno, use [`start_server_component`](#start_server_component)
/// instead.
///
pub fn start_actor(
  app: App(flags, model, msg),
  with flags: flags,
) -> Result(Subject(Action(msg, ServerComponent)), Error) {
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
) -> Result(Subject(Action(msg, ServerComponent)), Error) {
  let on_attribute_change = option.unwrap(app.on_attribute_change, dict.new())

  app.init(flags)
  |> runtime.start(app.update, app.view, on_attribute_change)
  |> result.map_error(ActorError)
}

/// Register a Lustre application as a Web Component. This lets you render that
/// application in another Lustre application's view or use it as a Custom Element
/// outside of Lustre entirely.The provided application can only have `Nil` flags
/// because there is no way to provide an initial value for flags when using a
/// Custom Element!
///
/// The second argument is the name of the Custom Element. This is the name you'd
/// use in HTML to render the component. For example, if you register a component
/// with the name `my-component`, you'd use it in HTML by writing `<my-component>`
/// or in Lustre by rendering `element("my-component", [], [])`.
///
/// **Note**: There are [some rules](https://developer.mozilla.org/en-US/docs/Web/API/CustomElementRegistry/define#valid_custom_element_names)
/// for what names are valid for a Custom Element. The most important one is that
/// the name *must* contain a hypen so that it can be distinguished from standard
/// HTML elements.
///
/// **Note**: This function is only meaningful when running in the browser and will
/// produce a `NotABrowser` error if called anywhere else. For server contexts,
/// you can render a Lustre server component using [`start_server_component`](#start_server_component)
/// or [`start_actor`](#start_actor) instead.
///
@external(javascript, "./client-component.ffi.mjs", "register")
pub fn register(_app: App(Nil, model, msg), _name: String) -> Result(Nil, Error) {
  Error(NotABrowser)
}

// ACTIONS ---------------------------------------------------------------------

/// Dispatch a message to a running application's `update` function. This can be
/// used as a way for the outside world to communicate with a Lustre app without
/// the app needing to initiate things with an effect.
///
/// Both client SPAs and server components can have messages sent to them using
/// the `dispatch` action.
///
pub fn dispatch(msg: msg) -> Action(msg, runtime) {
  runtime.Dispatch(msg)
}

/// Instruct a running application to shut down. For client SPAs this will stop
/// the runtime and unmount the app from the DOM. For server components, this will
/// stop the runtime and prevent any further patches from being sent to connected
/// clients.
///
pub fn shutdown() -> Action(msg, runtime) {
  runtime.Shutdown
}

// UTILS -----------------------------------------------------------------------

/// Gleam's conditional compilation makes it possible to have different implementations
/// of a function for different targets, but it's not possible to know what runtime
/// you're targetting at compile-time.
///
/// This is problematic if you're using server components with a JavaScript
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
pub fn is_registered(_name: String) -> Bool {
  False
}
