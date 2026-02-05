//// Lustre is a framework for rendering Web applications and components using
//// Gleam. This module contains the core API for constructing and communicating
//// with Lustre applications. If you're new to Lustre or frontend development in
//// general, make sure you check out the [examples](https://github.com/lustre-labs/lustre/tree/main/examples)
//// or the [quickstart guide](./guide/01-quickstart.html) to get up to speed!
////
//// Lustre currently has three kinds of application:
////
//// 1. A client-side single-page application: think Elm or React or Vue. These
////    are applications that run in the client's browser and are responsible for
////    rendering the entire page.
////
//// 2. A client-side component: an encapsulated Lustre application that can be
////    rendered inside another Lustre application as a Web Component. Communication
////    happens via attributes and event listeners, like any other HTML element.
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
////    client-side runtime must be run in a browser. To use it, either render the
////    [provided script element](./lustre/server_component.html#script) or serve
////    the pre-bundled scripts found in Lustre's `priv/` directory directly.
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
//// - A `message` type that represents all the different ways the outside world can
////   communicate with your application and an `update` function that modifies
////   your model in response to those messages.
////
//// - A `view` function that renders your model to HTML, represented as an
////   `Element`.
////
//// To see how those pieces fit together, here's a little diagram:
////
//// ```text
////                                            +--------+
////                                            |        |
////                                            | update |
////                                            |        |
////                                            +--------+
////                                              ^    |
////                                              |    |
////                                      message |    | #(Model, Effect(message))
////                                              |    |
////                                              |    v
//// +------+                           +------------------------+
//// |      | #(Model, Effect(message)) |                        |
//// | init |-------------------------->|     Lustre Runtime     |
//// |      |                           |                        |
//// +------+                           +------------------------+
////                                              ^    |
////                                              |    |
////                                      message |    | Model
////                                              |    |
////                                              |    v
////                                            +--------+
////                                            |        |
////                                            |  view  |
////                                            |        |
////                                            +--------+
//// ```
////
//// The `Effect` type here encompasses things like HTTP requests and other kinds
//// of communication with the "outside world". You can read more about effects
//// and their purpose in the [`effect`](./effect.html) module.
////
//// For many kinds of apps, you can take these three building blocks and put
//// together a Lustre application capable of running *anywhere*. Because of that,
//// we like to describe Lustre as a **universal framework**.
////
//// ## Guides
////
//// A number of guides have been written to teach you how to use Lustre to build
//// different kinds of applications. If you're just getting started with Lustre
//// or frontend development, we recommend reading through them in order:
////
//// - [`01-quickstart`](./guide/01-quickstart.html)
//// - [`02-state-management`](./guide/02-state-management.html)
//// - [`03-side-effects`](./guide/03-side-effects.html)
//// - [`04-spa-deployments`](./guide/04-spa-deployments.html)
//// - [`05-server-side-rendering`](./guide/05-server-side-rendering.html)
//// - [`06-full-stack-applications`](./guide/06-full-stack-applications.html)
//// - [`07-full-stack-deployments`](./guide/07-full-stack-deployments.html)
//// - [`08-components`](./guide/08-components.html)
//// - [`09-server-components`](./guide/09-server-components.html)
////
//// This list of guides is likely to grow over time, so be sure to check back
//// every now and then to see what's new!
////
//// ## Examples
////
//// If you prefer to learn by seeing and adapting existing code, there are also
//// a number of examples in the [Lustre GitHub repository](https://github.com/lustre-labs/lustre)
//// that each demonstrate a different concept or idea. While we can't list them
//// all here, some of the more important ones are:
////
//// - [`Controlled inputs`](https://github.com/lustre-labs/lustre/tree/main/examples/02-inputs/01-controlled-inputs)
//// - [`Handling forms`](https://github.com/lustre-labs/lustre/tree/main/examples/02-inputs/04-forms)
//// - [`Making HTTP requests`](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/01-http-requests)
//// - [`Routing`](https://github.com/lustre-labs/lustre/tree/main/examples/04-applications/01-routing)
//// - [`Creating components`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/01-basic-setup)
//// - [`Creating server components`](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/01-basic-setup)
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
//// ## Getting help
////
//// If you're having trouble with Lustre or not sure what the right way to do
//// something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
//// You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
////
//// ## Contributing
////
//// The best way to contribute to Lustre is by building things! If you've built
//// something cool with Lustre you want to share then please share it on the
//// `#sharing` channel in the  [Gleam Discord server](https://discord.gg/Fm8Pwmy).
//// You can also tag Hayleigh on BlueSky [@hayleigh.dev](https://bsky.app/profile/hayleigh.dev).
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
import gleam/erlang/process.{type Name, type Subject}
import gleam/option
import gleam/otp/actor
import gleam/otp/factory_supervisor.{type Builder}
import gleam/otp/supervision.{type ChildSpecification}
import lustre/component.{type Option}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/runtime/app.{App}
import lustre/runtime/server/runtime

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
///   browser.
///
/// - Use [`register`](#register) to register a component in the browser to be
///   used as a Custom Element. This is useful even if you're not using Lustre
///   to build a SPA.
///
/// If you're only interested in using Lustre as a HTML templating engine, you
/// don't need an `App` at all! You can render an element directly using the
/// [`element.to_string`](./lustre/element.html#to_string) function.
///
pub type App(arguments, model, message) =
  app.App(arguments, model, message)

/// Starting a Lustre application might fail for a number of reasons. This error
/// type enumerates all those reasons, even though some of them are only possible
/// on certain targets.
///
pub type Error {
  ActorError(reason: actor.StartError)
  BadComponentName(name: String)
  ComponentAlreadyRegistered(name: String)
  ElementNotFound(selector: String)
  NotABrowser
}

/// Once you start a Lustre application, you get back a `Runtime` you can later
/// use to send messages to your application using the [`dispatch`](#dispatch)
/// function.
///
pub type Runtime(message)

/// From outside your Lustre applications, it is possible to communicate with the
/// runtime by sending more than just messages to your app's `update` function.
/// Communication to the runtime itself is mediated by the `RuntimeMessage` type
/// and can be constructed in a few different ways:
///
/// - [`dispatch`](#dispatch) lets you send a message to your app's `update`
///   function the same way effects do.
///
/// - [`shutdown`](#shutdown) instructs a running application to stop and clean
///   up. For JavaScript applications, this is often imperfect and may leave an
///   empty shell app running. For Erlang server components, this will stop the
///   process.
///
/// - When running a server component, you can decode messages from the client
///   runtime using [`runtime_message_decoder`](./lustre/server_component.html#runtime_message_decoder)
///   and [`send`](#send) them manually.
///
pub type RuntimeMessage(message) =
  runtime.Message(message)

// CONSTRUCTORS ----------------------------------------------------------------

/// The simplest type of Lustre application. The `element` application is
/// primarily used for demonstration purposes. It renders a static Lustre `Element`
/// on the page and does not have any state or update logic.
///
pub fn element(view: Element(message)) -> App(arguments, Nil, message) {
  application(
    init: fn(_) { #(Nil, effect.none()) },
    update: fn(_, _) { #(Nil, effect.none()) },
    view: fn(_) { view },
  )
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
  init init: fn(arguments) -> model,
  update update: fn(model, message) -> model,
  view view: fn(model) -> Element(message),
) -> App(arguments, model, message) {
  let init = fn(arguments) { #(init(arguments), effect.none()) }
  let update = fn(model, message) { #(update(model, message), effect.none()) }

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
  init init: fn(arguments) -> #(model, Effect(message)),
  update update: fn(model, message) -> #(model, Effect(message)),
  view view: fn(model) -> Element(message),
) -> App(arguments, model, message) {
  App(name: option.None, init:, update:, view:, config: app.default_config)
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
/// > **Note**: Lustre components are conceptually a lot "heavier" than components
/// > in frameworks like React. They should be used for more complex UI widgets
/// > like a combobox with complex keyboard interactions rather than simple things
/// > like buttons or text inputs. Where possible try to think about how to build
/// > your UI with simple view functions (functions that return [Elements](./lustre/element.html#Element))
/// > and only reach for components when you really need to encapsulate that update
/// > loop.
///
pub fn component(
  init init: fn(arguments) -> #(model, Effect(message)),
  update update: fn(model, message) -> #(model, Effect(message)),
  view view: fn(model) -> Element(message),
  options options: List(Option(message)),
) -> App(arguments, model, message) {
  App(name: option.None, init:, update:, view:, config: app.configure(options))
}

/// Assign a [`Name`](https://hexdocs.pm/gleam_erlang/gleam/erlang/process.html#Name)
/// to a Lustre application. This is useful for [_supervised_](#supervised) server
/// components as it allows other processes to find and communicate with the
/// runtime even if it is restarted.
///
/// > **Note**: names must **never** be created dynamically as too many names
/// > will exhaust the atom table and cause the VM to crash. Names should be
/// > created at the start of your program and passed down where needed.
///
/// > **Note**: a named application should **never** be used to create a
/// > [factory supervisor](#factory) as only one process can be registered under
/// > a given name.
///
pub fn named(
  app: App(arguments, model, message),
  name: Name(RuntimeMessage(message)),
) -> App(arguments, model, message) {
  App(..app, name: option.Some(name))
}

// RUNTIME ---------------------------------------------------------------------

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
  app: App(arguments, model, message),
  onto selector: String,
  with arguments: arguments,
) -> Result(Runtime(message), Error) {
  use <- bool.guard(!is_browser(), Error(NotABrowser))

  do_start(app, selector, arguments)
}

@external(javascript, "./lustre/runtime/client/spa.ffi.mjs", "start")
fn do_start(
  _app: App(arguments, model, message),
  _selector: String,
  _arguments: arguments,
) -> Result(Runtime(message), Error) {
  Error(NotABrowser)
}

/// Start an application as a server component. This runs in a headless mode and
/// doesn't render anything to the DOM. Instead, clients must be connected to the
/// server component through [`register_callback`](./lustre/server_component.html#register_callback)
/// or [`register_subject`](./lustre/server_component.html#register_subject) to
/// receive patches and updates.
///
/// If a server component starts successfully, this function will return a
/// [`Runtime`](#Runtime) that can be used to send messages to the running application
/// using the [`send`](#send) function.
///
/// A server component will keep running until the program is terminated or the
/// [`shutdown`](#shutdown) action is sent to it.
///
/// > **Note**: applications targeting Erlang should strongly prefer the
/// > [`supervised`](#supervised) or [`factory`](#factory) functions to ensure
/// > proper supervision and fault-tolerance.
///
/// On the Erlang target, the runtime's [`Subject`](https://hexdocs.pm/gleam_erlang/gleam/erlang/process.html#Subject)
/// and [`Pid`](https://hexdocs.pm/gleam_erlang/gleam/erlang/process.html#Pid)
/// can be recovered using [`server_component.subject`](./lustre/server_component.html#subject)
/// and [`server_component.pid`](./lustre/server_component.html#pid) respectively.
///
@external(javascript, "./lustre/runtime/server/runtime.ffi.mjs", "start")
pub fn start_server_component(
  app: App(arguments, model, message),
  with arguments: arguments,
) -> Result(Runtime(message), Error) {
  let result =
    runtime.start(
      app.name,
      app.init,
      app.update,
      app.view,
      app.configure_server_component(app.config),
      arguments,
    )

  case result {
    Ok(actor.Started(data: subject, ..)) -> Ok(hide_subject(subject))
    Error(error) -> Error(ActorError(error))
  }
}

/// Create a server component child specification suitable for supervision in a
/// [static supervisor](https://hexdocs.pm/gleam_otp/gleam/otp/static_supervisor.html).
/// This is the preferred way of starting Lustre server components on the Erlang
/// target.
///
pub fn supervised(
  app: App(arguments, model, message),
  arguments: arguments,
) -> ChildSpecification(Subject(RuntimeMessage(message))) {
  use <- supervision.worker

  runtime.start(
    app.name,
    app.init,
    app.update,
    app.view,
    app.configure_server_component(app.config),
    arguments,
  )
}

/// Create a [factory supervisor](https://hexdocs.pm/gleam_otp/gleam/otp/factory_supervisor.html)
/// capable of starting many instances of a Lustre server component dynamically.
/// Along with [`supervised`](#supervised), this is one of the ways to ensure
/// proper supervision and fault-tolerance for Lustre server components on the
/// Erlang target.
///
pub fn factory(
  app: App(arguments, model, message),
) -> Builder(arguments, Subject(RuntimeMessage(message))) {
  use arguments <- factory_supervisor.worker_child

  runtime.start(
    app.name,
    app.init,
    app.update,
    app.view,
    app.configure_server_component(app.config),
    arguments,
  )
}

/// Register a Lustre application as a Web Component. This lets you render that
/// application in another Lustre application's view or use it as a Custom Element
/// outside of Lustre entirely.The provided application can only have `Nil` start
/// arguments because there is no way to provide an initial value for arguments
/// when using a Custom Element!
///
/// The second argument is the name of the Custom Element. This is the name you'd
/// use in HTML to render the component. For example, if you register a component
/// with the name `my-component`, you'd use it in HTML by writing `<my-component>`
/// or in Lustre by rendering `element("my-component", [], [])`.
///
/// > **Note**: There are [some rules](https://developer.mozilla.org/en-US/docs/Web/API/CustomElementRegistry/define#valid_custom_element_names)
/// > for what names are valid for a Custom Element. The most important one is that
/// > the name *must* contain a hypen so that it can be distinguished from standard
/// > HTML elements.
///
/// > **Note**: This function is only meaningful when running in the browser and will
/// > produce a `NotABrowser` error if called anywhere else. For server contexts,
/// > you can render a Lustre server component using [`start_server_component`](#start_server_component)
/// > or [`start_actor`](#start_actor) instead.
///
@external(javascript, "./lustre/runtime/client/component.ffi.mjs", "make_component")
pub fn register(
  _app: App(Nil, model, message),
  _name: String,
) -> Result(Nil, Error) {
  Error(NotABrowser)
}

// MESSAGES --------------------------------------------------------------------

/// Send a message to a running application's runtime directly. This function is
/// primarily used for sending decoded client messages to a server component's
/// runtime.
///
@external(erlang, "gleam@erlang@process", "send")
@external(javascript, "./lustre/runtime/client/runtime.ffi.mjs", "send")
pub fn send(
  to runtime: Runtime(message),
  message message: RuntimeMessage(message),
) -> Nil

/// Build a message for a running application's `update` function.
///
/// This message can be delivered to the runtime using [`send`](#send), allowing
/// communication with a Lustre app without having to use an effect.
///
pub fn dispatch(message: message) -> RuntimeMessage(message) {
  runtime.EffectDispatchedMessage(message)
}

/// Instruct a running application to shut down. For client SPAs this will stop
/// the runtime and unmount the app from the DOM. For server components, this will
/// stop the runtime and prevent any further patches from being sent to connected
/// clients.
///
pub fn shutdown() -> RuntimeMessage(message) {
  runtime.SystemRequestedShutdown
}

// UTILS -----------------------------------------------------------------------

/// Gleam's conditional compilation makes it possible to have different implementations
/// of a function for different targets, but it's not possible to know what runtime
/// you're targeting at compile-time.
///
/// This is problematic if you're using server components with a JavaScript
/// backend because you'll want to know whether you're currently running on your
/// server or in the browser: this function tells you that!
///
@external(javascript, "./lustre/runtime/client/runtime.ffi.mjs", "is_browser")
pub fn is_browser() -> Bool {
  False
}

/// Check if the given component name has already been registered as a Custom
/// Element. This is particularly useful in contexts where _other web components_
/// may have been registered and you must avoid collisions.
///
@external(javascript, "./lustre/runtime/client/runtime.ffi.mjs", "is_registered")
pub fn is_registered(_name: String) -> Bool {
  False
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../gleam_stdlib/gleam/function.mjs", "identity")
fn hide_subject(subject: Subject(RuntimeMessage(message))) -> Runtime(message)
