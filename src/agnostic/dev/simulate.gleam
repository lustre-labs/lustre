// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json.{type Json}
import gleam/list
import gleam/pair
import gleam/result
import lustre/dev/query.{type Query}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/vdom/cache
import lustre/vdom/path

// TYPES -----------------------------------------------------------------------

/// A simulated [`lustre.App`](https://hexdocs.pm/lustre/lustre.html#App) ready
/// to be started. This module exposes constructor functions that mirrors those
/// provided by the main `lustre` module:
///
/// - [`simple`](#simple)
///
/// - [`application`](#application)
///
/// > **Note**: running a simulated app is not the same as running a real app!
/// > Any effects that would normally be run after update will be discarded. If
/// > you want to simulate messages coming from the outside world, you can use
/// > the [`message`](#message) or [`event`](#event) functions.
///
pub opaque type App(args, model, message) {
  App(
    init: fn(args) -> #(model, Effect(message)),
    update: fn(model, message) -> #(model, Effect(message)),
    view: fn(model) -> Element(message),
  )
}

/// A running simulation of a Lustre application, produced by calling
/// [`start`](#start).This is similar to the [`Runtime`](https://hexdocs.pm/lustre/lustre.html#Runtime)
/// type and both DOM events and messages dispatched by effects can be simulated
/// using the [`event`](#event) and [`message`](#message) functions respectively.
///
/// Each simulated event returns an updated simulation, making it convenient to
/// pipe multiple events in sequence.
///
pub opaque type Simulation(model, message) {
  Simulation(
    update: fn(model, message) -> #(model, Effect(message)),
    view: fn(model) -> Element(message),
    history: List(Event(message)),
    model: model,
    html: Element(message),
  )
}

/// The simulation keeps a log of events that occur. This includes both simulated
/// DOM events and messages dispatched using the [`event`](#event) and
/// [`message`](#message) functions but also entries for failures like an event
/// target not existing in the current view or an event that was fired but not
/// handled.
///
/// The event log is primarily useful for debugging simulations that don't produce
/// the results you expect. You can use the [`history`](#history) function to
/// introspect a simulation for this event log.
///
pub type Event(message) {
  Dispatch(message: message)
  Event(target: Query, name: String, data: Json)
  Problem(name: String, message: String)
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Construct a simulated simple Lustre application. The simulation can be started
/// with the [`start`](#start) function by providing the initial arguments for
/// your app's `init` function.
///
/// DOM events and messages dispatched by effects can be simulated using the
/// [`event`](#event) and [`messgae`](#message) functions.
///
pub fn simple(
  init init: fn(args) -> model,
  update update: fn(model, message) -> model,
  view view: fn(model) -> Element(message),
) -> App(args, model, message) {
  App(
    init: fn(args) { #(init(args), effect.none()) },
    update: fn(model, message) { #(update(model, message), effect.none()) },
    view:,
  )
}

/// Construct a simulated Lustre application. The simulation can be started
/// with the [`start`](#start) function by providing the initial arguments for
/// your app's `init` function.
///
/// DOM events and messages dispatched by effects can be simulated using the
/// [`event`](#event) and [`messgae`](#message) functions.
///
/// > **Note**: simulated apps do not run any effects! You can simulate the result
/// > of an effect by using the [`message`](#message) function, but to test side
/// > effects you should test your application in a real environment.
///
pub fn application(
  init init: fn(args) -> #(model, Effect(message)),
  update update: fn(model, message) -> #(model, Effect(message)),
  view view: fn(model) -> Element(message),
) -> App(args, model, message) {
  App(init:, update:, view:)
}

// RUNNING SIMULATIONS ---------------------------------------------------------

/// Start a simulated Lustre application. Once a simulation is running you can
/// use the [`message`](#message) and [`event`](#event) functions to simulate
/// events
///
pub fn start(
  app: App(args, model, message),
  args: args,
) -> Simulation(model, message) {
  let #(model, _) = app.init(args)
  let html = app.view(model)

  Simulation(update: app.update, view: app.view, history: [], model:, html:)
}

/// Simulate a message sent directly to the runtime. This is often used to mimic
/// the result of some effect you would have run in a real environment. For example,
/// you might simulate a click event on a login button and then simulate the
/// successful response from the server by calling this function with the message
/// you would dispatch from the effect:
///
/// ```gleam
/// import birdie
/// import lustre/dev/simulate
/// import lustre/dev/query
/// import lustre/element
///
/// pub fn login_test() {
///   let app = simulate.application(init:, update:, view:)
///   let login_button = query.element(matching: query.id("login"))
///   let user = User(name: "Lucy")
///
///   simulate.start(app, Nil)
///   |> simulate.event(on: login_button, name: "click", data: [])
///   // Simulate a successful response from the server
///   |> simulate.message(ApiReturnedUser(Ok(user)))
///   |> simulate.view
///   |> element.to_readable_string
///   |> birdie.snap("Successful login")
/// }
/// ```
///
/// > **Note**: your app's `view` function will probably be rendering quite a lot
/// > of HTML! To make your snapshots more meaningful, you might want to couple
/// > this with the [`query`](./query.html) module to only snapshot parts of the
/// > page that are relevant to the test.
///
pub fn message(
  simulation: Simulation(model, message),
  message: message,
) -> Simulation(model, message) {
  let #(model, _) = simulation.update(simulation.model, message)
  let html = simulation.view(model)
  let history = [Dispatch(message: message), ..simulation.history]

  Simulation(..simulation, history:, model:, html:)
}

/// Simulate a DOM event on the first element that matches the given query. The
/// payload represents a simulated event object, and should be used to pass data
/// you expect your event handlers to decode.
///
/// If no element matches the query, an [`EventTargetNotFound`](#Event) event is
/// logged in the simulation history. If an element is found, but the application
/// has no handler for the event, the [`EventHandlerNotFound`](#Event) event is
/// logged instead.
///
/// > **Note**: this is not a perfect simulation of a real DOM event. There is no
/// > capture phase of a simulated event and simulated events will not bubble up
/// > to parent elements.
///
pub fn event(
  simulation: Simulation(model, message),
  on query: Query,
  name event: String,
  data payload: List(#(String, Json)),
) -> Simulation(model, message) {
  let result = {
    use #(_, path) <- result.try(result.replace_error(
      query.find_path(
        in: simulation.html,
        matching: query,
        from: path.root,
        index: 0,
      ),
      problem(
        simulation,
        name: "EventTargetNotFound",
        message: "No element matching " <> query.to_readable_string(query),
      ),
    ))

    let events = cache.from_node(simulation.html)
    let data = json.object(payload)

    use handler <- result.try(result.replace_error(
      pair.second(cache.handle(
        events,
        path.to_string(path),
        event,
        data
          |> json.to_string
          |> json.parse(decode.dynamic)
          |> result.unwrap(erase(Nil)),
      )),
      problem(
        simulation,
        name: "EventHandlerNotFound",
        message: "No "
          <> event
          <> " handler for element matching "
          <> query.to_readable_string(query),
      ),
    ))

    let #(model, _) = simulation.update(simulation.model, handler.message)
    let html = simulation.view(model)
    let history = [
      Event(target: query, name: event, data:),
      ..simulation.history
    ]

    Ok(Simulation(..simulation, history:, model:, html:))
  }

  case result {
    Ok(simulation) -> simulation
    Error(problem) -> problem
  }
}

/// A convenience function that simulates a click event on the first element
/// matching the given query. This event will have no payload and is only
/// appropriate for event handlers that use Lustre's `on_click` handler or custom
/// handlers that do not decode the event payload.
///
pub fn click(
  simulation: Simulation(model, message),
  on query: Query,
) -> Simulation(model, message) {
  event(simulation, on: query, name: "click", data: [])
}

/// Simulate an input event on the first element matching the given query. This
/// helper has an event payload that looks like this:
///
/// ```json
/// {
///   "target": {
///     "value": value
///   }
/// }
/// ```
///
/// and is appropriate for event handlers that use Lustre's `on_input` handler
/// or custom handlers that only decode the event target value.
///
pub fn input(
  simulation: Simulation(model, message),
  on query: Query,
  value value: String,
) -> Simulation(model, message) {
  event(simulation, on: query, name: "input", data: [
    #("target", json.object([#("value", json.string(value))])),
  ])
}

/// Simulate a submit event on the first element matching the given query. The
/// simulated event payload looks like this:
///
/// ```json
/// {
///   "detail": {
///     "formData": [
///       ...
///     ]
///   }
/// }
/// ```
///
/// and is appropriate for event handlers that use Lustre's `on_submit` handler
/// or custom handlers that only decode the non-standard `detail.formData`
/// property.
///
pub fn submit(
  simulation: Simulation(model, message),
  on query: Query,
  fields form_data: List(#(String, String)),
) -> Simulation(model, message) {
  event(simulation, on: query, name: "submit", data: [
    #(
      "detail",
      json.object([
        #(
          "formData",
          json.array(form_data, fn(entry) {
            json.preprocessed_array([json.string(entry.0), json.string(entry.1)])
          }),
        ),
      ]),
    ),
  ])
}

/// Log a problem that occured during the simulation. This function is useful for
/// external packages that want to provide functions to simulate certain effects
/// that may fail in the real world. For example, a routing package may log a
/// problem if a link has an invalid `href` attribute that would cause no message
/// to be dispatched.
///
/// > **Note**: logging a problem will not stop the simulation from running, just
/// > like a real application!
///
pub fn problem(
  simulation: Simulation(model, message),
  name name: String,
  message message: String,
) -> Simulation(model, message) {
  let history = [Problem(name:, message:), ..simulation.history]

  Simulation(..simulation, history:)
}

// INTROSPECTION ---------------------------------------------------------------

/// Introspect the current `model` of a running simulation. This can be useful
/// to debug why a simulation is not producing the view you expect.
///
pub fn model(simulation: Simulation(model, message)) -> model {
  simulation.model
}

/// Introspect the current `view` of a running simulation. Typically you would
/// use this with a snapshot testing library like [`birdie`](https://hexdocs.pm/birdie/index.html)
/// and/or with the [`query`](./query.html) api to make assertions about the state
/// of the page.
///
pub fn view(simulation: Simulation(model, message)) -> Element(message) {
  simulation.html
}

/// Receive the current [`Event`](#Event) log of a running simulation. You can
/// use this to produce more detailed snapshots by also rendering the sequence of
/// events that produced the given view.
///
/// In addition to simulated DOM events and message dispatch, the event log will
/// also include entries for when the queried event target could not be found in
/// the view and cases where an event was fired but not handled by your application.
///
pub fn history(simulation: Simulation(model, message)) -> List(Event(message)) {
  simulation.history |> list.reverse
}

// UTILS -----------------------------------------------------------------------

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn erase(value: a) -> Dynamic
