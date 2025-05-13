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
import lustre/vdom/events
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
pub opaque type App(args, model, msg) {
  App(
    init: fn(args) -> #(model, Effect(msg)),
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
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
pub opaque type Simulation(model, msg) {
  Simulation(
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    history: List(Event(msg)),
    model: model,
    html: Element(msg),
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
pub type Event(msg) {
  Dispatch(message: msg)
  Event(target: Query, name: String, data: Json)
  EventTargetNotFound(matching: Query)
  EventHandlerNotFound(target: Query, name: String)
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
  update update: fn(model, msg) -> model,
  view view: fn(model) -> Element(msg),
) -> App(args, model, msg) {
  App(
    init: fn(args) { #(init(args), effect.none()) },
    update: fn(model, msg) { #(update(model, msg), effect.none()) },
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
  init init: fn(args) -> #(model, Effect(msg)),
  update update: fn(model, msg) -> #(model, Effect(msg)),
  view view: fn(model) -> Element(msg),
) -> App(args, model, msg) {
  App(init:, update:, view:)
}

// RUNNING SIMULATIONS ---------------------------------------------------------

/// Start a simulated Lustre application. Once a simulation is running you can
/// use the [`message`](#message) and [`event`](#event) functions to simulate
/// events
///
pub fn start(app: App(args, model, msg), args: args) -> Simulation(model, msg) {
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
pub fn message(
  simulation: Simulation(model, msg),
  msg: msg,
) -> Simulation(model, msg) {
  let #(model, _) = simulation.update(simulation.model, msg)
  let html = simulation.view(model)
  let history = [Dispatch(message: msg), ..simulation.history]

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
  simulation: Simulation(model, msg),
  on query: Query,
  name event: String,
  data payload: List(#(String, Json)),
) -> Simulation(model, msg) {
  let result = {
    use #(_, path) <- result.try(query.find_path(
      in: simulation.html,
      matching: query,
      // In Lustre's vdom the path always starts with `0` to represent the root
      // node, so we need to account for that here.
      //
      // TODO: maybe Lustre's internal `path` module should account for that
      // automatically?
      //
      from: path.root |> path.add(0, ""),
    ))

    let events = events.from_node(simulation.html)
    let data = json.object(payload)

    use msg <- result.try(
      events.handle(
        events,
        path.to_string(path),
        event,
        data
          |> json.to_string
          |> json.parse(decode.dynamic)
          |> result.unwrap(erase(Nil)),
      )
      |> pair.second
      |> result.replace_error(Nil),
    )

    let #(model, _) = simulation.update(simulation.model, msg)
    let html = simulation.view(model)
    let history = [
      Event(target: query, name: event, data:),
      ..simulation.history
    ]

    Ok(Simulation(..simulation, history:, model:, html:))
  }

  case result {
    Ok(simulation) -> simulation

    Error(_) ->
      Simulation(..simulation, history: [
        EventTargetNotFound(matching: query),
        ..simulation.history
      ])
  }
}

/// A convenience function that simulates a click event on the first element
/// matching the given query. This event will have no payload and is only
/// appropriate for event handlers that use Lustre's `on_click` handler or custom
/// handlers that do not decode the event payload.
///
pub fn click(
  simulation: Simulation(model, msg),
  on query: Query,
) -> Simulation(model, msg) {
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
  simulation: Simulation(model, msg),
  on query: Query,
  value value: String,
) -> Simulation(model, msg) {
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
  simulation: Simulation(model, msg),
  on query: Query,
  fields form_data: List(#(String, String)),
) -> Simulation(model, msg) {
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

// INTROSPECTION ---------------------------------------------------------------

/// Introspect the current `model` of a running simulation. This can be useful
/// to debug why a simulation is not producing the view you expect.
///
pub fn model(simulation: Simulation(model, msg)) -> model {
  simulation.model
}

/// Introspect the current `view` of a running simulation. Typically you would
/// use this with a snapshot testing library like [`birdie`](https://hexdocs.pm/birdie/index.html)
/// or with the [`query`](./query.html) api to make assertions about the state of
/// the page.
///
pub fn view(simulation: Simulation(model, msg)) -> Element(msg) {
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
pub fn history(simulation: Simulation(model, msg)) -> List(Event(msg)) {
  simulation.history |> list.reverse
}

// UTILS -----------------------------------------------------------------------

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn erase(value: a) -> Dynamic
