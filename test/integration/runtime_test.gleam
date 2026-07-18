// IMPORTS ---------------------------------------------------------------------

@target(erlang)
import agnostic
@target(erlang)
import agnostic/effect
@target(erlang)
import agnostic/element/html
@target(erlang)
import agnostic/event
@target(erlang)
import agnostic/internals/mutable_map
@target(erlang)
import agnostic/platform
@target(erlang)
import agnostic/platform/dom
@target(erlang)
import agnostic/runtime/headless
@target(erlang)
import agnostic/runtime/transport
@target(erlang)
import agnostic/server_component
@target(erlang)
import agnostic/vdom/patch
@target(erlang)
import agnostic/vdom/path
@target(erlang)
import gleam/dict
@target(erlang)
import gleam/dynamic
@target(erlang)
import gleam/erlang/process
@target(erlang)
import gleam/int
@target(erlang)
import gleam/json
@target(erlang)
import lustre_test

// CLIENT INTERACTION TESTS ----------------------------------------------------

@target(erlang)
pub fn client_connect_test() {
  use <- lustre_test.test_filter("client_connect_test")
  use client, _ <- with_erlang_runtime

  assert process.receive_forever(client)
    == transport.mount(
      True,
      True,
      [],
      [],
      [],
      dict.new(),
      view(0),
      mutable_map.new(),
    )
}

@target(erlang)
pub fn client_send_event_test() {
  use <- lustre_test.test_filter("client_send_event_test")
  use client, runtime <- with_erlang_runtime

  // Discard the `Mount` message
  let _ = process.receive_forever(client)

  let click = transport.event_fired(incr, "click", dynamic.nil())

  headless.ClientDispatchedMessage(click) |> agnostic.send(to: runtime)

  let patch =
    patch.new(0, 0, [patch.replace_text("1")], [])
    |> patch.add_parent(1)
    |> patch.add_parent(0)
    |> patch.add_parent(0)

  assert process.receive_forever(client)
    == transport.reconcile(patch, mutable_map.new())
}

@target(erlang)
pub fn client_send_multiple_events_test() {
  use <- lustre_test.test_filter("client_send_multiple_events_test")
  use client, runtime <- with_erlang_runtime

  // Discard the `Mount` message
  let _ = process.receive_forever(client)

  let click = transport.event_fired(incr, "click", dynamic.nil())

  headless.ClientDispatchedMessage(click) |> agnostic.send(to: runtime)
  headless.ClientDispatchedMessage(click) |> agnostic.send(to: runtime)

  // Discard the first `Reconcile` message
  let _ = process.receive_forever(client)

  let patch =
    patch.new(0, 0, [patch.replace_text("2")], [])
    |> patch.add_parent(1)
    |> patch.add_parent(0)
    |> patch.add_parent(0)

  assert process.receive_forever(client)
    == transport.reconcile(patch, mutable_map.new())
}

// EFFECT MESSAGE TESTS --------------------------------------------------------

@target(erlang)
pub fn effect_send_event_test() {
  use <- lustre_test.test_filter("effect_send_event_test")
  use client, runtime <- with_erlang_runtime

  // Discard the `Mount` message
  let _ = process.receive_forever(client)

  headless.EffectDispatchedMessage(Incr)
  |> agnostic.send(to: runtime)

  let patch =
    patch.new(0, 0, [patch.replace_text("1")], [])
    |> patch.add_parent(1)
    |> patch.add_parent(0)
    |> patch.add_parent(0)

  assert process.receive_forever(client)
    == transport.reconcile(patch, mutable_map.new())
}

// SERVER MESSAGE TESTS --------------------------------------------------------

@target(erlang)
pub fn server_emit_event_test() {
  use <- lustre_test.test_filter("server_emit_event_test")
  use client, runtime <- with_erlang_runtime

  // Discard the `Mount` message
  let _ = process.receive_forever(client)

  let click = transport.event_fired(reset, "click", dynamic.nil())

  headless.ClientDispatchedMessage(click) |> agnostic.send(to: runtime)

  // Discard the first `Reconcile` message
  let _ = process.receive_forever(client)

  let emit = transport.emit("reset", json.null())

  assert process.receive_forever(client) == emit
}

// DEFERRED EFFECT TESTS -------------------------------------------------------

@target(erlang)
pub fn headless_drops_deferred_effects_test() {
  use <- lustre_test.test_filter("headless_drops_deferred_effects_test")

  // The synchronous effect dispatches Incr; the deferred effect would dispatch
  // Decr, but headless platforms declare no phases so it must never run.
  let init = fn(count) {
    #(
      count,
      effect.batch([
        effect.from(fn(dispatch) { dispatch(Incr) }),
        dom.before_paint(fn(dispatch, _root) { dispatch(Decr) }),
      ]),
    )
  }

  let app = agnostic.application(init, update, view)
  let assert Ok(runtime) = agnostic.start(app, on: platform.headless(), with: 0)
  let client = process.new_subject()

  server_component.register_subject(client) |> agnostic.send(to: runtime)

  // The synchronous effect ran before the client connected: the mounted view
  // shows the incremented count.
  assert process.receive_forever(client)
    == transport.mount(
      True,
      True,
      [],
      [],
      [],
      dict.new(),
      view(1),
      mutable_map.new(),
    )

  // The deferred effect is dropped: no patch ever arrives.
  let assert Error(Nil) = process.receive(client, 100)

  server_component.deregister_subject(client) |> agnostic.send(to: runtime)

  agnostic.shutdown() |> agnostic.send(to: runtime)
}

// UTILS -----------------------------------------------------------------------

@target(erlang)
fn with_erlang_runtime(run_test) {
  let app = agnostic.application(init, update, view)
  let assert Ok(runtime) = agnostic.start(app, on: platform.headless(), with: 0)
  let client = process.new_subject()

  server_component.register_subject(client) |> agnostic.send(to: runtime)

  run_test(client, runtime)

  server_component.deregister_subject(client) |> agnostic.send(to: runtime)

  agnostic.shutdown() |> agnostic.send(to: runtime)
}

// COUNTER APP -----------------------------------------------------------------

@target(erlang)
fn init(count) {
  #(count, effect.none())
}

@target(erlang)
type Message {
  Incr
  Decr
  Reset
}

@target(erlang)
fn update(model, message) {
  case message {
    Incr -> #(model + 1, effect.none())
    Decr -> #(model - 1, effect.none())
    Reset -> #(0, event.emit("reset", json.null()))
  }
}

@target(erlang)
const incr = "0" <> path.separator_element <> "2"

@target(erlang)
const reset = "0" <> path.separator_element <> "3"

@target(erlang)
fn view(model) {
  html.div([], [
    html.button([event.on_click(Decr)], [html.text("-")]),
    html.p([], [html.text(int.to_string(model))]),
    html.button([event.on_click(Incr)], [html.text("+")]),
    html.button([event.on_click(Reset)], [html.text("reset")]),
  ])
}
