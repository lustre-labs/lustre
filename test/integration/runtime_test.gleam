// IMPORTS ---------------------------------------------------------------------

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
import lustre
@target(erlang)
import lustre/effect
@target(erlang)
import lustre/element/html
@target(erlang)
import lustre/event
@target(erlang)
import lustre/internals/mutable_map
@target(erlang)
import lustre/runtime/server/runtime
@target(erlang)
import lustre/runtime/transport
@target(erlang)
import lustre/server_component
@target(erlang)
import lustre/vdom/patch
@target(erlang)
import lustre/vdom/path
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

  runtime.ClientDispatchedMessage(click) |> lustre.send(to: runtime)

  let patch =
    patch.new(0, 0, [], [
      patch.new(0, 0, [], [
        patch.new(1, 0, [], [patch.new(0, 0, [patch.replace_text("1")], [])]),
      ]),
    ])

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

  runtime.ClientDispatchedMessage(click) |> lustre.send(to: runtime)
  runtime.ClientDispatchedMessage(click) |> lustre.send(to: runtime)

  // Discard the first `Reconcile` message
  let _ = process.receive_forever(client)

  let patch =
    patch.new(0, 0, [], [
      patch.new(0, 0, [], [
        patch.new(1, 0, [], [patch.new(0, 0, [patch.replace_text("2")], [])]),
      ]),
    ])

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

  runtime.EffectDispatchedMessage(Incr)
  |> lustre.send(to: runtime)

  let patch =
    patch.new(0, 0, [], [
      patch.new(0, 0, [], [
        patch.new(1, 0, [], [patch.new(0, 0, [patch.replace_text("1")], [])]),
      ]),
    ])

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

  runtime.ClientDispatchedMessage(click) |> lustre.send(to: runtime)

  // Discard the first `Reconcile` message
  let _ = process.receive_forever(client)

  let emit = transport.emit("reset", json.null())

  assert process.receive_forever(client) == emit
}

// UTILS -----------------------------------------------------------------------

@target(erlang)
fn with_erlang_runtime(run_test) {
  let app = lustre.application(init, update, view)
  let assert Ok(runtime) = lustre.start_server_component(app, 0)
  let client = process.new_subject()

  server_component.register_subject(client) |> lustre.send(to: runtime)

  run_test(client, runtime)

  server_component.deregister_subject(client) |> lustre.send(to: runtime)

  lustre.shutdown() |> lustre.send(to: runtime)
}

// COUNTER APP -----------------------------------------------------------------

@target(erlang)
fn init(count) {
  #(count, effect.none())
}

@target(erlang)
type Msg {
  Incr
  Decr
  Reset
}

@target(erlang)
fn update(model, msg) {
  case msg {
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
