// IMPORTS ---------------------------------------------------------------------

@target(erlang)
import gleam/dict
import gleam/dynamic
@target(erlang)
import gleam/erlang/process
import gleam/int
import gleam/json
import lustre
import lustre/effect
@target(javascript)
import lustre/element
import lustre/element/html
import lustre/event
import lustre/internals/mutable_map
import lustre/runtime/server/runtime
import lustre/runtime/transport
import lustre/server_component
import lustre/vdom/patch
import lustre/vdom/path
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

  runtime.ClientDispatchedMessage(click) |> lustre.send(to: runtime)
  runtime.ClientDispatchedMessage(click) |> lustre.send(to: runtime)

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

  runtime.EffectDispatchedMessage(Incr)
  |> lustre.send(to: runtime)

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

  runtime.ClientDispatchedMessage(click) |> lustre.send(to: runtime)

  // Discard the first `Reconcile` message
  let _ = process.receive_forever(client)

  let emit = transport.emit("reset", json.null())

  assert process.receive_forever(client) == emit
}

// JAVASCRIPT RUNTIME TESTS ----------------------------------------------------

@target(javascript)
pub fn javascript_client_connect_test() {
  use <- lustre_test.test_filter("javascript_client_connect_test")
  use received, _ <- with_javascript_runtime

  // Event handlers are closures, which JavaScript can't compare structurally,
  // so compare the rendered HTML instead.
  let assert [transport.Mount(vdom:, ..)] = received()

  assert element.to_string(vdom) == element.to_string(view(0))
}

@target(javascript)
pub fn javascript_client_send_event_test() {
  use <- lustre_test.test_filter("javascript_client_send_event_test")
  use received, runtime <- with_javascript_runtime

  // Discard the `Mount` message
  let _ = received()

  let click = transport.event_fired(incr, "click", dynamic.nil())

  runtime.ClientDispatchedMessage(click) |> lustre.send(to: runtime)

  let patch =
    patch.new(0, 0, [patch.replace_text("1")], [])
    |> patch.add_parent(1)
    |> patch.add_parent(0)
    |> patch.add_parent(0)

  assert received() == [transport.reconcile(patch, mutable_map.new())]
}

@target(javascript)
pub fn javascript_effect_send_event_test() {
  use <- lustre_test.test_filter("javascript_effect_send_event_test")
  use received, runtime <- with_javascript_runtime

  // Discard the `Mount` message
  let _ = received()

  lustre.dispatch(Incr) |> lustre.send(to: runtime)

  let patch =
    patch.new(0, 0, [patch.replace_text("1")], [])
    |> patch.add_parent(1)
    |> patch.add_parent(0)
    |> patch.add_parent(0)

  assert received() == [transport.reconcile(patch, mutable_map.new())]
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

@target(javascript)
fn with_javascript_runtime(run_test) {
  let app = lustre.application(init, update, view)
  let assert Ok(runtime) = lustre.start_server_component(app, 0)
  let #(client, received) = new_client()

  server_component.register_callback(client) |> lustre.send(to: runtime)

  run_test(received, runtime)

  server_component.deregister_callback(client) |> lustre.send(to: runtime)

  lustre.shutdown() |> lustre.send(to: runtime)
}

@target(javascript)
@external(javascript, "./runtime_test.ffi.mjs", "new_client")
fn new_client() -> #(
  fn(transport.ClientMessage(message)) -> Nil,
  fn() -> List(transport.ClientMessage(message)),
)

// COUNTER APP -----------------------------------------------------------------

fn init(count) {
  #(count, effect.none())
}

type Message {
  Incr
  Decr
  Reset
}

fn update(model, message) {
  case message {
    Incr -> #(model + 1, effect.none())
    Decr -> #(model - 1, effect.none())
    Reset -> #(0, event.emit("reset", json.null()))
  }
}

const incr = "0" <> path.separator_element <> "2"

@target(erlang)
const reset = "0" <> path.separator_element <> "3"

fn view(model) {
  html.div([], [
    html.button([event.on_click(Decr)], [html.text("-")]),
    html.p([], [html.text(int.to_string(model))]),
    html.button([event.on_click(Incr)], [html.text("+")]),
    html.button([event.on_click(Reset)], [html.text("reset")]),
  ])
}
