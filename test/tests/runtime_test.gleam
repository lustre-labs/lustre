// IMPORTS ---------------------------------------------------------------------

@target(erlang)
import gleam/dynamic
@target(erlang)
import gleam/erlang/process
@target(erlang)
import gleam/int
@target(erlang)
import gleam/json
@target(erlang)
import gleeunit/should
@target(erlang)
import lustre
@target(erlang)
import lustre/effect
@target(erlang)
import lustre/element/html
@target(erlang)
import lustre/event
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

  process.receive_forever(client)
  |> should.equal(transport.mount(False, True, [], [], view(0)))
}

@target(erlang)
pub fn client_send_event_test() {
  use <- lustre_test.test_filter("client_send_event_test")
  use client, runtime <- with_erlang_runtime

  // Discard the `Mount` message
  let _ = process.receive_forever(client)

  let click = transport.event_fired(incr, "click", dynamic.from(Nil))

  lustre.send(runtime, runtime.ClientDispatchedMessage(click))

  let patch =
    patch.new(0, 0, [], [
      patch.new(0, 0, [], [
        patch.new(1, 0, [], [patch.new(0, 0, [patch.replace_text("1")], [])]),
      ]),
    ])

  process.receive_forever(client) |> should.equal(transport.reconcile(patch))
}

@target(erlang)
pub fn client_send_multiple_events_test() {
  use <- lustre_test.test_filter("client_send_multiple_events_test")
  use client, runtime <- with_erlang_runtime

  // Discard the `Mount` message
  let _ = process.receive_forever(client)

  let click = transport.event_fired(incr, "click", dynamic.from(Nil))

  lustre.send(runtime, runtime.ClientDispatchedMessage(click))
  lustre.send(runtime, runtime.ClientDispatchedMessage(click))

  // Discard the first `Reconcile` message
  let _ = process.receive_forever(client)

  let patch =
    patch.new(0, 0, [], [
      patch.new(0, 0, [], [
        patch.new(1, 0, [], [patch.new(0, 0, [patch.replace_text("2")], [])]),
      ]),
    ])

  process.receive_forever(client) |> should.equal(transport.reconcile(patch))
}

// SERVER MESSAGE TESTS --------------------------------------------------------

@target(erlang)
pub fn server_emit_event_test() {
  use <- lustre_test.test_filter("server_emit_event_test")
  use client, runtime <- with_erlang_runtime

  // Discard the `Mount` message
  let _ = process.receive_forever(client)

  let click = transport.event_fired(reset, "click", dynamic.from(Nil))

  lustre.send(runtime, runtime.ClientDispatchedMessage(click))

  // Discard the first `Reconcile` message
  let _ = process.receive_forever(client)

  let emit = transport.emit("reset", json.null())

  process.receive_forever(client) |> should.equal(emit)
}

// UTILS -----------------------------------------------------------------------

@target(erlang)
fn with_erlang_runtime(run_test) {
  let app = lustre.application(init, update, view)
  let assert Ok(runtime) = lustre.start_server_component(app, 0)
  let client = process.new_subject()

  server_component.register_subject(runtime, client)

  run_test(client, runtime)

  server_component.deregister_subject(runtime, client)

  lustre.shutdown(runtime)
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
const incr = "0" <> path.separator_index <> "2"

@target(erlang)
const reset = "0" <> path.separator_index <> "3"

@target(erlang)
fn view(model) {
  html.div([], [
    html.button([event.on_click(Decr)], [html.text("-")]),
    html.p([], [html.text(int.to_string(model))]),
    html.button([event.on_click(Incr)], [html.text("+")]),
    html.button([event.on_click(Reset)], [html.text("reset")]),
  ])
}
