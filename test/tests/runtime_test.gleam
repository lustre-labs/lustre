// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/erlang/process
import gleam/int
import gleam/json
import gleeunit/should
import lustre
import lustre/effect
import lustre/element/html
import lustre/event
import lustre/runtime/server/runtime
import lustre/runtime/transport
import lustre/server_component
import lustre/vdom/diff.{Patch}
import lustre_test

// CLIENT INTERACTION TESTS ----------------------------------------------------

@target(erlang)
pub fn client_connect_test() {
  use <- lustre_test.test_filter("client_connect_test")
  use client, _ <- with_erlang_runtime

  process.receive_forever(client) |> should.equal(transport.Mount(view(0)))
}

@target(erlang)
pub fn client_send_event_test() {
  use <- lustre_test.test_filter("client_send_event_test")
  use client, runtime <- with_erlang_runtime

  // Discard the `Mount` message
  let _ = process.receive_forever(client)

  let click = transport.EventFired(incr, "click", dynamic.from(Nil))

  process.send(runtime, runtime.ClientDispatchedMessage(click))

  let patch =
    Patch(0, 0, [], [
      Patch(0, 0, [], [
        Patch(1, 0, [], [Patch(0, 0, [diff.ReplaceText("1")], [])]),
      ]),
    ])

  process.receive_forever(client) |> should.equal(transport.Reconcile(patch))
}

@target(erlang)
pub fn client_send_multiple_events_test() {
  use <- lustre_test.test_filter("client_send_multiple_events_test")
  use client, runtime <- with_erlang_runtime

  // Discard the `Mount` message
  let _ = process.receive_forever(client)

  let click = transport.EventFired(incr, "click", dynamic.from(Nil))

  process.send(runtime, runtime.ClientDispatchedMessage(click))
  process.send(runtime, runtime.ClientDispatchedMessage(click))

  // Discard the first `Reconcile` message
  let _ = process.receive_forever(client)

  let patch =
    Patch(0, 0, [], [
      Patch(0, 0, [], [
        Patch(1, 0, [], [Patch(0, 0, [diff.ReplaceText("2")], [])]),
      ]),
    ])

  process.receive_forever(client) |> should.equal(transport.Reconcile(patch))
}

// SERVER MESSAGE TESTS --------------------------------------------------------

@target(erlang)
pub fn server_emit_event_test() {
  use <- lustre_test.test_filter("server_emit_event_test")
  use client, runtime <- with_erlang_runtime

  // Discard the `Mount` message
  let _ = process.receive_forever(client)

  let click = transport.EventFired(reset, "click", dynamic.from(Nil))

  process.send(runtime, runtime.ClientDispatchedMessage(click))

  // Discard the first `Reconcile` message
  let _ = process.receive_forever(client)

  let emit = transport.Emit("reset", json.null())

  process.receive_forever(client) |> should.equal(emit)
}

// UTILS -----------------------------------------------------------------------

@target(erlang)
fn with_erlang_runtime(run_test) {
  let app = lustre.application(init, update, view)
  let assert Ok(runtime) = lustre.start_actor(app, 0)
  let client = process.new_subject()

  server_component.register_subject(runtime, client)

  run_test(client, runtime)

  server_component.deregister_subject(runtime, client)

  lustre.shutdown() |> process.send(runtime, _)
}

// COUNTER APP -----------------------------------------------------------------

fn init(count) {
  #(count, effect.none)
}

type Msg {
  Incr
  Decr
  Reset
}

fn update(model, msg) {
  case msg {
    Incr -> #(model + 1, effect.none)
    Decr -> #(model - 1, effect.none)
    Reset -> #(0, event.emit("reset", json.null()))
  }
}

const incr = ["0", "2"]

const reset = ["0", "3"]

fn view(model) {
  html.div([], [
    html.button([event.on_click(Decr)], [html.text("-")]),
    html.p([], [html.text(int.to_string(model))]),
    html.button([event.on_click(Incr)], [html.text("+")]),
    html.button([event.on_click(Reset)], [html.text("reset")]),
  ])
}
