// IMPORTS ---------------------------------------------------------------------

import apps/counter
import apps/fragment
import apps/static
import birdie
import gleam/erlang/process
import gleam/json
import gleeunit
import lustre
import lustre/element
import lustre/internals/patch
import lustre/internals/runtime.{Debug, Dispatch, Shutdown, View}

// MAIN ------------------------------------------------------------------------

pub fn main() {
  gleeunit.main()
}

// TESTS -----------------------------------------------------------------------

pub fn static_test() {
  let title = "Can render static HTML"
  let el = static.view()

  birdie.snap(element.to_string(el), title)
}

@target(erlang)
pub fn counter_init_test() {
  let title = "Can render an application's initial state."
  let app = lustre.simple(counter.init, counter.update, counter.view)
  let assert Ok(runtime) = lustre.start_actor(app, 0)
  let el =
    process.call(
      runtime,
      fn(reply) {
        process.send(reply, _)
        |> View
        |> Debug
      },
      100,
    )

  birdie.snap(element.to_string(el), title)
  process.send(runtime, Shutdown)
}

@target(erlang)
pub fn counter_update_test() {
  let title = "Can render an application's state after some updates."
  let app = lustre.simple(counter.init, counter.update, counter.view)
  let assert Ok(runtime) = lustre.start_actor(app, 0)

  process.send(runtime, Dispatch(counter.Increment))
  process.send(runtime, Dispatch(counter.Increment))
  process.send(runtime, Dispatch(counter.Increment))

  let el =
    process.call(
      runtime,
      fn(reply) {
        process.send(reply, _)
        |> View
        |> Debug
      },
      100,
    )

  birdie.snap(element.to_string(el), title)
  process.send(runtime, Shutdown)
}

@target(erlang)
pub fn counter_diff_test() {
  let title = "Can compute a diff from one render to the next"
  let app = lustre.simple(counter.init, counter.update, counter.view)
  let assert Ok(runtime) = lustre.start_actor(app, 0)

  let prev =
    process.call(
      runtime,
      fn(reply) {
        process.send(reply, _)
        |> View
        |> Debug
      },
      100,
    )

  process.send(runtime, Dispatch(counter.Increment))
  process.send(runtime, Dispatch(counter.Increment))
  process.send(runtime, Dispatch(counter.Increment))

  let next =
    process.call(
      runtime,
      fn(reply) {
        process.send(reply, _)
        |> View
        |> Debug
      },
      100,
    )

  let diff = patch.elements(prev, next)

  birdie.snap(json.to_string(patch.element_diff_to_json(diff)), title)
  process.send(runtime, Shutdown)
}

@target(erlang)
pub fn fragment_init_test() {
  let title = "Can render an application's initial state when using fragments"
  let app = lustre.simple(fragment.init, fragment.update, fragment.view)
  let assert Ok(runtime) = lustre.start_actor(app, 0)
  let el =
    process.call(
      runtime,
      fn(reply) {
        process.send(reply, _)
        |> View
        |> Debug
      },
      100,
    )

  birdie.snap(element.to_string(el), title)
  process.send(runtime, Shutdown)
}

@target(erlang)
pub fn fragment_counter_diff_test() {
  let title = "Can compute a diff from one render to the next with fragments"
  let app = lustre.simple(fragment.init, fragment.update, fragment.view)
  let assert Ok(runtime) = lustre.start_actor(app, 0)

  let prev =
    process.call(
      runtime,
      fn(reply) {
        process.send(reply, _)
        |> View
        |> Debug
      },
      100,
    )

  process.send(runtime, Dispatch(fragment.Increment))
  process.send(runtime, Dispatch(fragment.Increment))
  process.send(runtime, Dispatch(fragment.Increment))

  let next =
    process.call(
      runtime,
      fn(reply) {
        process.send(reply, _)
        |> View
        |> Debug
      },
      100,
    )

  let diff = patch.elements(prev, next)

  birdie.snap(json.to_string(patch.element_diff_to_json(diff)), title)
  process.send(runtime, Shutdown)
}

pub fn escaped_attribute_test() {
  let title = "Can safely escape dangerous symbols in attributes"
  let el = static.escaped_attribute()
  birdie.snap(element.to_string(el), title)
}
