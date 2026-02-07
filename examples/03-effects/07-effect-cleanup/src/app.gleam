// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/time/calendar
import gleam/time/duration.{type Duration}
import gleam/time/timestamp.{type Timestamp}
import gleam_community/maths
import lustre
import lustre/attribute.{attribute}
import lustre/effect.{type Effect, type Key}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let timezone = calendar.local_offset()
  let now = timestamp.system_time()

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Arguments(timezone:, now:))

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(timezone: Duration, time: Timestamp, clock: Option(Key))
}

type Arguments {
  Arguments(timezone: Duration, now: Timestamp)
}

fn init(arguments: Arguments) -> #(Model, Effect(Msg)) {
  let model =
    Model(timezone: arguments.timezone, time: arguments.now, clock: None)
  let effect = start()

  #(model, effect)
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  ClockStarted(key: Key)
  ClockTickedForward
  UserToggledClock
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ClockStarted(key:) -> #(Model(..model, clock: Some(key)), effect.none())
    ClockTickedForward -> #(
      Model(..model, time: timestamp.add(model.time, duration.seconds(1))),
      effect.none(),
    )
    UserToggledClock ->
      case model.clock {
        // The key stored in the model is a unique identifier for an effect. By
        // calling `effect.cleanup` any cleanup callbacks that the effect registered
        // will be called: in our case that stops the timer!
        Some(key) -> #(Model(..model, clock: None), effect.cleanup(key))
        None -> #(model, start())
      }
  }
}

fn start() -> Effect(Msg) {
  // We can wrap effects with `effect.with_cleanup`, which gives us access to two
  // things: a unique `key` that can be used in the future to trigger the effect
  // cleanup, and a `cleanup` callback that can be used to provide the cleanup
  // behaviour we want.
  use key, cleanup <- effect.with_cleanup
  use dispatch <- effect.from

  // First we start the timer. JavaScript gives us an integer id that can be used
  // to cancel the timer in the future.
  let id = set_interval(1000, fn() { dispatch(ClockTickedForward) })

  // Here we register a callback to run when this effect is cancelled that calls
  // the `clear_interval` FFI function to stop the timer.
  cleanup(fn() { clear_interval(id) })

  // Now we have a cleanup callback setup, we dispatch a message back to our app
  // with the unique key for this effect. This key is supposed to be passed to
  // `effect.cleanup` to trigger any cleanup callbacks that have been registered.
  dispatch(ClockStarted(key:))
}

@external(javascript, "./app.ffi.mjs", "setInterval")
fn set_interval(_delay: Int, _callback: fn() -> Nil) -> Int {
  0
}

@external(javascript, "./app.ffi.mjs", "clearInterval")
fn clear_interval(_id: Int) -> Nil {
  Nil
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let #(_, time) = timestamp.to_calendar(model.time, model.timezone)
  let hours = int.to_float(time.hours)
  let minutes = int.to_float(time.minutes)
  let seconds = int.to_float(time.seconds)

  html.div(
    [attribute.class("w-screen h-screen flex justify-center items-center")],
    [
      html.button([event.on_click(UserToggledClock)], [
        case model.clock {
          Some(_) -> html.text("Freeze")
          None -> html.text("Unfreeze")
        },
      ]),
      view_clock(hours:, minutes:, seconds:),
    ],
  )
}

fn view_clock(
  hours hour: Float,
  minutes minute: Float,
  seconds second: Float,
) -> Element(msg) {
  html.svg(
    [
      attribute("viewBox", "0 0 400 400"),
      attribute("width", "400"),
      attribute("height", "400"),
    ],
    [
      svg.circle([
        attribute("cx", "200"),
        attribute("cy", "200"),
        attribute("r", "120"),
        attribute("fill", "#ffaff3"),
      ]),
      view_hand(6, 60.0, hour /. 12.0),
      view_hand(6, 90.0, minute /. 60.0),
      view_hand(3, 90.0, second /. 60.0),
    ],
  )
}

fn view_hand(width: Int, length: Float, turns: Float) -> Element(msg) {
  let t = 2.0 *. maths.pi() *. { turns -. 0.25 }
  let x = 200.0 +. length *. maths.cos(t)
  let y = 200.0 +. length *. maths.sin(t)

  svg.line([
    attribute("x1", "200"),
    attribute("y1", "200"),
    attribute("x2", float.to_string(x)),
    attribute("y2", float.to_string(y)),
    attribute("stroke", "white"),
    attribute("stroke-width", int.to_string(width)),
    attribute("stroke-linecap", "round"),
  ])
}
