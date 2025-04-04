// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/int
import gleam/time/calendar
import gleam/time/duration.{type Duration}
import gleam/time/timestamp.{type Timestamp}
import gleam_community/maths
import lustre
import lustre/attribute.{attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // Both getting the user's timezone and the current time are side effects, but
  // we can perform them before our app starts and create the initial model right
  // away.
  let timezone = calendar.local_offset()
  let now = timestamp.system_time()
  let model = Model(timezone:, time: now)

  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", model)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(timezone: Duration, time: Timestamp)
}

fn init(model: Model) -> #(Model, Effect(Msg)) {
  // Calling the `tick` effect immediately on init kicks off our clock!
  #(model, tick())
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  ClockTickedForward
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ClockTickedForward -> #(
      Model(..model, time: timestamp.add(model.time, duration.seconds(1))),
      // Every tick of the clock, we schedule another one to happen in 1 second.
      // If our app wants to stop the clock, it can just stop returning the
      // effect.
      tick(),
    )
  }
}

fn tick() -> Effect(Msg) {
  use dispatch <- effect.from
  use <- set_timeout(1000)

  dispatch(ClockTickedForward)
}

/// When writing custom effects that need FFI, it's common practice to define the
/// externals separate to the effect itself.
@external(javascript, "./app.ffi.mjs", "set_timeout")
fn set_timeout(_delay: Int, _cb: fn() -> a) -> Nil {
  // It's good practice to provide a fallback for side effects that rely on FFI
  // where possible. This means your app can run - without the side effect - in
  // environments other than the browser.
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
    [view_clock(hours:, minutes:, seconds:)],
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
