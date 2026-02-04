// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/platform/opentui
import lustre/platform/opentui/attribute
import lustre/platform/opentui/effect as tui_effect
import lustre/platform/opentui/element as tui

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // Get the current time before starting the app
  let now = get_current_time()

  opentui.platform(opentui.default_config(), fn(platform) {
    let app = lustre.application(init, update, view)
    let assert Ok(_) = lustre.start(app, on: platform, with: now)
    Nil
  })
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(hours: Int, minutes: Int, seconds: Int)
}

fn init(now: #(Int, Int, Int)) -> #(Model, Effect(Msg)) {
  let #(hours, minutes, seconds) = now
  // Start the tick loop immediately
  #(
    Model(hours:, minutes:, seconds:),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      tick(),
    ]),
  )
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  ClockTickedForward
  KeyPressed(tui_effect.KeyEvent)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ClockTickedForward -> {
      // Increment time by 1 second
      let new_seconds = model.seconds + 1
      let #(seconds, carry_minutes) = case new_seconds >= 60 {
        True -> #(new_seconds - 60, 1)
        False -> #(new_seconds, 0)
      }
      let new_minutes = model.minutes + carry_minutes
      let #(minutes, carry_hours) = case new_minutes >= 60 {
        True -> #(new_minutes - 60, 1)
        False -> #(new_minutes, 0)
      }
      let hours = { model.hours + carry_hours } % 24

      #(Model(hours:, minutes:, seconds:), tick())
    }

    KeyPressed(key_event) ->
      case key_event.key {
        "q" -> #(model, tui_effect.destroy())
        _ -> #(model, effect.none())
      }
  }
}

fn tick() -> Effect(Msg) {
  use dispatch <- effect.from
  use <- set_timeout(1000)

  dispatch(ClockTickedForward)
}

// FFI -------------------------------------------------------------------------

@external(javascript, "./timers.ffi.ts", "set_timeout")
fn set_timeout(_delay: Int, _cb: fn() -> a) -> Nil {
  Nil
}

@external(javascript, "./timers.ffi.ts", "get_current_time")
fn get_current_time() -> #(Int, Int, Int) {
  // Returns hours, minutes, seconds
  #(12, 0, 0)
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let time_str =
    pad_zero(model.hours)
    <> ":"
    <> pad_zero(model.minutes)
    <> ":"
    <> pad_zero(model.seconds)

  tui.box(
    [
      attribute.flex_direction("column"),
      attribute.align_items("center"),
      attribute.justify_content("center"),
      attribute.width_("100%"),
      attribute.height_("100%"),
    ],
    [
      tui.box(
        [
          attribute.flex_direction("column"),
          attribute.align_items("center"),
          attribute.border_style("double"),
          attribute.border_color("#ffaff3"),
          attribute.padding(3),
          attribute.gap(1),
          attribute.width_("90%"),
          attribute.title(" Clock "),
          attribute.title_alignment("center"),
        ],
        [
          tui.ascii_font([
            attribute.ascii_text(time_str),
            attribute.ascii_color("#ffaff3"),
            attribute.font("block"),
          ]),
          tui.text_node([attribute.dim(True), attribute.color("#888")], [
            tui.text("Press 'q' to quit"),
          ]),
        ],
      ),
    ],
  )
}

fn pad_zero(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}
