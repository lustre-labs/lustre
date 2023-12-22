// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre
import lustre/attribute
import lustre/element.{type Element, text}
import lustre/element/html.{button, div, p}
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // A `simple` lustre application doesn't produce `Effect`s. These are best to 
  // start with if you're just getting started with lustre or you know you don't
  // need the runtime to manage any side effects.
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "[data-lustre-app]", Nil)
}

// MODEL -----------------------------------------------------------------------

pub type Model {
  Model(count: Int, active: Bool)
}

pub fn init(_) -> Model {
  Model(count: 0, active: True)
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Incr
  Decr
  Reset
  Toggle
}

pub fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Incr -> Model(..model, count: model.count + 1)
    Decr -> Model(..model, count: model.count - 1)
    Reset -> Model(..model, count: 0)
    Toggle -> Model(..model, active: !model.active)
  }
}

// VIEW ------------------------------------------------------------------------

pub fn view(model: Model) -> Element(Msg) {
  let on_click = fn(msg) {
    case model.active {
      True -> event.on_click(msg)
      False -> attribute.none()
    }
  }

  div([], [
    button([on_click(Incr)], [text("+")]),
    button([on_click(Decr)], [text("-")]),
    button([on_click(Reset)], [text("Reset")]),
    button([event.on_click(Toggle)], [
      case model.active {
        True -> text("Disable")
        False -> text("Enable")
      },
    ]),
    p([], [text(int.to_string(model.count))]),
  ])
}
