// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre
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

pub type Model =
  Int

pub fn init(_) -> Model {
  0
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Incr
  Decr
  Reset
}

pub fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
    Reset -> 0
  }
}

// VIEW ------------------------------------------------------------------------

pub fn view(model: Model) -> Element(Msg) {
  div(
    [],
    [
      button([event.on_click(Incr)], [text("+")]),
      button([event.on_click(Decr)], [text("-")]),
      button([event.on_click(Reset)], [text("Reset")]),
      p([], [text(int.to_string(model))]),
    ],
  )
}
