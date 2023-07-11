// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre
import lustre/element.{Element, button, div, p, text}
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // A `simple` lustre application doesn't produce `Cmd`s. These are best to 
  // start with if you're just getting started with lustre or you know you don't
  // need the runtime to manage any side effects.
  let app = lustre.simple(init, update, render)
  let assert Ok(dispatch) = lustre.start(app, "body")

  dispatch(Incr)
  dispatch(Incr)
  dispatch(Incr)
}

// MODEL -----------------------------------------------------------------------

pub type Model =
  Int

pub fn init() -> Model {
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

pub fn render(model: Model) -> Element(Msg) {
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
