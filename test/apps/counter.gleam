// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre/element.{text}
import lustre/element/html.{button, div, p}
import lustre/event

// MODEL -----------------------------------------------------------------------

pub type Model =
  Int

pub fn init(count) {
  count
}

// UPDATE ----------------------------------------------------------------------

pub type Msg {
  Increment
  Decrement
}

pub fn update(model, msg) {
  case msg {
    Increment -> model + 1
    Decrement -> model - 1
  }
}

// VIEW ------------------------------------------------------------------------

pub fn view(model) {
  let count = int.to_string(model)

  div([], [
    p([], [text(count)]),
    button([event.on_click(Decrement)], [text("-")]),
    button([event.on_click(Increment)], [text("+")]),
  ])
}
