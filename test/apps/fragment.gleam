// Similar to count app, with fragments and edge cases

// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre/element.{text}
import lustre/element/html.{button, p}
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
  element.fragment([
    element.fragment([p([], [element.text("start fragment")])]),
    element.fragment([p([], [element.text("middle fragment")])]),
    element.fragment([p([], [element.text(count)])]),
    button([event.on_click(Decrement)], [text("-")]),
    button([event.on_click(Increment)], [text("+")]),
    p([], [element.text("order check, last element")]),
  ])
}
