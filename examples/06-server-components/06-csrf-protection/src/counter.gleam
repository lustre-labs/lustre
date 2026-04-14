// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre.{type App}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn component() -> App(_, Model, Message) {
  lustre.simple(init, update, view)
}

// MODEL -----------------------------------------------------------------------

pub type Model =
  Int

fn init(_) -> Model {
  0
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Message {
  UserClickedIncrement
  UserClickedDecrement
}

fn update(model: Model, message: Message) -> Model {
  case message {
    UserClickedIncrement -> model + 1
    UserClickedDecrement -> model - 1
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Message) {
  let count = int.to_string(model)
  let styles = [#("display", "flex"), #("justify-content", "space-between")]

  html.div([], [
    html.h1([], [html.text("Protected counter")]),
    html.div([attribute.styles(styles)], [
      view_button(label: "-", on_click: UserClickedDecrement),
      html.p([], [html.text("Count: "), html.text(count)]),
      view_button(label: "+", on_click: UserClickedIncrement),
    ]),
  ])
}

fn view_button(
  label label: String,
  on_click handle_click: message,
) -> Element(message) {
  html.button([event.on_click(handle_click)], [html.text(label)])
}
