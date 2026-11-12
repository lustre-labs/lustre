// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MODEL -----------------------------------------------------------------------

/// The state for our module must be public as it is managed by the update loop
/// of the parent module
pub type Model =
  Int

pub fn init(init_value) -> Model {
  init_value
}

// UPDATE ----------------------------------------------------------------------

/// Just like our `Model`, the `Msg` type is public and needs to be
/// handled by the parent app. This makes it a bit less convenient than using 
/// components but it does allow us to encapsulate complex functionality and rich
/// user interaction without full web components.
///
pub type Msg {
  UserClickedIncrement
  UserClickedDecrement
}

pub fn update(model: Model, msg: Msg) -> Model {
  case msg {
    UserClickedIncrement -> model + 1
    UserClickedDecrement -> model - 1
  }
}

// VIEW ------------------------------------------------------------------------

pub fn view(model: Model) -> Element(Msg) {
  let count = int.to_string(model)

  html.div([attribute.class("py-4 flex items-center gap-2")], [
    view_button(label: "-", on_click: UserClickedDecrement),
    html.p([attribute.class("flex-1")], [html.text("Count: "), html.text(count)]),
    view_button(label: "+", on_click: UserClickedIncrement),
  ])
}

fn view_button(
  label label: String,
  on_click handle_click: msg,
) -> Element(msg) {
  html.button(
    [
      attribute.class("bg-blue-500 text-white w-12 py-1 rounded"),
      event.on_click(handle_click),
    ],
    [html.text(label)],
  )
}
