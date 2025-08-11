// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

//
pub fn register() -> Result(Nil, lustre.Error) {
  let component = lustre.simple(init, update, view)

  // The `register` function does not create an app directly, instead it registers
  // a Lustre app as a Custom Element with the name `"my-counter"`. The main app
  // can then render this component like any other HTML element.
  lustre.register(component, "my-counter")
}

/// It's good practice to provide an `element` function that encapsulates the
/// underlying element construction: this way users don't have to remember the
/// exact name of the component.
///
pub fn element() -> Element(msg) {
  element.element("my-counter", [], [])
}

// MODEL -----------------------------------------------------------------------

/// The state for our component will never leak outside the component itself.
/// It's encapsulated the same way internal state for native HTML elements is.
///
type Model =
  Int

fn init(_) -> Model {
  0
}

// UPDATE ----------------------------------------------------------------------

/// Just like our component's `Model`, the `Msg` type is also private to the
/// component and doesn't need to be handled by the parent app. This makes it
/// convenient to use components to encapsulate complex functionality and rich
/// user interaction patterns without complicating the parent app.
///
type Msg {
  UserClickedIncrement
  UserClickedDecrement
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    UserClickedIncrement -> model + 1
    UserClickedDecrement -> model - 1
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let count = int.to_string(model)

  html.div([attribute.class("py-4 flex items-center gap-2")], [
    view_button(label: "-", on_click: UserClickedDecrement),
    html.p([attribute.class("flex-1")], [html.text("Count: "), html.text(count)]),
    view_button(label: "+", on_click: UserClickedIncrement),
  ])
}

fn view_button(label label: String, on_click handle_click: msg) -> Element(msg) {
  html.button(
    [
      attribute.class("bg-blue-500 text-white w-12 py-1 rounded"),
      event.on_click(handle_click),
    ],
    [html.text(label)],
  )
}
