// IMPORTS ---------------------------------------------------------------------

import gleam/string
import lustre
import lustre/platform
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(platform) = platform.dom("#app")
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  String

fn init(_) -> Model {
  "Lucy"
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserUpdatedName(String)
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    UserUpdatedName(name) ->
      case string.length(name) <= 10 {
        True -> name
        // If we don't update the state of a controlled input, then it won't
        // change in the DOM even though the input is still receiving keyboard
        // events.
        False -> model
      }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("p-32 mx-auto w-full max-w-2xl space-y-4")], [
    html.label([attribute.class("flex gap-2")], [
      html.span([], [html.text("Enter a name: ")]),
      html.input([
        // A "controlled" input has both its `"value"` attribute set and a handler
        // for `"input"` events. This way it is always in sync with your model.
        attribute.value(model),
        event.on_input(UserUpdatedName),
        attribute.class("border border-slate-400 px-1 rounded"),
        attribute.class("focus:outline-none focus:border-blue-500"),
      ]),
    ]),
    html.p([], [
      html.text("Hello there, "),
      // As we type, the value stored in the model is updated on every keystroke
      // and reflected here.
      html.text(model),
      html.text("!"),
    ]),
  ])
}
