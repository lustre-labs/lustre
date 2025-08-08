// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode
import gleam/io
import gleam/string
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

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
  OnKeyDown(String)
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
    OnKeyDown(key) -> {
      case key {
        "a" -> io.println("Correct this should have been called (twice)!!")
        _else -> io.println("This should not have been called!!")
      }

      model
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let on_keydown = {
    use key <- decode.field("key", decode.string)

    case key {
      "a" ->
        event.send(
          dispatch: OnKeyDown("a"),
          prevent_default: False,
          stop_propagation: False,
        )
      _ -> event.retain(prevent_default: False, stop_propagation: True)
    }
  }

  html.div(
    [
      attribute.class("p-32 mx-auto w-full max-w-2xl space-y-4"),
      // Standard on keydown
      event.on_keydown(OnKeyDown),
    ],
    [
      html.label([attribute.class("flex gap-2")], [
        html.span([], [html.text("Enter a name: ")]),
        html.input([
          attribute.value(model),
          event.on_input(UserUpdatedName),
          // Advanced on keydown
          event.advanced("keydown", on_keydown),
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
    ],
  )
}
