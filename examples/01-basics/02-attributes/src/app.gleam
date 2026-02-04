// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/platform/dom

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(platform) = dom.platform("#app")
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

fn init(_) -> Model {
  0
}

// UPDATE ----------------------------------------------------------------------

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
  html.div(
    [attribute.class("h-screen flex flex-col justify-center items-center")],
    [
      // Most elements will receive two arguments. The first is a list of "attributes"
      // that includes HTML attributes, DOM properties, and event handlers. The
      // second is a list of children to render inside the element.
      html.button([event.on_click(UserClickedDecrement)], [html.text("-")]),
      html.p(
        [
          attribute.class("my-4"),
          case model > 10 {
            // In most cases, when an attribute is added multiple times to an
            // element, only the last one will be used. However in the case of
            // `class` and `style` attributes, Lustre will _merge_ multiple values
            // together.
            True -> attribute.class("text-red-500 font-bold")
            False -> attribute.none()
          },
        ],
        [html.text("Count: "), html.text(int.to_string(model))],
      ),
      html.button([event.on_click(UserClickedIncrement)], [html.text("+")]),
    ],
  )
}
