import gleam/list
import lustre
import lustre/attribute
import lustre/element.{type Element, element}
import lustre/element/html
import lustre/event
// These examples are written with `lustre/ui` in mind. They'll work regardless,
// but to see what `lustre/ui` can do make sure to run each of these examples with
// the `--use-example-styles` flag:
//
//   $ gleam run -m lustre/dev start --use-example-styles
//
// In your own apps, make sure to add the `lustre/ui` dependency and include the
// stylesheet somewhere.
import lustre/ui

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL -----------------------------------------------------------------------

type Model =
  Bool

fn init(_flags) -> Model {
  False
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Swap
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Swap -> !model
  }
}

// VIEW ------------------------------------------------------------------------

const names = ["hayleigh", "jak", "john"]

fn view(model: Model) -> Element(Msg) {
  let styles = [#("width", "100vw"), #("height", "100vh"), #("padding", "1rem")]

  ui.centre(
    [attribute.style(styles)],
    ui.stack([], [
      ui.button([event.on_click(Swap)], [element.text("toggle content")]),
      case model {
        True ->
          element.keyed(
            ui.stack([], _),
            list.map(names, fn(name) {
              #(name, html.span([], [html.text(name)]))
            }),
          )
        False -> element.none()
      },
    ]),
  )
}
