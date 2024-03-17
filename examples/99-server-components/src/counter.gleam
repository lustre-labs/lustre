import gleam/int
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
// These examples are written with `lustre/ui` in mind. They'll work regardless,
// but to see what `lustre/ui` can do make sure to run each of these examples with
// the `--use-lustre-ui` flag:
//
//   $ gleam run -m lustre dev --use-lustre-ui
//
// In your own apps, make sure to add the `lustre/ui` dependency and include the
// stylesheet somewhere.
import lustre/ui

// MAIN ------------------------------------------------------------------------

pub fn app() {
  lustre.simple(init, update, view)
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

fn init(initial_count: Int) -> Model {
  case initial_count < 0 {
    True -> 0
    False -> initial_count
  }
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Incr
  Decr
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let styles = [#("width", "100vw"), #("height", "100vh"), #("padding", "1rem")]
  let count = int.to_string(model)

  ui.centre(
    [attribute.style(styles)],
    ui.stack([], [
      ui.button([event.on_click(Incr)], [element.text("+")]),
      html.p([attribute.style([#("text-align", "center")])], [
        element.text(count),
      ]),
      ui.button([event.on_click(Decr)], [element.text("-")]),
    ]),
  )
}
