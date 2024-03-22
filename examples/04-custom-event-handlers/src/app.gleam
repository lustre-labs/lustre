import gleam/dynamic
import gleam/int
import gleam/result
import gleam/string
import lustre
import lustre/attribute
import lustre/element.{type Element}
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
import lustre/ui/aside

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(value: String, length: Int, max: Int)
}

fn init(_) -> Model {
  Model(value: "", length: 0, max: 10)
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  GotInput(value: String)
  Reset
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    GotInput(value) -> {
      let length = string.length(value)
      case length <= model.max {
        True -> Model(..model, value: value, length: length)
        False -> model
      }
    }
    Reset -> Model(..model, value: "", length: 0)
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let styles = [#("width", "100vw"), #("height", "100vh"), #("padding", "1rem")]
  let length = int.to_string(model.length)
  let max = int.to_string(model.max)
  let make_it_loud = fn(event) -> Result(Msg, List(dynamic.DecodeError)) {
    use target <- result.try(dynamic.field("target", dynamic.dynamic)(event))
    use value <- result.try(dynamic.field("value", dynamic.string)(target))

    let loud = string.uppercase(value)

    Ok(GotInput(loud))
  }

  ui.centre(
    [attribute.style(styles)],
    ui.aside(
      [aside.content_first(), aside.align_centre()],
      ui.field(
        [],
        [element.text("Write a LOUD message:")],
        ui.input([attribute.value(model.value), event.on("input", make_it_loud)]),
        [element.text(length <> "/" <> max)],
      ),
      ui.button([event.on_click(Reset)], [element.text("Reset")]),
    ),
  )
}
