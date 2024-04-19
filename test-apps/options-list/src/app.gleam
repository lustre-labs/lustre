import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/ui

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL -----------------------------------------------------------------------

type Model =
  String

fn init(_flags) -> Model {
  "a"
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Select(String)
}

fn update(_model: Model, msg: Msg) -> Model {
  case msg {
    Select(tag) -> tag
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let styles = [#("width", "100vw"), #("height", "100vh"), #("padding", "1rem")]

  ui.centre(
    [attribute.style(styles)],
    html.select([event.on_input(Select)], [
      html.option([attribute.value("a"), attribute.selected("a" == model)], "a"),
      html.option([attribute.value("b"), attribute.selected("b" == model)], "b"),
      html.option([attribute.value("c"), attribute.selected("c" == model)], "c"),
    ]),
  )
}
