import gleam/int
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/ui

// MAIN ------------------------------------------------------------------------

pub fn register(name: String) -> Result(Nil, lustre.Error) {
  lustre.simple(init, update, view) |> lustre.register(name)
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

fn init(_) -> Model {
  0
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
  let count = int.to_string(model)

  ui.stack([], [
    ui.button([event.on_click(Incr)], [element.text("+")]),
    html.p([attribute.style([#("text-align", "center")])], [element.text(count)]),
    ui.button([event.on_click(Decr)], [element.text("-")]),
  ])
}
