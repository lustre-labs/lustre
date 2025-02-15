import gleam/int
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(to_runtime) = lustre.start(app, "#app", 0)

  Incr |> lustre.dispatch |> to_runtime
  Incr |> lustre.dispatch |> to_runtime
  Incr |> lustre.dispatch |> to_runtime

  Nil
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
  let count = int.to_string(model)

  html.div([], [
    html.button([event.on_click(Incr)], [element.text("+")]),
    html.p([], [element.text(count)]),
    html.button([event.on_click(Decr)], [element.text("-")]),
  ])
}
