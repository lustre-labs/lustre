// IMPORTS ---------------------------------------------------------------------

import counter
import gleam/int
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)

  let assert Ok(_) = counter.register()
  let assert Ok(_) = lustre.start(app, "#app", Nil)

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
  CounterUpdatedValue(Int)
}

fn update(_model: Model, msg: Msg) -> Model {
  case msg {
    CounterUpdatedValue(value) -> value
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("p-32 mx-auto w-full max-w-2xl space-y-4")], [
    html.div([attribute.class("border rounded p-2")], [
      counter.element([
        counter.value(model),
        counter.on_change(CounterUpdatedValue),
      ]),
    ]),
    html.p([], [
      html.text("The last saved count was: "),
      html.text(int.to_string(model)),
    ]),
  ])
}
