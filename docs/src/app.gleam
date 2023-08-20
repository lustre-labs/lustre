// IMPORTS ---------------------------------------------------------------------

import lustre
import lustre/attribute
import lustre/element.{Element}
import lustre/element/html

// MAIN ------------------------------------------------------------------------

pub fn main() -> fn(Msg) -> Nil {
  let app = lustre.simple(init, update, view)
  let assert Ok(dispatch) = lustre.start(app, "body", Nil)

  dispatch
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model
}

fn init(_) -> Model {
  Model
}

// UPDATE ----------------------------------------------------------------------

pub type Msg {
  None
}

fn update(model: Model, msg: Msg) -> Model {
  model
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.body([], [html.h1([], [element.text("Hello, world!")])])
}
