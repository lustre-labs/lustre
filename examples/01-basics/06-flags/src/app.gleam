// IMPORTS ---------------------------------------------------------------------

import gleam/int
import lustre
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.simple(init, update, view)

  // When starting a Lustre app, you can pass in initial data as "flags" to your
  // application. Because the pieces of your app are supposed to be *pure*, flags
  // are a good opportunity to pass in initial data from side effects such as
  // randomness or HTTP requests that you want to have immediately available.
  let initial_count = int.random(20)
  let assert Ok(_) = lustre.start(app, "#app", initial_count)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

/// Flags passed to your app are then passed to your `init` function where you
/// can use them to set up your initial model.
///
fn init(initial_count) -> Model {
  initial_count / 2
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
  let count = int.to_string(model)

  html.div([], [
    html.button([event.on_click(UserClickedDecrement)], [html.text("-")]),
    html.p([], [html.text("Count: "), html.text(count)]),
    html.button([event.on_click(UserClickedIncrement)], [html.text("+")]),
  ])
}
