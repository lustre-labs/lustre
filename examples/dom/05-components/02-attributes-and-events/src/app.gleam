// IMPORTS ---------------------------------------------------------------------

import agnostic
import agnostic/attribute
import agnostic/element.{type Element}
import agnostic/element/html
import agnostic/platform/dom
import counter
import gleam/int

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(platform) = dom.platform("#app")
  let app = agnostic.simple(init, update, view)

  let assert Ok(_) = counter.register()
  let assert Ok(_) = agnostic.start(app, on: platform, with: Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

fn init(_) -> Model {
  0
}

// UPDATE ----------------------------------------------------------------------

type Message {
  CounterUpdatedValue(Int)
}

fn update(_model: Model, message: Message) -> Model {
  case message {
    CounterUpdatedValue(value) -> value
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Message) {
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
