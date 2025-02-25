import gleam/int
import gleam/string
import lustre
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

//

type Model {
  Waiting
  Received(Msg)
}

fn init(_) -> Model {
  Waiting
}

type Msg {
  SingleEventMapper(Int)
  NestedEventMapper(Int)
  FragmentEventMapper(Int)
}

fn update(_: Model, msg: Msg) -> Model {
  Received(msg)
}

fn view(model: Model) -> Element(Msg) {
  html.div([], [
    html.h1([], [html.text("Last message:"), html.text(string.inspect(model))]),
    //
    element.map(
      html.section([], [
        html.button([event.on_click(1)], [html.text("1")]),
        html.button([event.on_click(2)], [html.text("2")]),
        html.button([event.on_click(3)], [html.text("3")]),
      ]),
      SingleEventMapper,
    ),
    //
    element.map(
      html.section([], [
        element.map(
          html.button([event.on_click(4)], [html.text("4")]),
          int.multiply(_, 2),
        ),
      ]),
      NestedEventMapper,
    ),
    //
    html.section([], [
      element.map(
        element.fragment([
          html.button([event.on_click(5)], [html.text("5")]),
          html.button([event.on_click(6)], [html.text("6")]),
        ]),
        FragmentEventMapper,
      ),
    ]),
  ])
}
