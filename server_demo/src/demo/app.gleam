// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic
import gleam/int
import gleam/result
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/server
import lustre/ui

// MODEL -----------------------------------------------------------------------

pub type Model {
  Model(count: Int, input: String, mouse: #(Int, Int))
}

pub fn init(count: Int) -> #(Model, Effect(Msg)) {
  #(Model(count, "", #(0, 0)), effect.none())
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Incr
  Decr
  Change(String)
  Move(Int, Int)
}

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Incr -> #(Model(..model, count: model.count + 1), effect.none())
    Decr -> #(Model(..model, count: model.count - 1), effect.none())
    Change(input) -> #(Model(..model, input: input), effect.none())
    Move(x, y) -> #(Model(..model, mouse: #(x, y)), effect.none())
  }
}

// VIEW ------------------------------------------------------------------------

pub fn view(model: Model) -> Element(Msg) {
  let count = int.to_string(model.count)
  let on_mouse_move = fn(event) {
    use x <- result.try(dynamic.field("offsetX", dynamic.int)(event))
    use y <- result.try(dynamic.field("offsetY", dynamic.int)(event))

    Ok(Move(x, y))
  }

  ui.stack([], [
    ui.sequence([], [
      ui.button([event.on_click(Decr)], [element.text("-")]),
      ui.centre([], html.span([], [element.text(count)])),
      ui.button([event.on_click(Incr)], [element.text("+")]),
    ]),
    ui.cluster([], [
      ui.input([event.on_input(Change), attribute.value(model.input)]),
      html.span([], [element.text(model.input)]),
    ]),
    ui.centre(
      [
        event.on("mousemove", on_mouse_move),
        server.include(["offsetX", "offsetY"]),
        attribute.style([
          #("aspect-ratio", "1 / 1 "),
          #("background-color", "var(--element-background)"),
        ]),
      ],
      html.div([], [
        html.p([], [element.text("x: " <> int.to_string(model.mouse.0))]),
        html.p([], [element.text("y: " <> int.to_string(model.mouse.1))]),
      ]),
    ),
  ])
}
