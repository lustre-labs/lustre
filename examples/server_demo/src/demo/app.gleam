// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder}
import gleam/int
import gleam/json
import gleam/result
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/server
import lustre/ui

// MODEL -----------------------------------------------------------------------

pub type Model =
  Int

pub fn init(count: Int) -> #(Model, Effect(Msg)) {
  #(count, effect.none())
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Incr
  Decr
  Reset(Int)
}

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Incr -> #(model + 1, effect.none())
    Decr -> #(model - 1, effect.none())
    Reset(count) -> #(
      count,
      effect.event(
        "changed",
        json.string("You reset the count to: " <> int.to_string(count)),
      ),
    )
  }
}

pub fn on_attribute_change() -> Dict(String, Decoder(Msg)) {
  dict.from_list([
    #("count", fn(dyn) {
      dyn
      |> dynamic.int
      |> result.map(Reset)
    }),
  ])
}

// VIEW ------------------------------------------------------------------------

pub fn view(model: Model) -> Element(Msg) {
  let count = int.to_string(model)

  ui.centre(
    [attribute.style([#("width", "100vw"), #("height", "100vh")])],
    ui.sequence([], [
      ui.button([event.on_click(Decr)], [element.text("-")]),
      ui.centre([], html.span([], [element.text(count)])),
      ui.button([event.on_click(Incr)], [element.text("+")]),
    ]),
  )
  // ui.cluster([], [
  //   ui.input([event.on_input(Change), attribute.value(model.input)]),
  //   html.span([], [element.text(model.input)]),
  // ]),
  // ui.centre(
  //   [
  //     event.on("mousemove", on_mouse_move),
  //     server.include(["offsetX", "offsetY"]),
  //     attribute.style([
  //       #("aspect-ratio", "1 / 1 "),
  //       #("background-color", "var(--element-background)"),
  //     ]),
  //   ],
  //   html.div([], [
  //     html.p([], [element.text("x: " <> int.to_string(model.mouse.0))]),
  //     html.p([], [element.text("y: " <> int.to_string(model.mouse.1))]),
  //   ]),
  // ),
}
