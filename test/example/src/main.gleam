// IMPORTS ---------------------------------------------------------------------

import gleam/javascript/promise.{Promise}
import gleam/string
import lustre
import lustre/cmd.{Cmd}
import lustre/element.{Element}
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() -> Promise(fn(Msg) -> Nil) {
  let selector = "[data-lustre-container]"
  let program = lustre.application(init(), update, render)

  use _ <- promise.tap(lustre.start(program, selector))
  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

fn init() -> #(Model, Cmd(Msg)) {
  #(0, cmd.none())
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  SetCount(Int)
}

fn update(_: Model, msg: Msg) -> #(Model, Cmd(Msg)) {
  case msg {
    SetCount(n) -> #(n, cmd.none())
  }
}

// RENDER ----------------------------------------------------------------------

fn render(model: Model) -> Element(Msg) {
  element.div(
    [],
    [
      element.map(
        fn() {
          element.button([event.on_click(model + 1)], [element.text("+")])
        },
        SetCount,
      ),
      element.text(string.inspect(model)),
    ],
  )
}
