// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode
import gleam/int
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/platform/dom

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(platform) = dom.platform("#app")
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(x: Int, y: Int)
}

fn init(_) -> Model {
  Model(x: 0, y: 0)
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserMovedMouse(x: Int, y: Int)
}

fn update(_, msg: Msg) -> Model {
  case msg {
    UserMovedMouse(x:, y:) -> Model(x:, y:)
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div(
    [attribute.class("w-screen h-screen flex justify-center items-center")],
    [view_xy_pad(x: model.x, y: model.y, on_mousemove: UserMovedMouse)],
  )
}

fn view_xy_pad(
  x x: Int,
  y y: Int,
  on_mousemove handle_mousemove: fn(Int, Int) -> msg,
) -> Element(msg) {
  let on_mousemove =
    // Custom event handlers can be created using `event.on` and providing a
    // decoder for the event object. In this case we have a `MouseEvent` which we
    // decode the x and y coordinates from.
    //
    // For other properties you could decode from this specific event, see the
    // MouseEvent docs: https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent
    //
    event.on("mousemove", {
      use x <- decode.field("offsetX", decode.int)
      use y <- decode.field("offsetY", decode.int)

      decode.success(handle_mousemove(x, y))
    })

  html.div(
    [
      on_mousemove,
      attribute.class("flex justify-center items-center"),
      attribute.class("size-64 bg-slate-100 rounded hover:shadow"),
    ],
    [
      html.p([attribute.class("font-mono font-semibold text-slate-400")], [
        html.text("x: "),
        html.text(int.to_string(x)),
        html.text(", y: "),
        html.text(int.to_string(y)),
      ]),
    ],
  )
}
