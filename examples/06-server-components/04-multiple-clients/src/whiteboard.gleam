// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode
import gleam/int
import gleam/set.{type Set}
import lustre.{type App}
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg
import lustre/event
import lustre/server_component

// MAIN ------------------------------------------------------------------------

pub fn component() -> App(_, Model, Msg) {
  lustre.simple(init, update, view)
}

// MODEL -----------------------------------------------------------------------

pub type Model =
  Set(#(Int, Int))

fn init(_) -> Model {
  set.new()
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  UserDrewCircle(x: Int, y: Int)
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    UserDrewCircle(x:, y:) -> set.insert(model, #(x, y))
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let on_mouse_move =
    event.on("mousemove", {
      use button <- decode.field("buttons", decode.int)
      use client_x <- decode.field("clientX", decode.int)
      use client_y <- decode.field("clientY", decode.int)

      case button {
        1 -> decode.success(UserDrewCircle(x: client_x, y: client_y))
        _ -> decode.failure(UserDrewCircle(x: 0, y: 0), "Msg")
      }
    })
    |> server_component.include(["buttons", "clientX", "clientY"])
    |> event.throttle(10)

  element.fragment([
    html.style([], {
      "
      #controls {
        align-items: center;
        background-color: white;
        border-radius: 2rem;
        box-shadow: 0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1);
        display: flex;
        gap: 1rem;
        left: 1rem;
        padding: 1rem 1.5rem;
        position: fixed;
        bottom: 1rem;
        z-index: 1;
        width: max-content;

        .colour {
          border: none;
          width: 3rem;
          height: 3rem;
          border-radius: 50%;
          display: inline-block;
        }

        .colour.selected {
          filter: darken(0.2);
        }
      }

      svg {
        background-color: oklch(98.4% 0.003 247.858);
        position: fixed;
        top: 0;
        left: 0;
        width: 100vw;
        height: 100vh;
      }
      "
    }),
    html.div([attribute.id("controls")], [
      html.button(
        [attribute.class("colour"), attribute.style("background-color", "red")],
        [],
      ),
      html.button(
        [
          attribute.class("colour selected"),
          attribute.style("background-color", "green"),
        ],
        [],
      ),
      html.button(
        [attribute.class("colour"), attribute.style("background-color", "blue")],
        [],
      ),
      html.button(
        [
          attribute.class("colour"),
          attribute.style("background-color", "yellow"),
        ],
        [],
      ),
      html.button([], [html.text("Clear")]),
    ]),
    html.svg([on_mouse_move], {
      use points, #(x, y) <- set.fold(model, [])
      let point =
        svg.circle([
          attribute("cx", int.to_string(x)),
          attribute("cy", int.to_string(y)),
          attribute("r", "5"),
          attribute("fill", "red"),
        ])

      [point, ..points]
    }),
  ])
}
