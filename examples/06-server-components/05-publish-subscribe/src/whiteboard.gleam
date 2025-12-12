// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/option.{type Option, None, Some}
import lustre/attribute.{attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg
import lustre/event
import lustre/server_component
import server/pubsub.{type PubSubComponent}

// MODEL -----------------------------------------------------------------------

pub type Color {
  Blue
  Red
  Green
  Yellow
}

fn color_to_string(color: Color) -> String {
  case color {
    Red -> "red"
    Blue -> "blue"
    Green -> "green"
    Yellow -> "yellow"
  }
}

/// The model now contains only domain state - no registry or transport concerns.
pub type Model {
  Model(drawn_points: Dict(#(Int, Int), Color), selected_color: Color)
}

// MESSAGES --------------------------------------------------------------------

/// Messages that are shared/broadcast to all connected clients.
pub type SharedMsg {
  ClientDrewCircle(x: Int, y: Int, color: Color)
  ClientClearedScreen
}

/// Messages local to this component instance.
pub type LocalMsg {
  UserChangedColor(color: Color)
  UserDrewCircle(x: Int, y: Int, color: Color)
  UserClearedScreen
}

// COMPONENT -------------------------------------------------------------------

/// Create the whiteboard component using the PubSubComponent interface.
///
/// This cleanly separates:
/// - Domain logic (drawing, colors) in update/on_shared/view
/// - Pub/sub infrastructure (handled by the pubsub middleware)
/// - Transport concerns (handled by websocket_handler)
pub fn component() -> PubSubComponent(Model, LocalMsg, SharedMsg) {
  pubsub.PubSubComponent(
    topic: "whiteboard",
    init_model: Model(drawn_points: dict.new(), selected_color: Red),
    init_effect: effect.none(),
    update: update,
    on_shared: on_shared,
    view: view,
  )
}

// UPDATE ----------------------------------------------------------------------

/// Handle local messages from this client's UI.
/// Returns the updated model, effects, and optionally a message to broadcast.
fn update(
  model: Model,
  msg: LocalMsg,
) -> #(Model, Effect(LocalMsg), Option(SharedMsg)) {
  case msg {
    UserChangedColor(color) ->
      // Color selection is local to this client, no broadcast needed
      #(Model(..model, selected_color: color), effect.none(), None)

    UserDrewCircle(x, y, color) ->
      // User wants to draw - broadcast this to all clients
      // We don't update locally here; we'll receive our own broadcast back
      #(model, effect.none(), Some(ClientDrewCircle(x, y, color)))

    UserClearedScreen ->
      // User wants to clear - broadcast to all clients
      #(model, effect.none(), Some(ClientClearedScreen))
  }
}

/// Handle shared messages received from the pub/sub topic.
/// These come from any client (including ourselves).
fn on_shared(model: Model, msg: SharedMsg) -> #(Model, Effect(LocalMsg)) {
  case msg {
    ClientDrewCircle(x, y, color) -> {
      let points = dict.insert(model.drawn_points, #(x, y), color)
      #(Model(..model, drawn_points: points), effect.none())
    }

    ClientClearedScreen -> #(
      Model(..model, drawn_points: dict.new()),
      effect.none(),
    )
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(LocalMsg) {
  //html.div([], [html.text("TOAST")])
  let on_mouse_move =
    event.on("mousemove", {
      use button <- decode.field("buttons", decode.int)
      use client_x <- decode.field("clientX", decode.int)
      use client_y <- decode.field("clientY", decode.int)

      case button {
        1 ->
          decode.success(UserDrewCircle(
            x: client_x,
            y: client_y,
            color: model.selected_color,
          ))
        _ -> decode.failure(UserDrewCircle(x: 0, y: 0, color: Red), "Msg")
      }
    })
    |> server_component.include(["buttons", "clientX", "clientY"])
    |> event.throttle(5)

  element.fragment([
    html.style([], styles()),
    html.div([attribute.id("controls")], [
      color_button(Red, model.selected_color),
      color_button(Green, model.selected_color),
      color_button(Blue, model.selected_color),
      color_button(Yellow, model.selected_color),
      html.button([event.on_click(UserClearedScreen)], [html.text("Clear")]),
    ]),
    html.svg([on_mouse_move], {
      use points, #(x, y), color <- dict.fold(model.drawn_points, [])
      let point =
        svg.circle([
          attribute("cx", int.to_string(x)),
          attribute("cy", int.to_string(y)),
          attribute("r", "5"),
          attribute("fill", color_to_string(color)),
        ])

      [point, ..points]
    }),
  ])
}

fn color_button(color: Color, selected: Color) -> Element(LocalMsg) {
  let class = case color == selected {
    True -> "colour selected"
    False -> "colour"
  }

  html.button(
    [
      attribute.class(class),
      attribute.style("background-color", color_to_string(color)),
      event.on_click(UserChangedColor(color)),
    ],
    [],
  )
}

fn styles() -> String {
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
}
