// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/list
import group_registry.{type GroupRegistry}
import lustre.{type App}
import lustre/attribute.{attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg
import lustre/event
import lustre/server_component

// MAIN ------------------------------------------------------------------------

pub fn component() -> App(GroupRegistry(SharedMsg), Model, Msg) {
  lustre.application(init, update, view)
}

// MODEL -----------------------------------------------------------------------

pub type Color {
  Blue
  Red
  Green
  Yellow
}

fn to_string(color: Color) -> String {
  case color {
    Red -> "red"
    Blue -> "blue"
    Green -> "green"
    Yellow -> "yellow"
  }
}

pub type Model {
  Model(
    drawn_points: Dict(#(Int, Int), Color),
    selected_color: Color,
    registry: GroupRegistry(SharedMsg),
  )
}

fn init(registry: GroupRegistry(SharedMsg)) -> #(Model, Effect(Msg)) {
  let model = Model(drawn_points: dict.new(), selected_color: Red, registry:)

  // When the main app starts our server component, it passes us a process registry
  // that we can use to communicate with other instances of the whiteboard.
  //
  // On init, we want to subscribe to the "whiteboard" topic so we can receive
  // messages broadcast to the registry.
  //
  // Note: This still does not make a complete whiteboard app! Try drawing with
  // the first client before connecting the second one.
  #(model, subscribe(registry, AppReceivedSharedMsg))
}

fn subscribe(
  registry: GroupRegistry(topic),
  on_msg handle_msg: fn(topic) -> msg,
) -> Effect(msg) {
  // Using the special `select` effect lets us return a `Selector` so that we
  // can receive messages from processes outside of our server component runtime.
  use _, _ <- server_component.select

  // Joining a topic in the registry returns a `Subject` we need to select on
  // in order to subscribe to messages sent to that topic.
  let subject = group_registry.join(registry, "whiteboard", process.self())

  // We need to teach the server component runtime to listen for messages on
  // this subject by returning a `Selector` that matches our apps `msg` type.
  let selector =
    process.new_selector()
    |> process.select_map(subject, handle_msg)

  selector
}

// UPDATE ----------------------------------------------------------------------

/// We have 2 kinds of messages:
///
/// - Messages that originate from this instance of the server-component
/// - Messages that we receive from and send to the glubsub topic.
///
/// `SharedMsg` contains messages of the latter type.
pub opaque type SharedMsg {
  // Received or sent when any client wants to draw on the whiteboard.
  ClientDrewCircle(x: Int, y: Int, color: Color)
  /// Received or sent when any client wants to clear the screen.
  ClientClearedScreen
}

/// `Msg` is the usual message type in a Lustre app and contains all messages
/// that a single instance of our server component can receive.
pub opaque type Msg {
  /// We received some SharedMsg from the glubsub topic.
  AppReceivedSharedMsg(msg: SharedMsg)
  /// The user wants to change their selected color;
  /// This color can be different for every single client.
  UserChangedColor(color: Color)
  /// We have a way to get notified if any client wants to modify the whiteboard
  /// through the shared topic, but we still need a way for the user to tell us
  /// that they want to in the first place!
  UserDrewCircle(x: Int, y: Int, color: Color)
  ///
  UserClearedScreen
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    AppReceivedSharedMsg(ClientDrewCircle(x:, y:, color:)) -> {
      // If any client wants to draw on the screen, we do that update locally
      // to reflect that change.
      //
      // Note: This means that we duplicate this state in every single client!
      let new_points =
        model.drawn_points
        |> dict.insert(#(x, y), color)

      #(Model(..model, drawn_points: new_points), effect.none())
    }

    AppReceivedSharedMsg(ClientClearedScreen) -> {
      let model = Model(..model, drawn_points: dict.new())

      #(model, effect.none())
    }

    UserChangedColor(color:) -> {
      #(Model(..model, selected_color: color), effect.none())
    }

    UserDrewCircle(x:, y:, color:) -> {
      // If a user wants to draw, instead of doing that directly we broadcast
      // that intent as a `SharedMsg` over our topic.
      // Later, we will receive that same message ourselves again, at which point
      // we will update our whiteboard.
      #(model, broadcast(model.registry, ClientDrewCircle(x:, y:, color:)))
    }

    UserClearedScreen -> {
      #(model, broadcast(model.registry, ClientClearedScreen))
    }
  }
}

fn broadcast(registry: GroupRegistry(msg), msg: msg) -> Effect(any) {
  use _ <- effect.from
  use member <- list.each(group_registry.members(registry, "whiteboard"))

  process.send(member, msg)
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
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
        [
          attribute.class("colour"),
          attribute.style("background-color", "red"),
          event.on_click(UserChangedColor(Red)),
        ],
        [],
      ),
      html.button(
        [
          attribute.class("colour selected"),
          attribute.style("background-color", "green"),
          event.on_click(UserChangedColor(Green)),
        ],
        [],
      ),
      html.button(
        [
          attribute.class("colour"),
          attribute.style("background-color", "blue"),
          event.on_click(UserChangedColor(Blue)),
        ],
        [],
      ),
      html.button(
        [
          attribute.class("colour"),
          attribute.style("background-color", "yellow"),
          event.on_click(UserChangedColor(Yellow)),
        ],
        [],
      ),
      html.button([event.on_click(UserClearedScreen)], [html.text("Clear")]),
    ]),
    html.svg([on_mouse_move], {
      use points, #(x, y), color <- dict.fold(model.drawn_points, [])
      let point =
        svg.circle([
          attribute("cx", int.to_string(x)),
          attribute("cy", int.to_string(y)),
          attribute("r", "5"),
          attribute("fill", to_string(color)),
        ])

      [point, ..points]
    }),
  ])
}
