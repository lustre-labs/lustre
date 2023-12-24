// IMPORTS ---------------------------------------------------------------------

import gleam/dict
import gleam/erlang/process
import gleam/http/request.{type Request as HttpRequest}
import gleam/int
import lustre
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/server
import lustre/ui
import mist.{type Connection}
import socket
import web

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app =
    lustre.server_component(
      init,
      update,
      view,
      // Listen to incoming client events and decode them into app messages. In
      // the future you'll be able to tell Lustre what properties of the event
      // object to send over the wire, but for now it's always empty.
      dict.from_list([
        #("incr", fn(_) { Ok(Incr) }),
        #("decr", fn(_) { Ok(Decr) }),
      ]),
    )

  let assert Ok(app) = lustre.start_server(app, 0)
  let assert Ok(_) =
    // Wisp doesn't currently support websockets so we have to handle the request
    // before we pass it to Wisp in case it's a websocket request.
    fn(req: HttpRequest(Connection)) {
      case req.path {
        "/ws" -> socket.handle(req, app)
        _ -> web.handle(req)
      }
    }
    |> mist.new
    |> mist.port(8000)
    |> mist.start_http

  process.sleep_forever()
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

fn init(count: Int) -> #(Model, Effect(Msg)) {
  #(count, effect.none())
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  Incr
  Decr
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Incr -> #(model + 1, effect.none())
    Decr -> #(model - 1, effect.none())
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model) -> Element(msg) {
  let count = int.to_string(model)

  ui.sequence([], [
    ui.button([server.on_click("decr")], [element.text("-")]),
    ui.centre([], html.span([], [element.text(count)])),
    ui.button([server.on_click("incr")], [element.text("+")]),
  ])
}
