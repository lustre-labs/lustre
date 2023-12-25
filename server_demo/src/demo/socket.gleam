// IMPORTS ---------------------------------------------------------------------

import demo/app
import gleam/bit_array
import gleam/dynamic.{DecodeError}
import gleam/erlang/process.{type Subject}
import gleam/function.{identity}
import gleam/http/request.{type Request as HttpRequest}
import gleam/http/response.{type Response as HttpResponse}
import gleam/json
import gleam/option.{Some}
import gleam/otp/actor
import gleam/result
import lustre
import lustre/element.{type Element}
import lustre/server/runtime.{type Message}
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage,
}

// 

pub fn handle(req: HttpRequest(Connection)) -> HttpResponse(ResponseData) {
  mist.websocket(req, update, init, function.constant(Nil))
}

type Model(flags, model, msg) {
  Model(self: Subject(Element(msg)), app: Subject(Message(msg)))
}

fn init(_) {
  let assert Ok(app) = lustre.start_server(app.new(), 0)
  let self = process.new_subject()
  let model = Model(self, app)
  let selector = process.selecting(process.new_selector(), self, identity)

  runtime.add_renderer(app, process.send(self, _))

  #(model, Some(selector))
}

fn update(
  model: Model(flags, model, msg),
  conn: WebsocketConnection,
  msg: WebsocketMessage(Element(a)),
) {
  case msg {
    mist.Text(bits) -> {
      json.decode_bits(bits, fn(dyn) {
        use kind <- result.try(dynamic.field("$", dynamic.string)(dyn))
        let handle_client_event = fn(tag, event) {
          runtime.handle_client_event(model.app, tag, event)
        }

        case kind {
          "Event" ->
            dynamic.decode2(
              handle_client_event,
              dynamic.field("tag", dynamic.string),
              dynamic.field("event", dynamic.dynamic),
            )(dyn)
          _ -> Error([DecodeError(expected: "Event", found: kind, path: ["$"])])
        }
      })
      |> result.unwrap(Nil)

      actor.continue(model)
    }
    mist.Binary(_) -> actor.continue(model)
    mist.Closed -> actor.continue(model)
    mist.Shutdown -> actor.Stop(process.Normal)
    mist.Custom(el) -> {
      json.object([#("$", json.string("Patch")), #("vdom", element.encode(el))])
      |> json.to_string
      |> bit_array.from_string
      |> mist.send_text_frame(conn, _)

      actor.continue(model)
    }
  }
}
