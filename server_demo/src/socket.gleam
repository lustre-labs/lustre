// IMPORTS ---------------------------------------------------------------------

import gleam/bit_array
import gleam/dynamic
import gleam/erlang/process.{type Subject}
import gleam/function.{identity}
import gleam/http/request.{type Request as HttpRequest}
import gleam/http/response.{type Response as HttpResponse}
import gleam/json
import gleam/option.{Some}
import gleam/otp/actor
import lustre/element.{type Element}
import lustre/server_runtime.{type Message}
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage,
}

// MAIN ------------------------------------------------------------------------

pub fn handle(
  req: HttpRequest(Connection),
  app: Subject(Message(model, msg)),
) -> HttpResponse(ResponseData) {
  mist.websocket(req, update, init(app), close)
}

type Model(model, msg) {
  Model(self: Subject(Element(msg)), app: Subject(Message(model, msg)))
}

fn init(app: Subject(Message(model, msg))) {
  fn(_) {
    let self = process.new_subject()
    let model = Model(self, app)
    let selector = process.selecting(process.new_selector(), self, identity)

    server_runtime.add_renderer(app, process.send(self, _))

    #(model, Some(selector))
  }
}

fn update(
  model: Model(model, msg),
  conn: WebsocketConnection,
  msg: WebsocketMessage(Element(a)),
) {
  case msg {
    mist.Text(bits) -> {
      let _ =
        json.decode_bits(bits, fn(dyn) {
          case dynamic.field("$", dynamic.string)(dyn) {
            Ok("Event") ->
              dyn
              |> dynamic.decode2(
                fn(tag, event) {
                  server_runtime.handle_client_event(model.app, tag, event)
                },
                dynamic.field("tag", dynamic.string),
                dynamic.field("event", dynamic.dynamic),
              )
            _ -> Error([])
          }
        })

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

fn close(_) -> Nil {
  Nil
}
