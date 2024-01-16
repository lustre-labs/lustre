// IMPORTS ---------------------------------------------------------------------

import demo/app
import gleam/bool
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
import lustre.{type Action, type ServerComponent}
import lustre/element.{type Patch}
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage,
}

// 

pub fn handle(req: HttpRequest(Connection)) -> HttpResponse(ResponseData) {
  mist.websocket(req, update, init, function.constant(Nil))
}

type Model(flags, model, msg) {
  Model(self: Subject(Patch(msg)), app: Subject(Action(ServerComponent, msg)))
}

fn init(_) {
  let assert Ok(app) =
    lustre.server_component(app.init, app.update, app.view)
    |> lustre.start_actor(0)
  let self = process.new_subject()
  let model = Model(self, app)
  let selector = process.selecting(process.new_selector(), self, identity)

  actor.send(app, lustre.add_renderer(process.send(self, _)))
  #(model, Some(selector))
}

fn update(
  model: Model(flags, model, msg),
  conn: WebsocketConnection,
  msg: WebsocketMessage(Patch(a)),
) {
  case msg {
    mist.Text(bits) -> {
      let _ = {
        use dyn <- json.decode_bits(bits)
        use kind <- result.try(dynamic.field("$", dynamic.string)(dyn))
        use <- bool.guard(
          kind != "Event",
          Error([DecodeError(expected: "Event", found: kind, path: ["$"])]),
        )
        use tag <- result.try(dynamic.field("tag", dynamic.string)(dyn))
        use event <- result.try(dynamic.field("event", dynamic.dynamic)(dyn))

        actor.send(model.app, lustre.event(tag, event))
        Ok(Nil)
      }

      actor.continue(model)
    }
    mist.Binary(_) -> actor.continue(model)
    mist.Closed -> actor.continue(model)
    mist.Shutdown -> actor.Stop(process.Normal)
    mist.Custom(patch) -> {
      element.encode_patch(patch)
      |> json.to_string
      |> bit_array.from_string
      |> mist.send_text_frame(conn, _)

      actor.continue(model)
    }
  }
}
