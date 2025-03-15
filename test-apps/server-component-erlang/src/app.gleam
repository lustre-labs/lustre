import counter
import gleam/erlang
import gleam/erlang/process.{type Selector, type Subject}
import gleam/function
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import lustre
import lustre/server_component
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage,
}

pub fn main() {
  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      case request.path_segments(req) {
        // Set up the websocket connection to the client. This is how we send
        // DOM updates to the browser and receive events from the client.
        ["ws"] ->
          mist.websocket(
            request: req,
            on_init: socket_init,
            on_close: socket_close,
            handler: socket_update,
          )

        ["sse"] ->
          mist.server_sent_events(
            request: req,
            initial_response: response.new(200),
            init: fn() {
              let self = process.new_subject()
              let app = counter.app()
              let assert Ok(counter) = lustre.start_actor(app, 0)

              server_component.register_subject(counter, self)

              actor.Ready(
                counter,
                process.selecting(
                  process.new_selector(),
                  self,
                  function.identity,
                ),
              )
            },
            loop: fn(patch, conn, state) {
              let assert Ok(_) =
                patch
                |> server_component.client_message_to_json
                |> json.to_string_tree
                |> mist.event
                |> mist.send_event(conn, _)

              actor.continue(state)
            },
          )

        // We need to serve the server component runtime. There's also a minified
        // version of this script for production.
        ["lustre-server-component.mjs"] -> {
          let assert Ok(priv) = erlang.priv_directory("lustre")
          let path = priv <> "/static/lustre-server-component.min.mjs"
          let assert Ok(script) = mist.send_file(path, offset: 0, limit: None)

          response.new(200)
          |> response.prepend_header("content-type", "application/javascript")
          |> response.set_body(script)
        }

        // For all other requests we'll just serve some HTML that renders the
        // server component.
        _ -> {
          let assert Ok(priv) = erlang.priv_directory("app")
          let path = priv <> "/static/index.html"
          let assert Ok(html) = mist.send_file(path, offset: 0, limit: None)

          response.new(200)
          |> response.prepend_header("content-type", "text/html")
          |> response.set_body(html)
        }
      }
    }
    |> mist.new
    |> mist.port(3000)
    |> mist.start_http

  process.sleep_forever()
}

//

type Counter =
  Subject(lustre.RuntimeMessage(counter.Msg))

fn socket_init(
  _,
) -> #(Counter, Option(Selector(server_component.ClientMessage(counter.Msg)))) {
  let self = process.new_subject()
  let app = counter.app()
  let assert Ok(counter) = lustre.start_actor(app, 0)

  server_component.register_subject(counter, self)

  #(
    counter,
    Some(process.selecting(process.new_selector(), self, function.identity)),
  )
}

fn socket_update(
  counter: Counter,
  conn: WebsocketConnection,
  msg: WebsocketMessage(server_component.ClientMessage(counter.Msg)),
) {
  case msg {
    mist.Text(json) -> {
      // we attempt to decode the incoming text as an action to send to our
      // server component runtime.
      let message = json.parse(json, server_component.runtime_message_decoder())

      case message {
        Ok(message) -> process.send(counter, message)
        Error(_) -> Nil
      }

      actor.continue(counter)
    }

    mist.Binary(_) -> actor.continue(counter)
    mist.Custom(patch) -> {
      let assert Ok(_) =
        patch
        |> server_component.client_message_to_json
        |> json.to_string
        |> mist.send_text_frame(conn, _)

      actor.continue(counter)
    }
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
  }
}

fn socket_close(counter: Counter) {
  process.send(counter, lustre.shutdown())
}
