import counter
import gleam/bytes_builder
import gleam/erlang
import gleam/erlang/process.{type Selector, type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{type Option, None}
import gleam/otp/actor
import gleam/result
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html.{html}
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
        ["counter"] ->
          mist.websocket(
            request: req,
            on_init: socket_init,
            on_close: socket_close,
            handler: socket_update,
          )

        // We need to serve the server component runtime. There's also a minified
        // version of this script for production.
        ["lustre-server-component.mjs"] -> {
          let assert Ok(priv) = erlang.priv_directory("lustre")
          let path = priv <> "/static/lustre-server-component.mjs"

          mist.send_file(path, offset: 0, limit: None)
          |> result.map(fn(script) {
            response.new(200)
            |> response.prepend_header("content-type", "application/javascript")
            |> response.set_body(script)
          })
          |> result.lazy_unwrap(fn() {
            response.new(404)
            |> response.set_body(mist.Bytes(bytes_builder.new()))
          })
        }

        // For all other requests we'll just serve some HTML that renders the
        // server component.
        _ ->
          response.new(200)
          |> response.prepend_header("content-type", "text/html")
          |> response.set_body(
            html([], [
              html.head([], [
                html.link([
                  attribute.rel("stylesheet"),
                  attribute.href(
                    "https://cdn.jsdelivr.net/gh/lustre-labs/ui/priv/styles.css",
                  ),
                ]),
                html.script(
                  [
                    attribute.type_("module"),
                    attribute.src("/lustre-server-component.mjs"),
                  ],
                  "",
                ),
              ]),
              html.body([], [
                server_component.component([server_component.route("/counter")]),
              ]),
            ])
            |> element.to_document_string_builder
            |> bytes_builder.from_string_builder
            |> mist.Bytes,
          )
      }
    }
    |> mist.new
    |> mist.port(3000)
    |> mist.start_http

  process.sleep_forever()
}

//

type Counter =
  Subject(lustre.Action(counter.Msg, lustre.ServerComponent))

fn socket_init(
  conn: WebsocketConnection,
) -> #(Counter, Option(Selector(lustre.Patch(counter.Msg)))) {
  let app = counter.app()
  let assert Ok(counter) = lustre.start_actor(app, 0)

  process.send(
    counter,
    server_component.subscribe(
      // server components can have many connected clients, so we need a way to
      // identify this client.
      "ws",
      // this callback is called whenever the server component has a new patch
      // to send to the client. here we json encode that patch and send it to
      // via the websocket connection.
      //
      // a more involved version would have us sending the patch to this socket's
      // subject, and then it could be handled (perhaps with some other work) in
      // the `mist.Custom` branch of `socket_update` below.
      fn(patch) {
        let assert Ok(_) =
          patch
          |> server_component.encode_patch
          |> json.to_string
          |> mist.send_text_frame(conn, _)

        Nil
      },
    ),
  )

  #(
    // we store the server component's `Subject` as this socket's state so we
    // can shut it down when the socket is closed.
    counter,
    // the `None` here means we aren't planning on receiving any messages from
    // elsewhere and dont need a `Selector` to handle them.
    None,
  )
}

import gleam/io

fn socket_update(
  counter: Counter,
  _conn: WebsocketConnection,
  msg: WebsocketMessage(lustre.Patch(counter.Msg)),
) {
  case msg {
    mist.Text(json) -> {
      // we attempt to decode the incoming text as an action to send to our
      // server component runtime.
      let action = json.decode(json, server_component.decode_action)

      case action {
        Ok(action) -> process.send(counter, action)
        Error(_) -> Nil
      }

      actor.continue(counter)
    }

    mist.Binary(_) -> actor.continue(counter)
    mist.Custom(_) -> actor.continue(counter)
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
  }
}

fn socket_close(counter: Counter) {
  process.send(counter, lustre.shutdown())
}
