//// A "cross-site request forgery" (CSRF) attack is when a malicious website makes
//// requests to another website on behalf of an authenticated user without their
//// consent or knowledge.
//// 
//// This is possible because browsers automatically include HTTP cookies with
//// every request, but while browsers enforce CORS policies for cross-origin fetch
//// requests they don't do the same for WebSocket connections.
//// 
//// A common protection against CSRF attacks is to generate a random token on the
//// server and embed it in the HTML page. When a new WebSocket connection is
//// attempted, the server first checks that the token is included in the request
//// and matches the expected value before accepting the connection. This happens
//// *in addition* to any authentication checks the server might perform.
//// 

// IMPORTS ---------------------------------------------------------------------

import counter
import gleam/bytes_tree
import gleam/erlang/application
import gleam/erlang/process.{type Selector, type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html.{html}
import lustre/server_component
import mist.{type Connection, type ResponseData}
import youid/uuid

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // In a real application, the CSRF token should be tied to a user's session on
  // the server and *not* shared globally like this.
  let csrf_token =
    echo uuid.v4_string() as "Only accepting connections with this CSRF token: "

  let assert Ok(_) =
    fn(request: Request(Connection)) -> Response(ResponseData) {
      case request.path_segments(request) {
        [] -> serve_html(csrf_token)
        ["lustre", "runtime.mjs"] -> serve_runtime()
        ["ws"] -> serve_counter(request, csrf_token)
        _ -> response.set_body(response.new(404), mist.Bytes(bytes_tree.new()))
      }
    }
    |> mist.new
    |> mist.bind("localhost")
    |> mist.port(1234)
    |> mist.start

  process.sleep_forever()
}

// HTML ------------------------------------------------------------------------

fn serve_html(csrf_token: String) -> Response(ResponseData) {
  let html =
    html([attribute.lang("en")], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),
        html.meta([
          attribute.name("csrf-token"),
          // Embed the CSRF token in a meta tag. Lustre's server component runtime
          // will automatically read this token and include it in the WebSocket URL
          // when establishing the connection.
          attribute.content(csrf_token),
          // Swap out this valid token for the invalid one commented out below to
        // see how the server rejects the WebSocket connection if the token is
        // missing or incorrect.
        // attribute.content("invalid-token"),
        ]),
        html.title([], "06-server-components/06-csrf-protection"),
        html.script(
          [attribute.type_("module"), attribute.src("/lustre/runtime.mjs")],
          "",
        ),
      ]),
      html.body(
        [attribute.styles([#("max-width", "40rem"), #("margin", "3rem auto")])],
        [
          html.h1([], [html.text("CSRF-protected server component")]),
          html.p([], [
            html.text(
              "This page embeds a csrf-token meta tag. The Lustre client runtime reads that token and appends it to the initial WebSocket URL automatically.",
            ),
          ]),
          html.p([], [
            html.text(
              "The server will reject the WebSocket handshake with 403 Forbidden if the token is missing or invalid.",
            ),
          ]),
          server_component.element([server_component.route("/ws")], []),
        ],
      ),
    ])
    |> element.to_document_string_tree
    |> bytes_tree.from_string_tree

  response.new(200)
  |> response.set_body(mist.Bytes(html))
  |> response.set_header("content-type", "text/html")
}

// JAVASCRIPT ------------------------------------------------------------------

fn serve_runtime() -> Response(ResponseData) {
  let assert Ok(lustre_priv) = application.priv_directory("lustre")
  let file_path = lustre_priv <> "/static/lustre-server-component.mjs"

  case mist.send_file(file_path, offset: 0, limit: None) {
    Ok(file) ->
      response.new(200)
      |> response.prepend_header("content-type", "application/javascript")
      |> response.set_body(file)

    Error(_) ->
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_tree.new()))
  }
}

// WEBSOCKET -------------------------------------------------------------------

fn serve_counter(
  request: Request(Connection),
  expected_csrf_token: String,
) -> Response(ResponseData) {
  // Extract the CSRF token from the query parameters of the initial WebSocket
  // connection request. Browsers do not allow JavaScript to set custom headers
  // on WebSocket connections, so the token must be included in the URL directly
  // instead.
  //
  // The server component client runtime will automatically detect the presence
  // of a `<meta>` tag with the name `"csrf-token"` and include its value as a
  // `csrf-token` query parameter in the WebSocket URL when establishing the
  // connection.
  let provided_csrf_token =
    request
    |> request.get_query
    |> result.try(list.key_find(_, "csrf-token"))
    |> echo as "Validating provided CSRF token: "

  case provided_csrf_token {
    // Only when the provided token matches the one we expect do we upgrade the
    // connection to a WebSocket and start the server component.
    Ok(token) if token == expected_csrf_token ->
      mist.websocket(
        request:,
        on_init: init_counter_socket,
        handler: loop_counter_socket,
        on_close: close_counter_socket,
      )

    // When the token is invalid or missing, we reject the connection with a 403 
    // "forbidden" response.
    Ok(_) | Error(_) ->
      response.new(403)
      |> response.set_header("content-type", "text/plain")
      |> response.set_body(mist.Bytes(bytes_tree.from_string("Forbidden")))
  }
}

type CounterSocket {
  CounterSocket(
    component: lustre.Runtime(counter.Msg),
    self: Subject(server_component.ClientMessage(counter.Msg)),
  )
}

type CounterSocketMessage =
  server_component.ClientMessage(counter.Msg)

type CounterSocketInit =
  #(CounterSocket, Option(Selector(CounterSocketMessage)))

fn init_counter_socket(_) -> CounterSocketInit {
  let counter = counter.component()
  let assert Ok(component) = lustre.start_server_component(counter, Nil)

  let self = process.new_subject()
  let selector =
    process.new_selector()
    |> process.select(self)

  server_component.register_subject(self)
  |> lustre.send(to: component)

  #(CounterSocket(component:, self:), Some(selector))
}

fn loop_counter_socket(
  state: CounterSocket,
  message: mist.WebsocketMessage(CounterSocketMessage),
  connection: mist.WebsocketConnection,
) -> mist.Next(CounterSocket, CounterSocketMessage) {
  case message {
    mist.Text(json) -> {
      case json.parse(json, server_component.runtime_message_decoder()) {
        Ok(runtime_message) -> lustre.send(state.component, runtime_message)
        Error(_) -> Nil
      }

      mist.continue(state)
    }

    mist.Binary(_) -> {
      mist.continue(state)
    }

    mist.Custom(client_message) -> {
      let json = server_component.client_message_to_json(client_message)
      let assert Ok(_) = mist.send_text_frame(connection, json.to_string(json))

      mist.continue(state)
    }

    mist.Closed | mist.Shutdown -> mist.stop()
  }
}

fn close_counter_socket(state: CounterSocket) -> Nil {
  lustre.shutdown()
  |> lustre.send(to: state.component)
}
