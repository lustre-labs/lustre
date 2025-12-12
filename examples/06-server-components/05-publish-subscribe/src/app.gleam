// IMPORTS ---------------------------------------------------------------------

import gleam/bytes_tree
import gleam/erlang/application
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/option
import gleam/otp/actor
import group_registry
import lustre/attribute
import lustre/element
import lustre/element/html.{html}
import lustre/server_component
import mist.{type Connection, type ResponseData}
import server/component_contract
import server/pubsub
import server/websocket_handler.{WebSocketConfig}
import whiteboard

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // Create the shared registry for pub/sub communication between clients.
  // This is passed to all whiteboard component instances via the Resources system.
  let name = process.new_name("whiteboard-registry")
  let assert Ok(actor.Started(data: registry, ..)) = group_registry.start(name)

  // Create the whiteboard contract using the pub/sub middleware.
  // The middleware handles subscription and broadcasting automatically.
  let whiteboard_contract =
    whiteboard.component()
    |> pubsub.to_contract_with(fn(resources) {
      // Extract the registry from resources - we know it's there because we put it there
      component_contract.get_resources_unsafe(resources)
    })

  // Configure the websocket handler with the registry as a resource
  // and a simple connection ID generator.
  let connection_counter = new_counter()
  let ws_config =
    WebSocketConfig(
      resources: component_contract.resources(registry),
      generate_id: fn() {
        "conn-" <> int.to_string(increment(connection_counter))
      },
    )

  // Create the generic websocket handler
  let ws_handler = websocket_handler.handler(whiteboard_contract, ws_config)

  let assert Ok(_) =
    fn(request: Request(Connection)) -> Response(ResponseData) {
      case echo request.path_segments(request) {
        [] -> serve_html()
        ["lustre", "runtime.mjs"] -> serve_runtime()
        ["ws"] -> ws_handler(request)
        _ -> response.set_body(response.new(404), mist.Bytes(bytes_tree.new()))
      }
    }
    |> mist.new
    |> mist.bind("localhost")
    |> mist.port(1234)
    |> mist.start

  process.sleep_forever()
}

// CONNECTION ID COUNTER -------------------------------------------------------

/// Simple counter actor for generating unique connection IDs.
type CounterMsg {
  Increment(reply: process.Subject(Int))
}

fn new_counter() -> process.Subject(CounterMsg) {
  let assert Ok(actor.Started(data: subject, ..)) =
    actor.new(0)
    |> actor.on_message(fn(count, msg) {
      case msg {
        Increment(reply) -> {
          process.send(reply, count + 1)
          actor.continue(count + 1)
        }
      }
    })
    |> actor.start

  subject
}

fn increment(counter: process.Subject(CounterMsg)) -> Int {
  process.call(counter, waiting: 1000, sending: Increment)
}

// HTML ------------------------------------------------------------------------

fn serve_html() -> Response(ResponseData) {
  let page =
    html([attribute.lang("en")], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),
        html.title([], "06-server-components/05-publish-subscribe"),
        html.script(
          [attribute.type_("module"), attribute.src("/lustre/runtime.mjs")],
          "",
        ),
      ]),
      html.body([attribute.style("height", "100dvh")], [
        html.text("toarst"),
        server_component.element([server_component.route("/ws")], []),
      ]),
    ])
    |> element.to_document_string_tree
    |> bytes_tree.from_string_tree

  response.new(200)
  |> response.set_body(mist.Bytes(page))
  |> response.set_header("content-type", "text/html")
}

// JAVASCRIPT ------------------------------------------------------------------

fn serve_runtime() -> Response(ResponseData) {
  let assert Ok(lustre_priv) = application.priv_directory("lustre")
  let file_path = lustre_priv <> "/static/lustre-server-component.min.mjs"

  case mist.send_file(file_path, offset: 0, limit: option.None) {
    Ok(file) ->
      response.new(200)
      |> response.prepend_header("content-type", "application/javascript")
      |> response.set_body(file)

    Error(_) ->
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_tree.new()))
  }
}
