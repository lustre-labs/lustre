// IMPORTS ---------------------------------------------------------------------

import gleam/erlang/process.{type Selector, type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{type Option, Some}
import lustre
import lustre/server_component
import mist.{type Connection, type ResponseData, type WebsocketConnection}
import server/component_contract.{
  type ComponentContract, type Resources, ConnectionContext,
}

// TYPES -----------------------------------------------------------------------

/// Configuration for the WebSocket handler.
pub type WebSocketConfig {
  WebSocketConfig(
    /// Resources to make available to components via ConnectionContext
    resources: Resources,
    /// Function to generate unique connection IDs
    generate_id: fn() -> String,
  )
}

/// Internal state for the websocket - generic over msg type.
type SocketState(msg) {
  SocketState(
    component: lustre.Runtime(msg),
    self: Subject(server_component.ClientMessage(msg)),
    on_disconnect: fn(lustre.Runtime(msg)) -> Nil,
  )
}

type SocketMessage(msg) =
  server_component.ClientMessage(msg)

type SocketInit(msg) =
  #(SocketState(msg), Option(Selector(SocketMessage(msg))))

// PUBLIC API ------------------------------------------------------------------

/// Create a websocket request handler for any component contract.
///
/// This returns a function that can be used directly with mist routing:
///
/// ```gleam
/// let ws_handler = websocket_handler.handler(my_contract, config)
///
/// case request.path_segments(request) {
///   ["ws"] -> ws_handler(request)
///   // ...
/// }
/// ```
pub fn handler(
  contract: ComponentContract(init_args, model, msg),
  config: WebSocketConfig,
) -> fn(Request(Connection)) -> Response(ResponseData) {
  fn(request: Request(Connection)) {
    mist.websocket(
      request: request,
      on_init: fn(_socket) { init_socket(contract, config) },
      handler: loop_socket,
      on_close: close_socket,
    )
  }
}

// INTERNAL --------------------------------------------------------------------

fn init_socket(
  contract: ComponentContract(init_args, model, msg),
  config: WebSocketConfig,
) -> SocketInit(msg) {
  // Create connection context with unique ID and resources
  let context =
    ConnectionContext(
      connection_id: config.generate_id(),
      resources: config.resources,
    )
  echo "starting websocket"
  // Let the contract create init args from context
  let init_args = contract.make_init_args(context)

  // Start the Lustre server component runtime
  let assert Ok(component) =
    lustre.start_server_component(contract.app, init_args)

  // Set up the subject and selector for receiving messages from runtime
  let self = process.new_subject()
  let selector =
    process.new_selector()
    |> process.select(self)

  // Register this subject with the runtime to receive ClientMessage broadcasts
  server_component.register_subject(self)
  |> lustre.send(to: component)

  // Call the connect hook
  contract.on_connect(component)

  #(
    SocketState(
      component: component,
      self: self,
      on_disconnect: contract.on_disconnect,
    ),
    Some(selector),
  )
}

fn loop_socket(
  state: SocketState(msg),
  message: mist.WebsocketMessage(SocketMessage(msg)),
  connection: WebsocketConnection,
) -> mist.Next(SocketState(msg), SocketMessage(msg)) {
  case echo message {
    // Client sent a text message (JSON-encoded runtime message)
    mist.Text(json_string) -> {
      case json.parse(json_string, server_component.runtime_message_decoder()) {
        Ok(runtime_message) -> lustre.send(state.component, runtime_message)
        Error(_) -> Nil
      }
      mist.continue(state)
    }

    // Binary messages are not used by Lustre server components
    mist.Binary(_) -> {
      mist.continue(state)
    }

    // Custom message from the Lustre runtime (needs to be sent to client)
    mist.Custom(client_message) -> {
      let json = server_component.client_message_to_json(client_message)
      let assert Ok(_) = mist.send_text_frame(connection, json.to_string(json))
      mist.continue(state)
    }

    // Connection closed - stop the websocket handler
    mist.Closed | mist.Shutdown -> mist.stop()
  }
}

fn close_socket(state: SocketState(msg)) -> Nil {
  // Call the disconnect hook first (allows cleanup)
  state.on_disconnect(state.component)

  // Then shutdown the Lustre runtime to prevent memory leaks
  lustre.shutdown()
  |> lustre.send(to: state.component)
}
