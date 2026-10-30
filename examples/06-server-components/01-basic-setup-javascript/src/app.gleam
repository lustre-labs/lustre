// IMPORTS ---------------------------------------------------------------------

import counter
import gleam/http/request
import gleam/http/response.{type Response}
import gleam/javascript/promise.{type Promise}
import gleam/json
import gleam/option
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html.{html}
import lustre/server_component
import smol.{type ReadableStream}

// MAIN ------------------------------------------------------------------------

pub fn main() {
  smol.new(fn(request) {
    // In order to get started with server components, we'll need to handle at
    // least three things:
    case request.path_segments(request) {
      // 1. Serving the HTML document that will render the `<lustre-server-component />`
      //    custom element.
      [] -> serve_html()
      // 2. Serving the pre-build JavaScript runtime that registers the custom
      //    element and handles communication and rendering.
      ["lustre", "runtime.mjs"] -> serve_runtime()
      // 3. The websocket connection that the client runtime will connect to
      //    and the server runtime can push messages to.
      ["ws"] -> serve_counter()

      _ -> smol.send_status(404)
    }
  })
  |> smol.bind("localhost")
  |> smol.port(1234)
  |> smol.start
}

// HTML ------------------------------------------------------------------------

fn serve_html() -> Promise(Response(ReadableStream)) {
  let html =
    html([attribute.lang("en")], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),
        html.title([], "06-server-components/01-basic-setup"),
        html.script(
          // When serving the client runtime for server components, you must
          // remember to set the `type` attribute to `"module"` otherwise it won't
          // work!
          [attribute.type_("module"), attribute.src("/lustre/runtime.mjs")],
          "",
        ),
      ]),
      html.body(
        [attribute.styles([#("max-width", "32rem"), #("margin", "3rem auto")])],
        // If you're using Lustre to render your HTML like we are here, or if
        // you're also running Lustre in the browser, you can render the
        // `<lustre-server-component />` element using `server_component.element`.
        //
        // The server_component module also includes attributes that are relevant
        // to the component. The `server_component.route` attribute tells the
        // client runtime where to make the websocket connection to the server.
        // This path should match the path we used in our mist handler above.
        [server_component.element([server_component.route("/ws")], [])],
      ),
    ])

  html
  |> element.to_document_string
  |> smol.send_html
}

// JAVASCRIPT ------------------------------------------------------------------

fn serve_runtime() -> Promise(Response(ReadableStream)) {
  // Whenever you want to use server components, it's important that you serve
  // the client runtime to the browser. This small JavaScript module registers
  // the `<lustre-server-component />` custom element that you'll use in your HTML
  // to create server components.
  //
  // Lustre includes both a standard and a minified version of the runtime in
  // its `priv` directory as well as a function that returns the script content
  // as a string.
  let path =
    "build/dev/javascript/lustre/priv/static/lustre-server-component.mjs"

  smol.send_file(path, offset: 0, limit: option.None, or: fn(_) {
    smol.send_status(500)
  })
}

// WEBSOCKET -------------------------------------------------------------------

fn serve_counter() -> Promise(Response(ReadableStream)) {
  let counter = counter.component()
  // Rather than calling `lustre.start` as we do in the client, we construct the
  // Lustre runtime by calling `lustre.start_server_component`. This is the same
  // `Runtime` type we get from `lustre.start` but this function doesn't need a
  // CSS selector for the element to attach to: there's no DOM here!
  let assert Ok(component) = lustre.start_server_component(counter, Nil)

  // Our websocket state contains both the Lustre runtime for the server component
  // we're running and a reference to the callback the runtime uses to send messages
  // to the socket connection.
  //
  // Before the connection is established, we use a dummy callback here and wait
  // before registering the real callback once the connection is set up.
  let socket_state = CounterSocket(component:, send_to_smol: fn(_) { Nil })

  smol.websocket(
    init: socket_state,
    update: update_counter_socket,
    on_open: WebsocketConnected,
    on_message: WebsocketSentMessage,
    on_close: WebsocketDisconnected,
  )
}

type CounterSocket {
  CounterSocket(
    component: lustre.Runtime(counter.Message),
    send_to_smol: fn(server_component.ClientMessage(counter.Message)) -> Nil,
  )
}

type CounterSocketMessage {
  WebsocketConnected(send_to_client: fn(CounterSocketMessage) -> Nil)
  WebsocketDisconnected
  WebsocketSentMessage(message: smol.WebsocketMessage)
  LustreSentMessage(message: server_component.ClientMessage(counter.Message))
}

fn update_counter_socket(
  state: CounterSocket,
  message: CounterSocketMessage,
) -> Promise(smol.Step(CounterSocket, smol.WebsocketMessage)) {
  case message {
    WebsocketConnected(send_to_client:) -> {
      let send_to_smol = fn(message) {
        message
        |> LustreSentMessage
        |> send_to_client
      }

      server_component.register_callback(send_to_smol)
      |> lustre.send(to: state.component)

      // It's important we store the callback in the state so we can deregister
      // it later.
      smol.step(CounterSocket(..state, send_to_smol:))
    }

    WebsocketDisconnected -> {
      server_component.deregister_callback(state.send_to_smol)
      |> lustre.send(to: state.component)

      lustre.shutdown()
      |> lustre.send(to: state.component)

      smol.close()
    }

    WebsocketSentMessage(message: smol.Text(frame)) ->
      // Attempt to decode the incoming text frame as a message payload from the
      // `<lustre-server-component>` runtime. If it's OK, we forward the message
      // on to the server component runtime.
      case json.parse(frame, server_component.runtime_message_decoder()) {
        Ok(runtime_message) -> {
          lustre.send(state.component, runtime_message)
          smol.step(state)
        }

        // This case will only be hit if something other than Lustre's client
        // runtime sends us a message.
        Error(_) -> smol.step(state)
      }

    // Server components only communicate using text frames so we can ignore any
    // incoming binary frames entirely.
    WebsocketSentMessage(message: smol.Binary(_)) -> smol.step(state)

    // When Lustre wants to send a message down to the client, our socket gets
    // this message. All we need to do is encode it as JSON and forward it on to
    // the client
    LustreSentMessage(message:) ->
      smol.send(state, sending: {
        message
        |> server_component.client_message_to_json
        |> json.to_string
        |> smol.Text
      })
  }
}
