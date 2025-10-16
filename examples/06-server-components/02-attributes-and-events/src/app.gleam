// IMPORTS ---------------------------------------------------------------------

import counter
import gleam/bytes_tree
import gleam/erlang/application
import gleam/erlang/process.{type Selector, type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{type Option, None, Some}
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html.{html}
import lustre/server_component
import mist.{type Connection, type ResponseData}

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(_) =
    fn(request: Request(Connection)) -> Response(ResponseData) {
      case request.path_segments(request) {
        [] -> serve_html()
        ["lustre", "runtime.mjs"] -> serve_runtime()
        ["ws"] -> serve_counter(request)
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

fn serve_html() -> Response(ResponseData) {
  let html =
    html([attribute.lang("en")], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),
        html.title([], "06-server-components/02-attributes-and-events"),
        html.script(
          [attribute.type_("module"), attribute.src("/lustre/runtime.mjs")],
          "",
        ),
      ]),
      html.body(
        [attribute.styles([#("max-width", "32rem"), #("margin", "3rem auto")])],
        [
          server_component.element(
            [
              server_component.route("/ws"),
              // Our counter component is set up to listen to changes to the
              // `"value"` attribute. Setting it on the HTML here will make sure
              // our component initialises with a value of `10`.
              //
              // While the app is running in your browser, try opening the inspector
              // and changing the value of this attribute: it will update the
              // component!
              counter.value(10),
            ],
            [],
          ),
          // Our counter component emits a `"change"` event whenever the submit
          // button is clicked. Just like with Lustre's client components, these
          // events are real DOM events that you can listen to with JavaScript or
          // in a client Lustre app.
          html.script(
            [],
            "
            const counter = document.querySelector('lustre-server-component');

            counter.addEventListener('change', event => {
              window.alert(`The counter value is now ${event.detail}`);
            })
            ",
          ),
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

fn serve_counter(request: Request(Connection)) -> Response(ResponseData) {
  mist.websocket(
    request:,
    on_init: init_counter_socket,
    handler: loop_counter_socket,
    on_close: close_counter_socket,
  )
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
  // When the websocket connection closes, we need to also shut down the server
  // component runtime. If we forget to do this we'll end up with a memory leak
  // and a zombie process!
  lustre.shutdown()
  |> lustre.send(to: state.component)
}
