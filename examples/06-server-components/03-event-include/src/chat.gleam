// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode
import gleam/int
import gleam/list
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event
import lustre/server_component

// MAIN ------------------------------------------------------------------------

pub fn component() -> lustre.App(_, Model, Msg) {
  lustre.simple(init, update, view)
}

// MODEL -----------------------------------------------------------------------

pub opaque type Model {
  Model(key: Int, responses: List(String), history: List(#(Role, String)))
}

type Role {
  User
  Bot
}

fn init(_) -> Model {
  Model(
    key: 0,
    responses: [
      "Ok, tell me more.", "I see.", "Let's see if I can help...",
      "Unfortunately I couldn't resolve the issue.",
    ],
    history: [#(Bot, "Hello, how can I help you today?")],
  )
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  UserSubmittedMessage(message: String)
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    UserSubmittedMessage(message:) -> {
      let key = model.key + 1
      let #(history, responses) = case model.responses {
        [] -> #([#(User, message), ..model.history], model.responses)
        [reply, ..responses] -> #(
          [#(Bot, reply), #(User, message), ..model.history],
          responses,
        )
      }

      Model(key:, responses:, history:)
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  element.fragment([
    view_styles(),
    keyed.div([attribute.class("container")], [
      #("history", view_history(model.history)),
      // The input for the chat widget is *uncontrolled* meaning our component
      // leaves the state of the input to the browser. We use a changing key here
      // to force the browser to re-render the input when the key changes, clearing
      // its value on submission.
      #(int.to_string(model.key), view_input(UserSubmittedMessage)),
    ]),
  ])
}

fn view_history(messages: List(#(Role, String))) -> Element(msg) {
  html.ol([attribute.class("history")], {
    use #(role, content) <- list.map(messages)
    let class =
      attribute.class(case role {
        User -> "user"
        Bot -> "bot"
      })

    html.li([class], [html.text(content)])
  })
}

fn view_input(on_submit handle_keydown: fn(String) -> msg) -> Element(msg) {
  let on_keydown =
    // Writing your own event handlers for server components works just like it
    // does for client
    event.on("keydown", {
      use key <- decode.field("key", decode.string)
      use value <- decode.subfield(["target", "value"], decode.string)

      case key {
        "Enter" if value != "" -> decode.success(handle_keydown(value))
        _ -> decode.failure(handle_keydown(""), "")
      }
    })
    // JavaScript events cant be serialised to JSON by default: none of the event
    // properties are enumerable! When writing event handlers for server components
    // we need to explicitly tell which properties we want to include in the event
    // so that we can decode them on the server.
    |> server_component.include(["key", "target.value"])

  html.input([attribute.class("input"), on_keydown, attribute.autofocus(True)])
}

fn view_styles() -> Element(msg) {
  html.style([], {
    "
    :host {
      display: block;
      bottom: 1rem;
      font-family: sans-serif;
      position: fixed;
      box-shadow: 0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1);
      right: 1rem;
    }

    .container {
      display: grid;
      gap: 1rem;
      grid-template-rows: 1fr auto;
      width: 30ch;
      height: 300px;
      border-radius: 0.5rem;
      background-color: oklch(98.4% 0.003 247.858);
      padding: 0.5rem;
    }

    .history {
      display: flex;
      flex-direction: column-reverse;
      gap: 0.5rem;
      height: 100%;
      list-style: none;
      margin: 0;
      overflow-y: auto;
      padding: 0;

      & li {
        border-radius: 0.5rem;
        padding: 0.5rem 1rem;
      }

      & li.user {
        align-self: end;
        background-color: oklch(62.3% 0.214 259.815);
        color: white;
        margin-left: 2rem;
      }

      & li.bot {
        align-self: start;
        background-color: oklch(92.9% 0.013 255.508);
        margin-right: 2rem;
      }
    }

    .input {
      box-sizing: border-box;
      width: 100%;
      padding: 0.5rem;
    }
    "
  })
}
