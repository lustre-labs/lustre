// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/http
import gleam/http/request
import gleam/int
import gleam/json
import gleam/list
import lustre
import lustre/platform
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event

// The `Effect` type is used to describe *side effects*: communication with the
// world outside of our Lustre app. The runtime knows how to handle effects and
// return their results back to the app.
import lustre/effect.{type Effect}

// rsvp is a library for making HTTP requests specifically for Lustre. It provides
// functions for describing requests as effects that can be executed by the runtime.
import rsvp

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // In this example we've swapped out the `simple` app constructor for the
  // `application` constructor instead. This lets us return effects from the
  // `init` and `update` functions.
  let assert Ok(platform) = platform.dom("#app")
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  List(Todo)

type Todo {
  Todo(id: Int, title: String, completed: Bool)
}

/// Now our app can perform effects, the return type of the `init` and `update`
/// functions changes to return a tuple.
fn init(_) -> #(Model, Effect(Msg)) {
  let model = []
  let effect = fetch_todos(on_response: ApiReturnedTodos)

  #(model, effect)
}

fn fetch_todos(
  on_response handle_response: fn(Result(List(Todo), rsvp.Error)) -> msg,
) -> Effect(msg) {
  let url = "https://jsonplaceholder.typicode.com/todos/"
  let decoder = decode.list(todo_decoder()) |> decode.map(list.take(_, 10))
  let handler = rsvp.expect_json(decoder, handle_response)

  // When we call `rsvp.get` that doesn't immediately make the request. Instead,
  // it returns an effect that we give to the runtime to handle for us.
  rsvp.get(url, handler)
}

fn todo_decoder() -> Decoder(Todo) {
  use id <- decode.field("id", decode.int)
  use title <- decode.field("title", decode.string)
  use completed <- decode.field("completed", decode.bool)

  decode.success(Todo(id:, title:, completed:))
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  ApiReturnedTodos(Result(List(Todo), rsvp.Error))
  ApiUpdatedTodo(Result(Int, rsvp.Error))
  UserClickedComplete(id: Int, completed: Bool)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    // When we have no side effects to perform, we return `effect.none()`.
    ApiReturnedTodos(Ok(todos)) -> #(todos, effect.none())
    ApiReturnedTodos(Error(_)) -> #([], effect.none())

    ApiUpdatedTodo(Ok(id)) -> {
      let todos =
        list.map(model, fn(item) {
          case item.id == id {
            True -> Todo(..item, completed: !item.completed)
            False -> item
          }
        })

      #(todos, effect.none())
    }
    ApiUpdatedTodo(Error(_)) -> #(model, effect.none())

    UserClickedComplete(id, completed) -> #(
      model,
      complete_todo(id:, completed:, on_response: ApiUpdatedTodo),
    )
  }
}

fn complete_todo(
  id id: Int,
  completed completed: Bool,
  on_response handle_response: fn(Result(Int, rsvp.Error)) -> msg,
  // Just like the `Element` type, the `Effect` type is parametrised by the type
  // of messages it produces. This is how we know messages we get back from an
  // effect are type-safe and can be handled by the `update` function.
) -> Effect(msg) {
  let url = "https://jsonplaceholder.typicode.com/todos/" <> int.to_string(id)
  let handler = rsvp.expect_json(decode.success(id), handle_response)
  let body = json.object([#("completed", json.bool(completed))])

  case request.to(url) {
    Ok(request) ->
      request
      |> request.set_method(http.Patch)
      |> request.set_body(json.to_string(body))
      |> rsvp.send(handler)

    Error(_) -> panic as { "Failed to create request to " <> url }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("p-32 mx-auto w-full max-w-2xl space-y-8")], [
    html.h1([attribute.class("font-semibold text-2xl")], [html.text("Todo:")]),
    keyed.ul([attribute.class("flex flex-col gap-2")], {
      list.map(model, fn(item) {
        let key = int.to_string(item.id)
        let html =
          html.li([], [
            view_todo(item:, on_complete: UserClickedComplete(item.id, _)),
          ])

        #(key, html)
      })
    }),
  ])
}

fn view_todo(
  item item: Todo,
  on_complete handle_complete: fn(Bool) -> msg,
) -> Element(msg) {
  html.label([attribute.class("flex gap-2 items-baseline")], [
    html.p(
      [
        attribute.class("flex-1"),
        attribute.classes([#("line-through text-slate-400", item.completed)]),
      ],
      [html.text(item.title)],
    ),
    html.input([
      attribute.type_("checkbox"),
      attribute.checked(item.completed),
      event.on_check(handle_complete),
    ]),
  ])
}
