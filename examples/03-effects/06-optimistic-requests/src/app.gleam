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
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event

// Optimist is a library that gives us a data structure that abstracts over
// *optimistic* updates. That means we can update some data immediately with the
// result we expect to get, and then either commit or revert it in the future
// based on the result of an API call.
import optimist.{type Optimistic}
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
  Optimistic(List(Todo))

type Todo {
  Todo(id: Int, title: String, completed: Bool)
}

/// Now our app can perform effects, the return type of the `init` and `update`
/// functions changes to return a tuple.
fn init(_) -> #(Model, Effect(Msg)) {
  let model = optimist.from([])
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
  UserClickedComplete(id: Int, completed: Bool)
  ApiReturnedTodos(Result(List(Todo), rsvp.Error))
  ApiUpdatedTodo(Result(Int, rsvp.Error))
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserClickedComplete(id, completed) -> #(
      // An optimistic update means we update the model before we get an actual
      // response: it's what we *expect* to happen.
      optimist.update(model, {
        list.map(_, fn(item: Todo) {
          case item.id == id {
            True -> Todo(..item, completed:)
            False -> item
          }
        })
      }),
      complete_todo(id:, completed:, on_response: ApiUpdatedTodo),
    )

    ApiUpdatedTodo(response) -> #(
      // We can use `try` to take a `Result` and attempt to use it to resolve an
      // optimistic update. In this case we're updating the list of todos and
      // toggling the completed state of the correct todo item.
      //
      // If `response` is an `Error` then optimist will automatically revert the
      // optimistic update we performed in the branch above. If it's `Ok` we take
      // the commit to the updated value: if our optimistic update was correct,
      // the user won't notice a thing!
      optimist.try(model, response, fn(items, id) {
        list.map(items, fn(item: Todo) {
          case item.id == id {
            True -> Todo(..item, completed: !item.completed)
            False -> item
          }
        })
      }),
      effect.none(),
    )

    ApiReturnedTodos(Ok(todos)) -> #(optimist.from(todos), effect.none())
    ApiReturnedTodos(Error(_)) -> #(model, effect.none())
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
      // `optimist.unwrap` gives us the current value of the optimistic model,
      // without us knowing if it's an optimistic update or the real value: we
      // don't need to care about that in the view.
      list.map(optimist.unwrap(model), fn(item) {
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
