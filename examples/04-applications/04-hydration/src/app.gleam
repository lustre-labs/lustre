// IMPORTS ---------------------------------------------------------------------
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import lustre
import lustre/platform
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event
import rsvp

// MAIN ------------------------------------------------------------------------

pub fn main() {
  // Lustre apps can also be rendered on a server and served as just HTML,
  // using the `element.to_string` function. This is already very powerful for
  // static site generation (SSG) or server-side rendering (SSR), but a static
  // page is not sufficient for making the page interactive.
  //
  // Only rendering on the client however might be slow, imply multiple round-
  // trips to the server for the same pieces of data, and overall is less
  // friendly to search-engines and users using accessibility options.
  //
  // "Hydration" means turning those statically rendered pages back into fully
  // interactive Lustre apps. This involves 2 steps:
  //
  // - Take the existing HTML and attach event listeners to it, making minimal
  //   changes otherwise. Lustre calls this part "virtualisation" and does this
  //   by default for all apps and components.
  //
  // - Restore the apps `Model` to the state used to render the page on the
  //   server. This is necessary, since otherwise the Lustre client app would
  //   immediately override all differences with its values.
  //   Sending some values to the client runtime also means it doesn't have to
  //   fetch them again.
  //
  // This second step is what we call "hydration". Compared to other frameworks,
  // Lustre is very unopinionated on how and what state you send to the client
  // app. Since Lustre apps are usually singletons, we've found this approach
  // to work well while being very flexible.
  //
  // Here, we'll use gleam_json and a simple FFI function to load a serialised
  // JSON blob from our HTML, and turn it into a initial `flags` value that we
  // will pass to our app:
  let hydrated_todos = case get_text_content("#todos") {
    Ok(todos_json) -> {
      let decoder = decode.list(todo_decoder())
      let assert Ok(todos) = json.parse(todos_json, decoder)
      option.Some(todos)
    }

    // It is fine for our app in particular to load without hydrated data.
    Error(_) -> option.None
  }

  let assert Ok(platform) = platform.dom("#app")
  let app = lustre.application(init, update, view)

  // We (optionally) pass the hydrated model as `flags` to our init function.
  // Note that the flags could come from anywhere here, and you may fetch data
  // from more than one source before you're sure you can start your Lustre app!
  let assert Ok(_) = lustre.start(app, on: platform, with: hydrated_todos)

  Nil
}

@external(javascript, "./app.ffi.mjs", "get_text_content")
fn get_text_content(selector: String) -> Result(String, Nil)

// MODEL -----------------------------------------------------------------------

type Model {
  Loading
  Loaded(todos: List(Todo))
  LoadingFailed(rsvp.Error)
}

type Todo {
  Todo(id: Int, title: String, completed: Bool)
}

fn todo_decoder() -> decode.Decoder(Todo) {
  use id <- decode.field("id", decode.int)
  use title <- decode.field("title", decode.string)
  use completed <- decode.field("completed", decode.bool)

  decode.success(Todo(id:, title:, completed:))
}

fn init(flags: option.Option(List(Todo))) -> #(Model, Effect(Msg)) {
  // If we got an initial list of todos from our flags, we use that and can
  // immediately initialise our app in the `Loaded` state.
  // Only if we didn't we need to fetch the todos from the server!
  case flags {
    option.Some(todos) -> #(Loaded(todos), effect.none())
    option.None -> #(Loading, fetch_todos(on_response: ApiReturnedTodos))
  }
}

fn fetch_todos(
  on_response handle_response: fn(Result(List(Todo), rsvp.Error)) -> msg,
) -> Effect(msg) {
  let url = "https://jsonplaceholder.typicode.com/todos/"
  let decoder = decode.list(todo_decoder()) |> decode.map(list.take(_, 10))
  let handler = rsvp.expect_json(decoder, handle_response)
  rsvp.get(url, handler)
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  ApiReturnedTodos(Result(List(Todo), rsvp.Error))
  UserClickedComplete(id: Int, completed: Bool)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ApiReturnedTodos(Ok(todos)) -> #(Loaded(todos), effect.none())
    ApiReturnedTodos(Error(error)) -> #(LoadingFailed(error), effect.none())

    UserClickedComplete(id:, completed:) ->
      case model {
        Loaded(todos) -> {
          let todos =
            list.map(todos, fn(item) {
              case item.id == id {
                True -> Todo(..item, completed:)
                False -> item
              }
            })

          #(Loaded(todos), effect.none())
        }

        _ -> #(model, effect.none())
      }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div(
    [attribute.class("p-32 mx-auto w-full max-w-2xl space-y-4")],
    case model {
      Loading -> view_loading()
      Loaded(todos) -> view_todos(todos)
      LoadingFailed(error) -> view_failed(error)
    },
  )
}

fn view_loading() -> List(Element(Msg)) {
  [
    html.h1([attribute.class("font-semibold text-2xl")], [
      html.text("Loading..."),
    ]),
  ]
}

fn view_failed(_error: rsvp.Error) -> List(Element(Msg)) {
  [
    html.h1([attribute.class("font-semibold text-2xl")], [html.text("Oops!")]),
    html.p([attribute.class("text-lg")], [html.text("Something went wrong!")]),
    // TODO: show the error to the user in a nice way.
  ]
}

fn view_todos(todos: List(Todo)) -> List(Element(Msg)) {
  [
    html.h1([attribute.class("font-semibold text-2xl")], [html.text("Todo:")]),
    keyed.ul([attribute.class("flex flex-col gap-2")], {
      list.map(todos, fn(item) {
        let key = int.to_string(item.id)
        let html =
          html.li([], [
            view_todo(item:, on_complete: UserClickedComplete(item.id, _)),
          ])

        #(key, html)
      })
    }),
  ]
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
