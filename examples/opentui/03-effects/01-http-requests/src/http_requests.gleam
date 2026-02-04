// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/http
import gleam/http/request
import gleam/int
import gleam/json
import gleam/list
import lustre
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/keyed
import lustre/platform/opentui
import lustre/platform/opentui/attribute
import lustre/platform/opentui/effect as tui_effect
import lustre/platform/opentui/element as tui
import lustre/platform/opentui/event
import rsvp

// MAIN ------------------------------------------------------------------------

pub fn main() {
  opentui.platform(opentui.default_config(), fn(platform) {
    let app = lustre.application(init, update, view)
    let assert Ok(_) = lustre.start(app, on: platform, with: Nil)
    Nil
  })
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(todos: List(Todo), loading: Bool, focus_index: Int)
}

type Todo {
  Todo(id: Int, title: String, completed: Bool)
}

fn init(_) -> #(Model, Effect(Msg)) {
  #(
    Model(todos: [], loading: True, focus_index: 0),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      fetch_todos(on_response: ApiReturnedTodos),
    ]),
  )
}

fn fetch_todos(
  on_response handle_response: fn(Result(List(Todo), rsvp.Error)) -> msg,
) -> Effect(msg) {
  let url = "https://jsonplaceholder.typicode.com/todos/"
  let decoder = decode.list(todo_decoder()) |> decode.map(list.take(_, 10))
  let handler = rsvp.expect_json(decoder, handle_response)

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
  KeyPressed(tui_effect.KeyEvent)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ApiReturnedTodos(Ok(todos)) -> #(
      Model(..model, todos:, loading: False),
      case todos {
        [] -> effect.none()
        _ -> tui_effect.focus("todo-0")
      },
    )
    ApiReturnedTodos(Error(_)) -> #(
      Model(..model, loading: False),
      effect.none(),
    )

    ApiUpdatedTodo(Ok(id)) -> {
      let todos =
        list.map(model.todos, fn(item) {
          case item.id == id {
            True -> Todo(..item, completed: !item.completed)
            False -> item
          }
        })
      #(Model(..model, todos:), effect.none())
    }
    ApiUpdatedTodo(Error(_)) -> #(model, effect.none())

    UserClickedComplete(id, completed) -> #(
      model,
      complete_todo(id:, completed:, on_response: ApiUpdatedTodo),
    )

    KeyPressed(key_event) ->
      case key_event.key {
        "q" if key_event.ctrl -> #(model, tui_effect.destroy())
        "tab" | "down" -> {
          let len = list.length(model.todos)
          case len {
            0 -> #(model, effect.none())
            _ -> {
              let new_index = { model.focus_index + 1 } % len
              let child_id = "todo-" <> int.to_string(new_index)
              #(
                Model(..model, focus_index: new_index),
                effect.batch([
                  tui_effect.focus(child_id),
                  tui_effect.scroll_into_view("todo-list", child_id),
                ]),
              )
            }
          }
        }
        "up" -> {
          let len = list.length(model.todos)
          case len {
            0 -> #(model, effect.none())
            _ -> {
              let new_index = { model.focus_index - 1 + len } % len
              let child_id = "todo-" <> int.to_string(new_index)
              #(
                Model(..model, focus_index: new_index),
                effect.batch([
                  tui_effect.focus(child_id),
                  tui_effect.scroll_into_view("todo-list", child_id),
                ]),
              )
            }
          }
        }
        // " " | "return" | "enter" -> {
        //   case list.drop(model.todos, model.focus_index) |> list.first {
        //     Ok(item) -> #(
        //       model,
        //       complete_todo(
        //         id: item.id,
        //         completed: !item.completed,
        //         on_response: ApiUpdatedTodo,
        //       ),
        //     )
        //     Error(_) -> #(model, effect.none())
        //   }
        // }
        _ -> #(model, effect.none())
      }
  }
}

fn complete_todo(
  id id: Int,
  completed completed: Bool,
  on_response handle_response: fn(Result(Int, rsvp.Error)) -> msg,
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
  tui.box(
    [
      attribute.flex_direction("column"),
      attribute.align_items("center"),
      attribute.justify_content("center"),
      attribute.width_("100%"),
      attribute.height_("100%"),
    ],
    [
      tui.box(
        [
          attribute.flex_direction("column"),
          attribute.width(70),
          attribute.max_height(20),
          attribute.border_style("round"),
          attribute.border_color("#444"),
          attribute.padding(1),
          attribute.gap(0),
          attribute.title(" Todo List "),
          attribute.title_alignment("center"),
        ],
        [
          tui.text_node([attribute.dim(True), attribute.fg("#888")], [
            tui.text(
              "Arrows to navigate, Space/Enter to toggle, Ctrl+Q to quit",
            ),
          ]),
          case model.loading {
            True ->
              tui.text_node([attribute.fg("#f1c40f")], [
                tui.text("Loading todos..."),
              ])
            False ->
              tui.scrollbox(
                [
                  attribute.id("todo-list"),
                  attribute.height(6),
                  attribute.flex_grow(0),
                  attribute.flex_shrink(0),
                ],
                [
                  keyed.fragment(
                    list.index_map(model.todos, fn(item, index) {
                      let key = int.to_string(item.id)
                      #(key, view_todo(item, index, model.focus_index))
                    }),
                  ),
                ],
              )
          },
        ],
      ),
    ],
  )
}

fn view_todo(item: Todo, index: Int, focus_index: Int) -> Element(Msg) {
  let checkbox = case item.completed {
    True -> "[x]"
    False -> "[ ]"
  }

  let text_style = case item.completed {
    True -> [attribute.fg("#666"), attribute.strikethrough(True)]
    False -> [attribute.fg("#e0e0e0")]
  }

  let bg_attrs = case index == focus_index {
    True -> [attribute.background_color("#1a2a1a")]
    False -> []
  }

  tui.box(
    list.flatten([
      [
        attribute.id("todo-" <> int.to_string(index)),
        attribute.focusable(True),
        attribute.flex_direction("row"),
        attribute.gap(1),
        attribute.padding_left(1),
      ],
      bg_attrs,
      [
        event.on_click(UserClickedComplete(item.id, !item.completed)),
        event.on_activate(UserClickedComplete(item.id, !item.completed)),
      ],
    ]),
    [
      tui.text_node(
        [
          attribute.fg(case item.completed {
            True -> "#69db7c"
            False -> "#555"
          }),
        ],
        [tui.text(checkbox)],
      ),
      tui.text_node(text_style, [tui.text(item.title)]),
    ],
  )
}
