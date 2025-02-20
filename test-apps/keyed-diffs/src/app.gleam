import gleam/int
import gleam/io
import gleam/list
import gleam/string
import lustre
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

//

type Model {
  Model(
    revision: Int,
    keyed: Bool,
    prev: List(String),
    curr: List(String),
    next: String,
  )
}

fn init(_) -> Model {
  Model(
    revision: 1,
    keyed: True,
    prev: [],
    curr: ["a", "b", "c"],
    next: "a b c",
  )
}

type Msg {
  UserClickedUpdate
  UserClickedRegenerate
  UserToggledKeyed(Bool)
  UserChangedNext(String)
}

fn update(model: Model, msg: Msg) -> Model {
  case io.debug(msg) {
    UserChangedNext(next) -> Model(..model, next:)
    UserClickedRegenerate -> Model(..model, revision: model.revision + 1)
    UserToggledKeyed(keyed) -> Model(..model, keyed:)
    UserClickedUpdate -> {
      let curr =
        model.next
        |> string.split(on: " ")
        |> list.filter(fn(s) { s != "" })

      Model(..model, prev: model.curr, curr:)
    }
  }
}

fn view(model: Model) -> Element(Msg) {
  element.keyed(
    html.div(
      [
        attribute("data-revision", int.to_string(model.revision)),
        attribute.style([#("margin", "2em")]),
      ],
      _,
    ),
    [
      #(
        int.to_string(model.revision),
        html.div([], [
          html.div([], [
            html.text("Previous: "),
            html.text(string.inspect(model.prev)),
            html.br([]),
            html.text("Current: "),
            html.text(string.inspect(model.curr)),
          ]),
          html.div(
            [
              attribute.style([
                #("display", "flex"),
                #("align-items", "center"),
                #("gap", "0.5em"),
              ]),
            ],
            [
              html.input([
                attribute.value(model.next),
                event.on_input(UserChangedNext),
              ]),
              html.button([event.on_click(UserClickedUpdate)], [
                html.text("Update"),
              ]),
              html.button([event.on_click(UserClickedRegenerate)], [
                html.text("Regenerate"),
              ]),
              html.label([], [
                html.input([
                  attribute.type_("checkbox"),
                  attribute.checked(model.keyed),
                  event.on_check(UserToggledKeyed),
                ]),
                html.text(" Keyed"),
              ]),
            ],
          ),
          case model.keyed {
            True ->
              element.keyed(
                html.div(
                  [attribute.style([#("display", "flex"), #("gap", "1em")])],
                  _,
                ),
                list.map(model.curr, fn(key) {
                  let child =
                    html.div([attribute("data-key", key)], [html.text(key)])
                  #(key, child)
                }),
              )
            False ->
              html.div(
                [attribute.style([#("display", "flex"), #("gap", "1em")])],
                list.map(model.curr, fn(key) {
                  html.div([attribute("data-key", key)], [html.text(key)])
                }),
              )
          },
          html.textarea([attribute.placeholder("Scratch area :)")], ""),
        ]),
      ),
    ],
  )
}
