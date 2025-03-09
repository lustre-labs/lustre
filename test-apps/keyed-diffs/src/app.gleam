import gleam/int
import gleam/io
import gleam/list
import gleam/string
import lustre
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
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
    prev: List(Node),
    curr: List(Node),
    last_event: String,
    next: String,
  )
}

fn init(_) -> Model {
  Model(
    revision: 1,
    keyed: True,
    prev: [],
    curr: [Node("a"), Node("b"), Node("c")],
    last_event: "",
    next: "a b c",
  )
}

type Msg {
  UserClickedUpdate
  UserClickedShuffle
  UserClickedRegenerate
  UserChangedNext(String)
  UserClickedClassic(Int)
  UserClickedKeyed(Int, String)
}

fn update(model: Model, msg: Msg) -> Model {
  case io.debug(msg) {
    UserChangedNext(next) -> Model(..model, next:)
    UserClickedRegenerate -> Model(..model, revision: model.revision + 1)
    UserClickedShuffle -> {
      let shuffled = list.shuffle(model.curr)
      case shuffled == model.curr {
        True -> update(model, msg)
        False -> Model(..model, prev: model.curr, curr: shuffled)
      }
    }
    UserClickedUpdate ->
      Model(..model, prev: model.curr, curr: parse(model.next))

    UserClickedClassic(_) | UserClickedKeyed(_, _) ->
      Model(..model, last_event: string.inspect(msg))
  }
}

type Node {
  Fragment(children: List(Node))
  Node(key: String)
}

fn parse(str: String) -> List(Node) {
  let assert #(nodes, []) = parse_loop(string.split(str, on: " "), [])
  nodes
}

fn parse_loop(
  input: List(String),
  acc: List(Node),
) -> #(List(Node), List(String)) {
  case input {
    [] -> #(list.reverse(acc), [])
    ["", ..rest] -> parse_loop(rest, acc)
    [first, ..rest] ->
      case string.starts_with(first, "["), string.ends_with(first, "]") {
        True, True -> parse_loop(rest, [Fragment([Node(first)]), ..acc])
        True, False -> {
          let #(children, rest) = parse_loop(rest, [Node(first)])
          parse_loop(rest, [Fragment(children), ..acc])
        }
        False, True -> #(list.reverse([Node(first), ..acc]), rest)
        False, False -> parse_loop(rest, [Node(first), ..acc])
      }
  }
}

fn view(model: Model) -> Element(Msg) {
  keyed.div(
    [
      attribute("data-revision", int.to_string(model.revision)),
      attribute.style([#("margin", "2em")]),
    ],
    [
      #(
        int.to_string(model.revision),
        html.div(
          [
            attribute.style([
              #("display", "flex"),
              #("flex-direction", "column"),
              #("gap", "1em"),
              #("max-inline-size", "60ch"),
            ]),
          ],
          [
            html.div([], [
              html.text("Previous: "),
              html.text(string.inspect(model.prev)),
              html.br([]),
              html.text("Current: "),
              html.text(string.inspect(model.curr)),
              html.br([]),
              html.text("Last Event: " <> model.last_event),
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
                html.button([event.on_click(UserClickedShuffle)], [
                  html.text("Shuffle"),
                ]),
                html.button([event.on_click(UserClickedRegenerate)], [
                  html.text("Regenerate"),
                ]),
              ],
            ),
            html.div(
              [
                attribute.style([
                  #("display", "grid"),
                  #("grid-template-columns", "auto 1fr"),
                  #("column-gap", "1em"),
                ]),
              ],
              [
                html.text("classic"),
                view_nodes_classic(model.curr),
                html.text("keyed"),
                view_nodes_keyed(model.curr),
              ],
            ),
            html.textarea([attribute.placeholder("Scratch area :)")], ""),
          ],
        ),
      ),
    ],
  )
}

fn view_nodes_classic(children: List(Node)) {
  html.div(
    [attribute.style([#("display", "flex"), #("gap", "1em")])],
    list.index_map(children, view_node_classic),
  )
}

fn view_nodes_keyed(children: List(Node)) {
  keyed.div(
    [attribute.style([#("display", "flex"), #("gap", "1em")])],
    list.index_map(children, view_node_keyed),
  )
}

fn view_node_keyed(node: Node, index: Int) {
  case node {
    Node(key) -> #(
      key,
      html.div(
        [
          attribute("data-key", key),
          event.on_click(UserClickedKeyed(index, key)),
        ],
        [html.text(key)],
      ),
    )
    Fragment(children) -> #(
      fragment_key(children),
      children
        |> list.index_map(view_node_keyed)
        |> keyed.fragment,
    )
  }
}

fn view_node_classic(node: Node, index) {
  case node {
    Node(key) ->
      html.div(
        [attribute("data-key", key), event.on_click(UserClickedClassic(index))],
        [html.text(key)],
      )
    Fragment(children) ->
      children
      |> list.index_map(view_node_classic)
      |> element.fragment
  }
}

fn fragment_key(children: List(Node)) {
  list.flat_map(children, collect_keys)
  |> list.sort(by: string.compare)
  |> string.join("")
}

fn collect_keys(node: Node) {
  case node {
    Fragment(children) -> list.flat_map(children, collect_keys)
    Node(key) -> [key]
  }
}
