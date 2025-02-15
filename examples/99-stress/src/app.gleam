import gleam/int
import gleam/io
import gleam/list
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

type Model {
  Model(rows: Int, should_offset: Bool, waiting: Bool)
}

fn init(_) -> #(Model, Effect(Msg)) {
  #(Model(rows: 0, should_offset: False, waiting: False), effect.none())
}

type Msg {
  Refresh
  Offset
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Refresh -> #(
      Model(rows: 10_000, should_offset: False, waiting: True),
      after(2000, Offset),
    )
    Offset -> #(
      Model(..model, should_offset: True, waiting: False),
      effect.none(),
    )
  }
}

fn after(ms: Int, msg: Msg) -> Effect(Msg) {
  use dispatch <- effect.from
  use <- do_after(ms)

  dispatch(msg)
}

@external(javascript, "./app.ffi.mjs", "after")
fn do_after(ms: Int, cb: fn() -> Nil) -> Nil {
  Nil
}

fn view(model: Model) -> Element(Msg) {
  let count = int.to_string(model.rows)
  let nodes = int.to_string(model.rows * 10)

  html.div([], [
    html.header([], [
      html.h1([], [
        html.text("Rendering "),
        html.text(count),
        html.text(" rows"),
      ]),
      html.h2([], [html.text("~"), html.text(nodes), html.text(" nodes")]),
      html.button([event.on_click(Refresh), attribute.disabled(model.waiting)], [
        html.text("Refresh"),
      ]),
    ]),
    html.main([], [
      case model.rows {
        0 -> element.none()
        _ -> view_table(model.rows, model.should_offset)
      },
    ]),
  ])
}

fn view_table(rows: Int, should_offset: Bool) -> Element(Msg) {
  let offset = case should_offset {
    True -> rows / 2
    False -> 0
  }

  let ids = list.range(1 + offset, rows + offset)

  element.keyed(
    html.table(
      [attribute.class("table table-hover table-striped test-data")],
      _,
    ),
    list.map(ids, view_row),
  )
}

fn view_row(id: Int) -> #(String, Element(Msg)) {
  let key = int.to_string(id)
  let row =
    html.tr([], [
      html.td([], [html.text(key)]),
      html.td([], [html.a([], [html.text("Updated Row "), html.text(key)])]),
      html.td([], [html.button([], [html.text("Delete")])]),
    ])

  #(key, row)
}
