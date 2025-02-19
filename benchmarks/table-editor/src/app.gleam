import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import iv.{type Array}
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
  Model(name: String, description: String, columns: Array(Column), open: Bool)
}

type Column {
  Column(id: String, kind: ColumnKind, name: String, value: String, open: Bool)
}

type ColumnKind {
  ShortText
  LongText
  Integer
  Real
  Date
  DateTime
  Choice(List(Option))
  Unset
}

type Option {
  Option(id: String, name: String, value: String, open: Bool)
}

fn init(_) -> Model {
  let name = "My Table"
  let description = "Description for my table"
  let columns = iv.initialise(600, init_column)

  Model(name:, description:, columns:, open: False)
}

@external(javascript, "./app.ffi.mjs", "random_id")
fn random_id() -> String

fn init_column(index: Int) -> Column {
  let id = random_id()
  let kind = case index % 10 {
    0 -> ShortText
    1 -> LongText
    2 -> Integer
    3 -> Real
    4 -> Date
    5 -> DateTime
    6 -> Choice(list.range(0, 60) |> list.map(init_option))
    _ -> Unset
  }
  let name = "Column " <> id
  let value = ""
  let open = False

  Column(id:, kind:, name:, value:, open:)
}

fn init_option(index: Int) -> Option {
  let id = random_id()
  let name = "Option #" <> int.to_string(index)
  let value = int.to_string(index)
  let open = False

  Option(id:, name:, value:, open:)
}

type Msg {
  UserAddedColumn
  UserDeletedColumn(Int)
  UserInsertedColumnBefore(Int)
  UserInsertedColumnAfter(Int)
  UserMovedColumnUp(Int)
  UserMovedColumnDown(Int)
  UserToggledColumnOpen(Int)
  UserEditedColumnName(Int, String)
}

fn update(model: Model, msg: Msg) -> Model {
  case io.debug(msg) {
    UserAddedColumn -> {
      let columns =
        iv.append(model.columns, init_column(iv.length(model.columns)))

      Model(..model, columns:)
    }

    UserDeletedColumn(index) -> {
      let assert Ok(columns) = iv.delete(model.columns, index)

      Model(..model, columns:)
    }

    UserInsertedColumnBefore(index) -> {
      let assert Ok(columns) =
        iv.insert(model.columns, index, init_column(index))

      Model(..model, columns:)
    }

    UserInsertedColumnAfter(index) -> {
      let assert Ok(columns) = case index == iv.length(model.columns) - 1 {
        True -> Ok(iv.append(model.columns, init_column(index + 1)))
        False -> iv.insert(model.columns, index + 1, init_column(index + 1))
      }

      Model(..model, columns:)
    }

    UserMovedColumnUp(index) -> {
      use <- bool.guard(index == 0, model)
      let assert Ok(a) = iv.get(model.columns, index)
      let assert Ok(b) = iv.get(model.columns, index - 1)

      let columns =
        model.columns
        |> iv.try_set(index - 1, a)
        |> iv.try_set(index, b)

      Model(..model, columns:)
    }

    UserMovedColumnDown(index) -> {
      use <- bool.guard(index == iv.length(model.columns) - 1, model)
      let assert Ok(a) = iv.get(model.columns, index)
      let assert Ok(b) = iv.get(model.columns, index + 1)

      let columns =
        model.columns
        |> iv.try_set(index + 1, a)
        |> iv.try_set(index, b)

      Model(..model, columns:)
    }

    UserToggledColumnOpen(index) -> {
      let columns =
        model.columns
        |> iv.try_update(index, fn(column) {
          Column(..column, open: !column.open)
        })

      Model(..model, columns:)
    }

    UserEditedColumnName(index, name) -> {
      let columns =
        model.columns
        |> iv.try_update(index, fn(column) { Column(..column, name:) })

      Model(..model, columns:)
    }
  }
}

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("container mt-3")], [
    html.div([attribute.class("accordion")], [
      html.div([attribute.class("accordion-item"), attribute.open(True)], [
        html.summary([attribute.class("accordion-button px-2 py-2")], [
          html.div([attribute.class("accordion-header")], [
            html.text(model.name),
          ]),
        ]),
        html.div([attribute.class("accordion-body border-bottom px-2 py-2")], [
          html.div([], [
            html.div([attribute.class("input-group mb-2")], [
              html.label([attribute.class("input-group-text")], [
                html.text("Name"),
              ]),
              html.input([
                attribute.type_("text"),
                attribute.value(model.name),
                attribute.class("form-control"),
                attribute.placeholder("Table name..."),
                attribute("aria-label", "Table name"),
                attribute("aria-describedby", "basic-addon1"),
              ]),
            ]),
            html.textarea(
              [attribute.class("form-control mb-2"), attribute.rows(4)],
              model.description,
            ),
          ]),
          view_table(model.columns),
        ]),
      ]),
    ]),
  ])
}

fn view_table(columns: Array(Column)) -> Element(Msg) {
  let size = iv.length(columns) - 1
  let #(elements, _) = {
    use #(elements, index), column <- iv.fold_right(columns, #([], size))
    let #(key, element) = view_keyed_column(column, index)

    #([#(key, element), ..elements], index - 1)
  }

  html.div([], [
    html.h4([], [html.text("Columns")]),
    element.keyed(html.div([attribute.class("accordion")], _), elements),
  ])
}

fn view_keyed_column(column: Column, index: Int) -> #(String, Element(Msg)) {
  let key = column.id
  let element = view_column(column, index)

  #(key, element)
}

fn view_column(column: Column, index: Int) -> Element(Msg) {
  html.details(
    [
      attribute.class("accordion-item"),
      attribute.open(column.open),
      event.on("toggle", fn(_) { Ok(UserToggledColumnOpen(index)) }),
    ],
    [
      html.summary(
        [
          case column.open {
            True -> attribute.class("accordion-button px-2 py-2")
            False ->
              attribute.class("accordion-button px-2 py-2 collapsed bg-light")
          },
        ],
        [
          html.text(int.to_string(index)),
          html.text(" - "),
          html.text(column.name),
        ],
      ),
      html.div([attribute.class("accordion-body border-bottom px-2 py-2")], [
        html.div([attribute.class("row")], [
          html.div([attribute.class("col-md")], [
            html.div([attribute.class("input-group")], [
              html.label([attribute.class("input-group-text")], [
                html.text("Name"),
              ]),
              html.input([
                attribute.type_("text"),
                attribute.value(column.name),
                attribute.class("form-control"),
                attribute.placeholder("Column name..."),
                attribute("aria-label", "Column name"),
                event.on_input(UserEditedColumnName(index, _)),
              ]),
            ]),
          ]),
          html.div([attribute.class("col-md")], [
            html.div([attribute.class("input-group")], [
              html.label([attribute.class("input-group-text")], [
                html.text("Type"),
              ]),
              html.select([attribute.class("form-control")], [
                html.option([attribute.value("short_text")], "Text"),
                html.option([attribute.value("long_text")], "Long text"),
                html.option([attribute.value("integer")], "Integer"),
                html.option([attribute.value("real")], "Real"),
                html.option([attribute.value("date")], "Date"),
                html.option([attribute.value("date_time")], "Date and time"),
                html.option([attribute.value("choice")], "Choice"),
              ]),
            ]),
          ]),
        ]),
        html.textarea(
          [
            attribute.class("form-control mt-2"),
            attribute.rows(4),
            attribute.placeholder("Description"),
          ],
          "",
        ),
        html.button(
          [
            attribute.class("btn btn-sm btn-outline-danger mt-2"),
            event.on_click(UserDeletedColumn(index)),
          ],
          [html.text("Delete column")],
        ),
        html.div([attribute.class("btn-group ms-2")], [
          html.button(
            [
              attribute.class("btn btn-sm btn-outline-primary mt-2"),
              event.on_click(UserInsertedColumnBefore(index)),
            ],
            [html.text("Create above")],
          ),
          html.button(
            [
              attribute.class("btn btn-sm btn-outline-primary mt-2"),
              event.on_click(UserInsertedColumnAfter(index)),
            ],
            [html.text("Create below")],
          ),
        ]),
        html.div([attribute.class("btn-group ms-2")], [
          html.button(
            [
              attribute.class("btn btn-sm btn-outline-primary mt-2"),
              event.on_click(UserMovedColumnUp(index)),
            ],
            [html.text("Move up")],
          ),
          html.button(
            [
              attribute.class("btn btn-sm btn-outline-primary mt-2"),
              event.on_click(UserMovedColumnDown(index)),
            ],
            [html.text("Move down")],
          ),
        ]),
      ]),
    ],
  )
}
