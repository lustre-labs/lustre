import column.{type Column}
import gleam/bool
import gleam/io
import iv.{type Array}
import lustre
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed

pub fn main() {
  let assert Ok(_) = column.register()

  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

//

type Model {
  Model(name: String, description: String, columns: Array(Column), open: Bool)
}

fn init(_) -> Model {
  let name = "My Table"
  let description = "Description for my table"
  let columns = iv.initialise(600, column.new)

  Model(name:, description:, columns:, open: False)
}

type Msg {
  UserDeletedColumn(Int)
  UserInsertedColumnBefore(Int)
  UserInsertedColumnAfter(Int)
  UserMovedColumnUp(Int)
  UserMovedColumnDown(Int)
  ColumnChanged(Int, Column)
}

fn update(model: Model, msg: Msg) -> Model {
  case io.debug(msg) {
    UserDeletedColumn(index) -> {
      let columns = iv.try_delete(model.columns, index)

      Model(..model, columns:)
    }

    UserInsertedColumnBefore(index) -> {
      let columns =
        model.columns
        |> iv.insert_clamped(index, column.new(index))

      Model(..model, columns:)
    }

    UserInsertedColumnAfter(index) -> {
      let columns =
        model.columns
        |> iv.insert_clamped(index + 1, column.new(index + 1))

      Model(..model, columns:)
    }

    UserMovedColumnUp(index) ->
      case iv.get(model.columns, index - 1), iv.get(model.columns, index) {
        Ok(prev), Ok(curr) -> {
          let columns =
            model.columns
            |> iv.try_set(index - 1, curr)
            |> iv.try_set(index, prev)

          Model(..model, columns:)
        }
        _, _ -> model
      }

    UserMovedColumnDown(index) ->
      case iv.get(model.columns, index), iv.get(model.columns, index + 1) {
        Ok(curr), Ok(next) -> {
          let columns =
            model.columns
            |> iv.try_set(index + 1, curr)
            |> iv.try_set(index, next)

          Model(..model, columns:)
        }
        _, _ -> model
      }

    ColumnChanged(index, column) -> {
      let columns =
        model.columns
        |> iv.try_set(index, column)

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
    keyed.div([attribute.class("accordion")], elements),
  ])
}

fn view_keyed_column(column: Column, index: Int) -> #(String, Element(Msg)) {
  let key = column.id
  let element =
    column.column(
      column:,
      on_change: ColumnChanged(index, _),
      on_move_up: UserMovedColumnUp(index),
      on_move_down: UserMovedColumnDown(index),
      on_create_above: UserInsertedColumnBefore(index),
      on_create_below: UserInsertedColumnAfter(index),
      on_delete: UserDeletedColumn(index),
    )

  #(key, element)
}
