import gleam/bool
import gleam/dynamic/decode
import gleam/erlang
import gleam/erlang/process
import gleam/function
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import iv.{type Array}
import lustre
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/event
import lustre/server_component
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage,
}
import youid/uuid

pub fn main() {
  let assert Ok(_) =
    mist.new(handle_request)
    |> mist.port(1234)
    |> mist.start_http

  process.sleep_forever()
}

fn handle_request(req: Request(Connection)) -> Response(ResponseData) {
  case request.path_segments(req) {
    ["ws"] ->
      mist.websocket(
        req,
        on_init: socket_init,
        on_close: socket_close,
        handler: socket_update,
      )
    // We need to serve the server component runtime. There's also a minified
    // version of this script for production.
    ["lustre-server-component.mjs"] -> {
      let assert Ok(priv) = erlang.priv_directory("lustre")
      let path = priv <> "/static/lustre-server-component.min.mjs"
      let assert Ok(script) = mist.send_file(path, offset: 0, limit: None)

      response.new(200)
      |> response.prepend_header("content-type", "application/javascript")
      |> response.set_body(script)
    }

    _ -> {
      let assert Ok(priv) = erlang.priv_directory("app")
      let path = priv <> "/static/index.html"
      let assert Ok(html) = mist.send_file(path, offset: 0, limit: None)

      response.new(200)
      |> response.prepend_header("content-type", "text/html")
      |> response.set_body(html)
    }
  }
}

fn socket_init(_) {
  let self = process.new_subject()
  let app = lustre.simple(init, update, view)
  let assert Ok(app) = lustre.start_actor(app, Nil)

  server_component.register_subject(app, self)

  let selector =
    process.new_selector()
    |> process.selecting(self, function.identity)

  #(app, Some(selector))
}

fn socket_update(app, conn: WebsocketConnection, msg: WebsocketMessage(_)) {
  case msg {
    mist.Binary(_) -> actor.continue(app)
    mist.Text(json) -> {
      let msg = json.parse(json, server_component.runtime_message_decoder())
      case msg {
        Error(_) -> Nil
        Ok(msg) -> process.send(app, msg)
      }

      actor.continue(app)
    }
    mist.Custom(patch) -> {
      let assert Ok(_) =
        patch
        |> server_component.client_message_to_json
        |> json.to_string
        |> mist.send_text_frame(conn, _)

      actor.continue(app)
    }
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
  }
}

fn socket_close(app) {
  process.send(app, lustre.shutdown())
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

fn init_column(index: Int) -> Column {
  let id = uuid.v4_string()
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
  let id = uuid.v4_string()
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
    keyed.div([attribute.class("accordion")], elements),
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
      event.on("toggle", decode.success(UserToggledColumnOpen(index))),
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
