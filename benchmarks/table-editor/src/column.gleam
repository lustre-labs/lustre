import gleam/dict
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// -- MODEL --------------------------------------------------------------------

pub type Column {
  Column(id: String, kind: Kind, name: String, value: String, open: Bool)
}

fn decoder() -> Decoder(Column) {
  use id <- decode.field("id", decode.string)
  use kind <- decode.field("kind", kind_decoder())
  use name <- decode.field("name", decode.string)
  use value <- decode.field("value", decode.string)
  use open <- decode.field("open", decode.bool)
  decode.success(Column(id:, kind:, name:, value:, open:))
}

fn encode(column: Column) -> json.Json {
  json.object([
    #("id", json.string(column.id)),
    #("kind", encode_kind(column.kind)),
    #("name", json.string(column.name)),
    #("value", json.string(column.value)),
    #("open", json.bool(column.open)),
  ])
}

pub type Kind {
  ShortText
  LongText
  Integer
  Real
  Date
  DateTime
  Choice(options: List(Option))
  Unset
}

fn kind_decoder() -> Decoder(Kind) {
  decode.one_of(simple_kind_decoder(), [
    decode.list(option_decoder())
      |> decode.map(Choice),
    decode.success(Unset),
  ])
}

fn simple_kind_decoder() -> Decoder(Kind) {
  use str <- decode.then(decode.string)
  case str {
    "short-text" -> decode.success(ShortText)
    "long-text" -> decode.success(LongText)
    "integer" -> decode.success(Integer)
    "real" -> decode.success(Real)
    "date" -> decode.success(Date)
    "date-time" -> decode.success(DateTime)
    _ -> decode.failure(Unset, "invalid simple kind")
  }
}

fn encode_kind(kind: Kind) -> json.Json {
  case kind {
    Date -> json.string("date")
    DateTime -> json.string("date-time")
    Integer -> json.string("integer")
    LongText -> json.string("long-text")
    Real -> json.string("real")
    ShortText -> json.string("short-text")
    Choice(options:) -> json.array(options, encode_option)
    Unset -> json.string("")
  }
}

pub type Option {
  Option(id: String, name: String, value: String, open: Bool)
}

fn option_decoder() -> Decoder(Option) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use value <- decode.field("value", decode.string)
  use open <- decode.field("open", decode.bool)
  decode.success(Option(id:, name:, value:, open:))
}

fn encode_option(option: Option) -> Json {
  json.object([
    #("id", json.string(option.id)),
    #("name", json.string(option.name)),
    #("value", json.string(option.value)),
    #("open", json.bool(option.open)),
  ])
}

pub fn new(index: Int) -> Column {
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

// -- COMPONENT ----------------------------------------------------------------

const component_tag = "table-column"

pub fn register() {
  let component =
    lustre.component(
      init,
      update,
      view,
      dict.from_list([
        #("column", fn(column) {
          case decode.run(column, decoder()) {
            Ok(column) -> Ok(ParentUpdatedColumn(column))
            Error(_) -> Error([])
          }
        }),
      ]),
    )

  lustre.register(component, component_tag)
}

pub fn column(
  column column: Column,
  on_change on_change: fn(Column) -> msg,
  on_move_up on_move_up: msg,
  on_move_down on_move_down: msg,
  on_create_above on_create_above: msg,
  on_create_below on_create_below: msg,
  on_delete on_delete: msg,
) -> Element(msg) {
  element.element(
    component_tag,
    [
      attribute.property("column", encode(column)),
      event.on("change", {
        use column <- decode.field("detail", decoder())
        decode.success(on_change(column))
      }),
      event.on("move-up", decode.success(on_move_up)),
      event.on("move-down", decode.success(on_move_down)),
      event.on("create-above", decode.success(on_create_above)),
      event.on("create-below", decode.success(on_create_below)),
      event.on("delete", decode.success(on_delete)),
    ],
    [],
  )
}

fn init(_) {
  let empty_column =
    Column(id: random_id(), kind: Unset, name: "", value: "", open: False)
  #(empty_column, effect.none)
}

// -- UPDATE -------------------------------------------------------------------

type Msg {
  ParentUpdatedColumn(column: Column)
  UserToggledColumnOpen
  UserEditedColumnName(name: String)
  UserClickedMoveUp
  UserClickedMoveDown
  UserClickedCreateAbove
  UserClickedCreateBelow
  UserClickedDelete
}

fn update(column: Column, msg: Msg) -> #(Column, effect.Effect(Msg)) {
  case msg {
    ParentUpdatedColumn(column:) -> #(column, effect.none)
    UserClickedCreateAbove -> #(column, event.emit("create-above", json.null()))
    UserClickedCreateBelow -> #(column, event.emit("create-below", json.null()))
    UserClickedDelete -> #(column, event.emit("delete", json.null()))
    UserClickedMoveDown -> #(column, event.emit("move-down", json.null()))
    UserClickedMoveUp -> #(column, event.emit("move-up", json.null()))
    UserEditedColumnName(name:) -> send(Column(..column, name:))
    UserToggledColumnOpen -> send(Column(..column, open: !column.open))
  }
}

fn send(column: Column) -> #(Column, Effect(Msg)) {
  #(column, event.emit("change", encode(column)))
}

// -- VIEW ---------------------------------------------------------------------

fn view(column: Column) -> Element(Msg) {
  html.details(
    [
      attribute.class("accordion-item"),
      attribute.open(column.open),
      event.on("toggle", decode.success(UserToggledColumnOpen)),
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
        [html.text(column.id), html.text(" - "), html.text(column.name)],
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
                attribute.attribute("aria-label", "Column name"),
                event.on_input(UserEditedColumnName),
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
            event.on_click(UserClickedDelete),
          ],
          [html.text("Delete column")],
        ),
        html.div([attribute.class("btn-group ms-2")], [
          html.button(
            [
              attribute.class("btn btn-sm btn-outline-primary mt-2"),
              event.on_click(UserClickedCreateAbove),
            ],
            [html.text("Create above")],
          ),
          html.button(
            [
              attribute.class("btn btn-sm btn-outline-primary mt-2"),
              event.on_click(UserClickedCreateBelow),
            ],
            [html.text("Create below")],
          ),
        ]),
        html.div([attribute.class("btn-group ms-2")], [
          html.button(
            [
              attribute.class("btn btn-sm btn-outline-primary mt-2"),
              event.on_click(UserClickedMoveUp),
            ],
            [html.text("Move up")],
          ),
          html.button(
            [
              attribute.class("btn btn-sm btn-outline-primary mt-2"),
              event.on_click(UserClickedMoveDown),
            ],
            [html.text("Move down")],
          ),
        ]),
      ]),
    ],
  )
}

// -- FFI ----------------------------------------------------------------------

@external(javascript, "./app.ffi.mjs", "random_id")
fn random_id() -> String
