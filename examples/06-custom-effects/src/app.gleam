import gleam/option.{type Option, None, Some}
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/event
import lustre/ui

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL -----------------------------------------------------------------------

type Model {
  Model(message: Option(String))
}

fn init(_flags) -> #(Model, Effect(Msg)) {
  #(Model(message: None), read_localstorage("message"))
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  UserUpdatedMessage(String)
  CacheUpdatedMessage(Result(String, Nil))
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserUpdatedMessage(input) -> #(
      Model(message: Some(input)),
      write_localstorage("message", input),
    )
    CacheUpdatedMessage(Ok(message)) -> #(
      Model(message: Some(message)),
      effect.none(),
    )
    CacheUpdatedMessage(Error(_)) -> #(model, effect.none())
  }
}

fn read_localstorage(key: String) -> Effect(Msg) {
  effect.from(fn(dispatch) {
    do_read_localstorage(key)
    |> CacheUpdatedMessage
    |> dispatch
  })
}

@external(javascript, "./app.ffi.mjs", "read_localstorage")
fn do_read_localstorage(_key: String) -> Result(String, Nil) {
  Error(Nil)
}

fn write_localstorage(key: String, value: String) -> Effect(msg) {
  effect.from(fn(_) { do_write_localstorage(key, value) })
}

@external(javascript, "./app.ffi.mjs", "write_localstorage")
fn do_write_localstorage(_key: String, _value: String) -> Nil {
  Nil
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let styles = [#("width", "100vw"), #("height", "100vh")]
  let message = option.unwrap(model.message, "")

  ui.centre(
    [attribute.style(styles)],
    ui.field(
      [],
      [],
      ui.input([attribute.value(message), event.on_input(UserUpdatedMessage)]),
      [element.text("Type a message and refresh the page")],
    ),
  )
}
