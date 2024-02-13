import gleam/option.{type Option, None, Some}
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/event
// These examples are written with lustre_ui in mind. They'll work regardless,
// but to see what lustre_ui can do make sure to run each of these examples with
// the `--include-styles` flag:
//
//   $ gleam run -m lustre/try -- --include-styles
//
// In your own apps, make sure to add the `lustre_ui` dependency and include the
// stylesheet somewhere.
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

fn init(_) -> #(Model, Effect(Msg)) {
  #(Model(message: None), read_localstorage("message", GotMessage))
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  GotInput(String)
  GotMessage(Result(String, Nil))
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    GotInput(input) -> #(
      Model(message: Some(input)),
      write_localstorage("message", input),
    )
    GotMessage(Ok(message)) -> #(Model(message: Some(message)), effect.none())
    GotMessage(Error(_)) -> #(model, effect.none())
  }
}

fn read_localstorage(
  key: String,
  to_msg: fn(Result(String, Nil)) -> msg,
) -> Effect(msg) {
  effect.from(fn(dispatch) {
    do_read_localstorage(key)
    |> to_msg
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
      ui.input([attribute.value(message), event.on_input(GotInput)]),
      [element.text("Type a message and refresh the page")],
    ),
  )
}
