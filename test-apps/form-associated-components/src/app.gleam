// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode
import gleam/int
import gleam/result
import lustre
import lustre/attribute
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app =
    lustre.component(init, update, view, [
      component.on_attribute_change("value", fn(value) {
        case int.parse(value) {
          Ok(n) -> Ok(UserTypedNumber(n))
          Error(_) -> Error(Nil)
        }
      }),
      component.on_property_change("value", {
        use value <- decode.map(decode.int)
        let half = value / 2

        UserTypedNumber(half)
      }),
      component.form_associated(),
      component.on_form_restore(fn(value) {
        case int.parse(value) {
          Ok(n) -> UserTypedNumber(n / 2)
          Error(_) -> UserTypedNumber(0)
        }
      }),
    ])
  let assert Ok(_) = lustre.register(app, "double-number-input")

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

fn init(_) -> #(Model, Effect(Msg)) {
  #(0, effect.none())
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserTypedNumber(value: Int)
}

fn update(_model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserTypedNumber(value) -> #(
      value,
      component.set_form_value(int.to_string(value * 2)),
    )
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.input([
    attribute.type_("number"),
    attribute.value(int.to_string(model)),
    event.on("input", {
      use value <- decode.map(event.value())

      int.parse(value) |> result.unwrap(0) |> UserTypedNumber
    }),
  ])
}
