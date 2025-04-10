// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/result
import lustre
import lustre/attribute.{type Attribute}
import lustre/component
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn component() -> lustre.App(_, Model, Msg) {
  // Server components are still Lustre components, which means they get access
  // to the same features like listening to changes to attributes and properties
  // or emitting DOM events.
  lustre.component(init, update, view, [
    component.on_attribute_change("value", fn(value) {
      int.parse(value) |> result.map(ParentChangedValue)
    }),
    // Because properties can be any `Json` value, they unlock a way for the client
    // to send rich data to the server component: this includes arrays and objects
    // which can be decoded here!
    component.on_property_change("value", {
      decode.int |> decode.map(ParentChangedValue)
    }),
  ])
}

pub fn value(value: Int) -> Attribute(msg) {
  attribute.value(int.to_string(value))
}

// MODEL -----------------------------------------------------------------------

pub type Model =
  Int

fn init(_) -> #(Model, Effect(Msg)) {
  #(0, effect.none())
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  ParentChangedValue(Int)
  UserClickedIncrement
  UserClickedDecrement
  UserClickedSubmit
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ParentChangedValue(value) -> #(value, effect.none())
    UserClickedIncrement -> #(model + 1, effect.none())
    UserClickedDecrement -> #(model - 1, effect.none())
    UserClickedSubmit -> #(
      model,
      // The `event.emit` effect allows us to emit a real DOM event from our
      // component. Any data we encode will be available in the event's `detail`
      // property.
      event.emit("change", json.int(model)),
    )
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  let count = int.to_string(model)
  let styles = [#("display", "flex"), #("justify-content", "space-between")]

  html.div([attribute.styles(styles)], [
    view_button(label: "-", on_click: UserClickedDecrement),
    html.p([], [html.text("Count: "), html.text(count)]),
    view_button(label: "+", on_click: UserClickedIncrement),
    view_button(label: "Submit", on_click: UserClickedSubmit),
  ])
}

fn view_button(label label: String, on_click handle_click: msg) -> Element(msg) {
  html.button([event.on_click(handle_click)], [html.text(label)])
}
