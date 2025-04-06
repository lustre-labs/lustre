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

pub fn register() -> Result(Nil, lustre.Error) {
  let component =
    // Just like in earlier examples we upgraded the `lustre.simple` program to a
    // `lustre.application`, here we use the `lustre.component` program constructor
    // to get access to component-specific configuration like listening for attribute
    // changes.
    lustre.component(init, update, view, [
      // Attributes are string values that are set on the component's HTML element.
      // We can set up listeners for any attributes we care about and decode them
      // into a message for our update function. Any time the parent app changes
      // the attribute, this function will run and if it is `Ok` the message will
      // be sent to the component's update function.
      component.on_attribute_change("value", fn(value) {
        int.parse(value) |> result.map(ParentChangedValue)
      }),
      // Properties are similar to attributes, but they are not strings they are
      // arbitrary JavaScript values set of the component object directly. They
      // can be set using Lustre's `property` function, and are best used to send
      // structured data to the component.
      component.on_property_change("value", {
        decode.int |> decode.map(ParentChangedValue)
      }),
    ])

  lustre.register(component, "my-counter")
}

pub fn element(attributes: List(Attribute(msg))) -> Element(msg) {
  element.element("my-counter", attributes, [])
}

/// It's good practice to provide any custom attributes you want to support as
/// functions consumers of this component can call. Where possible, it's preferable
/// to use string attributes rather than rich properties so they can also be used
/// when server-rendering the component's HTML.
///
pub fn value(value: Int) -> Attribute(msg) {
  attribute.value(int.to_string(value))
}

/// Providing event attributes can be convenient don't need to know the exact
/// shape of the `detail` property on your component's custom events. Additionally,
/// you may want to provide the decoder itself in case users want to write their
/// own event listeners.
///
pub fn on_change(handler: fn(Int) -> msg) -> Attribute(msg) {
  event.on("change", {
    decode.at(["detail"], decode.int) |> decode.map(handler)
  })
}

// MODEL -----------------------------------------------------------------------

type Model =
  Int

fn init(_) -> #(Model, Effect(Msg)) {
  #(0, effect.none())
}

// UPDATE ----------------------------------------------------------------------

type Msg {
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

  html.div([attribute.class("flex items-center gap-2")], [
    view_button(label: "-", on_click: UserClickedDecrement),
    html.p([attribute.class("flex-1")], [html.text("Count: "), html.text(count)]),
    view_button(label: "+", on_click: UserClickedIncrement),
    view_button(label: "Submit", on_click: UserClickedSubmit),
  ])
}

fn view_button(label label: String, on_click handle_click: msg) -> Element(msg) {
  html.button(
    [
      attribute.class("bg-blue-500 text-white min-w-12 px-2 py-1 rounded"),
      event.on_click(handle_click),
    ],
    [html.text(label)],
  )
}
