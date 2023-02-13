////

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{DecodeError, Decoder, Dynamic}
import gleam/result
import lustre/attribute.{Attribute}

// TYPES -----------------------------------------------------------------------

type Decoded(a) =
  Result(a, List(DecodeError))

// CONSTRUCTORS ----------------------------------------------------------------

/// Attach custom event handlers to an element. A number of helper functions exist
/// in this module to cover the most common events and use-cases, so you should
/// check those out first.
///
/// If you need to handle an event that isn't covered by the helper functions,
/// then you can use `on` to attach a custom event handler. The callback receives
/// a `Dynamic` representing the JavaScript event object, and a dispatch function
/// you can use to send messages to the Lustre runtime.
///
/// As a simple example, you can implement `on_click` like so:
///
/// ```gleam
/// import lustre/attribute.{Attribute}
/// import lustre/event
/// 
/// pub fn on_click(msg: msg) -> Attribute(msg) {
///   use _, dispatch <- event.on("click")
///   dispatch(msg)
/// }
/// ```
///
/// By using `gleam/dynamic` you can decode the event object and pull out all sorts
/// of useful data. This is how `on_input` is implemented:
///
/// ```gleam
/// import gleam/dynamic
/// import lustre/attribute.{Attribute}
/// import lustre/event
/// 
/// pub fn on_input(msg: fn(String) -> msg) -> Attribute(msg) {
///   use event, dispatch <- on("input")
///   let decode_value = dynamic.field("target", dynamic.field("value", dynamic.string))
///   let emit_value = fn(value) { dispatch(msg(value)) }
/// 
///   event
///   |> decode_value
///   |> result.map(emit_value)
///   |> result.unwrap(Nil)
/// }
/// ```
///
/// You can take a look at the MDN reference for events
/// [here](https://developer.mozilla.org/en-US/docs/Web/API/Event) to see what
/// you can decode. 
///
/// Unlike the helpers in the rest of this module, it is possible to simply ignore
/// the dispatch function and not dispatch a message at all. In fact, we saw this
/// with the `on_input` example above: if we can't decode the event object, we
/// simply return `Nil` and do nothing.
///
/// Beyond error handling, this can be used to perform side effects we don't need
/// to observe in our main application loop, such as logging...
///
/// ```gleam
/// import gleam/io
/// import lustre/attribute.{Attribute}
/// import lustre/event
/// 
/// pub fn log_on_click(msg: String) -> Attribute(msg) {
///   use _, _ <- event.on("click")
///   io.println(msg)
/// }
/// ```
///
/// ...or calling `set_state` from a `stateful` Lustre element:
///
/// ```gleam
/// import gleam/int
/// import lustre/attribute.{Attribute}
/// import lustre/element.{Element}
/// import lustre/event
/// 
/// pub fn counter() -> Element(msg) {
///   use state, set_state = lustre.stateful(0)
/// 
///   let decr = event.on("click", fn(_, _) { set_state(state - 1) })
///   let incr = event.on("click", fn(_, _) { set_state(state + 1) })
/// 
///   element.div([], [
///     element.button([decr], [element.text("-")]),
///     element.text(int.to_string(state)),
///     element.button([incr], [element.text("+")]),
///   ])
/// }
/// ```
///
pub fn on(
  name: String,
  handler: fn(Dynamic, fn(msg) -> Nil) -> Nil,
) -> Attribute(msg) {
  attribute.event(name, handler)
}

// MOUSE EVENTS ----------------------------------------------------------------

///
pub fn on_click(msg: msg) -> Attribute(msg) {
  use _, dispatch <- on("click")
  dispatch(msg)
}

///
pub fn on_mouse_down(msg: msg) -> Attribute(msg) {
  use _, dispatch <- on("mouseDown")
  dispatch(msg)
}

///
pub fn on_mouse_up(msg: msg) -> Attribute(msg) {
  use _, dispatch <- on("mouseUp")
  dispatch(msg)
}

///
pub fn on_mouse_enter(msg: msg) -> Attribute(msg) {
  use _, dispatch <- on("mouseEnter")
  dispatch(msg)
}

///
pub fn on_mouse_leave(msg: msg) -> Attribute(msg) {
  use _, dispatch <- on("mouseLeave")
  dispatch(msg)
}

///
pub fn on_mouse_over(msg: msg) -> Attribute(msg) {
  use _, dispatch <- on("mouseOver")
  dispatch(msg)
}

///
pub fn on_mouse_out(msg: msg) -> Attribute(msg) {
  use _, dispatch <- on("mouseOut")
  dispatch(msg)
}

// KEYBOARD EVENTS -------------------------------------------------------------

/// Listens for key presses on an element, and dispatches a message with the
/// current key being pressed.
///
pub fn on_keypress(msg: fn(String) -> msg) -> Attribute(msg) {
  use event, dispatch <- on("keyPress")

  event
  |> dynamic.field("key", dynamic.string)
  |> result.map(msg)
  |> result.map(dispatch)
  |> result.unwrap(Nil)
}

/// Listens for key dow events on an element, and dispatches a message with the
/// current key being pressed.
///
pub fn on_keydown(msg: fn(String) -> msg) -> Attribute(msg) {
  use event, dispatch <- on("keyDown")

  event
  |> dynamic.field("key", dynamic.string)
  |> result.map(msg)
  |> result.map(dispatch)
  |> result.unwrap(Nil)
}

/// Listens for key up events on an element, and dispatches a message with the
/// current key being released.
///
pub fn on_keyup(msg: fn(String) -> msg) -> Attribute(msg) {
  use event, dispatch <- on("keyUp")

  event
  |> dynamic.field("key", dynamic.string)
  |> result.map(msg)
  |> result.map(dispatch)
  |> result.unwrap(Nil)
}

// FORM EVENTS -----------------------------------------------------------------

///
pub fn on_input(msg: fn(String) -> msg) -> Attribute(msg) {
  use event, dispatch <- on("change")

  event
  |> value
  |> result.map(msg)
  |> result.map(dispatch)
  |> result.unwrap(Nil)
}

pub fn on_check(msg: fn(Bool) -> msg) -> Attribute(msg) {
  use event, dispatch <- on("change")

  event
  |> dynamic.field("target", dynamic.field("checked", dynamic.bool))
  |> result.map(msg)
  |> result.map(dispatch)
  |> result.unwrap(Nil)
}

pub fn on_submit(msg: msg) -> Attribute(msg) {
  use _, dispatch <- on("submit")
  dispatch(msg)
}

// FOCUS EVENTS ----------------------------------------------------------------

pub fn on_focus(msg: msg) -> Attribute(msg) {
  use _, dispatch <- on("focus")
  dispatch(msg)
}

pub fn on_blur(msg: msg) -> Attribute(msg) {
  use _, dispatch <- on("blur")
  dispatch(msg)
}

// DECODERS --------------------------------------------------------------------

/// A helpful decoder to extract the `value` from an event object. This is handy
/// for getting the value as a string from an input event, for example.
///
pub fn value(event: Dynamic) -> Decoded(String) {
  event
  |> dynamic.field("target", dynamic.field("value", dynamic.string))
}

/// A helpful decoder to extract the `checked` property from an event triggered
/// by a checkbox.
///
pub fn checked(event: Dynamic) -> Decoded(Bool) {
  event
  |> dynamic.field("target", dynamic.field("checked", dynamic.bool))
}

/// A helpful decoder to grab the mouse's current x and y position in the
/// viewport from an event object.
///
pub fn mouse_position(event: Dynamic) -> Decoded(#(Float, Float)) {
  use x <- result.then(dynamic.field("clientX", dynamic.float)(event))
  use y <- result.then(dynamic.field("clientY", dynamic.float)(event))

  Ok(#(x, y))
}
