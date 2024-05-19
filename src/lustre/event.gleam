// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type DecodeError, type Decoder, type Dynamic}
import gleam/json.{type Json}
import gleam/result
import lustre/attribute.{type Attribute}
import lustre/effect.{type Effect}

// TYPES -----------------------------------------------------------------------

type Decoded(a) =
  Result(a, List(DecodeError))

// EFFECTS ---------------------------------------------------------------------

/// Dispatches a custom message from a Lustre component. This lets components
/// communicate with their parents the same way native DOM elements do.
///
/// Any JSON-serialisable payload can be attached as additional data for any
/// event listeners to decode. This data will be on the event's `detail` property.
///
pub fn emit(event: String, data: Json) -> Effect(msg) {
  effect.event(event, data)
}

// CUSTOM EVENTS ---------------------------------------------------------------

/// Listens for the given event and applies the handler to the event object. If
/// the handler returns an `Ok` the resulting message will be dispatched, otherwise
/// the event (and any decoding error) will be ignored.
///
/// The event name is typically an all-lowercase string such as "click" or "mousemove".
/// If you're listening for non-standard events (like those emitted by a custom
/// element) their event names might be slightly different.
///
pub fn on(name: String, handler: Decoder(msg)) -> Attribute(msg) {
  attribute.on(name, handler)
}

// MOUSE EVENTS ----------------------------------------------------------------

///
pub fn on_click(msg: msg) -> Attribute(msg) {
  use _ <- on("click")
  Ok(msg)
}

///
pub fn on_mouse_down(msg: msg) -> Attribute(msg) {
  use _ <- on("mousedown")
  Ok(msg)
}

///
pub fn on_mouse_up(msg: msg) -> Attribute(msg) {
  use _ <- on("mouseup")
  Ok(msg)
}

///
pub fn on_mouse_enter(msg: msg) -> Attribute(msg) {
  use _ <- on("mouseenter")
  Ok(msg)
}

///
pub fn on_mouse_leave(msg: msg) -> Attribute(msg) {
  use _ <- on("mouseleave")
  Ok(msg)
}

///
pub fn on_mouse_over(msg: msg) -> Attribute(msg) {
  use _ <- on("mouseover")
  Ok(msg)
}

///
pub fn on_mouse_out(msg: msg) -> Attribute(msg) {
  use _ <- on("mouseout")
  Ok(msg)
}

// KEYBOARD EVENTS -------------------------------------------------------------

/// Listens for key presses on an element, and dispatches a message with the
/// current key being pressed.
///
pub fn on_keypress(msg: fn(String) -> msg) -> Attribute(msg) {
  use event <- on("keypress")

  event
  |> dynamic.field("key", dynamic.string)
  |> result.map(msg)
}

/// Listens for key down events on an element, and dispatches a message with the
/// current key being pressed.
///
pub fn on_keydown(msg: fn(String) -> msg) -> Attribute(msg) {
  use event <- on("keydown")

  event
  |> dynamic.field("key", dynamic.string)
  |> result.map(msg)
}

/// Listens for key up events on an element, and dispatches a message with the
/// current key being released.
///
pub fn on_keyup(msg: fn(String) -> msg) -> Attribute(msg) {
  use event <- on("keyup")

  event
  |> dynamic.field("key", dynamic.string)
  |> result.map(msg)
}

// FORM EVENTS -----------------------------------------------------------------

///
pub fn on_input(msg: fn(String) -> msg) -> Attribute(msg) {
  use event <- on("input")

  value(event)
  |> result.map(msg)
}

pub fn on_check(msg: fn(Bool) -> msg) -> Attribute(msg) {
  use event <- on("change")

  checked(event)
  |> result.map(msg)
}

/// Listens for the form's `submit` event, and dispatches the given message. This
/// will automatically call [`prevent_default`](#prevent_default) to stop the form
/// from submitting.
///
pub fn on_submit(msg: msg) -> Attribute(msg) {
  use event <- on("submit")
  let _ = prevent_default(event)

  Ok(msg)
}

// FOCUS EVENTS ----------------------------------------------------------------

pub fn on_focus(msg: msg) -> Attribute(msg) {
  use _ <- on("focus")
  Ok(msg)
}

pub fn on_blur(msg: msg) -> Attribute(msg) {
  use _ <- on("blur")
  Ok(msg)
}

// DECODERS --------------------------------------------------------------------

/// Decoding an input element's `value` is such a common operation that we have
/// a dedicated decoder for it. This attempts to decoder `event.target.value` as
/// a string.
///
pub fn value(event: Dynamic) -> Decoded(String) {
  event
  |> dynamic.field("target", dynamic.field("value", dynamic.string))
}

/// Similar to [`value`](#value), decoding a checkbox's `checked` state is common
/// enough to warrant a dedicated decoder. This attempts to decode
/// `event.target.checked` as a boolean.
///
pub fn checked(event: Dynamic) -> Decoded(Bool) {
  event
  |> dynamic.field("target", dynamic.field("checked", dynamic.bool))
}

/// Decodes the mouse position from any event that has a `clientX` and `clientY`
/// property.
///
pub fn mouse_position(event: Dynamic) -> Decoded(#(Float, Float)) {
  use x <- result.then(dynamic.field("clientX", dynamic.float)(event))
  use y <- result.then(dynamic.field("clientY", dynamic.float)(event))

  Ok(#(x, y))
}

// UTILS -----------------------------------------------------------------------

/// Calls an event's `preventDefault` method. If the `Dynamic` does not have a
/// `preventDefault` method, this function does nothing.
///
/// As the name implies, `preventDefault` will prevent any default action associated
/// with an event from occuring. For example, if you call `preventDefault` on a
/// `submit` event, the form will not be submitted.
///
/// See: https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault
///
@external(javascript, "../client-runtime.ffi.mjs", "prevent_default")
pub fn prevent_default(_event: Dynamic) -> Nil {
  Nil
}

/// Calls an event's `stopPropagation` method. If the `Dynamic` does not have a
/// `stopPropagation` method, this function does nothing.
///
/// Stopping event propagation means the event will not "bubble" up to parent
/// elements. If any elements higher up in the DOM have event listeners for the
/// same event, they will not be called.
///
/// See: https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation
///
@external(javascript, "../client-runtime.ffi.mjs", "stop_propagation")
pub fn stop_propagation(_event: Dynamic) -> Nil {
  Nil
}
