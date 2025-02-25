// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import lustre/attribute.{type Attribute}
import lustre/effect.{type Effect}
import lustre/runtime/vdom

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

///
pub fn prevent_default(event: Attribute(msg)) -> Attribute(msg) {
  case event {
    vdom.Event(..) -> vdom.Event(..event, prevent_default: True)
    _ -> event
  }
}

///
pub fn stop_propagation(event: Attribute(msg)) -> Attribute(msg) {
  case event {
    vdom.Event(..) -> vdom.Event(..event, stop_propagation: True)
    _ -> event
  }
}

// MOUSE EVENTS ----------------------------------------------------------------

///
pub fn on_click(msg: msg) -> Attribute(msg) {
  on("click", decode.success(msg))
}

///
pub fn on_mouse_down(msg: msg) -> Attribute(msg) {
  on("mousedown", decode.success(msg))
}

///
pub fn on_mouse_up(msg: msg) -> Attribute(msg) {
  on("mouseup", decode.success(msg))
}

///
pub fn on_mouse_enter(msg: msg) -> Attribute(msg) {
  on("mouseenter", decode.success(msg))
}

///
pub fn on_mouse_leave(msg: msg) -> Attribute(msg) {
  on("mouseleave", decode.success(msg))
}

///
pub fn on_mouse_over(msg: msg) -> Attribute(msg) {
  on("mouseover", decode.success(msg))
}

///
pub fn on_mouse_out(msg: msg) -> Attribute(msg) {
  on("mouseout", decode.success(msg))
}

// KEYBOARD EVENTS -------------------------------------------------------------

/// Listens for key presses on an element, and dispatches a message with the
/// current key being pressed.
///
pub fn on_keypress(msg: fn(String) -> msg) -> Attribute(msg) {
  on("keypress", {
    use key <- decode.field("key", decode.string)

    key |> msg |> decode.success
  })
}

/// Listens for key down events on an element, and dispatches a message with the
/// current key being pressed.
///
pub fn on_keydown(msg: fn(String) -> msg) -> Attribute(msg) {
  on("keydown", {
    use key <- decode.field("key", decode.string)

    key |> msg |> decode.success
  })
}

/// Listens for key up events on an element, and dispatches a message with the
/// current key being released.
///
pub fn on_keyup(msg: fn(String) -> msg) -> Attribute(msg) {
  on("keyup", {
    use key <- decode.field("key", decode.string)

    key |> msg |> decode.success
  })
}

// FORM EVENTS -----------------------------------------------------------------

///
pub fn on_input(msg: fn(String) -> msg) -> Attribute(msg) {
  on("input", value() |> decode.map(msg))
}

pub fn on_check(msg: fn(Bool) -> msg) -> Attribute(msg) {
  on("change", checked() |> decode.map(msg))
}

/// Listens for the form's `submit` event, and dispatches the given message. This
/// will automatically call [`prevent_default`](#prevent_default) to stop the form
/// from submitting.
///
pub fn on_submit(msg: msg) -> Attribute(msg) {
  on("submit", decode.success(msg)) |> prevent_default
}

// FOCUS EVENTS ----------------------------------------------------------------

pub fn on_focus(msg: msg) -> Attribute(msg) {
  on("focus", decode.success(msg))
}

pub fn on_blur(msg: msg) -> Attribute(msg) {
  on("blur", decode.success(msg))
}

// DECODERS --------------------------------------------------------------------

/// Decoding an input element's `value` is such a common operation that we have
/// a dedicated decoder for it. This attempts to decoder `event.target.value` as
/// a string.
///
pub fn value() -> Decoder(String) {
  decode.at(["target", "value"], decode.string)
}

/// Similar to [`value`](#value), decoding a checkbox's `checked` state is common
/// enough to warrant a dedicated decoder. This attempts to decode
/// `event.target.checked` as a boolean.
///
pub fn checked() -> Decoder(Bool) {
  decode.at(["target", "checked"], decode.bool)
}

/// Decodes the mouse position from any event that has a `clientX` and `clientY`
/// property.
///
pub fn mouse_position() -> Decoder(#(Float, Float)) {
  use x <- decode.field("clientX", decode.float)
  use y <- decode.field("clientY", decode.float)

  decode.success(#(x, y))
}
