////

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{DecodeError, Dynamic}
import gleam/option.{None, Option, Some}
import gleam/result
import lustre/attribute.{Attribute}
import lustre/effect.{Effect}

// TYPES -----------------------------------------------------------------------

type Decoded(a) =
  Result(a, List(DecodeError))

// EFFECTS ---------------------------------------------------------------------

@target(javascript)
@external(javascript, "../lustre.ffi.mjs", "emit")
pub fn emit(event: String, data: any) -> Effect(msg)

// CUSTOM EVENTS ---------------------------------------------------------------

pub fn on(name: String, handler: fn(Dynamic) -> Option(msg)) -> Attribute(msg) {
  attribute.on(name, handler)
}

// MOUSE EVENTS ----------------------------------------------------------------

///
pub fn on_click(msg: msg) -> Attribute(msg) {
  use _ <- on("click")
  Some(msg)
}

///
pub fn on_mouse_down(msg: msg) -> Attribute(msg) {
  use _ <- on("mousedown")
  Some(msg)
}

///
pub fn on_mouse_up(msg: msg) -> Attribute(msg) {
  use _ <- on("mouseup")
  Some(msg)
}

///
pub fn on_mouse_enter(msg: msg) -> Attribute(msg) {
  use _ <- on("mouseenter")
  Some(msg)
}

///
pub fn on_mouse_leave(msg: msg) -> Attribute(msg) {
  use _ <- on("mouseleave")
  Some(msg)
}

///
pub fn on_mouse_over(msg: msg) -> Attribute(msg) {
  use _ <- on("mouseover")
  Some(msg)
}

///
pub fn on_mouse_out(msg: msg) -> Attribute(msg) {
  use _ <- on("mouseout")
  Some(msg)
}

// KEYBOARD EVENTS -------------------------------------------------------------

/// Listens for key presses on an element, and dispatches a message with the
/// current key being pressed.
///
pub fn on_keypress(msg: fn(String) -> msg) -> Attribute(msg) {
  use event <- on("keypress")

  case dynamic.field("key", dynamic.string)(event) {
    Ok(key) -> Some(msg(key))
    Error(_) -> None
  }
}

/// Listens for key dow events on an element, and dispatches a message with the
/// current key being pressed.
///
pub fn on_keydown(msg: fn(String) -> msg) -> Attribute(msg) {
  use event <- on("keydown")

  case dynamic.field("key", dynamic.string)(event) {
    Ok(key) -> Some(msg(key))
    Error(_) -> None
  }
}

/// Listens for key up events on an element, and dispatches a message with the
/// current key being released.
///
pub fn on_keyup(msg: fn(String) -> msg) -> Attribute(msg) {
  use event <- on("keyup")

  case dynamic.field("key", dynamic.string)(event) {
    Ok(key) -> Some(msg(key))
    Error(_) -> None
  }
}

// FORM EVENTS -----------------------------------------------------------------

///
pub fn on_input(msg: fn(String) -> msg) -> Attribute(msg) {
  use event <- on("input")

  case value(event) {
    Ok(val) -> Some(msg(val))
    Error(_) -> None
  }
}

pub fn on_check(msg: fn(Bool) -> msg) -> Attribute(msg) {
  use event <- on("change")

  case checked(event) {
    Ok(val) -> Some(msg(val))
    Error(_) -> None
  }
}

pub fn on_submit(msg: msg) -> Attribute(msg) {
  use _ <- on("submit")
  Some(msg)
}

// FOCUS EVENTS ----------------------------------------------------------------

pub fn on_focus(msg: msg) -> Attribute(msg) {
  use _ <- on("focus")
  Some(msg)
}

pub fn on_blur(msg: msg) -> Attribute(msg) {
  use _ <- on("blur")
  Some(msg)
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
