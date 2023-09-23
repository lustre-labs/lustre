//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre/event](https://lustre.build/api/lustre/event)

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{DecodeError, Dynamic}
import gleam/result
import lustre/attribute.{Attribute}
import lustre/effect.{Effect}

// TYPES -----------------------------------------------------------------------

type Decoded(a) =
  Result(a, List(DecodeError))

// EFFECTS ---------------------------------------------------------------------

///
@external(javascript, "../lustre.ffi.mjs", "emit")
pub fn emit(_event: String, _data: any) -> Effect(msg) {
  effect.none()
}

// CUSTOM EVENTS ---------------------------------------------------------------

///
pub fn on(
  name: String,
  handler: fn(Dynamic) -> Result(msg, error),
) -> Attribute(msg) {
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

/// Listens for key dow events on an element, and dispatches a message with the
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

///
pub fn value(event: Dynamic) -> Decoded(String) {
  event
  |> dynamic.field("target", dynamic.field("value", dynamic.string))
}

///
pub fn checked(event: Dynamic) -> Decoded(Bool) {
  event
  |> dynamic.field("target", dynamic.field("checked", dynamic.bool))
}

///
pub fn mouse_position(event: Dynamic) -> Decoded(#(Float, Float)) {
  use x <- result.then(dynamic.field("clientX", dynamic.float)(event))
  use y <- result.then(dynamic.field("clientY", dynamic.float)(event))

  Ok(#(x, y))
}

// UTILS -----------------------------------------------------------------------

@external(javascript, "../lustre.ffi.mjs", "prevent_default")
pub fn prevent_default(_event: Dynamic) -> Nil {
  Nil
}

@external(javascript, "../lustre.ffi.mjs", "stop_propagation")
pub fn stop_propagation(_event: Dynamic) -> Nil {
  Nil
}
