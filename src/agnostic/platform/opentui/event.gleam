//// TUI event helpers for OpenTUI. These provide event handlers for TUI-specific
//// events that map to @opentui/core event types.
////

// IMPORTS ---------------------------------------------------------------------

import agnostic/attribute.{type Attribute}
import agnostic/event
import agnostic/platform/opentui/effect.{type KeyEvent, KeyEvent}
import gleam/dynamic/decode

// KEYBOARD EVENTS -------------------------------------------------------------

/// Listen for key press events. The handler receives a `KeyEvent` containing
/// the key name and the state of the shift, ctrl, meta, and option modifiers.
///
pub fn on_key_press(handler: fn(KeyEvent) -> msg) -> Attribute(msg) {
  event.on("keypress", {
    use key_event <- decode.then(decode_key_event())
    decode.success(handler(key_event))
  })
}

/// Listen for key down events.
///
pub fn on_key_down(handler: fn(KeyEvent) -> msg) -> Attribute(msg) {
  event.on("keydown", {
    use key_event <- decode.then(decode_key_event())
    decode.success(handler(key_event))
  })
}

/// Listen for key up events.
///
pub fn on_key_up(handler: fn(KeyEvent) -> msg) -> Attribute(msg) {
  event.on("keyup", {
    use key_event <- decode.then(decode_key_event())
    decode.success(handler(key_event))
  })
}

// FOCUS EVENTS ----------------------------------------------------------------

/// Listen for focus events.
///
pub fn on_focus(msg: msg) -> Attribute(msg) {
  event.on("focus", decode.success(msg))
}

/// Listen for blur events.
///
pub fn on_blur(msg: msg) -> Attribute(msg) {
  event.on("blur", decode.success(msg))
}

// INPUT EVENTS ----------------------------------------------------------------

/// Listen for input value changes. The handler receives the new value.
///
pub fn on_input(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("input", {
    use value <- decode.subfield(["target", "plainText"], decode.string)
    decode.success(handler(value))
  })
}

/// Listen for submit events (e.g. pressing Enter in an input).
///
pub fn on_submit(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("submit", {
    use value <- decode.then(decode_detail_value())
    decode.success(handler(value))
  })
}

// SELECTION EVENTS ------------------------------------------------------------

/// Listen for select/change events.
///
pub fn on_change(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("change", {
    use value <- decode.then(decode_detail_value())
    decode.success(handler(value))
  })
}

/// Listen for item selection events (e.g. Enter in a select list). The handler
/// receives the selected index.
///
pub fn on_select(handler: fn(Int) -> msg) -> Attribute(msg) {
  event.on("select", {
    use index <- decode.then(decode.at(["detail"], decode.int))
    decode.success(handler(index))
  })
}

/// Listen for selection change events (e.g. navigating with j/k in a select
/// list). The handler receives the newly highlighted index.
///
pub fn on_selection_change(handler: fn(Int) -> msg) -> Attribute(msg) {
  event.on("selectionchange", {
    use index <- decode.then(decode.at(["detail"], decode.int))
    decode.success(handler(index))
  })
}

// SCROLL EVENTS ---------------------------------------------------------------

/// Listen for scroll events.
///
pub fn on_scroll(msg: msg) -> Attribute(msg) {
  event.on("scroll", decode.success(msg))
}

// MOUSE EVENTS ----------------------------------------------------------------

/// Listen for click events.
///
pub fn on_click(msg: msg) -> Attribute(msg) {
  event.on_click(msg)
}

/// Listen for mouse down events.
///
pub fn on_mouse_down(msg: msg) -> Attribute(msg) {
  event.on("mousedown", decode.success(msg))
}

/// Listen for mouse up events.
///
pub fn on_mouse_up(msg: msg) -> Attribute(msg) {
  event.on("mouseup", decode.success(msg))
}

/// Listen for mouse move events.
///
pub fn on_mouse_move(msg: msg) -> Attribute(msg) {
  event.on("mousemove", decode.success(msg))
}

/// Listen for mouse over events.
///
pub fn on_mouse_over(msg: msg) -> Attribute(msg) {
  event.on("mouseover", decode.success(msg))
}

/// Listen for mouse out events.
///
pub fn on_mouse_out(msg: msg) -> Attribute(msg) {
  event.on("mouseout", decode.success(msg))
}

/// Listen for mouse drag events.
///
pub fn on_mouse_drag(msg: msg) -> Attribute(msg) {
  event.on("mousedrag", decode.success(msg))
}

/// Listen for mouse drag end events.
///
pub fn on_mouse_drag_end(msg: msg) -> Attribute(msg) {
  event.on("mousedragend", decode.success(msg))
}

/// Listen for mouse drop events.
///
pub fn on_mouse_drop(msg: msg) -> Attribute(msg) {
  event.on("mousedrop", decode.success(msg))
}

// PASTE EVENTS ----------------------------------------------------------------

/// Listen for paste events. The handler receives the pasted text.
///
pub fn on_paste(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("paste", {
    use text <- decode.then(decode.at(["detail", "text"], decode.string))
    decode.success(handler(text))
  })
}

// RESIZE EVENTS ---------------------------------------------------------------

/// Listen for size change events.
///
pub fn on_size_change(msg: msg) -> Attribute(msg) {
  event.on("resize", decode.success(msg))
}

// CURSOR/CONTENT EVENTS -------------------------------------------------------

/// Listen for cursor change events. The handler receives the line and visual
/// column position.
///
pub fn on_cursor_change(handler: fn(Int, Int) -> msg) -> Attribute(msg) {
  event.on("cursorchange", {
    use line <- decode.then(decode.at(["detail", "line"], decode.int))
    use col <- decode.then(decode.at(["detail", "visualColumn"], decode.int))
    decode.success(handler(line, col))
  })
}

/// Listen for content change events. The handler receives the current text.
///
pub fn on_content_change(handler: fn(String) -> msg) -> Attribute(msg) {
  event.on("contentchange", {
    use value <- decode.subfield(["target", "plainText"], decode.string)
    decode.success(handler(value))
  })
}

/// Listen for highlight events.
///
pub fn on_highlight(msg: msg) -> Attribute(msg) {
  event.on("highlight", decode.success(msg))
}

// SLIDER EVENTS ---------------------------------------------------------------

/// Listen for slider value change events. The handler receives the new value.
///
pub fn on_slider_change(handler: fn(Float) -> msg) -> Attribute(msg) {
  event.on("sliderchange", {
    use value <- decode.then(decode.at(["detail"], decode.float))
    decode.success(handler(value))
  })
}

// ACTIVATION EVENTS -----------------------------------------------------------

/// Listen for Enter or Space key presses — useful for "activating" a focused
/// element.
///
pub fn on_activate(msg: msg) -> Attribute(msg) {
  event.on("keydown", {
    use key <- decode.then(decode_detail_key())
    case key {
      "return" | "Enter" | "enter" | " " | "space" -> decode.success(msg)
      _ -> decode.failure(msg, "Not an activation key")
    }
  })
}

// DECODERS --------------------------------------------------------------------

fn decode_detail_key() -> decode.Decoder(String) {
  decode.at(["detail", "key"], decode.string)
}

fn decode_key_event() -> decode.Decoder(KeyEvent) {
  use key <- decode.then(decode.at(["detail", "key"], decode.string))
  use ctrl <- decode.then(decode.at(["detail", "ctrl"], decode.bool))
  use shift <- decode.then(decode.at(["detail", "shift"], decode.bool))
  use meta <- decode.then(decode.at(["detail", "meta"], decode.bool))
  use option <- decode.then(decode.at(["detail", "option"], decode.bool))
  decode.success(KeyEvent(key:, ctrl:, shift:, meta:, option:))
}

fn decode_detail_value() -> decode.Decoder(String) {
  decode.at(["detail"], decode.string)
}
