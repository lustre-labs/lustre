//// Effects for OpenTUI keyboard input, focus management, terminal control,
//// clipboard, selection, lifecycle, and scrolling.
////
//// OpenTUI dispatches keyboard events through the renderer's `keyInput`
//// EventEmitter, not through individual nodes. This module provides effects
//// that subscribe to global keyboard input, manage focus programmatically,
//// control the terminal, and more — letting you wire these into your MVU loop.
////
//// The renderer is managed internally by `opentui.platform()` — effects
//// access it automatically without requiring a renderer parameter.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/option.{type Option, None, Some}
import lustre/effect.{type Effect}
import lustre/platform/opentui.{type Renderer}

// TYPES -----------------------------------------------------------------------

/// A keyboard event from the terminal.
///
pub type KeyEvent {
  KeyEvent(key: String, ctrl: Bool, shift: Bool, meta: Bool)
}

// CUSTOM EFFECTS --------------------------------------------------------------

/// Create a custom before-paint effect with access to the OpenTUI renderer.
/// This is like `effect.before_paint` but provides the renderer instead of the
/// raw root element, letting you interact with OpenTUI's renderer API directly.
///
/// Runs after the virtual DOM has been reconciled but before the terminal is
/// painted, so dispatched messages trigger a second re-render before painting.
///
/// ```gleam
/// import lustre/platform/opentui/effect as opentui_effect
///
/// fn my_custom_effect() -> Effect(msg) {
///   opentui_effect.before_paint(fn(dispatch, renderer) {
///     // interact with the renderer directly
///   })
/// }
/// ```
///
pub fn before_paint(
  handler: fn(fn(msg) -> Nil, Renderer) -> Nil,
) -> Effect(msg) {
  effect.before_paint(fn(dispatch, _root) {
    do_with_renderer(handler, dispatch)
  })
}

/// Create a custom after-paint effect with access to the OpenTUI renderer.
/// This is like `effect.after_paint` but provides the renderer instead of the
/// raw root element, letting you interact with OpenTUI's renderer API directly.
///
/// Runs after the terminal has been painted.
///
pub fn after_paint(
  handler: fn(fn(msg) -> Nil, Renderer) -> Nil,
) -> Effect(msg) {
  effect.after_paint(fn(dispatch, _root) {
    do_with_renderer(handler, dispatch)
  })
}

// KEYBOARD & FOCUS EFFECTS ----------------------------------------------------

/// Subscribe to all keyboard events from the terminal. Dispatches
/// `handler(KeyEvent)` on every keypress. Call this in your `init` function
/// to start receiving keyboard events.
///
pub fn subscribe_keyboard(handler: fn(KeyEvent) -> msg) -> Effect(msg) {
  effect.from(do_subscribe_keyboard(handler, _))
}

/// Focus the next focusable element in the renderable tree.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// attempting to traverse the renderable tree.
///
pub fn focus_next() -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) { do_focus_next(fn(_) { Nil }) })
}

/// Focus the previous focusable element in the renderable tree.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// attempting to traverse the renderable tree.
///
pub fn focus_previous() -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) { do_focus_previous(fn(_) { Nil }) })
}

/// Focus a specific element by its OpenTUI id.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// attempting to find the element in the renderable tree.
///
pub fn focus(id: String) -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) { do_focus(id, fn(_) { Nil }) })
}

/// Get the id of the currently focused element. The handler receives
/// `Some(id)` if an element is focused, or `None` if nothing is focused.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// querying focus state.
///
pub fn get_focused_id(handler: fn(Option(String)) -> msg) -> Effect(msg) {
  effect.before_paint(fn(dispatch, _root) {
    let id = do_get_focused_id_raw()
    case id {
      "" -> dispatch(handler(None))
      _ -> dispatch(handler(Some(id)))
    }
  })
}

/// Get the currently focused element and decode properties from it.
/// The decoder runs against the raw focused node — use `decode.field` to
/// access properties like "id" (String), "focused" (Bool), "width" (Int),
/// "height" (Int), "value" (String), etc.
///
/// The handler receives `Some(value)` when a node is focused and the decoder
/// succeeds, or `None` when nothing is focused. If a node is focused but the
/// decoder fails, the handler is not called.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// querying focus state.
///
pub fn get_focused(
  decoder: Decoder(a),
  handler: fn(Option(a)) -> msg,
) -> Effect(msg) {
  effect.before_paint(fn(dispatch, _root) {
    let node = do_get_focused_node_raw()
    case decode.run(node, decode.optional(decoder)) {
      Ok(value) -> dispatch(handler(value))
      Error(_) -> Nil
    }
  })
}

// TERMINAL CONTROL EFFECTS ----------------------------------------------------

/// Set the terminal window title.
///
pub fn set_terminal_title(title: String) -> Effect(msg) {
  effect.from(do_set_terminal_title(title, _))
}

/// Set the terminal background color.
///
pub fn set_background_color(color: String) -> Effect(msg) {
  effect.from(do_set_background_color(color, _))
}

/// Set the cursor position and visibility.
///
pub fn set_cursor_position(x: Int, y: Int, visible: Bool) -> Effect(msg) {
  effect.from(do_set_cursor_position(x, y, visible, _))
}

/// Set the cursor style and blinking behavior.
///
pub fn set_cursor_style(style: String, blinking: Bool) -> Effect(msg) {
  effect.from(do_set_cursor_style(style, blinking, _))
}

/// Set the cursor color.
///
pub fn set_cursor_color(color: String) -> Effect(msg) {
  effect.from(do_set_cursor_color(color, _))
}

/// Get the current terminal dimensions. The handler receives width and height.
///
pub fn get_terminal_dimensions(handler: fn(Int, Int) -> msg) -> Effect(msg) {
  effect.from(do_get_terminal_dimensions(handler, _))
}

/// Subscribe to terminal resize events. The handler receives the new width and height.
/// Call this in your `init` function alongside subscribe_keyboard.
///
pub fn subscribe_terminal_resize(handler: fn(Int, Int) -> msg) -> Effect(msg) {
  effect.from(do_subscribe_terminal_resize(handler, _))
}

/// Toggle the debug overlay.
///
pub fn toggle_debug_overlay() -> Effect(msg) {
  effect.from(do_toggle_debug_overlay)
}

// CLIPBOARD EFFECTS -----------------------------------------------------------

/// Copy text to the clipboard via OSC52.
///
pub fn copy_to_clipboard(text: String) -> Effect(msg) {
  effect.from(do_copy_to_clipboard(text, _))
}

/// Clear the clipboard via OSC52.
///
pub fn clear_clipboard() -> Effect(msg) {
  effect.from(do_clear_clipboard)
}

// SELECTION EFFECTS -----------------------------------------------------------

/// Get the current text selection. The handler receives Ok(text) if there is
/// a selection, or Error(Nil) if not.
///
pub fn get_selection(handler: fn(Result(String, Nil)) -> msg) -> Effect(msg) {
  effect.from(fn(dispatch) {
    let text = do_get_selection_raw()
    case text {
      "" -> dispatch(handler(Error(Nil)))
      _ -> dispatch(handler(Ok(text)))
    }
  })
}

/// Clear the current text selection.
///
pub fn clear_selection() -> Effect(msg) {
  effect.from(do_clear_selection)
}

// LIFECYCLE EFFECTS -----------------------------------------------------------

/// Pause the renderer.
///
pub fn pause() -> Effect(msg) {
  effect.from(do_pause)
}

/// Suspend the renderer (pauses and restores terminal state).
///
pub fn suspend() -> Effect(msg) {
  effect.from(do_suspend)
}

/// Resume a paused or suspended renderer.
///
pub fn resume() -> Effect(msg) {
  effect.from(do_resume)
}

/// Destroy the renderer and clean up resources.
///
pub fn destroy() -> Effect(msg) {
  effect.from(do_destroy)
}

/// Stop the renderer's render loop.
///
pub fn stop() -> Effect(msg) {
  effect.from(do_stop)
}

/// Subscribe to renderer destroy event. Dispatches the given msg when destroyed.
///
pub fn on_destroy(msg: msg) -> Effect(msg) {
  effect.from(fn(dispatch) { do_on_destroy(fn() { dispatch(msg) }) })
}

// SCROLLING EFFECTS -----------------------------------------------------------

/// Scroll an element by a delta. The element is found by its id.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// attempting to find the element in the renderable tree.
///
pub fn scroll_by(element_id: String, delta_x: Int, delta_y: Int) -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) {
    do_scroll_by(element_id, delta_x, delta_y, fn(_) { Nil })
  })
}

/// Scroll an element to an absolute position. The element is found by its id.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// attempting to find the element in the renderable tree.
///
pub fn scroll_to(element_id: String, x: Int, y: Int) -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) {
    do_scroll_to(element_id, x, y, fn(_) { Nil })
  })
}

/// Scroll a child element into view within a scrollable container.
/// Only scrolls if the child is not fully visible.
///
pub fn scroll_into_view(container_id: String, child_id: String) -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) {
    do_scroll_into_view(container_id, child_id, fn(_) { Nil })
  })
}

// FFI -------------------------------------------------------------------------

@external(javascript, "./effect.ffi.ts", "with_renderer")
fn do_with_renderer(
  _handler: fn(fn(msg) -> Nil, Renderer) -> Nil,
  _dispatch: fn(msg) -> Nil,
) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "subscribe_keyboard")
fn do_subscribe_keyboard(
  _handler: fn(KeyEvent) -> msg,
  _dispatch: fn(msg) -> Nil,
) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "focus_next")
fn do_focus_next(_dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "focus_previous")
fn do_focus_previous(_dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "focus")
fn do_focus(_id: String, _dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "get_focused_id_raw")
fn do_get_focused_id_raw() -> String {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "get_focused_node_raw")
fn do_get_focused_node_raw() -> Dynamic {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "set_terminal_title")
fn do_set_terminal_title(_title: String, _dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "set_background_color")
fn do_set_background_color(_color: String, _dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "set_cursor_position")
fn do_set_cursor_position(
  _x: Int,
  _y: Int,
  _visible: Bool,
  _dispatch: fn(msg) -> Nil,
) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "set_cursor_style")
fn do_set_cursor_style(
  _style: String,
  _blinking: Bool,
  _dispatch: fn(msg) -> Nil,
) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "set_cursor_color")
fn do_set_cursor_color(_color: String, _dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "get_terminal_dimensions")
fn do_get_terminal_dimensions(
  _handler: fn(Int, Int) -> msg,
  _dispatch: fn(msg) -> Nil,
) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "toggle_debug_overlay")
fn do_toggle_debug_overlay(_dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "subscribe_terminal_resize")
fn do_subscribe_terminal_resize(
  _handler: fn(Int, Int) -> msg,
  _dispatch: fn(msg) -> Nil,
) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "copy_to_clipboard")
fn do_copy_to_clipboard(_text: String, _dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "clear_clipboard")
fn do_clear_clipboard(_dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "get_selection_raw")
fn do_get_selection_raw() -> String {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "clear_selection")
fn do_clear_selection(_dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "pause")
fn do_pause(_dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "suspend")
fn do_suspend(_dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "resume")
fn do_resume(_dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "destroy")
fn do_destroy(_dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "stop")
fn do_stop(_dispatch: fn(msg) -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "on_destroy")
fn do_on_destroy(_callback: fn() -> Nil) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "scroll_by")
fn do_scroll_by(
  _element_id: String,
  _delta_x: Int,
  _delta_y: Int,
  _dispatch: fn(msg) -> Nil,
) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "scroll_to")
fn do_scroll_to(
  _element_id: String,
  _x: Int,
  _y: Int,
  _dispatch: fn(msg) -> Nil,
) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}

@external(javascript, "./effect.ffi.ts", "scroll_into_view")
fn do_scroll_into_view(
  _container_id: String,
  _child_id: String,
  _dispatch: fn(msg) -> Nil,
) -> Nil {
  panic as "lustre/platform/opentui/effect only runs on JavaScript"
}
