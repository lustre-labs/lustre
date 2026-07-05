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

@target(javascript)
import gleam/dynamic.{type Dynamic}
@target(javascript)
import gleam/dynamic/decode.{type Decoder}
@target(javascript)
import gleam/option.{type Option, None, Some}
@target(javascript)
import lustre/effect.{type Effect}
@target(javascript)
import lustre/platform/opentui.{type Renderer}

// TYPES -----------------------------------------------------------------------

/// A keyboard event from the terminal.
///
pub type KeyEvent {
  KeyEvent(key: String, ctrl: Bool, shift: Bool, meta: Bool, option: Bool)
}

/// A single EditBuffer's contribution to a multi-paragraph selection.
///
/// `id` is the OpenTUI Renderable id of the participating EditBuffer. `start`
/// and `end` are character offsets within that EditBuffer's local text.
///
pub type SelectionRange {
  SelectionRange(id: String, start: Int, end: Int)
}

/// A renderer-level selection. Used by both `subscribe_selection` (for
/// drag-completion events) and `get_selection` (for the current state).
///
/// `ranges` contains one entry per participating EditBufferRenderable, in the
/// order OpenTUI's `selectedRenderables` array reports them. Non-EditBuffer
/// selectables are dropped (they have no character-range API). `focused_id`
/// is the currently focused Renderable's id at the time of the event, or the
/// empty string if nothing is focused. `anchor` and `focus` are the drag's
/// starting and ending terminal cell coordinates `#(column, row)` in absolute
/// screen space.
///
pub type Selection {
  Selection(
    ranges: List(SelectionRange),
    focused_id: String,
    anchor: #(Int, Int),
    focus: #(Int, Int),
  )
}

// CUSTOM EFFECTS --------------------------------------------------------------

@target(javascript)
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

@target(javascript)
/// Create a custom after-paint effect with access to the OpenTUI renderer.
/// This is like `effect.after_paint` but provides the renderer instead of the
/// raw root element, letting you interact with OpenTUI's renderer API directly.
///
/// Runs after the terminal has been painted.
///
pub fn after_paint(
  handler: fn(fn(msg) -> Nil, Renderer) -> Nil,
) -> Effect(msg) {
  effect.after_paint(fn(dispatch, _root) { do_with_renderer(handler, dispatch) })
}

// KEYBOARD & FOCUS EFFECTS ----------------------------------------------------

@target(javascript)
/// Subscribe to all keyboard events from the terminal. Dispatches
/// `handler(KeyEvent)` on every keypress. Call this in your `init` function
/// to start receiving keyboard events.
///
pub fn subscribe_keyboard(handler: fn(KeyEvent) -> msg) -> Effect(msg) {
  effect.from(do_subscribe_keyboard(handler, _))
}

@target(javascript)
/// Subscribe to keyboard events, dispatching only when the predicate returns
/// `Some(msg)`. Events for which the predicate returns `None` are silently
/// ignored — no message is dispatched and no render cycle is triggered. This
/// is useful when only a subset of keys (e.g. escape, modifier combinations)
/// should trigger an update, avoiding unnecessary renders that can interfere
/// with focused input elements.
///
/// ```gleam
/// import gleam/option.{None, Some}
/// import lustre/platform/opentui/effect as opentui_effect
///
/// fn subscribe_shortcuts() -> Effect(Msg) {
///   opentui_effect.subscribe_keyboard_with(fn(key_event) {
///     let opentui_effect.KeyEvent(key:, ctrl:, ..) = key_event
///     case key, ctrl {
///       "z", True -> Some(Undo)
///       "escape", _ -> Some(Escape)
///       _, _ -> None
///     }
///   })
/// }
/// ```
///
pub fn subscribe_keyboard_with(
  predicate: fn(KeyEvent) -> Option(msg),
) -> Effect(msg) {
  effect.from(fn(dispatch) {
    do_subscribe_keyboard_raw(fn(key_event) {
      case predicate(key_event) {
        Some(msg) -> dispatch(msg)
        None -> Nil
      }
    })
  })
}

@target(javascript)
/// Focus the next focusable element in the renderable tree.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// attempting to traverse the renderable tree.
///
pub fn focus_next() -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) { do_focus_next(fn(_) { Nil }) })
}

@target(javascript)
/// Focus the previous focusable element in the renderable tree.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// attempting to traverse the renderable tree.
///
pub fn focus_previous() -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) { do_focus_previous(fn(_) { Nil }) })
}

@target(javascript)
/// Focus a specific element by its OpenTUI id.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// attempting to find the element in the renderable tree.
///
pub fn focus(id: String) -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) { do_focus(id, fn(_) { Nil }) })
}

@target(javascript)
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

@target(javascript)
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

@target(javascript)
/// Set the terminal window title.
///
pub fn set_terminal_title(title: String) -> Effect(msg) {
  effect.from(do_set_terminal_title(title, _))
}

@target(javascript)
/// Set the terminal background color.
///
pub fn set_background_color(color: String) -> Effect(msg) {
  effect.from(do_set_background_color(color, _))
}

@target(javascript)
/// Set the cursor position and visibility.
///
pub fn set_cursor_position(x: Int, y: Int, visible: Bool) -> Effect(msg) {
  effect.from(do_set_cursor_position(x, y, visible, _))
}

@target(javascript)
/// Set the cursor style and blinking behavior.
///
pub fn set_cursor_style(style: String, blinking: Bool) -> Effect(msg) {
  effect.from(do_set_cursor_style(style, blinking, _))
}

@target(javascript)
/// Set the cursor color.
///
pub fn set_cursor_color(color: String) -> Effect(msg) {
  effect.from(do_set_cursor_color(color, _))
}

@target(javascript)
/// Get the current terminal dimensions. The handler receives width and height.
///
pub fn get_terminal_dimensions(handler: fn(Int, Int) -> msg) -> Effect(msg) {
  effect.from(do_get_terminal_dimensions(handler, _))
}

@target(javascript)
/// Subscribe to terminal resize events. The handler receives the new width and height.
/// Call this in your `init` function alongside subscribe_keyboard.
///
pub fn subscribe_terminal_resize(handler: fn(Int, Int) -> msg) -> Effect(msg) {
  effect.from(do_subscribe_terminal_resize(handler, _))
}

@target(javascript)
/// Toggle the debug overlay.
///
pub fn toggle_debug_overlay() -> Effect(msg) {
  effect.from(do_toggle_debug_overlay)
}

// CLIPBOARD EFFECTS -----------------------------------------------------------

@target(javascript)
/// Copy text to the clipboard via OSC52.
///
pub fn copy_to_clipboard(text: String) -> Effect(msg) {
  effect.from(do_copy_to_clipboard(text, _))
}

@target(javascript)
/// Clear the clipboard via OSC52.
///
pub fn clear_clipboard() -> Effect(msg) {
  effect.from(do_clear_clipboard)
}

// SELECTION EFFECTS -----------------------------------------------------------

@target(javascript)
/// Get the current active renderer-level selection, or `None` if nothing is
/// selected. The returned `Selection` has the same shape as events delivered
/// by `subscribe_selection`.
///
pub fn get_selection(handler: fn(Option(Selection)) -> msg) -> Effect(msg) {
  effect.from(fn(dispatch) {
    case do_get_selection() {
      Ok(selection) -> dispatch(handler(Some(selection)))
      Error(_) -> dispatch(handler(None))
    }
  })
}

@target(javascript)
/// Subscribe to renderer-level drag-completion selection events. Fires once
/// per completed mouse drag. Does NOT fire during in-flight drag updates,
/// programmatic `set_selection`, or `clear_selection`. Call this in your
/// `init` function exactly once — calling it multiple times will install
/// duplicate listeners.
///
pub fn subscribe_selection(handler: fn(Selection) -> msg) -> Effect(msg) {
  effect.from(do_subscribe_selection(handler, _))
}

@target(javascript)
/// Clear the current text selection.
///
pub fn clear_selection() -> Effect(msg) {
  effect.from(do_clear_selection)
}

@target(javascript)
/// Highlight a range of text, spanning multiple paragraphs if needed. Selects from
/// `anchor_id`:`anchor_offset` to `focus_id`:`focus_offset` and moves the caret to
/// the focus end. Replaces any current selection; idempotent — safe every render.
/// Clear with `clear_selection`.
///
pub fn set_selection_span(
  anchor_id: String,
  anchor_offset: Int,
  focus_id: String,
  focus_offset: Int,
) -> Effect(msg) {
  effect.before_paint(fn(_, _) {
    do_set_selection_span(anchor_id, anchor_offset, focus_id, focus_offset)
  })
}

// LIFECYCLE EFFECTS -----------------------------------------------------------

@target(javascript)
/// Pause the renderer.
///
pub fn pause() -> Effect(msg) {
  effect.from(do_pause)
}

@target(javascript)
/// Suspend the renderer (pauses and restores terminal state).
///
pub fn suspend() -> Effect(msg) {
  effect.from(do_suspend)
}

@target(javascript)
/// Resume a paused or suspended renderer.
///
pub fn resume() -> Effect(msg) {
  effect.from(do_resume)
}

@target(javascript)
/// Destroy the renderer and clean up resources.
///
pub fn destroy() -> Effect(msg) {
  effect.from(do_destroy)
}

@target(javascript)
/// Stop the renderer's render loop.
///
pub fn stop() -> Effect(msg) {
  effect.from(do_stop)
}

@target(javascript)
/// Subscribe to renderer destroy event. Dispatches the given msg when destroyed.
///
pub fn on_destroy(msg: msg) -> Effect(msg) {
  effect.from(fn(dispatch) { do_on_destroy(fn() { dispatch(msg) }) })
}

// SCROLLING EFFECTS -----------------------------------------------------------

@target(javascript)
/// Scroll an element by a delta. The element is found by its id.
///
/// This uses `before_paint` to ensure the view has been reconciled before
/// attempting to find the element in the renderable tree.
///
pub fn scroll_by(
  element_id: String,
  delta_x: Int,
  delta_y: Int,
) -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) {
    do_scroll_by(element_id, delta_x, delta_y, fn(_) { Nil })
  })
}

@target(javascript)
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

@target(javascript)
/// Scroll a child element into view within a scrollable container.
/// Only scrolls if the child is not fully visible.
///
pub fn scroll_into_view(container_id: String, child_id: String) -> Effect(msg) {
  effect.before_paint(fn(_dispatch, _root) {
    do_scroll_into_view(container_id, child_id, fn(_) { Nil })
  })
}

// FFI -------------------------------------------------------------------------

@target(javascript)
@external(javascript, "./effect.ffi.ts", "with_renderer")
fn do_with_renderer(
  handler: fn(fn(msg) -> Nil, Renderer) -> Nil,
  dispatch: fn(msg) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "subscribe_keyboard")
fn do_subscribe_keyboard(
  handler: fn(KeyEvent) -> msg,
  dispatch: fn(msg) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "subscribe_keyboard_raw")
fn do_subscribe_keyboard_raw(callback: fn(KeyEvent) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "focus_next")
fn do_focus_next(dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "focus_previous")
fn do_focus_previous(dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "focus")
fn do_focus(id: String, dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "get_focused_id_raw")
fn do_get_focused_id_raw() -> String

@target(javascript)
@external(javascript, "./effect.ffi.ts", "get_focused_node_raw")
fn do_get_focused_node_raw() -> Dynamic

@target(javascript)
@external(javascript, "./effect.ffi.ts", "set_terminal_title")
fn do_set_terminal_title(title: String, dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "set_background_color")
fn do_set_background_color(color: String, dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "set_cursor_position")
fn do_set_cursor_position(
  x: Int,
  y: Int,
  visible: Bool,
  dispatch: fn(msg) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "set_cursor_style")
fn do_set_cursor_style(
  style: String,
  blinking: Bool,
  dispatch: fn(msg) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "set_cursor_color")
fn do_set_cursor_color(color: String, dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "get_terminal_dimensions")
fn do_get_terminal_dimensions(
  handler: fn(Int, Int) -> msg,
  dispatch: fn(msg) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "toggle_debug_overlay")
fn do_toggle_debug_overlay(dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "subscribe_terminal_resize")
fn do_subscribe_terminal_resize(
  handler: fn(Int, Int) -> msg,
  dispatch: fn(msg) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "copy_to_clipboard")
fn do_copy_to_clipboard(text: String, dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "clear_clipboard")
fn do_clear_clipboard(dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "get_selection")
fn do_get_selection() -> Result(Selection, Nil)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "subscribe_selection")
fn do_subscribe_selection(
  handler: fn(Selection) -> msg,
  dispatch: fn(msg) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "clear_selection")
fn do_clear_selection(dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "set_selection_span")
fn do_set_selection_span(
  anchor_id: String,
  anchor_offset: Int,
  focus_id: String,
  focus_offset: Int,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "pause")
fn do_pause(dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "suspend")
fn do_suspend(dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "resume")
fn do_resume(dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "destroy")
fn do_destroy(dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "stop")
fn do_stop(dispatch: fn(msg) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "on_destroy")
fn do_on_destroy(callback: fn() -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "scroll_by")
fn do_scroll_by(
  element_id: String,
  delta_x: Int,
  delta_y: Int,
  dispatch: fn(msg) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "scroll_to")
fn do_scroll_to(
  element_id: String,
  x: Int,
  y: Int,
  dispatch: fn(msg) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "scroll_into_view")
fn do_scroll_into_view(
  container_id: String,
  child_id: String,
  dispatch: fn(msg) -> Nil,
) -> Nil
