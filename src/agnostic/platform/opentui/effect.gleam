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
import agnostic/effect.{type Effect}
@target(javascript)
import agnostic/platform/opentui.{type Node, type Renderer}
@target(javascript)
import gleam/bool
@target(javascript)
import gleam/dynamic.{type Dynamic}
@target(javascript)
import gleam/dynamic/decode.{type Decoder}
@target(javascript)
import gleam/int
@target(javascript)
import gleam/list
@target(javascript)
import gleam/option.{type Option, None, Some}
@target(javascript)
import gleam/result

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

/// The absolute screen rectangle of a renderable, in terminal cells: `x`/`y`
/// are the top-left corner in absolute screen space, `width`/`height` the
/// renderable's size. Returned by [`Layout`](#Layout)'s `rect`.
///
pub type Rect {
  Rect(x: Int, y: Int, width: Int, height: Int)
}

/// The scroll geometry of a scrollable container, in terminal cells: the
/// content's total size, the viewport's size, and the current scroll
/// position. Returned by [`Layout`](#Layout)'s `scroll_extents`. The maximum
/// scroll position on an axis is the content size minus the viewport size.
///
pub type ScrollExtents {
  ScrollExtents(
    content_width: Int,
    content_height: Int,
    viewport_width: Int,
    viewport_height: Int,
    scroll_x: Int,
    scroll_y: Int,
  )
}

/// Fresh, layout-final geometry, provided to [`after_layout`](#after_layout)
/// callbacks. Both functions look an element up by its OpenTUI id and read
/// its geometry directly from the layout engine — the only safe source at
/// that timing, where the renderables' cached accessors still hold last
/// frame's values. Only valid for the duration of the callback: the next
/// update can invalidate everything.
///
/// - `rect` returns the element's absolute screen rectangle, or `Error(Nil)`
///   when no element has that id.
/// - `scroll_extents` returns a scroll container's fresh scroll geometry, or
///   `Error(Nil)` when the id is unresolvable or the element is not a
///   scrollable container.
///
pub type Layout {
  Layout(
    rect: fn(String) -> Result(Rect, Nil),
    scroll_extents: fn(String) -> Result(ScrollExtents, Nil),
  )
}

/// A scroll padding for
/// [`scroll_into_view_with_padding`](#scroll_into_view_with_padding): extra
/// clearance, in terminal cells, given between the revealed target and the
/// viewport edge when reconciling the target into view. The same type serves
/// both axes — `by` means rows or columns depending on which argument
/// carries it.
///
/// The padding is landing-edge-relative and symmetric: it applies at
/// whichever edge the reveal lands the target at — revealing downward leaves
/// `by` cells visible below the target, revealing upward leaves `by` cells
/// above it (left/right on the horizontal axis). Because the reveal's scroll
/// can approach from either edge, signed past-the-edge semantics would be
/// ambiguous; the value is simply the extra given beyond the minimal reveal.
/// Negative values clamp to zero, and `by: 0` is the minimal flush reveal.
///
/// - `OnScroll(by:)` applies the padding only when the reveal actually
///   scrolls on that axis. A target that is already visible is never
///   touched — no jitter, reveals stay safe to fire liberally.
/// - `Always(by:)` enforces the clearance from *both* viewport edges even
///   for an already-visible target: crowding either edge within `by` cells
///   triggers the shortfall scroll away from that edge. This is the behavior
///   of vim's `scrolloff`, CSS `scroll-margin`, and OpenTUI's own editor
///   `scrollMargin`, applied at the scrollbox level.
///
/// Paddings are capped so the target always stays fully in view (`OnScroll`
/// caps at the viewport size minus the target size; `Always` at half that,
/// so the clearances at both edges can hold simultaneously), and clamp
/// silently at the content extents — no context exists past the last row,
/// so a padding there rests at the maximum scroll position.
///
pub type ScrollPadding {
  OnScroll(by: Int)
  Always(by: Int)
}

// CUSTOM EFFECTS --------------------------------------------------------------

@target(javascript)
/// Schedule a side effect guaranteed to run after the frame containing this
/// update has been flushed to the terminal. This anchors on OpenTUI's FRAME
/// event. Because the platform starts OpenTUI's continuous render loop, FRAME
/// fires on every normal tick — even ones whose updates changed nothing
/// visible — so this effect cannot be starved by an "inert" update. Ticks
/// whose flush is blocked or backpressured drain at the next successful
/// flush; a paused, suspended, or stopped renderer runs no ticks and so no
/// phase effects at all.
///
/// In addition to the `dispatch` function, your callback receives a
/// [`Layout`](#Layout) and the OpenTUI renderer. Everything is readable and
/// fresh here: `Layout` — the only geometry source reachable from Gleam, its
/// lookups valid only for the duration of the callback — now agrees with the
/// renderables' cached accessors, and renderer-level reads — the current
/// selection, the focused renderable, terminal dimensions, render stats —
/// reflect the update. Writes are safe but target the *next* frame: a message
/// dispatched inside this effect starts an ordinary update cycle whose output
/// appears in the next flushed frame.
///
/// > **Note**: this timing is for coordinates fed to renderer APIs whose
/// > internals depend on painted state (the selection API, the hit grid) and
/// > for visibility-tied reads (persist a scroll position, record what was
/// > shown). Ordinary geometry reads and scroll math belong in
/// > [`after_layout`](#after_layout), where the values are identical and
/// > writes still make the frame.
///
/// ```gleam
/// import agnostic/platform/opentui/effect as opentui_effect
///
/// fn my_custom_effect() -> Effect(msg) {
///   opentui_effect.after_flush(fn(dispatch, layout, renderer) {
///     // interact with the renderer directly
///   })
/// }
/// ```
///
/// > **Note**: platforms that do not declare this phase — including server
/// > components — drop this effect and never run it.
///
pub fn after_flush(
  handler: fn(fn(msg) -> Nil, Layout, Renderer) -> Nil,
) -> Effect(msg) {
  effect.deferred(opentui.after_flush_phase, fn(dispatch, _root) {
    do_with_renderer(
      fn(dispatch, renderer) {
        let layout =
          Layout(
            rect: fn(id) { do_fresh_rect(renderer, id) },
            scroll_extents: fn(id) { do_fresh_extents(renderer, id) },
          )
        handler(dispatch, layout, renderer)
      },
      dispatch,
    )
  })
}

@target(javascript)
/// Schedule a side effect guaranteed to run after this update has been
/// reconciled into the renderable tree — in the same render-loop tick, before
/// OpenTUI computes layout or paints. This anchors on OpenTUI's frame
/// callbacks slot, which runs on every loop iteration — including ticks whose
/// flush ends up blocked, where [`after_flush`](#after_flush) waits for the
/// next successful flush.
///
/// In addition to the `dispatch` function, your callback receives the OpenTUI
/// renderer. The tree is fresh: id lookups and traversal resolve elements
/// mounted by this same update, focus setters land and paint in this frame,
/// and focus reads are final for the update. Uniquely, property writes made
/// here — including layout-affecting ones — still make *this* frame, because
/// layout has not run yet.
///
/// > **Note**: for the same reason, all geometry is stale here — the layout
/// > engine has not recomputed anything this update changed, and the cached
/// > accessors hold last frame's values. Positioning or scroll math belongs
/// > in [`after_layout`](#after_layout).
///
/// > **Note**: platforms that do not declare this phase — including server
/// > components — drop this effect and never run it.
///
pub fn frame_callbacks(
  handler: fn(fn(msg) -> Nil, Renderer) -> Nil,
) -> Effect(msg) {
  effect.deferred(opentui.frame_callbacks_phase, fn(dispatch, _root) {
    do_with_renderer(handler, dispatch)
  })
}

@target(javascript)
/// Schedule a side effect guaranteed to run after yoga layout is final for
/// the frame containing this update, before it paints — in the same
/// render-loop tick. When the update dirtied layout this anchors on OpenTUI's
/// `LAYOUT_CHANGED` event; when it did not, a frame-callback fallback runs
/// the effect over the previous — still current — layout. Either way the
/// effect runs on every loop iteration, flushed or not.
///
/// In addition to the `dispatch` function, your callback receives a
/// [`Layout`](#Layout) and the OpenTUI renderer. `Layout` is the *only* safe
/// geometry source at this timing: it reads positions, sizes, and scroll
/// extents directly from the layout engine, while the renderables' cached
/// `x`/`y`/`width`/`height` accessors are refreshed only during the later
/// paint walk and still hold last frame's values here. Its lookups are valid
/// only for the duration of the callback. The renderer is what makes fresh
/// geometry actionable in the same frame: id lookups are fresh, scroll writes
/// land in this frame, and the selection API accepts layout-final screen
/// coordinates.
///
/// > **Note**: layout-affecting writes made here *miss* this frame — layout
/// > already ran, so they only dirty the next one. Make them in
/// > [`frame_callbacks`](#frame_callbacks) instead. And since this slot sits
/// > between layout and paint, heavy work here delays the flush.
///
/// > **Note**: platforms that do not declare this phase — including server
/// > components — drop this effect and never run it.
///
pub fn after_layout(
  handler: fn(fn(msg) -> Nil, Layout, Renderer) -> Nil,
) -> Effect(msg) {
  effect.deferred(opentui.after_layout_phase, fn(dispatch, _root) {
    do_with_renderer(
      fn(dispatch, renderer) {
        let layout =
          Layout(
            rect: fn(id) { do_fresh_rect(renderer, id) },
            scroll_extents: fn(id) { do_fresh_extents(renderer, id) },
          )
        handler(dispatch, layout, renderer)
      },
      dispatch,
    )
  })
}

// KEYBOARD & FOCUS EFFECTS ----------------------------------------------------

@target(javascript)
/// Subscribe to all keyboard events from the terminal. Dispatches
/// `handler(KeyEvent)` on every keypress. Call this in your `init` function
/// to start receiving keyboard events.
///
pub fn subscribe_keyboard(handler: fn(KeyEvent) -> msg) -> Effect(msg) {
  effect.from(fn(dispatch) {
    do_subscribe_keyboard_raw(fn(raw) {
      case decode.run(raw, key_event_decoder()) {
        Ok(key_event) -> dispatch(handler(key_event))
        Error(_) -> Nil
      }
    })
  })
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
/// import agnostic/platform/opentui/effect as opentui_effect
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
    do_subscribe_keyboard_raw(fn(raw) {
      case decode.run(raw, key_event_decoder()) {
        Ok(key_event) ->
          case predicate(key_event) {
            Some(msg) -> dispatch(msg)
            None -> Nil
          }
        Error(_) -> Nil
      }
    })
  })
}

@target(javascript)
/// Decode OpenTUI's raw keypress payload. Every field defaults — `name` can
/// be absent for some key sequences and the modifier flags are only set when
/// known — so a partial payload still yields a `KeyEvent` rather than being
/// dropped.
///
fn key_event_decoder() -> Decoder(KeyEvent) {
  use key <- permissive_field("name", "", decode.string)
  use ctrl <- permissive_field("ctrl", False, decode.bool)
  use shift <- permissive_field("shift", False, decode.bool)
  use meta <- permissive_field("meta", False, decode.bool)
  use option <- permissive_field("option", False, decode.bool)
  decode.success(KeyEvent(key:, ctrl:, shift:, meta:, option:))
}

@target(javascript)
/// A field that falls back to `default` when it is missing *or* fails to
/// decode (e.g. present but `undefined` on a JavaScript object).
///
fn permissive_field(
  name: String,
  default: a,
  decoder: Decoder(a),
  next: fn(a) -> Decoder(b),
) -> Decoder(b) {
  decode.optional_field(
    name,
    default,
    decode.one_of(decoder, or: [decode.success(default)]),
    next,
  )
}

@target(javascript)
/// Focus the next focusable element in the renderable tree.
///
/// This runs in the [`frame_callbacks`](#frame_callbacks) phase — after this
/// update has been reconciled, in the same render-loop tick — so the
/// traversal sees the tree this update produced: elements it mounted are
/// candidates and elements it removed are not. OpenTUI's focus setters
/// request a render themselves, so the change is picked up by the very next
/// flushed frame.
///
pub fn focus_next() -> Effect(msg) {
  frame_callbacks(fn(_dispatch, renderer) {
    focus_step(focusables(renderer), forward: True)
  })
}

@target(javascript)
/// Focus the previous focusable element in the renderable tree.
///
/// This runs in the [`frame_callbacks`](#frame_callbacks) phase — after this
/// update has been reconciled, in the same render-loop tick — so the
/// traversal sees the tree this update produced: elements it mounted are
/// candidates and elements it removed are not. OpenTUI's focus setters
/// request a render themselves, so the change is picked up by the very next
/// flushed frame.
///
pub fn focus_previous() -> Effect(msg) {
  frame_callbacks(fn(_dispatch, renderer) {
    focus_step(focusables(renderer), forward: False)
  })
}

@target(javascript)
/// Focus a specific element by its OpenTUI id.
///
/// This runs in the [`frame_callbacks`](#frame_callbacks) phase — after this
/// update has been reconciled, in the same render-loop tick — so an element
/// created by the *same* update (including a keyed remount) is resolvable and
/// receives the focus. OpenTUI's focus setters request a render themselves,
/// so the change is picked up by the very next flushed frame.
///
pub fn focus(id: String) -> Effect(msg) {
  frame_callbacks(fn(_dispatch, renderer) {
    case list.find(focusables(renderer), fn(node) { do_node_id(node) == id }) {
      Ok(node) -> do_focus_node(node)
      Error(Nil) -> Nil
    }
  })
}

@target(javascript)
/// Get the id of the currently focused element. The handler receives
/// `Some(id)` if an element is focused, or `None` if nothing is focused.
///
/// This uses [`frame_callbacks`](#frame_callbacks): focus state is final once
/// this update has been reconciled — nothing in layout, paint, or flush
/// changes it — so the read reflects this update, happens in the same
/// render-loop tick, and also runs for blocked frames where nothing flushed.
///
pub fn get_focused_id(handler: fn(Option(String)) -> msg) -> Effect(msg) {
  frame_callbacks(fn(dispatch, renderer) {
    case list.find(focusables(renderer), do_is_focused) {
      Ok(node) -> dispatch(handler(Some(do_node_id(node))))
      Error(Nil) -> dispatch(handler(None))
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
/// This uses [`frame_callbacks`](#frame_callbacks): focus state is final once
/// this update has been reconciled — nothing in layout, paint, or flush
/// changes it — so the read reflects this update, happens in the same
/// render-loop tick, and also runs for blocked frames where nothing flushed.
/// Geometry fields read through the decoder are last paint walk's cached
/// values, as at any timing.
///
pub fn get_focused(
  decoder: Decoder(a),
  handler: fn(Option(a)) -> msg,
) -> Effect(msg) {
  frame_callbacks(fn(dispatch, renderer) {
    case list.find(focusables(renderer), do_is_focused) {
      Ok(node) ->
        case decode.run(do_to_dynamic(node), decoder) {
          Ok(value) -> dispatch(handler(Some(value)))
          Error(_) -> Nil
        }
      Error(Nil) -> dispatch(handler(None))
    }
  })
}

@target(javascript)
/// The focus ring: every live focusable in the renderable tree, in traversal
/// (pre-)order.
///
fn focusables(renderer: Renderer) -> List(Node) {
  collect_focusables(do_root(renderer))
}

@target(javascript)
/// Collect focusable nodes recursively. Portal children are skipped — they
/// are teleported to their target and will be found there during traversal;
/// descending here would double-count them. Destroyed subtrees are skipped
/// too, so every collected node is live.
///
fn collect_focusables(node: Node) -> List(Node) {
  case do_is_portal(node) || do_is_destroyed(node) {
    True -> []
    False -> {
      let descendants = list.flat_map(do_children(node), collect_focusables)
      case do_is_focusable(node) {
        True -> [node, ..descendants]
        False -> descendants
      }
    }
  }
}

@target(javascript)
/// Focus the ring neighbor of the currently focused node: the next one
/// (wrapping to the first), or the previous one (wrapping to the last). When
/// nothing is focused, forward starts at the first node and backward at the
/// last.
///
fn focus_step(focusables: List(Node), forward forward: Bool) -> Nil {
  use <- bool.guard(focusables == [], Nil)
  let length = list.length(focusables)
  let target_index = case forward, index_where(focusables, do_is_focused, 0) {
    True, Ok(index) -> { index + 1 } % length
    True, Error(Nil) -> 0
    False, Ok(0) -> length - 1
    False, Ok(index) -> index - 1
    False, Error(Nil) -> length - 1
  }
  case list.drop(focusables, target_index) |> list.first {
    Ok(node) -> do_focus_node(node)
    Error(Nil) -> Nil
  }
}

@target(javascript)
/// The index of the first element satisfying the predicate.
///
fn index_where(
  list: List(a),
  predicate: fn(a) -> Bool,
  index: Int,
) -> Result(Int, Nil) {
  case list {
    [] -> Error(Nil)
    [first, ..rest] ->
      case predicate(first) {
        True -> Ok(index)
        False -> index_where(rest, predicate, index + 1)
      }
  }
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
    let renderer = do_get_renderer()
    case do_get_selection_raw(renderer) {
      Ok(raw) -> dispatch(handler(Some(build_selection(renderer, raw))))
      Error(Nil) -> dispatch(handler(None))
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
  effect.from(fn(dispatch) {
    do_subscribe_selection_raw(fn(raw) {
      dispatch(handler(build_selection(do_get_renderer(), raw)))
    })
  })
}

@target(javascript)
/// Assemble a [`Selection`](#Selection) from OpenTUI's raw selection payload:
/// one `SelectionRange` per live EditBuffer participant that actually holds a
/// range (non-EditBuffer selectables have no character-range API and are
/// dropped), plus the focused id and the drag's screen-cell endpoints.
///
fn build_selection(renderer: Renderer, raw: Dynamic) -> Selection {
  let ranges =
    do_selected_renderables(raw)
    |> list.filter_map(fn(node) {
      use <- bool.guard(
        !do_is_edit_buffer(node) || do_is_destroyed(node),
        Error(Nil),
      )
      use #(start, end) <- result.try(do_node_selection_range(node))
      Ok(SelectionRange(id: do_node_id(node), start:, end:))
    })
  let focused_id = case do_current_focused(renderer) {
    Ok(node) -> do_node_id(node)
    Error(Nil) -> ""
  }
  Selection(
    ranges:,
    focused_id:,
    anchor: do_selection_anchor(raw),
    focus: do_selection_focus(raw),
  )
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
/// This runs in the [`after_flush`](#after_flush) phase — after the frame
/// containing this update has painted — because OpenTUI's selection API
/// converts the coordinates it is given back through geometry cached during
/// the paint walk (both to find candidate selectables and to anchor the
/// selection locally). Feeding it coordinates from any earlier point in the
/// frame mismatches that cache exactly when the same update mounts, moves,
/// or scrolls the target, silently dropping or misplacing the selection.
/// Post-paint, every coordinate source agrees — including for editors
/// mounted by this same update — and the selection paints on the next tick.
///
pub fn set_selection_span(
  anchor_id: String,
  anchor_offset: Int,
  focus_id: String,
  focus_offset: Int,
) -> Effect(msg) {
  after_flush(fn(_dispatch, layout, renderer) {
    // Guard chain: both ids resolve to live EditBuffers, the anchor is
    // selectable (startSelection() bails silently otherwise), and both rects
    // are fresh. Any failure drops the effect silently, matching the other
    // id-addressed effects in this module.
    let selection = {
      use anchor <- result.try(live_edit_buffer(renderer, anchor_id))
      use focus <- result.try(live_edit_buffer(renderer, focus_id))
      use <- bool.guard(!do_is_selectable(anchor), Error(Nil))
      use anchor_rect <- result.try(layout.rect(anchor_id))
      use focus_rect <- result.try(layout.rect(focus_id))
      let #(anchor_col, anchor_row) =
        do_measure_visual_cursor(anchor, anchor_offset)
      let #(focus_col, focus_row) =
        do_measure_visual_cursor(focus, focus_offset)
      // startSelection() clears any prior selection first -> calling this
      // every render is idempotent. updateSelection with finishDragging
      // settles the selection WITHOUT emitting CliRenderEvents.SELECTION
      // (only the private finishSelection emits), so nothing loops back to
      // the app.
      do_start_selection(
        renderer,
        anchor,
        anchor_rect.x + anchor_col,
        anchor_rect.y + anchor_row,
      )
      do_update_selection(
        renderer,
        focus,
        focus_rect.x + focus_col,
        focus_rect.y + focus_row,
      )
      Ok(Nil)
    }
    let _ = selection
    Nil
  })
}

@target(javascript)
/// Resolve an id to a live (non-destroyed) EditBuffer renderable.
///
fn live_edit_buffer(renderer: Renderer, id: String) -> Result(Node, Nil) {
  use node <- result.try(do_find_node(renderer, id))
  case do_is_edit_buffer(node) && !do_is_destroyed(node) {
    True -> Ok(node)
    False -> Error(Nil)
  }
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
/// This runs in the [`after_layout`](#after_layout) phase — after yoga layout
/// is final for the frame containing this update, before it paints — so an
/// element created by the *same* update is resolvable, and the scroll clamps
/// against this frame's content and viewport extents rather than last
/// frame's: scrolling toward content grown by this update reaches it. The
/// write still lands in the same frame.
///
pub fn scroll_by(
  element_id: String,
  delta_x: Int,
  delta_y: Int,
) -> Effect(msg) {
  after_layout(fn(_dispatch, layout, renderer) {
    // scroll_extents verifies the element is a live scrollbox and re-syncs
    // its scrollbars to this frame's extents, so the write below clamps
    // against exactly what the extents report.
    case layout.scroll_extents(element_id), do_find_node(renderer, element_id) {
      Ok(extents), Ok(node) ->
        do_set_scroll(
          node,
          clamp_scroll(
            extents.scroll_x + delta_x,
            extents.content_width,
            extents.viewport_width,
          ),
          clamp_scroll(
            extents.scroll_y + delta_y,
            extents.content_height,
            extents.viewport_height,
          ),
        )
      _, _ -> Nil
    }
  })
}

@target(javascript)
/// Scroll an element to an absolute position. The element is found by its id.
///
/// This runs in the [`after_layout`](#after_layout) phase — after yoga layout
/// is final for the frame containing this update, before it paints — so an
/// element created by the *same* update is resolvable, and the position
/// clamps against this frame's content and viewport extents rather than last
/// frame's: scrolling to the bottom of content grown by this update lands at
/// the true bottom. The write still lands in the same frame.
///
pub fn scroll_to(element_id: String, x: Int, y: Int) -> Effect(msg) {
  after_layout(fn(_dispatch, layout, renderer) {
    // scroll_extents verifies the element is a live scrollbox and re-syncs
    // its scrollbars to this frame's extents, so the write below clamps
    // against exactly what the extents report.
    case layout.scroll_extents(element_id), do_find_node(renderer, element_id) {
      Ok(extents), Ok(node) ->
        do_set_scroll(
          node,
          clamp_scroll(x, extents.content_width, extents.viewport_width),
          clamp_scroll(y, extents.content_height, extents.viewport_height),
        )
      _, _ -> Nil
    }
  })
}

@target(javascript)
/// Clamp a scroll position to the valid range: 0 up to the content size
/// minus the viewport size (never negative).
///
fn clamp_scroll(position: Int, content: Int, viewport: Int) -> Int {
  int.clamp(position, min: 0, max: int.max(0, content - viewport))
}

@target(javascript)
/// Scroll a child element into view within a scrollable container. The
/// container must be a `scrollbox`; both elements are found by their ids.
/// Only scrolls if the child is not fully visible, moving the minimal amount
/// per axis (nearest edge).
///
/// This runs in the [`after_layout`](#after_layout) phase — after this
/// update's layout is final, but before the frame paints. The first flushed
/// frame containing this update therefore already shows the correct scroll
/// position. This holds whether the target already existed, was mounted by
/// this same update, or the update changed layout or was paint-only.
///
/// When the target is a textarea or input taller than the container's viewport
/// (or wider, for non-wrapping editors), the reveal targets the *caret* cell
/// rather than merely an edge of the editor's box, so the caret row ends up
/// visible.
///
/// If either id is unresolvable when the effect runs — or the container
/// is not a `scrollbox` — the reveal is silently dropped, matching the other
/// id-addressed effects in this module.
///
/// > **Note**: revealing a child away from a sticky edge counts as a manual
/// > scroll: `sticky_scroll`/`sticky_start` are released for that container
/// > until the scroll position returns to the sticky edge.
///
/// > **Note**: platforms that do not declare the `after_layout` phase —
/// > including server components — drop this effect and never run it.
///
/// To leave extra clearance between the revealed target and the viewport
/// edge, see
/// [`scroll_into_view_with_padding`](#scroll_into_view_with_padding).
///
pub fn scroll_into_view(container_id: String, child_id: String) -> Effect(msg) {
  reveal(container_id, child_id, None, None)
}

@target(javascript)
/// Like [`scroll_into_view`](#scroll_into_view), with an optional
/// [`ScrollPadding`](#ScrollPadding) per axis: `cols` for horizontal clearance
/// in columns, `rows` for vertical clearance in rows. Pass `None` for an
/// axis to keep the minimal flush reveal there.
///
/// ```gleam
/// import gleam/option
/// import agnostic/platform/opentui/effect as opentui_effect
///
/// // Reveal "item-9", keeping 3 rows of clearance between it and the
/// // viewport edge — even when it is already visible but crowding an edge.
/// opentui_effect.scroll_into_view_with_padding(
///   "list",
///   "item-9",
///   cols: option.None,
///   rows: option.Some(opentui_effect.Always(by: 3)),
/// )
/// ```
///
/// See [`ScrollPadding`](#ScrollPadding) for the padding semantics:
/// landing-edge relative and symmetric; `OnScroll` applies only when the
/// axis actually scrolls, `Always` also nudges an already-visible target out
/// of the padding zone; negative values clamp to zero; paddings are capped so
/// the target always stays fully in view and clamp silently at the content
/// extents.
///
/// > **Note**: a padding that lands the scroll position within 1 cell of a
/// > sticky edge re-engages `sticky_scroll`/`sticky_start` for the
/// > container.
///
pub fn scroll_into_view_with_padding(
  container_id: String,
  child_id: String,
  cols cols: Option(ScrollPadding),
  rows rows: Option(ScrollPadding),
) -> Effect(msg) {
  reveal(container_id, child_id, cols, rows)
}

@target(javascript)
/// The reveal body shared by `scroll_into_view` and
/// `scroll_into_view_with_padding`.
///
fn reveal(
  container_id: String,
  child_id: String,
  cols: Option(ScrollPadding),
  rows: Option(ScrollPadding),
) -> Effect(msg) {
  after_layout(fn(_dispatch, layout, renderer) {
    let reveal = {
      use container <- result.try(do_find_node(renderer, container_id))
      use child <- result.try(do_find_node(renderer, child_id))
      // scroll_extents verifies the container is a live scrollbox and
      // re-syncs its scrollbars to this frame's extents, so the scroll write
      // below clamps against fresh sizes, not stale ones.
      use extents <- result.try(layout.scroll_extents(container_id))
      use child_rect <- result.try(layout.rect(child_id))
      let content = do_scroll_content(container)
      // Drop the reveal if the child is not inside this container's scroll
      // content.
      use <- bool.guard(!is_within(child, content), Error(Nil))
      // The child's offset within the scroll content, in scroll-space: both
      // rects are absolute and layout-fresh, so their difference cancels the
      // current scroll translate along with everything above the content.
      let content_rect = do_node_rect(content)
      let offset_x = child_rect.x - content_rect.x
      let offset_y = child_rect.y - content_rect.y

      // Caret narrowing: for an editor bigger than the viewport, revealing
      // "some edge of the box" can leave the caret offscreen. visualCursor is
      // relative to the editor's internal viewport, which matches the
      // editor's own box, so the caret cell within content is
      // offset + visualRow/Col. The x mirror only applies to non-wrapping
      // editors: wrapping editors never x-scroll and their visualCol is not
      // viewport-converted. (If the editor was resized by this same update,
      // the internal viewport still has the previous size here, so the
      // narrowed cell can be approximate for one self-correcting frame.)
      let #(start_y, end_y) = case
        do_is_edit_buffer(child) && child_rect.height > extents.viewport_height
      {
        True -> {
          let #(_, caret_row) = do_visual_cursor(child)
          #(offset_y + caret_row, offset_y + caret_row + 1)
        }
        False -> #(offset_y, offset_y + child_rect.height)
      }
      let #(start_x, end_x) = case
        do_is_edit_buffer(child)
        && child_rect.width > extents.viewport_width
        && do_wrap_mode(child) == "none"
      {
        True -> {
          let #(caret_col, _) = do_visual_cursor(child)
          #(offset_x + caret_col, offset_x + caret_col + 1)
        }
        False -> #(offset_x, offset_x + child_rect.width)
      }

      let delta_y =
        axis_delta(
          start_y,
          end_y,
          extents.scroll_y,
          extents.viewport_height,
          rows,
        )
      let delta_x =
        axis_delta(
          start_x,
          end_x,
          extents.scroll_x,
          extents.viewport_width,
          cols,
        )
      case delta_x, delta_y {
        0, 0 -> Nil
        _, _ -> do_scroll_rel(container, delta_x, delta_y)
      }
      Ok(Nil)
    }
    let _ = reveal
    Nil
  })
}

@target(javascript)
/// Whether `node` sits (strictly) below `ancestor` in the renderable tree.
///
fn is_within(node: Node, ancestor: Node) -> Bool {
  case do_parent(node) {
    Error(Nil) -> False
    Ok(parent) ->
      case do_same_node(parent, ancestor) {
        True -> True
        False -> is_within(parent, ancestor)
      }
  }
}

@target(javascript)
/// The nearest-edge rule of ScrollBox.scrollChildIntoView, applied over
/// scroll-space intervals instead of cached screen coordinates. Deliberate
/// divergence from upstream: its strict comparisons skip elements exactly
/// the viewport's size; here equality reveals (both alignments coincide at
/// that size).
///
fn nearest_edge_delta(
  element_start: Int,
  element_end: Int,
  viewport_start: Int,
  viewport_end: Int,
) -> Int {
  let element_size = element_end - element_start
  let viewport_size = viewport_end - viewport_start
  let start_outside = element_start < viewport_start
  let end_outside = element_end > viewport_end
  case start_outside, end_outside {
    True, True -> 0
    True, False if element_size <= viewport_size ->
      element_start - viewport_start
    True, False if element_size > viewport_size -> element_end - viewport_end
    False, True if element_size > viewport_size ->
      element_start - viewport_start
    False, True if element_size <= viewport_size -> element_end - viewport_end
    _, _ -> 0
  }
}

@target(javascript)
/// Applies an optional scroll padding around the nearest-edge rule. Paddings
/// are landing-edge-relative and unsigned: the extra clearance goes at
/// whichever edge the reveal lands the target at. Effective paddings are
/// capped so the target always stays fully in view; the scrollbar setter
/// clamps the final position at the content extents (best-effort near a
/// content boundary).
///
fn axis_delta(
  start: Int,
  end: Int,
  scroll: Int,
  viewport: Int,
  padding: Option(ScrollPadding),
) -> Int {
  let size = end - start
  case padding {
    // Enforce the clearance from both edges, even when the target is
    // already visible (vim-scrolloff): test against the symmetrically
    // shrunk window. The halved cap keeps both clearances satisfiable
    // simultaneously; a target as big as the viewport degrades to the
    // plain minimal reveal.
    Some(Always(by:)) -> {
      let clearance =
        int.min(int.max(0, by), int.max(0, { viewport - size } / 2))
      nearest_edge_delta(
        start,
        end,
        scroll + clearance,
        scroll + viewport - clearance,
      )
    }
    Some(OnScroll(by:)) -> {
      let delta = nearest_edge_delta(start, end, scroll, scroll + viewport)
      case delta {
        // OnScroll gate: no scroll on this axis → no padding. An
        // already-visible target is never touched, so reveals stay
        // jitter-free and idempotent.
        0 -> 0
        // Extend the minimal delta away from the landing edge. The cap
        // guarantees the target remains fully in view at the opposite edge.
        _ -> {
          let clearance = int.min(int.max(0, by), int.max(0, viewport - size))
          case delta < 0 {
            True -> delta - clearance
            False -> delta + clearance
          }
        }
      }
    }
    None -> nearest_edge_delta(start, end, scroll, scroll + viewport)
  }
}

// FFI -------------------------------------------------------------------------

@target(javascript)
@external(javascript, "../opentui.ffi.ts", "get_renderer")
fn do_get_renderer() -> Renderer

@target(javascript)
@external(javascript, "./effect.ffi.ts", "with_renderer")
fn do_with_renderer(
  handler: fn(fn(msg) -> Nil, Renderer) -> Nil,
  dispatch: fn(msg) -> Nil,
) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "fresh_rect")
fn do_fresh_rect(renderer: Renderer, id: String) -> Result(Rect, Nil)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "fresh_extents")
fn do_fresh_extents(
  renderer: Renderer,
  id: String,
) -> Result(ScrollExtents, Nil)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "subscribe_keyboard_raw")
fn do_subscribe_keyboard_raw(callback: fn(Dynamic) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "root")
fn do_root(renderer: Renderer) -> Node

@target(javascript)
@external(javascript, "./effect.ffi.ts", "children")
fn do_children(node: Node) -> List(Node)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "is_portal")
fn do_is_portal(node: Node) -> Bool

@target(javascript)
@external(javascript, "./effect.ffi.ts", "is_destroyed")
fn do_is_destroyed(node: Node) -> Bool

@target(javascript)
@external(javascript, "./effect.ffi.ts", "is_focusable")
fn do_is_focusable(node: Node) -> Bool

@target(javascript)
@external(javascript, "./effect.ffi.ts", "is_focused")
fn do_is_focused(node: Node) -> Bool

@target(javascript)
@external(javascript, "./effect.ffi.ts", "node_id")
fn do_node_id(node: Node) -> String

@target(javascript)
@external(javascript, "./effect.ffi.ts", "focus_node")
fn do_focus_node(node: Node) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "to_dynamic")
fn do_to_dynamic(node: Node) -> Dynamic

@target(javascript)
@external(javascript, "./effect.ffi.ts", "find_node")
fn do_find_node(renderer: Renderer, id: String) -> Result(Node, Nil)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "parent_node")
fn do_parent(node: Node) -> Result(Node, Nil)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "same_node")
fn do_same_node(a: Node, b: Node) -> Bool

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
@external(javascript, "./effect.ffi.ts", "get_selection_raw")
fn do_get_selection_raw(renderer: Renderer) -> Result(Dynamic, Nil)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "subscribe_selection_raw")
fn do_subscribe_selection_raw(callback: fn(Dynamic) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "selected_renderables")
fn do_selected_renderables(selection: Dynamic) -> List(Node)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "selection_anchor")
fn do_selection_anchor(selection: Dynamic) -> #(Int, Int)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "selection_focus")
fn do_selection_focus(selection: Dynamic) -> #(Int, Int)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "node_selection_range")
fn do_node_selection_range(node: Node) -> Result(#(Int, Int), Nil)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "current_focused")
fn do_current_focused(renderer: Renderer) -> Result(Node, Nil)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "is_edit_buffer")
fn do_is_edit_buffer(node: Node) -> Bool

@target(javascript)
@external(javascript, "./effect.ffi.ts", "is_selectable")
fn do_is_selectable(node: Node) -> Bool

@target(javascript)
@external(javascript, "./effect.ffi.ts", "measure_visual_cursor")
fn do_measure_visual_cursor(node: Node, offset: Int) -> #(Int, Int)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "start_selection")
fn do_start_selection(renderer: Renderer, node: Node, x: Int, y: Int) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "update_selection")
fn do_update_selection(renderer: Renderer, node: Node, x: Int, y: Int) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "clear_selection")
fn do_clear_selection(dispatch: fn(msg) -> Nil) -> Nil

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
@external(javascript, "./effect.ffi.ts", "scroll_content")
fn do_scroll_content(node: Node) -> Node

@target(javascript)
@external(javascript, "./effect.ffi.ts", "node_rect")
fn do_node_rect(node: Node) -> Rect

@target(javascript)
@external(javascript, "./effect.ffi.ts", "set_scroll_position")
fn do_set_scroll(node: Node, x: Int, y: Int) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "scroll_node_by")
fn do_scroll_rel(node: Node, delta_x: Int, delta_y: Int) -> Nil

@target(javascript)
@external(javascript, "./effect.ffi.ts", "visual_cursor")
fn do_visual_cursor(node: Node) -> #(Int, Int)

@target(javascript)
@external(javascript, "./effect.ffi.ts", "wrap_mode")
fn do_wrap_mode(node: Node) -> String
