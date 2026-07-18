//// This module contains the platform abstraction used by Lustre's reconciler.
//// A platform configuration provides the low-level mutation methods needed to
//// create, modify, and remove nodes in a render target.
////

// IMPORTS ---------------------------------------------------------------------

import agnostic/element.{type Element}

// TYPES -----------------------------------------------------------------------

/// An error type returned by platform operations.
///
pub type PlatformError {
  NotABrowser
  ElementNotFound(selector: String)
  NotMountable
}

/// A deferred effect phase declared by a platform. `name` identifies which
/// tagged tasks the phase drains: effects constructed with
/// [`effect.deferred`](./effect.html#deferred) — or a platform module's typed
/// constructor built on it — carry a phase name, and at the end of each render
/// pass the runtime hands the pending tasks of every declared phase to that
/// phase's `schedule` function. Phases are drained in the platform's
/// declaration order; the relative timing between phases is then determined by
/// the schedulers themselves.
///
/// Tasks tagged with a phase name the running platform does not declare are
/// silently dropped.
///
/// > **Note**: `schedule` must defer — it must never invoke its callback
/// > synchronously. The runtime's render bookkeeping assumes control returns
/// > to it before any scheduled callback runs.
///
pub type Phase {
  Phase(name: String, schedule: fn(fn() -> Nil) -> Nil)
}

/// A platform configuration provides the low-level mutation methods needed to
/// create, modify, and remove nodes in a render target. Lustre's reconciler
/// calls these methods instead of hardcoded DOM APIs, allowing applications to
/// render to arbitrary targets beyond the browser DOM.
///
/// Use [`dom.dom()`](platform/dom.html#dom) from `agnostic/platform/dom` for the browser DOM.
/// Use [`headless`](#headless) for server components that don't render to a DOM.
/// Custom targets can provide their own implementations via [`new`](#new).
///
pub opaque type Platform(node, target, value, event, message, raw) {
  Headless
  Platform(
    target: target,
    mount: fn(target) -> #(node, Element(message)),
    create_element: fn(String, String) -> node,
    create_text_node: fn(String) -> node,
    create_fragment: fn() -> node,
    create_comment: fn(String) -> node,
    insert_before: fn(node, node, Result(node, Nil)) -> Nil,
    move_before: fn(node, node, Result(node, Nil)) -> Nil,
    remove_child: fn(node, node) -> Nil,
    next_sibling: fn(node) -> Result(node, Nil),
    get_attribute: fn(node, String) -> Result(String, Nil),
    set_attribute: fn(node, String, String) -> Nil,
    remove_attribute: fn(node, String) -> Nil,
    set_property: fn(node, String, value) -> Nil,
    set_text: fn(node, String) -> Nil,
    set_raw_content: fn(node, raw) -> Nil,
    create_raw_node: fn(raw) -> node,
    add_event_listener: fn(node, String, fn(event) -> Nil, Bool) -> Nil,
    remove_event_listener: fn(node, String, fn(event) -> Nil) -> Nil,
    schedule_render: fn(fn() -> Nil) -> fn() -> Nil,
    after_render: fn() -> Nil,
    phases: List(Phase),
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Returns a headless [`Platform`](#Platform) for server components. Server
/// components don't render to a DOM target — they send patches over the network
/// to connected clients instead.
///
/// Headless platforms declare no deferred phases: effects deferred to a
/// platform phase (such as `dom.before_paint` or `dom.after_paint`) are
/// dropped and never run.
///
/// ## Example
///
/// ```gleam
/// import agnostic/platform
/// import agnostic/platform/dom
///
/// let p = platform.headless()
/// ```
///
pub fn headless() -> Platform(node, target, value, event, message, raw) {
  Headless
}

/// Construct a custom [`Platform`](#Platform) with user-provided mutation
/// methods. This is useful for rendering to non-DOM targets.
///
/// Non-DOM targets can no-op `create_comment`, `create_fragment`,
/// `set_raw_content`, `create_raw_node`, `add_event_listener`, and
/// `remove_event_listener` and provide minimal implementations for the rest.
///
/// `phases` declares the platform's deferred effect [`Phase`](#Phase)s in
/// drain order. Pass an empty list for no deferred phases — deferred effects
/// are then dropped. To run effects constructed by another platform module's
/// typed constructors, declare a phase whose name matches that platform's
/// public phase-name constant (for example
/// [`dom.before_paint_phase`](./platform/dom.html#before_paint_phase)).
///
/// > **Note**: `schedule_render` and every `Phase`'s `schedule` function must
/// > defer — they must never invoke their callback synchronously. The
/// > runtime's render bookkeeping assumes control returns to it first.
///
pub fn new(
  target target: target,
  mount mount: fn(target) -> #(node, Element(message)),
  create_element create_element: fn(String, String) -> node,
  create_text_node create_text_node: fn(String) -> node,
  create_fragment create_fragment: fn() -> node,
  create_comment create_comment: fn(String) -> node,
  insert_before insert_before: fn(node, node, Result(node, Nil)) -> Nil,
  move_before move_before: fn(node, node, Result(node, Nil)) -> Nil,
  remove_child remove_child: fn(node, node) -> Nil,
  next_sibling next_sibling: fn(node) -> Result(node, Nil),
  get_attribute get_attribute: fn(node, String) -> Result(String, Nil),
  set_attribute set_attribute: fn(node, String, String) -> Nil,
  remove_attribute remove_attribute: fn(node, String) -> Nil,
  set_property set_property: fn(node, String, value) -> Nil,
  set_text set_text: fn(node, String) -> Nil,
  set_raw_content set_raw_content: fn(node, raw) -> Nil,
  create_raw_node create_raw_node: fn(raw) -> node,
  add_event_listener add_event_listener: fn(
    node,
    String,
    fn(event) -> Nil,
    Bool,
  ) -> Nil,
  remove_event_listener remove_event_listener: fn(
    node,
    String,
    fn(event) -> Nil,
  ) -> Nil,
  schedule_render schedule_render: fn(fn() -> Nil) -> fn() -> Nil,
  after_render after_render: fn() -> Nil,
  phases phases: List(Phase),
) -> Platform(node, target, value, event, message, raw) {
  Platform(
    target:,
    mount:,
    create_element:,
    create_text_node:,
    create_fragment:,
    create_comment:,
    insert_before:,
    move_before:,
    remove_child:,
    next_sibling:,
    get_attribute:,
    set_attribute:,
    remove_attribute:,
    set_property:,
    set_text:,
    set_raw_content:,
    create_raw_node:,
    add_event_listener:,
    remove_event_listener:,
    schedule_render:,
    after_render:,
    phases:,
  )
}

// INTERNAL ACCESSORS ----------------------------------------------------------

/// Mount the platform and return the root node and initial virtual DOM.
///
@internal
pub fn mount(
  platform: Platform(node, target, value, event, message, raw),
) -> Result(#(node, Element(message)), PlatformError) {
  case platform {
    Headless -> Error(NotMountable)
    Platform(target: t, mount: m, ..) -> Ok(m(t))
  }
}

/// Check whether a platform is headless (i.e. for server components).
///
@internal
pub fn is_headless(
  platform: Platform(node, target, value, event, message, raw),
) -> Bool {
  case platform {
    Headless -> True
    Platform(..) -> False
  }
}

/// Gleam's conditional compilation makes it possible to have different implementations
/// of a function for different targets, but it's not possible to know what runtime
/// you're targeting at compile-time.
///
/// This is problematic if you're using server components with a JavaScript
/// backend because you'll want to know whether you're currently running on your
/// server or in the browser: this function tells you that!
///
@external(javascript, "./runtime/platform/base.ffi.mjs", "is_browser")
pub fn is_browser() -> Bool {
  False
}
