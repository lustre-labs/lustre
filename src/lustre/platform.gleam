//// This module contains the platform abstraction used by Lustre's reconciler.
//// A platform configuration provides the low-level mutation methods needed to
//// create, modify, and remove nodes in a render target.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/result
import lustre/element.{type Element}

// TYPES -----------------------------------------------------------------------

/// A type representing a DOM node in JavaScript.
///
pub type DomNode

/// A type representing a DOM event in JavaScript.
///
pub type DomEvent

/// An error type returned by platform operations.
///
pub type PlatformError {
  NotABrowser
  ElementNotFound(selector: String)
  NotMountable
}

/// A platform configuration provides the low-level mutation methods needed to
/// create, modify, and remove nodes in a render target. Lustre's reconciler
/// calls these methods instead of hardcoded DOM APIs, allowing applications to
/// render to arbitrary targets beyond the browser DOM.
///
/// Use [`dom`](#dom) to get a platform configuration for the browser DOM.
/// Use [`headless`](#headless) for server components that don't render to a DOM.
/// Custom targets can provide their own implementations via [`new`](#new).
///
pub opaque type Platform(node, target, value, event, msg) {
  Headless
  Platform(
    target: target,
    mount: fn(target) -> #(node, Element(msg)),
    create_element: fn(String, String) -> node,
    create_text_node: fn(String) -> node,
    create_fragment: fn() -> node,
    create_comment: fn(String) -> node,
    insert_before: fn(node, node, Result(node, Nil)) -> Nil,
    move_before: fn(node, node, Result(node, Nil)) -> Nil,
    remove_child: fn(node, node) -> Nil,
    get_attribute: fn(node, String) -> Result(String, Nil),
    set_attribute: fn(node, String, String) -> Nil,
    remove_attribute: fn(node, String) -> Nil,
    set_property: fn(node, String, value) -> Nil,
    set_text: fn(node, String) -> Nil,
    set_inner_html: fn(node, String) -> Nil,
    add_event_listener: fn(node, String, fn(event) -> Nil, Bool) -> Nil,
    remove_event_listener: fn(node, String, fn(event) -> Nil) -> Nil,
    schedule_render: fn(fn() -> Nil) -> fn() -> Nil,
    after_render: fn() -> Nil,
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Returns a [`Platform`](#Platform) configured for the browser DOM. This is the
/// standard platform used by client-side Lustre applications.
///
/// The `target` argument is a CSS selector used to locate the DOM element where
/// the application will be mounted. The selector is resolved at construction
/// time, and the resolved DOM node is stored as the platform's target.
///
/// On Erlang this always returns `Error(NotABrowser)`. On JavaScript this
/// succeeds when running in a browser environment and the selector matches an
/// element.
///
pub fn dom(
  onto target: String,
) -> Result(Platform(DomNode, DomNode, DomNode, DomEvent, msg), PlatformError) {
  use <- bool.guard(!is_browser(), Error(NotABrowser))
  use root <- result.try(do_dom_query_selector(target))

  Ok(dom_strict(root))
}

/// Returns a [`Platform`](#Platform) configured for the browser DOM, using a
/// known-good DOM node as the root. Unlike [`dom`](#dom), this function does
/// not perform a selector query and cannot fail.
///
/// This is used internally for web components (where the shadow root is already
/// available) and can be used by advanced users who already have a reference to
/// a DOM node.
///
@internal
pub fn dom_strict(
  onto root: DomNode,
) -> Platform(DomNode, DomNode, DomNode, DomEvent, msg) {
  new(
    target: root,
    mount: do_dom_mount_strict,
    create_element: do_dom_create_element,
    create_text_node: do_dom_create_text_node,
    create_fragment: do_dom_create_fragment,
    create_comment: do_dom_create_comment,
    insert_before: do_dom_insert_before,
    move_before: do_dom_move_before,
    remove_child: do_dom_remove_child,
    get_attribute: do_dom_get_attribute,
    set_attribute: do_dom_set_attribute,
    remove_attribute: do_dom_remove_attribute,
    set_property: do_dom_set_property,
    set_text: do_dom_set_text,
    set_inner_html: do_dom_set_inner_html,
    add_event_listener: do_dom_add_event_listener,
    remove_event_listener: do_dom_remove_event_listener,
    schedule_render: do_dom_schedule_render,
    after_render: do_dom_after_render,
  )
}

/// Returns a headless [`Platform`](#Platform) for server components. Server
/// components don't render to a DOM target — they send patches over the network
/// to connected clients instead.
///
pub fn headless() -> Platform(node, target, value, event, msg) {
  Headless
}

/// Construct a custom [`Platform`](#Platform) with user-provided mutation
/// methods. This is useful for rendering to non-DOM targets.
///
/// Non-DOM targets can no-op `create_comment`, `create_fragment`,
/// `set_inner_html`, `add_event_listener`, and `remove_event_listener` and
/// provide minimal implementations for the rest.
///
pub fn new(
  target target: target,
  mount mount: fn(target) -> #(node, Element(msg)),
  create_element create_element: fn(String, String) -> node,
  create_text_node create_text_node: fn(String) -> node,
  create_fragment create_fragment: fn() -> node,
  create_comment create_comment: fn(String) -> node,
  insert_before insert_before: fn(node, node, Result(node, Nil)) -> Nil,
  move_before move_before: fn(node, node, Result(node, Nil)) -> Nil,
  remove_child remove_child: fn(node, node) -> Nil,
  get_attribute get_attribute: fn(node, String) -> Result(String, Nil),
  set_attribute set_attribute: fn(node, String, String) -> Nil,
  remove_attribute remove_attribute: fn(node, String) -> Nil,
  set_property set_property: fn(node, String, value) -> Nil,
  set_text set_text: fn(node, String) -> Nil,
  set_inner_html set_inner_html: fn(node, String) -> Nil,
  add_event_listener add_event_listener: fn(
    node,
    String,
    fn(event) -> Nil,
    Bool,
  ) ->
    Nil,
  remove_event_listener remove_event_listener: fn(
    node,
    String,
    fn(event) -> Nil,
  ) ->
    Nil,
  schedule_render schedule_render: fn(fn() -> Nil) -> fn() -> Nil,
  after_render after_render: fn() -> Nil,
) -> Platform(node, target, value, event, msg) {
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
    get_attribute:,
    set_attribute:,
    remove_attribute:,
    set_property:,
    set_text:,
    set_inner_html:,
    add_event_listener:,
    remove_event_listener:,
    schedule_render:,
    after_render:,
  )
}

// INTERNAL ACCESSORS ----------------------------------------------------------

/// Mount the platform and return the root node and initial virtual DOM.
///
@internal
pub fn mount(
  platform: Platform(node, target, value, event, msg),
) -> Result(#(node, Element(msg)), PlatformError) {
  case platform {
    Headless -> Error(NotMountable)
    Platform(target: t, mount: m, ..) -> Ok(m(t))
  }
}

/// Check whether a platform is headless (i.e. for server components).
///
@internal
pub fn is_headless(platform: Platform(node, target, value, event, msg)) -> Bool {
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
@external(javascript, "./runtime/client/runtime.ffi.mjs", "is_browser")
pub fn is_browser() -> Bool {
  False
}

// DOM FFI FUNCTIONS -----------------------------------------------------------

fn do_dom_query_selector(selector: String) -> Result(DomNode, PlatformError) {
  case do_dom_query_selector_raw(selector) {
    Ok(node) -> Ok(node)
    Error(sel) -> Error(ElementNotFound(sel))
  }
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "query_selector")
fn do_dom_query_selector_raw(_selector: String) -> Result(DomNode, String) {
  Error("")
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "mount_strict")
fn do_dom_mount_strict(_root: DomNode) -> #(DomNode, Element(msg)) {
  panic as "Cannot mount DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "create_element")
fn do_dom_create_element(_ns: String, _tag: String) -> DomNode {
  panic as "Cannot create DOM elements on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "create_text_node")
fn do_dom_create_text_node(_content: String) -> DomNode {
  panic as "Cannot create DOM text nodes on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "create_fragment")
fn do_dom_create_fragment() -> DomNode {
  panic as "Cannot create DOM fragments on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "create_comment")
fn do_dom_create_comment(_data: String) -> DomNode {
  panic as "Cannot create DOM comments on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "insert_before")
fn do_dom_insert_before(
  _parent: DomNode,
  _node: DomNode,
  _ref: Result(DomNode, Nil),
) -> Nil {
  panic as "Cannot manipulate DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "move_before")
fn do_dom_move_before(
  _parent: DomNode,
  _node: DomNode,
  _ref: Result(DomNode, Nil),
) -> Nil {
  panic as "Cannot manipulate DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "remove_child")
fn do_dom_remove_child(_parent: DomNode, _child: DomNode) -> Nil {
  panic as "Cannot manipulate DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "get_attribute")
fn do_dom_get_attribute(_node: DomNode, _name: String) -> Result(String, Nil) {
  panic as "Cannot read DOM attributes on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "set_attribute")
fn do_dom_set_attribute(_node: DomNode, _name: String, _value: String) -> Nil {
  panic as "Cannot manipulate DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "remove_attribute")
fn do_dom_remove_attribute(_node: DomNode, _name: String) -> Nil {
  panic as "Cannot manipulate DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "set_property")
fn do_dom_set_property(_node: DomNode, _name: String, _value: DomNode) -> Nil {
  panic as "Cannot manipulate DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "set_text")
fn do_dom_set_text(_node: DomNode, _content: String) -> Nil {
  panic as "Cannot manipulate DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "set_inner_html")
fn do_dom_set_inner_html(_node: DomNode, _html: String) -> Nil {
  panic as "Cannot manipulate DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "add_event_listener")
fn do_dom_add_event_listener(
  _node: DomNode,
  _name: String,
  _handler: fn(DomEvent) -> Nil,
  _passive: Bool,
) -> Nil {
  panic as "Cannot manipulate DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "remove_event_listener")
fn do_dom_remove_event_listener(
  _node: DomNode,
  _name: String,
  _handler: fn(DomEvent) -> Nil,
) -> Nil {
  panic as "Cannot manipulate DOM on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "schedule_render")
fn do_dom_schedule_render(_callback: fn() -> Nil) -> fn() -> Nil {
  panic as "Cannot schedule DOM renders on Erlang"
}

@external(javascript, "./runtime/client/dom.ffi.mjs", "after_render")
fn do_dom_after_render() -> Nil {
  Nil
}
