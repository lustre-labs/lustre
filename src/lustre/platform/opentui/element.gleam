//// TUI element constructors for OpenTUI. These map to @opentui/core renderables.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/json.{type Json}
import gleam/option.{Some}
import lustre/attribute.{type Attribute}
import lustre/element.{type Element}
import lustre/platform/opentui

// TYPES -----------------------------------------------------------------------

/// A factory function that receives the OpenTUI renderer and returns a raw node.
/// Cleanup is handled by overriding `destroySelf()` on your renderable class —
/// OpenTUI calls this automatically when the node is removed from the tree.
///
/// In JavaScript FFI, this is a function of type:
/// `(renderer) => RenderableNode`
pub type RawNodeFactory =
  fn(opentui.Renderer) -> opentui.Node

// ELEMENTS --------------------------------------------------------------------

/// A box container — the primary layout primitive in TUI. Maps to BoxRenderable.
///
pub fn box(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("box", attributes, children)
}

/// A text element — displays styled text. Maps to TextRenderable.
///
pub fn text(content: String) -> Element(msg) {
  element.text(content)
}

/// A styled text element with attributes. Maps to TextRenderable.
///
pub fn text_node(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("text", attributes, children)
}

/// A text input field. Maps to InputRenderable.
///
pub fn input(attributes: List(Attribute(msg))) -> Element(msg) {
  element.element("input", attributes, [])
}

/// A multi-line text area. Maps to TextareaRenderable.
///
pub fn textarea(attributes: List(Attribute(msg))) -> Element(msg) {
  element.element("textarea", attributes, [])
}

/// A scrollable container. Maps to ScrollBoxRenderable.
///
pub fn scrollbox(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("scrollbox", attributes, children)
}

/// A select dropdown. Maps to SelectRenderable.
///
pub fn select(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("select", attributes, children)
}

/// A code block with syntax highlighting. Maps to CodeRenderable.
///
pub fn code(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("code", attributes, children)
}

/// A markdown renderer. Maps to MarkdownRenderable.
///
pub fn markdown(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("markdown", attributes, children)
}

/// A diff viewer. Maps to DiffRenderable.
///
pub fn diff(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("diff", attributes, children)
}

/// An ASCII art font element. Maps to ASCIIFontRenderable.
///
pub fn ascii_font(attributes: List(Attribute(msg))) -> Element(msg) {
  element.element("asciifont", attributes, [])
}

/// A tab-based select element. Maps to TabSelectRenderable.
///
pub fn tab_select(attributes: List(Attribute(msg))) -> Element(msg) {
  element.element("tabselect", attributes, [])
}

/// A line number gutter element. Maps to LineNumberRenderable. Wrap a code or
/// textarea element as a child.
///
pub fn line_number(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("linenumber", attributes, children)
}

/// A slider element. Maps to SliderRenderable.
///
pub fn slider(attributes: List(Attribute(msg))) -> Element(msg) {
  element.element("slider", attributes, [])
}

/// A frame buffer element for direct pixel-level rendering. Maps to
/// FrameBufferRenderable.
///
pub fn frame_buffer(attributes: List(Attribute(msg))) -> Element(msg) {
  element.element("framebuffer", attributes, [])
}

/// Create a FrameBuffer element with a custom JS handler.
/// The handler receives the FrameBufferRenderable node when mounted,
/// allowing direct manipulation via node.setCell().
///
/// Example usage:
/// ```gleam
/// // spinner.ffi.mjs
/// export function make_spinner_handler(color, interval_ms) {
///   return (node) => {
///     let frameIndex = 0;
///     const fg = parseColor(color);
///     const bg = { r: 0, g: 0, b: 0, a: 0 };
///     setInterval(() => {
///       node.setCell(0, 0, ["|", "/", "-", "\\"][frameIndex], fg, bg);
///       frameIndex = (frameIndex + 1) % 4;
///     }, interval_ms);
///   };
/// }
///
/// // spinner.gleam
/// @external(javascript, "./spinner.ffi.mjs", "make_spinner_handler")
/// fn make_handler(color: String, interval_ms: Int) -> Json
///
/// pub fn spinner(handler: Json, attrs: List(Attribute(msg))) -> Element(msg) {
///   element.frame_buffer_with_handler(handler, attrs)
/// }
/// ```
pub fn frame_buffer_with_handler(
  handler: Json,
  attrs: List(Attribute(msg)),
) -> Element(msg) {
  element.element(
    "framebuffer",
    [attribute.property("__fb_handler", handler), ..attrs],
    [],
  )
}

/// A named raw node content bundle: (name, factory).
/// The name is used for diffing comparisons while the factory creates the node.
pub type RawNodeContent =
  #(String, RawNodeFactory)

/// Create a raw element using a named factory function.
/// The factory receives the OpenTUI renderer at mount time and should return
/// the created renderable node.
///
/// The `name` parameter is used for diffing: when the name changes, the old
/// node is destroyed and a new one is created from the factory. When the name
/// stays the same, the existing node persists across renders (factories are
/// closures that get recreated each render, so we can't compare them directly).
///
/// This allows creating custom OpenTUI renderables (spinners, animated widgets, etc.)
/// without needing direct access to the renderer instance.
///
/// For cleanup (clearing intervals, subscriptions, etc.), override `destroySelf()`
/// on your renderable class — OpenTUI calls this automatically when the node is
/// removed from the tree.
///
/// Example usage:
/// ```gleam
/// // spinner.ffi.mjs
/// import { TextRenderable } from "@opentui/core";
///
/// class Spinner extends TextRenderable {
///   #intervalId = null;
///   #frameIndex = 0;
///
///   constructor(renderer, color, interval_ms) {
///     super(renderer, {});
///     this.content = FRAMES[0];
///     this.fg = color;
///     this.#intervalId = setInterval(() => {
///       this.#frameIndex = (this.#frameIndex + 1) % FRAMES.length;
///       this.content = FRAMES[this.#frameIndex];
///     }, interval_ms);
///   }
///
///   destroySelf() {
///     if (this.#intervalId) clearInterval(this.#intervalId);
///     super.destroySelf();
///   }
/// }
///
/// export function spinner_factory(color, interval_ms) {
///   return (renderer) => new Spinner(renderer, color, interval_ms);
/// }
///
/// // spinner.gleam
/// @external(javascript, "./spinner.ffi.mjs", "spinner_factory")
/// fn spinner_factory(color: String, interval_ms: Int) -> RawNodeFactory
///
/// pub fn spinner(color: String) -> Element(msg) {
///   raw_node("text-spinner", "box", [], spinner_factory(color, 150))
/// }
/// ```
pub fn raw_node(
  name: String,
  tag: String,
  attrs: List(Attribute(msg)),
  factory: RawNodeFactory,
) -> Element(msg) {
  // Bundle the name and factory together. The comparator only compares names,
  // so the factory can be a fresh closure each render without triggering replacement.
  let content: RawNodeContent = #(name, factory)
  element.unsafe_raw_content(
    "",
    "",
    tag,
    attrs,
    content,
    Some(fn(a: RawNodeContent, b: RawNodeContent) { a.0 == b.0 }),
  )
}
