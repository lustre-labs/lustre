//// Lustre wouldn't be much use as a frontend framework if it didn't provide a
//// way to create HTML elements. This module contains the basic functions
//// necessary to construct and manipulate different HTML elements.
////
//// It is also possible to use Lustre as a HTML templating library, without
//// using its runtime or framework features.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/function
import gleam/string
import gleam/string_tree.{type StringTree}
import lustre/attribute.{type Attribute}
import lustre/internals/mutable_map
import lustre/vdom/events
import lustre/vdom/vnode.{Element, Fragment, Text, UnsafeInnerHtml}

// TYPES -----------------------------------------------------------------------

/// The `Element` type is how Lustre represents chunks of HTML. The `msg` type
/// variable is used to represent the types of messages that can be produced from
/// events on the element or its children.
///
/// > **Note**: Just because an element _can_ produces messages of a given type,
/// > doesn't mean that it _will_! The `msg` type variable is used to represent the
/// > potential for messages to be produced, not a guarantee.
///
/// The most basic ways to create elements are:
///
/// - The [`element`](#element) function to construct arbitrary HTML elements.
///   You can also use this render Custom Elements (like those registered as
///   Lustre components).
///
/// - The [`text`](#text) function to turn a Gleam string into a text node.
///
/// - The [`none`](#none) function to render nothing - useful for conditional
///   rendering.
///
/// If you have more complex needs, there are two more-advanced ways to construct
/// HTML elements:
///
/// - The [`namespaced`](#namespaced) function to create elements in a specific
///   XML namespace. This is useful for SVG or MathML elements, for example.
///
/// - The [`advanced`](#advanced) function to create elements with more control
///   over how the element is rendered when converted to a string. This is
///   necessary because some HTML, SVG, and MathML elements are self-closing or
///   void elements, and Lustre needs to know how to render them correctly!
///
/// Finally, for other niche use cases there are two additional functions:
///
/// - The [`fragment`](#fragment) function lets you wrap a list of `Element`s up
///   as a single `Element`, making it useful to avoid wrapping elements in a
///   `<div/>` or other container when you don't want to.
///
/// - The [`unsafe_raw_html`](#unsafe_raw_html) function lets you render raw HTML
///   directly into an element. This function is primarily useful in cases where
///   you have _pre-sanitised_ HTML or are working with libraries outside of Lustre
///   that produce plain HTML strings.
///
///   Lustre will _not_ escape the HTML string provided to this functio, meaning
///   inappropriate use can expose your application to XSS attacks. Make sure you
///   never take untrusted user input and pass it to this function!
///
pub type Element(msg) =
  vnode.Element(msg)

// CONSTRUCTORS ----------------------------------------------------------------

/// A general function for constructing any kind of element. In most cases you
/// will want to use the [`lustre/element/html`](./element/html.html) instead but this
/// function is particularly handy when constructing custom elements, either
/// from your own Lustre components or from external JavaScript libraries.
///
/// > **Note**: Because Lustre is primarily used to create HTML, this function
/// > special-cases the following tags which render as
/// > [void elements](https://developer.mozilla.org/en-US/docs/Glossary/Void_element):
/// >
/// >   - area
/// >   - base
/// >   - br
/// >   - col
/// >   - embed
/// >   - hr
/// >   - img
/// >   - input
/// >   - link
/// >   - meta
/// >   - param
/// >   - source
/// >   - track
/// >   - wbr
/// >
/// > This will only affect the output of `to_string` and `to_string_builder`!
/// > If you need to render any of these tags with children, *or* you want to
/// > render some other tag as self-closing or void, use [`advanced`](#advanced)
/// > to construct the element instead.
///
pub fn element(
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  vnode.element(
    key: "",
    mapper: function.identity,
    namespace: "",
    tag: tag,
    attributes:,
    children: children,
    keyed_children: mutable_map.new(),
    self_closing: False,
    void: False,
  )
}

/// A function for constructing elements in a specific XML namespace. This can
/// be used to construct SVG or MathML elements, for example.
///
pub fn namespaced(
  namespace: String,
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  vnode.element(
    key: "",
    mapper: function.identity,
    namespace:,
    tag:,
    attributes:,
    children:,
    keyed_children: mutable_map.new(),
    self_closing: False,
    void: False,
  )
}

/// A function for constructing elements with more control over how the element
/// is rendered when converted to a string. This is necessary because some HTML,
/// SVG, and MathML elements are self-closing or void elements, and Lustre needs
/// to know how to render them correctly!
///
pub fn advanced(
  namespace: String,
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
  self_closing: Bool,
  void: Bool,
) -> Element(msg) {
  vnode.element(
    key: "",
    mapper: function.identity,
    namespace:,
    tag:,
    attributes:,
    children:,
    keyed_children: mutable_map.new(),
    self_closing:,
    void:,
  )
}

/// A function for turning a Gleam string into a text node. Gleam doesn't have
/// union types like some other languages you may be familiar with, like TypeScript.
/// Instead, we need a way to take a `String` and turn it into an `Element` somehow:
/// this function is exactly that!
///
pub fn text(content: String) -> Element(msg) {
  vnode.text(key: "", mapper: function.identity, content:)
}

/// A function for rendering nothing. This is mostly useful for conditional
/// rendering, where you might want to render something only if a certain
/// condition is met.
///
pub fn none() -> Element(msg) {
  vnode.text(key: "", mapper: function.identity, content: "")
}

/// A function for constructing a wrapper element with no tag name. This is
/// useful for wrapping a list of elements together without adding an extra
/// `<div>` or other container element, or returning multiple elements in places
/// where only one `Element` is expected.
///
pub fn fragment(children: List(Element(msg))) -> Element(msg) {
  vnode.fragment(
    key: "",
    mapper: function.identity,
    children:,
    keyed_children: mutable_map.new(),
    children_count: count_fragment_children(children, 0),
  )
}

fn count_fragment_children(children: List(Element(msg)), count: Int) -> Int {
  case children {
    [child, ..rest] ->
      count_fragment_children(rest, count + vnode.advance(child))

    [] -> count
  }
}

/// A function for constructing a wrapper element with custom raw HTML as its
/// content. Lustre will render the provided HTML verbatim, and will not touch
/// its children except when replacing the entire inner html on changes.
///
/// > **Note:** The provided HTML will not be escaped automatically and may expose
/// > your applications to XSS attacks! Make sure you absolutely trust the HTML you
/// > pass to this function. In particular, never use this to display un-sanitised
/// > user HTML!
///
pub fn unsafe_raw_html(
  namespace: String,
  tag: String,
  attributes: List(Attribute(msg)),
  inner_html: String,
) -> Element(msg) {
  vnode.unsafe_inner_html(
    key: "",
    namespace:,
    tag:,
    mapper: function.identity,
    attributes:,
    inner_html:,
  )
}

// MANIPULATIONS ---------------------------------------------------------------

/// The `Element` type is parameterised by the type of messages it can produce
/// from events. Sometimes you might end up with a fragment of HTML from another
/// library or module that produces a different type of message: this function lets
/// you map the messages produced from one type to another.
///
/// Think of it like `list.map` or `result.map` but for HTML events!
///
pub fn map(element: Element(a), f: fn(a) -> b) -> Element(b) {
  let mapper = coerce(events.compose_mapper(coerce(f), element.mapper))

  case element {
    Fragment(children:, keyed_children:, ..) ->
      Fragment(
        ..element,
        mapper:,
        children: coerce(children),
        keyed_children: coerce(keyed_children),
      )

    Element(attributes:, children:, keyed_children:, ..) ->
      Element(
        ..element,
        mapper:,
        attributes: coerce(attributes),
        children: coerce(children),
        keyed_children: coerce(keyed_children),
      )

    UnsafeInnerHtml(attributes:, ..) ->
      UnsafeInnerHtml(..element, mapper:, attributes: coerce(attributes))

    Text(..) -> coerce(element)
  }
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b

// CONVERSIONS -----------------------------------------------------------------

/// Convert a Lustre `Element` to a string. This is _not_ pretty-printed, so
/// there are no newlines or indentation. If you need to pretty-print an element,
/// reach out on the [Gleam Discord](https://discord.gg/Fm8Pwmy) or
/// [open an issue](https://github.com/lustre-labs/lustre/issues/new) with your
/// use case and we'll see what we can do!
///
pub fn to_string(element: Element(msg)) -> String {
  vnode.to_string(element)
}

/// Converts an element to a string like [`to_string`](#to_string), but prepends
/// a `<!doctype html>` declaration to the string. This is useful for rendering
/// complete HTML documents.
///
/// If the provided element is not an `html` element, it will be wrapped in both
/// a `html` and `body` element.
///
pub fn to_document_string(el: Element(msg)) -> String {
  vnode.to_string(case el {
    Element(tag: "html", ..) -> el
    Element(tag: "head", ..) | Element(tag: "body", ..) ->
      element("html", [], [el])
    _ -> element("html", [], [element("body", [], [el])])
  })
  |> string.append("<!doctype html>\n", _)
}

/// Convert a Lustre `Element` to a `StringTree`. This is _not_ pretty-printed,
/// so there are no newlines or indentation. If you need to pretty-print an element,
/// reach out on the [Gleam Discord](https://discord.gg/Fm8Pwmy) or
/// [open an issue](https://github.com/lustre-labs/lustre/issues/new) with your
/// use case and we'll see what we can do!
///
pub fn to_string_tree(element: Element(msg)) -> StringTree {
  vnode.to_string_tree(element)
}

/// Converts an element to a `StringTree` like [`to_string_builder`](#to_string_builder),
/// but prepends a `<!doctype html>` declaration. This is useful for rendering
/// complete HTML documents.
///
/// If the provided element is not an `html` element, it will be wrapped in both
/// a `html` and `body` element.
///
pub fn to_document_string_tree(el: Element(msg)) -> StringTree {
  vnode.to_string_tree(case el {
    Element(tag: "html", ..) -> el
    Element(tag: "head", ..) | Element(tag: "body", ..) ->
      element("html", [], [el])
    _ -> element("html", [], [element("body", [], [el])])
  })
  |> string_tree.prepend("<!doctype html>\n")
}

/// Converts a Lustre `Element` to a human-readable string by inserting new lines
/// and indentation where appropriate. This is useful for debugging and testing,
/// but for production code you should use [`to_string`](#to_string) or
/// [`to_document_string`](#to_document_string) instead.
///
/// 💡 This function works great with the snapshot testing library
///    [birdie](https://hexdocs.pm/birdie)!
///
/// ## Using `to_string`:
///
/// ```html
/// <header><h1>Hello, world!</h1></header>
/// ```
///
/// ## Using `to_readable_string`
///
/// ```html
/// <header>
///   <h1>
///     Hello, world!
///   </h1>
/// </header>
/// ```
///
pub fn to_readable_string(el: Element(msg)) -> String {
  vnode.to_snapshot(el)
}
