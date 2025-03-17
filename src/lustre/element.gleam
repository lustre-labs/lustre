//// Lustre wouldn't be much use as a frontend framework if it didn't provide a
//// way to create HTML elements. This module contains the basic functions
//// necessary to construct and manipulate different HTML elements.
////
//// It is also possible to use Lustre as a HTML templating library, without
//// using its runtime or framework features.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/option.{None, Some}
import gleam/string
import gleam/string_tree.{type StringTree}
import lustre/attribute.{type Attribute} as _
import lustre/internals/constants
import lustre/internals/mutable_map
import lustre/vdom/attribute
import lustre/vdom/node.{Element, Fragment, Text, UnsafeInnerHtml}

// TYPES -----------------------------------------------------------------------

/// The `Element` type is how Lustre represents chunks of HTML. The `msg` type
/// variable is used to represent the types of messages that can be produced from
/// events on the element or its children.
///
/// **Note**: Just because an element _can_ produces messages of a given type,
/// doesn't mean that it _will_! The `msg` type variable is used to represent the
/// potential for messages to be produced, not a guarantee.
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
/// If you have more complex needs, there are two more-advanced functions:
///
/// - The [`namespaced`](#namespaced) function to create elements in a specific
///   XML namespace. This is useful for SVG or MathML elements, for example.
///
/// - The [`advanced`](#advanced) function to create elements with more control
///   over how the element is rendered when converted to a string. This is
///   necessary because some HTML, SVG, and MathML elements are self-closing or
///   void elements, and Lustre needs to know how to render them correctly!
///
/// For most applications, you'll only need to use the simpler functions; usually
/// the [`text`](#text) and [`none`](#none) functions are enough. This is because
/// Lustre already provides a module with all the standard HTML and SVG elements
/// ready to use in [`lustre/element/html`](./element/html.html) and
/// [`lustre/element/svg`](./element/svg.html).
///
pub type Element(msg) =
  node.Node(msg)

// CONSTRUCTORS ----------------------------------------------------------------

/// A general function for constructing any kind of element. In most cases you
/// will want to use the [`lustre/element/html`](./element/html.html) instead but this
/// function is particularly handy when constructing custom elements, either
/// from your own Lustre components or from external JavaScript libraries.
///
/// **Note**: Because Lustre is primarily used to create HTML, this function
/// special-cases the following tags which render as
/// [void elements](https://developer.mozilla.org/en-US/docs/Glossary/Void_element):
///
///   - area
///   - base
///   - br
///   - col
///   - embed
///   - hr
///   - img
///   - input
///   - link
///   - meta
///   - param
///   - source
///   - track
///   - wbr
///
///  This will only affect the output of `to_string` and `to_string_builder`!
///  If you need to render any of these tags with children, *or* you want to
///  render some other tag as self-closing or void, use [`advanced`](#advanced)
///  to construct the element instead.
///
pub fn element(
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  case tag {
    "area"
    | "base"
    | "br"
    | "col"
    | "embed"
    | "hr"
    | "img"
    | "input"
    | "link"
    | "meta"
    | "param"
    | "source"
    | "track"
    | "wbr" ->
      node.element(
        key: "",
        mapper: constants.option_none,
        namespace: "",
        tag: tag,
        attributes: attribute.prepare(attributes),
        children: constants.empty_list,
        keyed_children: mutable_map.new(),
        self_closing: False,
        void: True,
      )

    _ ->
      node.element(
        key: "",
        mapper: constants.option_none,
        namespace: "",
        tag: tag,
        attributes: attribute.prepare(attributes),
        children:,
        keyed_children: mutable_map.new(),
        self_closing: False,
        void: False,
      )
  }
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
  node.element(
    key: "",
    mapper: constants.option_none,
    namespace:,
    tag:,
    attributes: attribute.prepare(attributes),
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
  node.element(
    key: "",
    mapper: constants.option_none,
    namespace:,
    tag:,
    attributes: attribute.prepare(attributes),
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
  node.text(key: "", mapper: constants.option_none, content:)
}

/// A function for rendering nothing. This is mostly useful for conditional
/// rendering, where you might want to render something only if a certain
/// condition is met.
///
pub fn none() -> Element(msg) {
  node.text(key: "", mapper: constants.option_none, content: "")
}

/// A function for wrapping elements to be rendered within a parent container without
/// specififying the container on definition. Allows the treatment of List(Element(msg))
/// as if it were Element(msg). Useful when generating a list of elements from data but
/// used downstream.
///
pub fn fragment(children: List(Element(msg))) -> Element(msg) {
  node.fragment(
    key: "",
    mapper: constants.option_none,
    children:,
    keyed_children: mutable_map.new(),
    children_count: count_fragment_children(children, 0),
  )
}

fn count_fragment_children(children: List(Element(msg)), count: Int) -> Int {
  case children {
    [] -> count

    [Fragment(children_count:, ..), ..rest] ->
      count_fragment_children(rest, count + children_count)

    [_, ..rest] -> count_fragment_children(rest, count + 1)
  }
}

/// A function for constructing a wrapper element with custom raw HTML as its
/// content. Lustre will render the provided HTML verbatim, and will not touch
/// its children except when replacing the entire inner html on changes.
///
/// **Warning:** The provided HTML will not be escaped and may therefore break
/// rendering and be a XSS attack vector! Make sure you absolutely trust the
/// HTML you pass to this function. In particular, never use this to display
/// un-sanitised user HTML!
///
pub fn unsafe_inner_html(
  namespace: String,
  tag: String,
  attributes: List(Attribute(msg)),
  inner_html: String,
) -> Element(msg) {
  node.unsafe_inner_html(
    key: "",
    namespace:,
    tag:,
    mapper: constants.option_none,
    attributes: attribute.prepare(attributes),
    inner_html:,
  )
}

///
// MANIPULATIONS ---------------------------------------------------------------

/// The `Element` type is parameterised by the type of messages it can produce
/// from events. Sometimes you might end up with a fragment of HTML from another
/// library or module that produces a different type of message: this function lets
/// you map the messages produced from one type to another.
///
/// Think of it like `list.map` or `result.map` but for HTML events!
///
pub fn map(element: Element(a), f: fn(a) -> b) -> Element(b) {
  let mapper = case element.mapper {
    Some(m) -> Some(fn(msg) { msg |> m |> coerce(f) })
    None -> Some(coerce(f))
  }

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
  node.to_string(element)
}

/// Converts an element to a string like [`to_string`](#to_string), but prepends
/// a `<!doctype html>` declaration to the string. This is useful for rendering
/// complete HTML documents.
///
/// If the provided element is not an `html` element, it will be wrapped in both
/// a `html` and `body` element.
///
pub fn to_document_string(el: Element(msg)) -> String {
  node.to_string(case el {
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
  node.to_string_tree(element)
}

/// Converts an element to a `StringTree` like [`to_string_builder`](#to_string_builder),
/// but prepends a `<!doctype html>` declaration. This is useful for rendering
/// complete HTML documents.
///
/// If the provided element is not an `html` element, it will be wrapped in both
/// a `html` and `body` element.
///
pub fn to_document_string_tree(el: Element(msg)) -> StringTree {
  node.to_string_tree(case el {
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
  node.to_snapshot(el)
}
