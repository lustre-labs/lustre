//// Lustre wouldn't be much use as a frontend framework if it didn't provide a
//// way to create HTML elements. This module contains the basic functions
//// necessary to construct and manipulate different HTML elements.
////
//// It is also possible to use Lustre as a HTML templating library, without
//// using its runtime or framework features.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/option.{type Option}
import lustre/attribute.{type Attribute}
import lustre/internals/mutable_map
import lustre/internals/ref
import lustre/vdom/vnode

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
/// If you have more complex needs, there is one additional way to construct
/// HTML elements:
///
/// - The [`namespaced`](#namespaced) function to create elements in a specific
///   XML namespace. This is useful for SVG or MathML elements, for example.
///
/// For controlling how elements are rendered to strings (void elements,
/// self-closing tags), use a custom [`SerializerConfig`](./platform/dom.html#SerializerConfig)
/// with [`dom.serialize`](./platform/dom.html#serialize).
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

/// `Ref`s are used as dependencies for memoised elements created using the
/// [`memo`](#memo) function. They wrap arbitrary Gleam values and are used by
/// Lustre to perform _reference equality_ checks to determine whether a memoised
/// element needs to be re-rendered or not.
///
pub type Ref =
  ref.Ref

// CONSTRUCTORS ----------------------------------------------------------------

/// A general function for constructing any kind of element. In most cases you
/// will want to use the [`lustre/element/html`](./element/html.html) instead but this
/// function is particularly handy when constructing custom elements, either
/// from your own Lustre components or from external JavaScript libraries.
///
/// When rendering elements to strings with [`to_string`](#to_string), the default
/// HTML serializer will render standard HTML void elements (like `<br>`, `<img>`,
/// `<input>`) without closing tags. To customize which elements are treated as
/// void or self-closing, use [`dom.serialize`](./platform/dom.html#serialize)
/// with a custom [`SerializerConfig`](./platform/dom.html#SerializerConfig).
///
pub fn element(
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  vnode.element(
    key: "",
    namespace: "",
    tag: tag,
    attributes:,
    children: children,
    keyed_children: mutable_map.new(),
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
    namespace:,
    tag:,
    attributes:,
    children:,
    keyed_children: mutable_map.new(),
  )
}

/// A function for turning a Gleam string into a text node. Gleam doesn't have
/// union types like some other languages you may be familiar with, like TypeScript.
/// Instead, we need a way to take a `String` and turn it into an `Element` somehow:
/// this function is exactly that!
///
pub fn text(content: String) -> Element(msg) {
  vnode.text(key: "", content:)
}

/// A function for rendering nothing. This is mostly useful for conditional
/// rendering, where you might want to render something only if a certain
/// condition is met.
///
pub fn none() -> Element(msg) {
  vnode.text(key: "", content: "")
}

/// A function for constructing a wrapper element with no tag name. This is
/// useful for wrapping a list of elements together without adding an extra
/// `<div>` or other container element, or returning multiple elements in places
/// where only one `Element` is expected.
///
pub fn fragment(children: List(Element(msg))) -> Element(msg) {
  vnode.fragment(key: "", children:, keyed_children: mutable_map.new())
}

/// A function for constructing a wrapper element with platform-specific raw
/// content. This is the generic version of `unsafe_raw_html` that accepts any
/// content type, allowing platforms to inject their own node types.
///
/// For DOM platforms, use `unsafe_raw_html` with a String instead.
/// For other platforms (like OpenTUI), this allows inserting raw platform nodes.
///
/// The platform's `set_raw_content` function receives this content and decides
/// how to handle it.
///
/// The optional `compare` parameter allows custom equality checking for the content.
/// When provided, the diff algorithm uses this comparator instead of `==` to
/// determine if the content has changed. This is useful for content types like
/// closures where reference equality isn't appropriate.
///
pub fn unsafe_raw_content(
  key key: String,
  namespace namespace: String,
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  content content: a,
  compare compare: Option(fn(a, a) -> Bool),
) -> Element(msg) {
  vnode.raw_container(key:, namespace:, tag:, attributes:, content:, compare:)
}

// MEMOISATION -----------------------------------------------------------------

/// A function for creating "memoised" or "lazy" elements. Lustre will use the
/// dependencies list to skip calling the provided view function if all of the
/// dependencies a _reference equal_ to their previous values.
///
/// `memo` can be used to optimise performance-critical parts of your application,
/// for example in cases where many instances of the same element are rendered but
/// only one may change at a time, or cases where a part of your view may update
/// very frequently but other parts remain largely static. When Lustre can tell
/// that the dependencies haven't changed, almost all the work typically done to
/// update the DOM can be skipped.
///
/// In many cases `memo` will not be necessary, so think twice before considering
/// its use! Lustre is designed to handle rerenders and large vdom trees efficiently,
/// so in most cases the naive approach of re-rendering everything will be perfectly
/// fine.
///
/// > **Note**: reference equality is not the same as Gleam's normal equality.
/// > Two custom types with the same values are not reference equal unless they
/// > are the exact same instance in memory! Because of this, it's important to
/// > avoid list literals or constructing custom types in the dependencies list.
///
/// > **Note**: memoisation comes with its own trade-offs and can cause performance
/// > regressions in two ways. First, every use of `memo` increases your application's
/// > memory usage slightly, as Lustre needs to keep dependencies around to compare
/// > them on subsequent renders. Second, if dependencies change regularly, the
/// > overhead of comparing dependencies and managing memoisation may be more than
/// > the naive cost of re-rendering the element each time.
///
pub fn memo(dependencies: List(Ref), view: fn() -> Element(msg)) -> Element(msg) {
  vnode.memo(key: "", dependencies:, view:)
}

/// Create a `Ref` dependency value used for [`memo`](#memo) elements.
///
/// Lustre uses reference equality to compare dependencies. On JavaScript, values
/// are compared using [same-value-zero](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Equality_comparisons_and_sameness#same-value-zero_equality)
/// semantics. This means Lustre will treat `+0` and `-0` as equal, and any errant
/// `NaN` values (which are not typically producible in Gleam code) as equal. On
/// Erlang, there is no difference between reference equality and value equality,
/// so all values are compared using normal equality semantics.
///
pub fn ref(value: a) -> Ref {
  ref.from(value)
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
  vnode.map(element, f)
}

// LUSTRE_DEV_TOOLS COMPATIBILITY SHIMS ----------------------------------------
//
// WARNING: These functions are DEPRECATED and exist only for backward
// compatibility with lustre_dev_tools. They will be removed in a future
// release. Use the functions in `lustre/platform/dom` instead:
//
//   - element.to_string        -> dom.to_string
//   - element.to_document_string -> dom.to_document_string
//   - element.to_readable_string -> dom.to_readable_string
//   - element.unsafe_raw_html  -> element.unsafe_raw_content
//

/// DEPRECATED: Use `lustre/platform/dom.to_string` instead.
/// This shim exists only for lustre_dev_tools compatibility.
///
@deprecated("Use lustre/platform/dom.to_string instead")
@external(erlang, "lustre@platform@dom", "to_string")
pub fn to_string(_el: Element(msg)) -> String {
  panic as "element.to_string is deprecated: use lustre/platform/dom.to_string"
}

/// DEPRECATED: Use `lustre/platform/dom.to_document_string` instead.
/// This shim exists only for lustre_dev_tools compatibility.
///
@deprecated("Use lustre/platform/dom.to_document_string instead")
@external(erlang, "lustre@platform@dom", "to_document_string")
pub fn to_document_string(_el: Element(msg)) -> String {
  panic as "element.to_document_string is deprecated: use lustre/platform/dom.to_document_string"
}

/// DEPRECATED: Use `lustre/platform/dom.to_readable_string` instead.
/// This shim exists only for lustre_dev_tools compatibility.
///
@deprecated("Use lustre/platform/dom.to_readable_string instead")
@external(erlang, "lustre@platform@dom", "to_readable_string")
pub fn to_readable_string(_el: Element(msg)) -> String {
  panic as "element.to_readable_string is deprecated: use lustre/platform/dom.to_readable_string"
}

/// DEPRECATED: Use `element.unsafe_raw_content` instead.
/// This shim exists only for lustre_dev_tools compatibility.
///
@deprecated("Use element.unsafe_raw_content instead")
pub fn unsafe_raw_html(
  namespace: String,
  tag: String,
  attributes: List(Attribute(msg)),
  content: String,
) -> Element(msg) {
  vnode.raw_container(
    key: "",
    namespace:,
    tag:,
    attributes:,
    content:,
    compare: option.None,
  )
}
