//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre/element](https://lustre.build/api/lustre/element)

// IMPORTS ---------------------------------------------------------------------

import gleam/json.{type Json}
import gleam/list
import gleam/string
import gleam/string_builder.{type StringBuilder}
import lustre/attribute.{type Attribute, attribute}
import lustre/internals/vdom.{Element, Text}
import lustre/internals/patch

// TYPES -----------------------------------------------------------------------

/// 
pub type Element(msg) =
  vdom.Element(msg)

/// Patches are sent by server components to any connected renderers. Because
/// server components are not opinionated about your network layer or how your
/// wider application is organised, it is your responsibility to make sure a `Patch`
/// makes its way to the server component client runtime.
/// 
pub type Patch(msg) =
  patch.Patch(msg)

// CONSTRUCTORS ----------------------------------------------------------------

/// 
/// 
/// 🚨 Because Lustre is primarily used to create HTML, this function spcieal-cases
///    the following tags to be self-closing:
/// 
///    - area
///    - base
///    - br
///    - col
///    - embed
///    - hr
///    - img
///    - input
///    - link
///    - meta
///    - param
///    - source
///    - track
///    - wbr
/// 
///    This will only affect the output of `to_string` and `to_string_builder`!
///    If you need to render any of these tags with children, *or* you want to
///    render some other tag as self-closing or void, use [`advanced`](#advanced)
///    instead.
/// 
pub fn element(
  tag: String,
  attrs: List(Attribute(msg)),
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
    | "wbr" -> Element("", tag, attrs, [], False, True)
    _ ->
      Element(
        namespace: "",
        tag: tag,
        attrs: attrs,
        children: children,
        self_closing: False,
        void: False,
      )
  }
}

///
pub fn namespaced(
  namespace: String,
  tag: String,
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  Element(
    namespace: namespace,
    tag: tag,
    attrs: attrs,
    children: children,
    self_closing: False,
    void: False,
  )
}

///
pub fn advanced(
  namespace: String,
  tag: String,
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
  self_closing: Bool,
  void: Bool,
) -> Element(msg) {
  Element(tag, namespace, attrs, children, self_closing, void)
}

/// 
pub fn text(content: String) -> Element(msg) {
  Text(content)
}

///
pub fn none() -> Element(msg) {
  Text("")
}

fn escape(escaped: String, content: String) -> String {
  case content {
    "<" <> rest -> escape(escaped <> "&lt;", rest)
    ">" <> rest -> escape(escaped <> "&gt;", rest)
    "&" <> rest -> escape(escaped <> "&amp;", rest)
    "\"" <> rest -> escape(escaped <> "&quot;", rest)
    "'" <> rest -> escape(escaped <> "&#39;", rest)
    _ ->
      case string.pop_grapheme(content) {
        Ok(#(x, xs)) -> escape(escaped <> x, xs)
        Error(_) -> escaped
      }
  }
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
  case element {
    Text(content) -> Text(content)
    Element(namespace, tag, attrs, children, self_closing, void) ->
      Element(
        namespace,
        tag,
        list.map(attrs, attribute.map(_, f)),
        list.map(children, map(_, f)),
        self_closing,
        void,
      )
  }
}

// CONVERSIONS -----------------------------------------------------------------

/// 
pub fn to_string(element: Element(msg)) -> String {
  vdom.element_to_string(element)
}

pub fn to_string_builder(element: Element(msg)) -> StringBuilder {
  vdom.element_to_string_builder(element)
}

pub fn encode(element: Element(msg)) -> Json {
  vdom.element_to_json(element)
}

pub fn encode_patch(patch: Patch(msg)) -> Json {
  patch.patch_to_json(patch)
}
