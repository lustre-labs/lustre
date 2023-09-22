//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre/element](https://lustre.build/api/lustre/element)

// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/string
import gleam/string_builder.{StringBuilder}
import lustre/attribute.{Attribute}

// TYPES -----------------------------------------------------------------------

/// 
pub opaque type Element(msg) {
  Text(String)
  Element(String, List(Attribute(msg)), List(Element(msg)))
  ElementNs(String, List(Attribute(msg)), List(Element(msg)), String)
}

// CONSTRUCTORS ----------------------------------------------------------------

/// 
pub fn element(
  tag: String,
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  Element(tag, attrs, children)
}

///
pub fn namespaced(
  namespace: String,
  tag: String,
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  ElementNs(tag, attrs, children, namespace)
}

/// 
pub fn text(content: String) -> Element(msg) {
  Text(content)
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

/// 
pub fn map(element: Element(a), f: fn(a) -> b) -> Element(b) {
  case element {
    Text(content) -> Text(content)
    Element(tag, attrs, children) ->
      Element(
        tag,
        list.map(attrs, attribute.map(_, f)),
        list.map(children, map(_, f)),
      )
    ElementNs(tag, attrs, children, namespace) ->
      ElementNs(
        tag,
        list.map(attrs, attribute.map(_, f)),
        list.map(children, map(_, f)),
        namespace,
      )
  }
}

// CONVERSIONS -----------------------------------------------------------------

/// 
pub fn to_string(element: Element(msg)) -> String {
  to_string_builder(element, False)
  |> string_builder.to_string
}

/// 
pub fn to_string_builder(element: Element(msg), raw_text: Bool) -> StringBuilder {
  case element {
    Text(content) if raw_text -> string_builder.from_string(content)
    Text(content) -> string_builder.from_string(escape("", content))

    Element("area" as tag, attrs, _)
    | Element("base" as tag, attrs, _)
    | Element("br" as tag, attrs, _)
    | Element("col" as tag, attrs, _)
    | Element("embed" as tag, attrs, _)
    | Element("hr" as tag, attrs, _)
    | Element("img" as tag, attrs, _)
    | Element("input" as tag, attrs, _)
    | Element("link" as tag, attrs, _)
    | Element("meta" as tag, attrs, _)
    | Element("param" as tag, attrs, _)
    | Element("source" as tag, attrs, _)
    | Element("track" as tag, attrs, _)
    | Element("wbr" as tag, attrs, _) ->
      string_builder.from_string("<" <> tag)
      |> attrs_to_string_builder(attrs)
      |> string_builder.append(">")

    Element("style" as tag, attrs, children)
    | Element("script" as tag, attrs, children) ->
      string_builder.from_string("<" <> tag)
      |> attrs_to_string_builder(attrs)
      |> string_builder.append(">")
      |> children_to_string_builder(children, True)
      |> string_builder.append("</" <> tag <> ">")

    Element(tag, attrs, children) ->
      string_builder.from_string("<" <> tag)
      |> attrs_to_string_builder(attrs)
      |> string_builder.append(">")
      |> children_to_string_builder(children, raw_text)
      |> string_builder.append("</" <> tag <> ">")

    ElementNs(tag, attrs, children, namespace) ->
      string_builder.from_string("<" <> tag)
      |> attrs_to_string_builder(attrs)
      |> string_builder.append(" xmlns=\"" <> namespace <> "\"")
      |> string_builder.append(">")
      |> children_to_string_builder(children, raw_text)
      |> string_builder.append("</" <> tag <> ">")
  }
}

fn attrs_to_string_builder(
  html: StringBuilder,
  attrs: List(Attribute(msg)),
) -> StringBuilder {
  use html, attr <- list.fold(attrs, html)
  html
  |> string_builder.append(" ")
  |> string_builder.append_builder(attribute.to_string_builder(attr))
}

fn children_to_string_builder(
  html: StringBuilder,
  children: List(Element(msg)),
  raw_text: Bool,
) -> StringBuilder {
  use html, child <- list.fold(children, html)
  string_builder.append_builder(html, to_string_builder(child, raw_text))
}
