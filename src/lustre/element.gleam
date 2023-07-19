// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/string
import gleam/string_builder.{StringBuilder}
import lustre/attribute.{Attribute}

// TYPES -----------------------------------------------------------------------

///
/// 
pub opaque type Element(msg) {
  Text(String)
  Element(String, List(Attribute(msg)), List(Element(msg)))
}

// CONSTRUCTORS ----------------------------------------------------------------

///
/// 
pub fn h(
  tag: String,
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  Element(tag, attrs, children)
}

///
/// 
pub fn t(content: String) -> Element(msg) {
  Text(content)
}

fn escape(escaped: String, content: String) -> String {
  case string.pop_grapheme(content) {
    Ok(#("<", xs)) -> escape(escaped <> "&lt;", xs)
    Ok(#(">", xs)) -> escape(escaped <> "&gt;", xs)
    Ok(#("&", xs)) -> escape(escaped <> "&amp;", xs)
    Ok(#("\"", xs)) -> escape(escaped <> "&quot;", xs)
    Ok(#("'", xs)) -> escape(escaped <> "&#x27;", xs)
    Ok(#(x, xs)) -> escape(escaped <> x, xs)
    Error(_) -> escaped <> content
  }
}

// MANIPULATIONS ---------------------------------------------------------------

///
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
  }
}

// CONVERSIONS -----------------------------------------------------------------

///
/// 
pub fn to_string(element: Element(msg)) -> String {
  to_string_builder(element)
  |> string_builder.to_string
}

///
/// 
pub fn to_string_builder(element: Element(msg)) -> StringBuilder {
  case element {
    Text(content) -> string_builder.from_string(escape("", content))
    Element(tag, attrs, children) -> {
      string_builder.from_string("<" <> tag)
      |> attrs_to_string_builder(attrs)
      |> string_builder.append(">")
      |> children_to_string_builder(children)
      |> string_builder.append("</" <> tag <> ">")
    }
  }
}

fn attrs_to_string_builder(
  html: StringBuilder,
  attrs: List(Attribute(msg)),
) -> StringBuilder {
  use html, attr <- list.fold(attrs, html)
  string_builder.append_builder(html, attribute.to_string_builder(attr))
}

fn children_to_string_builder(
  html: StringBuilder,
  children: List(Element(msg)),
) -> StringBuilder {
  use html, child <- list.fold(children, html)
  string_builder.append_builder(html, to_string_builder(child))
}
