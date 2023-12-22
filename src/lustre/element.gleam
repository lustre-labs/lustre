//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre/element](https://lustre.build/api/lustre/element)

// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/string
import gleam/string_builder.{type StringBuilder}
import lustre/attribute.{type Attribute}

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
  to_string_builder_helper(element, False)
  |> string_builder.to_string
}

pub fn to_string_builder(element: Element(msg)) -> StringBuilder {
  to_string_builder_helper(element, False)
}

fn to_string_builder_helper(
  element: Element(msg),
  raw_text: Bool,
) -> StringBuilder {
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
    | Element("wbr" as tag, attrs, _) -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, _) = attrs_to_string_builder(attrs)

      html
      |> string_builder.append_builder(attrs)
      |> string_builder.append(">")
    }

    Element("style" as tag, attrs, children)
    | Element("script" as tag, attrs, children) -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, _) = attrs_to_string_builder(attrs)

      html
      |> string_builder.append_builder(attrs)
      |> string_builder.append(">")
      |> children_to_string_builder(children, True)
      |> string_builder.append("</" <> tag <> ">")
    }

    Element(tag, attrs, children) -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, inner_html) = attrs_to_string_builder(attrs)

      case inner_html {
        "" ->
          html
          |> string_builder.append_builder(attrs)
          |> string_builder.append(">")
          |> children_to_string_builder(children, raw_text)
          |> string_builder.append("</" <> tag <> ">")
        _ ->
          html
          |> string_builder.append_builder(attrs)
          |> string_builder.append(">" <> inner_html <> "</" <> tag <> ">")
      }
    }

    ElementNs(tag, attrs, children, namespace) -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, inner_html) = attrs_to_string_builder(attrs)

      case inner_html {
        "" ->
          html
          |> string_builder.append_builder(attrs)
          |> string_builder.append(" xmlns=\"" <> namespace <> "\"")
          |> string_builder.append(">")
          |> children_to_string_builder(children, raw_text)
          |> string_builder.append("</" <> tag <> ">")
        _ ->
          html
          |> string_builder.append_builder(attrs)
          |> string_builder.append(" xmlns=\"" <> namespace <> "\"")
          |> string_builder.append(">" <> inner_html <> "</" <> tag <> ">")
      }
    }
  }
}

fn attrs_to_string_builder(
  attrs: List(Attribute(msg)),
) -> #(StringBuilder, String) {
  let #(html, class, style, inner_html) = {
    let init = #(string_builder.new(), "", "", "")
    use #(html, class, style, inner_html), attr <- list.fold(attrs, init)

    case attribute.to_string_parts(attr) {
      Ok(#("dangerous-unescaped-html", val)) -> #(
        html,
        class,
        style,
        inner_html
        <> val,
      )
      Ok(#("class", val)) if class == "" -> #(html, val, style, inner_html)
      Ok(#("class", val)) -> #(html, class <> " " <> val, style, inner_html)
      Ok(#("style", val)) if style == "" -> #(html, class, val, inner_html)
      Ok(#("style", val)) -> #(html, class, style <> " " <> val, inner_html)
      Ok(#(key, val)) -> #(
        string_builder.append(html, " " <> key <> "=\"" <> val <> "\""),
        class,
        style,
        inner_html,
      )
      Error(_) -> #(html, class, style, inner_html)
    }
  }

  #(
    case class, style {
      "", "" -> html
      _, "" -> string_builder.append(html, " class=\"" <> class <> "\"")
      "", _ -> string_builder.append(html, " style=\"" <> style <> "\"")
      _, _ ->
        string_builder.append(
          html,
          " class=\""
          <> class
          <> "\" style=\""
          <> style
          <> "\"",
        )
    },
    inner_html,
  )
}

fn children_to_string_builder(
  html: StringBuilder,
  children: List(Element(msg)),
  raw_text: Bool,
) -> StringBuilder {
  use html, child <- list.fold(children, html)
  string_builder.append_builder(html, to_string_builder_helper(child, raw_text))
}
