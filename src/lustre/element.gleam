//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre/element](https://lustre.build/api/lustre/element)

// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/string
import gleam/string_builder.{type StringBuilder}
import lustre/attribute.{type Attribute, attribute}

// TYPES -----------------------------------------------------------------------

/// 
pub opaque type Element(msg) {
  Text(content: String)
  Element(
    namespace: String,
    tag: String,
    attrs: List(Attribute(msg)),
    children: List(Element(msg)),
    self_closing: Bool,
    void: Bool,
  )
}

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
    Text("") -> string_builder.new()
    Text(content) if raw_text -> string_builder.from_string(content)
    Text(content) -> string_builder.from_string(escape("", content))

    Element(namespace, tag, attrs, _, self_closing, _) if self_closing -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, _) =
        attrs_to_string_builder(case namespace {
          "" -> attrs
          _ -> [attribute("xmlns", namespace), ..attrs]
        })

      html
      |> string_builder.append_builder(attrs)
      |> string_builder.append("/>")
    }

    Element(namespace, tag, attrs, _, _, void) if void -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, _) =
        attrs_to_string_builder(case namespace {
          "" -> attrs
          _ -> [attribute("xmlns", namespace), ..attrs]
        })

      html
      |> string_builder.append_builder(attrs)
      |> string_builder.append(">")
    }

    // Style and script tags are special beacuse they need to contain unescape
    // text content and not escaped HTML content.
    Element("", "style" as tag, attrs, children, False, False)
    | Element("", "script" as tag, attrs, children, False, False) -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, _) = attrs_to_string_builder(attrs)

      html
      |> string_builder.append_builder(attrs)
      |> string_builder.append(">")
      |> children_to_string_builder(children, True)
      |> string_builder.append("</" <> tag <> ">")
    }

    Element(namespace, tag, attrs, children, _, _) -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, inner_html) =
        attrs_to_string_builder(case namespace {
          "" -> attrs
          _ -> [attribute("xmlns", namespace), ..attrs]
        })

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
