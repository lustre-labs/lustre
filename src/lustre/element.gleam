//// To read the full documentation for this module, please visit
//// [https://lustre.build/api/lustre/element](https://lustre.build/api/lustre/element)

// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
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

// QUERIES ---------------------------------------------------------------------

pub fn handlers(
  element: Element(msg),
) -> Dict(String, fn(Dynamic) -> Result(msg, Nil)) {
  do_handlers(element, dict.new(), "", 0)
}

fn do_handlers(
  element: Element(msg),
  handlers: Dict(String, fn(Dynamic) -> Result(msg, Nil)),
  key: String,
  pos: Int,
) -> Dict(String, fn(Dynamic) -> Result(msg, Nil)) {
  case element {
    Text(_) -> handlers
    Element(_, _, attrs, children, _, _) -> {
      let key = key <> int.to_string(pos)
      let handlers =
        attrs
        |> list.filter_map(attribute.handler(_, key))
        |> dict.from_list
        |> dict.merge(handlers)

      use handlers, child, pos <- list.index_fold(children, handlers)

      do_handlers(child, handlers, key, pos)
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

pub type Diff(a) {
  Diff(
    created: Dict(String, Element(a)),
    removed: Set(String),
    updated: Dict(String, List(Attribute(a))),
    handlers: Dict(String, fn(Dynamic) -> Result(a, Nil)),
  )
}

///
/// 
pub fn diff(old: Element(a), new: Element(a)) -> Diff(a) {
  Diff(dict.new(), set.new(), dict.new(), dict.new())
  |> do_diff(Some(old), Some(new), "0")
}

fn do_diff(
  diff: Diff(a),
  old: Option(Element(a)),
  new: Option(Element(a)),
  key: String,
) -> Diff(a) {
  case old, new {
    None, None -> diff
    Some(_), None -> Diff(..diff, removed: set.insert(diff.removed, key))
    None, Some(new) ->
      Diff(..diff, created: dict.insert(diff.created, key, new))

    Some(old), Some(new) -> {
      case old, new {
        Text(old), Text(new) if old == new -> diff
        Text(_), Text(_) ->
          Diff(..diff, created: dict.insert(diff.created, key, new))

        Element(_, _, _, _, _, _), Text(_) ->
          Diff(..diff, created: dict.insert(diff.created, key, new))

        Text(_), Element(_, _, _, _, _, _) ->
          Diff(..diff, created: dict.insert(diff.created, key, new))

        Element(old_ns, _, _, _, _, _), Element(new_ns, _, _, _, _, _) if old_ns != new_ns ->
          Diff(..diff, created: dict.insert(diff.created, key, new))

        Element(_, old_tag, _, _, _, _), Element(_, new_tag, _, _, _, _) if old_tag != new_tag ->
          Diff(..diff, created: dict.insert(diff.created, key, new))

        Element(_, _, old_attrs, old_children, _, _), Element(
          _,
          _,
          new_attrs,
          new_children,
          _,
          _,
        ) -> {
          let diff = case old_attrs == new_attrs {
            True -> diff
            False ->
              Diff(..diff, updated: dict.insert(diff.updated, key, new_attrs))
          }
          let handlers =
            new_attrs
            |> list.filter_map(attribute.handler(_, key))
            |> dict.from_list
          let diff = Diff(..diff, handlers: dict.merge(diff.handlers, handlers))
          let children = zip_children(old_children, new_children)
          use diff, #(old, new), pos <- list.index_fold(children, diff)
          let key = key <> int.to_string(pos)

          do_diff(diff, old, new, key)
        }
      }
    }
  }
}

fn zip_children(xs: List(a), ys: List(a)) -> List(#(Option(a), Option(a))) {
  case xs, ys {
    [x, ..xs], [y, ..ys] -> [#(Some(x), Some(y)), ..zip_children(xs, ys)]
    [x, ..xs], [] -> [#(Some(x), None), ..zip_children(xs, [])]
    [], [y, ..ys] -> [#(None, Some(y)), ..zip_children([], ys)]
    [], [] -> []
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
          " class=\"" <> class <> "\" style=\"" <> style <> "\"",
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

pub fn encode(element: Element(msg)) -> Json {
  do_encode(element, "", 0)
}

pub fn do_encode(element: Element(msg), key: String, pos: Int) -> Json {
  case element {
    Text(content) ->
      json.object([
        #("$", json.string("Text")),
        #("content", json.string(content)),
      ])

    Element(namespace, tag, attrs, children, self_closing, void) -> {
      let key = key <> int.to_string(pos)
      let attrs =
        attrs
        |> list.prepend(attribute("data-lustre-key", key))
        |> list.filter_map(attribute.encode(_, key))
        |> json.preprocessed_array
      let children =
        children
        |> list.index_map(fn(child, pos) { do_encode(child, key, pos) })
        |> json.preprocessed_array

      json.object([
        #("$", json.string("Element")),
        #("namespace", json.string(namespace)),
        #("tag", json.string(tag)),
        #("attrs", attrs),
        #("children", children),
        #("self_closing", json.bool(self_closing)),
        #("void", json.bool(void)),
      ])
    }
  }
}

pub fn encode_diff(diff: Diff(a)) -> Json {
  let created =
    json.preprocessed_array({
      use arr, key, val <- dict.fold(diff.created, [])
      let element = json.preprocessed_array([json.string(key), encode(val)])
      [element, ..arr]
    })
  let removed =
    json.preprocessed_array({
      use arr, key <- set.fold(diff.removed, [])
      [json.string(key), ..arr]
    })
  let updated =
    json.preprocessed_array({
      use arr, key, attrs <- dict.fold(diff.updated, [])
      let attrs =
        json.preprocessed_array([
          json.string(key),
          json.preprocessed_array(
            list.filter_map(attrs, attribute.encode(_, key)),
          ),
        ])
      [attrs, ..arr]
    })

  json.preprocessed_array([json.string("Diff"), created, removed, updated])
}
