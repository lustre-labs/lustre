// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/string
import gleam/string_builder.{type StringBuilder}

// TYPES -----------------------------------------------------------------------

pub type Element(msg) {
  Text(content: String)
  Element(
    key: String,
    namespace: String,
    tag: String,
    attrs: List(Attribute(msg)),
    children: List(Element(msg)),
    self_closing: Bool,
    void: Bool,
  )
  // The lambda here defers the creation of the mapped subtree until it is necessary.
  // This means we pay the cost of mapping multiple times only *once* during rendering.
  Map(subtree: fn() -> Element(msg))
}

pub type Attribute(msg) {
  Attribute(String, Dynamic, as_property: Bool)
  Event(String, Decoder(msg))
}

// QUERIES ---------------------------------------------------------------------

pub fn handlers(element: Element(msg)) -> Dict(String, Decoder(msg)) {
  do_handlers(element, dict.new(), "0")
}

fn do_handlers(
  element: Element(msg),
  handlers: Dict(String, Decoder(msg)),
  key: String,
) -> Dict(String, Decoder(msg)) {
  case element {
    Text(_) -> handlers
    Map(subtree) -> do_handlers(subtree(), handlers, key)
    Element(_, _, _, attrs, children, _, _) -> {
      let handlers =
        list.fold(attrs, handlers, fn(handlers, attr) {
          case attribute_to_event_handler(attr) {
            Ok(#(name, handler)) ->
              dict.insert(handlers, key <> "-" <> name, handler)
            Error(_) -> handlers
          }
        })

      use handlers, child, index <- list.index_fold(children, handlers)
      let key = key <> "-" <> int.to_string(index)
      do_handlers(child, handlers, key)
    }
  }
}

// CONVERSIONS: JSON -----------------------------------------------------------

pub fn element_to_json(element: Element(msg)) -> Json {
  do_element_to_json(element, "0")
}

fn do_element_to_json(element: Element(msg), key: String) -> Json {
  case element {
    Text(content) -> json.object([#("content", json.string(content))])
    Map(subtree) -> do_element_to_json(subtree(), key)
    Element(_, namespace, tag, attrs, children, self_closing, void) -> {
      let attrs =
        json.preprocessed_array({
          list.filter_map(attrs, attribute_to_json(_, key))
        })
      let children =
        json.preprocessed_array({
          use child, index <- list.index_map(children)
          let key = key <> "-" <> int.to_string(index)
          do_element_to_json(child, key)
        })

      json.object([
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

pub fn attribute_to_json(
  attribute: Attribute(msg),
  key: String,
) -> Result(Json, Nil) {
  case attribute {
    Attribute(_, _, True) -> Error(Nil)
    Attribute(name, value, as_property: False) -> {
      case dynamic.classify(value) {
        "String" ->
          Ok(
            json.object([
              #("0", json.string(name)),
              #("1", json.string(dynamic.unsafe_coerce(value))),
            ]),
          )

        "Boolean" ->
          Ok(
            json.object([
              #("0", json.string(name)),
              #("1", json.bool(dynamic.unsafe_coerce(value))),
            ]),
          )

        "Int" ->
          Ok(
            json.object([
              #("0", json.string(name)),
              #("1", json.int(dynamic.unsafe_coerce(value))),
            ]),
          )

        "Float" ->
          Ok(
            json.object([
              #("0", json.string(name)),
              #("1", json.float(dynamic.unsafe_coerce(value))),
            ]),
          )

        _ -> Error(Nil)
      }
    }

    Event(name, _) -> {
      let name = string.drop_left(name, 2)

      Ok(
        json.object([
          #("0", json.string("data-lustre-on-" <> name)),
          #("1", json.string(key <> "-" <> name)),
        ]),
      )
    }
  }
}

// CONVERSIONS: STRING ---------------------------------------------------------

pub fn element_to_string(element: Element(msg)) -> String {
  element
  |> do_element_to_string_builder(False)
  |> string_builder.to_string
}

pub fn element_to_string_builder(element: Element(msg)) -> StringBuilder {
  do_element_to_string_builder(element, False)
}

fn do_element_to_string_builder(
  element: Element(msg),
  raw_text: Bool,
) -> StringBuilder {
  case element {
    Text("") -> string_builder.new()
    Text(content) if raw_text -> string_builder.from_string(content)
    Text(content) -> string_builder.from_string(escape("", content))

    Map(subtree) -> do_element_to_string_builder(subtree(), raw_text)

    Element(_, namespace, tag, attrs, _, self_closing, _) if self_closing -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, _) =
        attributes_to_string_builder(case namespace {
          "" -> attrs
          _ -> [Attribute("xmlns", dynamic.from(namespace), False), ..attrs]
        })

      html
      |> string_builder.append_builder(attrs)
      |> string_builder.append("/>")
    }

    Element(_, namespace, tag, attrs, _, _, void) if void -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, _) =
        attributes_to_string_builder(case namespace {
          "" -> attrs
          _ -> [Attribute("xmlns", dynamic.from(namespace), False), ..attrs]
        })

      html
      |> string_builder.append_builder(attrs)
      |> string_builder.append(">")
    }

    // Style and script tags are special beacuse they need to contain unescape
    // text content and not escaped HTML content.
    Element(_, "", "style" as tag, attrs, children, False, False)
    | Element(_, "", "script" as tag, attrs, children, False, False) -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, _) = attributes_to_string_builder(attrs)

      html
      |> string_builder.append_builder(attrs)
      |> string_builder.append(">")
      |> children_to_string_builder(children, True)
      |> string_builder.append("</" <> tag <> ">")
    }

    Element(_, namespace, tag, attrs, children, _, _) -> {
      let html = string_builder.from_string("<" <> tag)
      let #(attrs, inner_html) =
        attributes_to_string_builder(case namespace {
          "" -> attrs
          _ -> [Attribute("xmlns", dynamic.from(namespace), False), ..attrs]
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

fn children_to_string_builder(
  html: StringBuilder,
  children: List(Element(msg)),
  raw_text: Bool,
) -> StringBuilder {
  use html, child <- list.fold(children, html)

  child
  |> do_element_to_string_builder(raw_text)
  |> string_builder.append_builder(html, _)
}

fn attributes_to_string_builder(
  attrs: List(Attribute(msg)),
) -> #(StringBuilder, String) {
  let #(html, class, style, inner_html) = {
    let init = #(string_builder.new(), "", "", "")
    use #(html, class, style, inner_html), attr <- list.fold(attrs, init)

    case attribute_to_string_parts(attr) {
      Ok(#("dangerous-unescaped-html", val)) -> #(
        html,
        class,
        style,
        inner_html <> val,
      )
      Ok(#("class", val)) if class == "" -> #(html, val, style, inner_html)
      Ok(#("class", val)) -> #(html, class <> " " <> val, style, inner_html)
      Ok(#("style", val)) if style == "" -> #(html, class, val, inner_html)
      Ok(#("style", val)) -> #(html, class, style <> " " <> val, inner_html)
      Ok(#(key, "")) -> #(
        string_builder.append(html, " " <> key),
        class,
        style,
        inner_html,
      )
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

// UTILS -----------------------------------------------------------------------

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

fn attribute_to_string_parts(
  attr: Attribute(msg),
) -> Result(#(String, String), Nil) {
  case attr {
    Attribute("", _, _) -> Error(Nil)
    Attribute(name, value, as_property) -> {
      let true_atom = dynamic.from(True)

      case dynamic.classify(value) {
        "String" -> Ok(#(name, dynamic.unsafe_coerce(value)))

        // Boolean attributes are determined based on their presence, eg we don't
        // want to render `disabled="false"` if the value is `false` we simply
        // want to omit the attribute altogether.
        //
        // On the Erlang target, booleans are actually just the atoms `true` and
        // `false`!
        "Atom" | "Boolean" if value == true_atom -> Ok(#(name, ""))
        "Atom" | "Boolean" -> Error(Nil)

        // For everything else, we care whether or not the attribute is actually
        // a property. Properties are *Javascript* values that aren't necessarily
        // reflected in the DOM.
        _ if as_property -> Error(Nil)
        _ -> Ok(#(name, string.inspect(value)))
      }
    }
    _ -> Error(Nil)
  }
}

pub fn attribute_to_event_handler(
  attribute: Attribute(msg),
) -> Result(#(String, Decoder(msg)), Nil) {
  case attribute {
    Attribute(_, _, _) -> Error(Nil)
    Event(name, handler) -> {
      let name = string.drop_left(name, 2)
      Ok(#(name, handler))
    }
  }
}
