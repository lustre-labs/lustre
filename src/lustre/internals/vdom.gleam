// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/float
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/string
import gleam/string_tree.{type StringTree}
import lustre/internals/escape.{escape}

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

      do_element_list_handlers(children, handlers, key)
    }
  }
}

fn do_element_list_handlers(
  elements: List(Element(msg)),
  handlers: Dict(String, Decoder(msg)),
  key: String,
) {
  use handlers, element, index <- list.index_fold(elements, handlers)
  let key = key <> "-" <> int.to_string(index)
  do_handlers(element, handlers, key)
}

// CONVERSIONS: JSON -----------------------------------------------------------

pub fn element_to_json(element: Element(msg), key: String) -> Json {
  case element {
    Text(content) -> json.object([#("content", json.string(content))])
    Map(subtree) -> element_to_json(subtree(), key)
    Element(_, namespace, tag, attrs, children, self_closing, void) -> {
      let attrs =
        json.preprocessed_array({
          list.filter_map(attrs, attribute_to_json(_, key))
        })
      let children = do_element_list_to_json(children, key)

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

fn do_element_list_to_json(elements: List(Element(msg)), key: String) {
  json.preprocessed_array({
    use element, index <- list.index_map(elements)
    let key = key <> "-" <> int.to_string(index)
    element_to_json(element, key)
  })
}

pub fn attribute_to_json(
  attribute: Attribute(msg),
  key: String,
) -> Result(Json, Nil) {
  let true_atom = dynamic.from(True)
  let false_atom = dynamic.from(False)

  case attribute {
    Attribute(_, _, True) -> Error(Nil)
    Attribute(name, value, as_property: False) -> {
      case dynamic.classify(value) {
        "String" ->
          Ok(
            json.object([
              #("0", json.string(name)),
              #("1", json.string(unsafe_coerce(value))),
            ]),
          )

        "Atom" if value == true_atom || value == false_atom ->
          Ok(
            json.object([
              #("0", json.string(name)),
              #("1", json.bool(unsafe_coerce(value))),
            ]),
          )

        "Bool" | "Boolean" ->
          Ok(
            json.object([
              #("0", json.string(name)),
              #("1", json.bool(unsafe_coerce(value))),
            ]),
          )

        "Int" ->
          Ok(
            json.object([
              #("0", json.string(name)),
              #("1", json.int(unsafe_coerce(value))),
            ]),
          )

        "Float" ->
          Ok(
            json.object([
              #("0", json.string(name)),
              #("1", json.float(unsafe_coerce(value))),
            ]),
          )

        _ -> Error(Nil)
      }
    }

    Event(name, _) -> {
      let name = string.drop_start(name, 2)

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
  |> string_tree.to_string
}

pub fn element_to_string_builder(element: Element(msg)) -> StringTree {
  do_element_to_string_builder(element, False)
}

fn do_element_to_string_builder(
  element: Element(msg),
  raw_text: Bool,
) -> StringTree {
  case element {
    Text("") -> string_tree.new()
    Text(content) if raw_text -> string_tree.from_string(content)
    Text(content) -> string_tree.from_string(escape(content))

    Map(subtree) -> do_element_to_string_builder(subtree(), raw_text)

    Element(_, namespace, tag, attrs, _, self_closing, _) if self_closing -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attrs, _) =
        attributes_to_string_builder(case namespace {
          "" -> attrs
          _ -> [Attribute("xmlns", dynamic.from(namespace), False), ..attrs]
        })

      html
      |> string_tree.append_tree(attrs)
      |> string_tree.append("/>")
    }

    Element(_, namespace, tag, attrs, _, _, void) if void -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attrs, _) =
        attributes_to_string_builder(case namespace {
          "" -> attrs
          _ -> [Attribute("xmlns", dynamic.from(namespace), False), ..attrs]
        })

      html
      |> string_tree.append_tree(attrs)
      |> string_tree.append(">")
    }

    // Style and script tags are special beacuse they need to contain unescape
    // text content and not escaped HTML content.
    Element(_, "", "style" as tag, attrs, children, False, False)
    | Element(_, "", "script" as tag, attrs, children, False, False) -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attrs, _) = attributes_to_string_builder(attrs)

      html
      |> string_tree.append_tree(attrs)
      |> string_tree.append(">")
      |> children_to_string_builder(children, True)
      |> string_tree.append("</" <> tag <> ">")
    }

    Element(_, namespace, tag, attrs, children, _, _) -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attrs, inner_html) =
        attributes_to_string_builder(case namespace {
          "" -> attrs
          _ -> [Attribute("xmlns", dynamic.from(namespace), False), ..attrs]
        })

      case inner_html {
        "" ->
          html
          |> string_tree.append_tree(attrs)
          |> string_tree.append(">")
          |> children_to_string_builder(children, raw_text)
          |> string_tree.append("</" <> tag <> ">")
        _ ->
          html
          |> string_tree.append_tree(attrs)
          |> string_tree.append(">" <> inner_html <> "</" <> tag <> ">")
      }
    }
  }
}

fn children_to_string_builder(
  html: StringTree,
  children: List(Element(msg)),
  raw_text: Bool,
) -> StringTree {
  use html, child <- list.fold(children, html)

  child
  |> do_element_to_string_builder(raw_text)
  |> string_tree.append_tree(html, _)
}

pub fn element_to_snapshot(element: Element(msg)) -> String {
  element
  |> do_element_to_snapshot_builder(False, 0)
  |> string_tree.to_string
}

fn do_element_to_snapshot_builder(
  element: Element(msg),
  raw_text: Bool,
  indent: Int,
) -> StringTree {
  let spaces = string.repeat("  ", indent)

  case element {
    Text("") -> string_tree.new()
    Text(content) if raw_text -> string_tree.from_strings([spaces, content])
    Text(content) -> string_tree.from_strings([spaces, escape(content)])

    Map(subtree) -> do_element_to_snapshot_builder(subtree(), raw_text, indent)

    Element(_, namespace, tag, attrs, _, self_closing, _) if self_closing -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attrs, _) =
        attributes_to_string_builder(case namespace {
          "" -> attrs
          _ -> [Attribute("xmlns", dynamic.from(namespace), False), ..attrs]
        })

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attrs)
      |> string_tree.append("/>")
    }

    Element(_, namespace, tag, attrs, _, _, void) if void -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attrs, _) =
        attributes_to_string_builder(case namespace {
          "" -> attrs
          _ -> [Attribute("xmlns", dynamic.from(namespace), False), ..attrs]
        })

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attrs)
      |> string_tree.append(">")
    }

    Element(_, "", tag, attrs, [], _, _) -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attrs, _) = attributes_to_string_builder(attrs)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attrs)
      |> string_tree.append(">")
      |> string_tree.append("</" <> tag <> ">")
    }

    // Style and script tags are special beacuse they need to contain unescape
    // text content and not escaped HTML content.
    Element(_, "", "style" as tag, attrs, children, False, False)
    | Element(_, "", "script" as tag, attrs, children, False, False) -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attrs, _) = attributes_to_string_builder(attrs)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attrs)
      |> string_tree.append(">")
      |> children_to_snapshot_builder(children, True, indent + 1)
      |> string_tree.append(spaces)
      |> string_tree.append("</" <> tag <> ">")
    }

    Element(_, namespace, tag, attrs, children, _, _) -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attrs, inner_html) =
        attributes_to_string_builder(case namespace {
          "" -> attrs
          _ -> [Attribute("xmlns", dynamic.from(namespace), False), ..attrs]
        })

      case inner_html {
        "" ->
          html
          |> string_tree.prepend(spaces)
          |> string_tree.append_tree(attrs)
          |> string_tree.append(">\n")
          |> children_to_snapshot_builder(children, raw_text, indent + 1)
          |> string_tree.append(spaces)
          |> string_tree.append("</" <> tag <> ">")
        _ ->
          html
          |> string_tree.append_tree(attrs)
          |> string_tree.append(">" <> inner_html <> "</" <> tag <> ">")
      }
    }
  }
}

fn children_to_snapshot_builder(
  html: StringTree,
  children: List(Element(msg)),
  raw_text: Bool,
  indent: Int,
) -> StringTree {
  case children {
    [Text(a), Text(b), ..rest] ->
      children_to_snapshot_builder(
        html,
        [Text(a <> b), ..rest],
        raw_text,
        indent,
      )
    [child, ..rest] ->
      child
      |> do_element_to_snapshot_builder(raw_text, indent)
      |> string_tree.append("\n")
      |> string_tree.append_tree(html, _)
      |> children_to_snapshot_builder(rest, raw_text, indent)
    [] -> html
  }
}

fn attributes_to_string_builder(
  attrs: List(Attribute(msg)),
) -> #(StringTree, String) {
  let #(html, class, style, inner_html) = {
    let init = #(string_tree.new(), "", "", "")
    use #(html, class, style, inner_html), attr <- list.fold(attrs, init)

    case attribute_to_string_parts(attr) {
      Ok(#("dangerous-unescaped-html", val)) -> #(
        html,
        class,
        style,
        inner_html <> val,
      )
      Ok(#("class", val)) if class == "" -> #(
        html,
        escape(val),
        style,
        inner_html,
      )
      Ok(#("class", val)) -> #(
        html,
        class <> " " <> escape(val),
        style,
        inner_html,
      )
      Ok(#("style", val)) if style == "" -> #(
        html,
        class,
        escape(val),
        inner_html,
      )
      Ok(#("style", val)) -> #(
        html,
        class,
        style <> " " <> escape(val),
        inner_html,
      )
      Ok(#(key, "")) -> #(
        string_tree.append(html, " " <> key),
        class,
        style,
        inner_html,
      )
      Ok(#(key, val)) -> #(
        string_tree.append(html, " " <> key <> "=\"" <> escape(val) <> "\""),
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
      _, "" -> string_tree.append(html, " class=\"" <> class <> "\"")
      "", _ -> string_tree.append(html, " style=\"" <> style <> "\"")
      _, _ ->
        string_tree.append(
          html,
          " class=\"" <> class <> "\" style=\"" <> style <> "\"",
        )
    },
    inner_html,
  )
}

// UTILS -----------------------------------------------------------------------

fn attribute_to_string_parts(
  attr: Attribute(msg),
) -> Result(#(String, String), Nil) {
  case attr {
    Attribute("", _, _) -> Error(Nil)
    Attribute(name, value, as_property) -> {
      let true_atom = dynamic.from(True)

      case dynamic.classify(value) {
        "String" -> Ok(#(name, unsafe_coerce(value)))

        // Boolean attributes are determined based on their presence, eg we don't
        // want to render `disabled="false"` if the value is `false` we simply
        // want to omit the attribute altogether.
        //
        // The behaviour of `dynamic.classify` on booleans on the Erlang target
        // depends on what version of the standard library you have. <= 0.36.0
        // will classify `true` and `false` as `"Atom"` but >= 0.37.0 will be
        // smarter and classify them as `"Bool"`.
        //
        "Atom" | "Bool" | "Boolean" if value == true_atom -> Ok(#(name, ""))
        "Atom" | "Bool" | "Boolean" -> Error(Nil)

        "Int" -> Ok(#(name, int.to_string(unsafe_coerce(value))))
        "Float" -> Ok(#(name, float.to_string(unsafe_coerce(value))))

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
      let name = string.drop_start(name, 2)
      Ok(#(name, handler))
    }
  }
}

// FFI -------------------------------------------------------------------------

@external(erlang, "lustre_escape_ffi", "coerce")
@external(javascript, "../../lustre-escape.ffi.mjs", "coerce")
fn unsafe_coerce(value: a) -> b
