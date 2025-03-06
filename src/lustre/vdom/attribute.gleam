// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/list
import gleam/order.{type Order}
import gleam/string
import gleam/string_tree.{type StringTree}
import lustre/internals/constants
import lustre/internals/escape.{escape}

// TYPES -----------------------------------------------------------------------

pub type Attribute(msg) {
  Attribute(name: String, value: String)
  Property(name: String, value: Json)
  Event(
    name: String,
    handler: Decoder(msg),
    include: List(String),
    prevent_default: Bool,
    stop_propagation: Bool,
    immediate: Bool,
  )
}

//

pub fn prepare(attributes: List(Attribute(msg))) -> List(Attribute(msg)) {
  attributes
  // Sort in reverse because `merge` will build the list in reverse anyway.
  |> list.sort(by: fn(a, b) { compare(b, a) })
  |> merge(constants.empty_list)
}

pub fn merge(
  attributes: List(Attribute(msg)),
  merged: List(Attribute(msg)),
) -> List(Attribute(msg)) {
  case attributes {
    [] -> merged

    [
      Attribute(name: "class", value: class1),
      Attribute(name: "class", value: class2),
      ..rest
    ] -> {
      let value = class1 <> " " <> class2
      let attribute = Attribute(name: "class", value: value)

      merge([attribute, ..rest], merged)
    }

    [
      Attribute(name: "style", value: style1),
      Attribute(name: "style", value: style2),
      ..rest
    ] -> {
      let value = style1 <> ";" <> style2
      let attribute = Attribute(name: "style", value: value)

      merge([attribute, ..rest], merged)
    }

    [attribute, ..rest] -> merge(rest, [attribute, ..merged])
  }
}

@external(javascript, "./attribute.ffi.mjs", "compare")
pub fn compare(a: Attribute(msg), b: Attribute(msg)) -> Order {
  string.compare(a.name, b.name)
}

// STRING RENDERING ------------------------------------------------------------

pub fn to_string_tree(attributes: List(Attribute(msg))) -> StringTree {
  let #(html, class, style) = {
    let init = #(string_tree.new(), "", "")
    use #(html, class, style), attr <- list.fold(attributes, init)

    case to_string_parts(attr) {
      Ok(#("class", val)) if class == "" -> #(html, escape(val), style)
      Ok(#("class", val)) -> #(html, class <> " " <> escape(val), style)
      Ok(#("style", val)) if style == "" -> #(html, class, escape(val))
      Ok(#("style", val)) -> #(html, class, style <> " " <> escape(val))
      Ok(#(key, "")) -> #(string_tree.append(html, " " <> key), class, style)
      Ok(#(key, val)) -> #(
        string_tree.append(html, " " <> key <> "=\"" <> escape(val) <> "\""),
        class,
        style,
      )
      Error(_) -> #(html, class, style)
    }
  }

  case class, style {
    "", "" -> html
    _, "" -> string_tree.append(html, " class=\"" <> class <> "\"")
    "", _ -> string_tree.append(html, " style=\"" <> style <> "\"")
    _, _ ->
      string_tree.append(
        html,
        " class=\"" <> class <> "\" style=\"" <> style <> "\"",
      )
  }
}

pub fn to_string_parts(attr: Attribute(msg)) -> Result(#(String, String), Nil) {
  case attr {
    Attribute("", _) -> constants.error_nil
    Attribute(name, value) -> Ok(#(name, value))
    _ -> constants.error_nil
  }
}
