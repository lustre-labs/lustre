// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/list
import gleam/order.{type Order}
import gleam/string
import gleam/string_tree.{type StringTree}
import houdini.{escape}
import lustre/internals/constants
import lustre/internals/json_object_builder

// TYPES -----------------------------------------------------------------------

pub type Attribute(msg) {
  Attribute(kind: Int, name: String, value: String)
  Property(kind: Int, name: String, value: Json)
  Event(
    kind: Int,
    name: String,
    handler: Decoder(msg),
    include: List(String),
    prevent_default: Bool,
    stop_propagation: Bool,
    immediate: Bool,
    limit: Limit,
  )
}

pub type Limit {
  NoLimit(kind: Int)
  Debounce(kind: Int, delay: Int)
  Throttle(kind: Int, delay: Int)
}

// CONSTRUCTORS ----------------------------------------------------------------

pub const attribute_kind: Int = 0

pub fn attribute(name name: String, vaue value: String) -> Attribute(msg) {
  Attribute(kind: attribute_kind, name:, value:)
}

pub const property_kind: Int = 1

pub fn property(name name: String, value value: Json) -> Attribute(msg) {
  Property(kind: property_kind, name:, value:)
}

pub const event_kind: Int = 2

pub fn event(
  name name: String,
  handler handler: Decoder(msg),
  include include: List(String),
  prevent_default prevent_default: Bool,
  stop_propagation stop_propagation: Bool,
  immediate immediate: Bool,
  limit limit: Limit,
) -> Attribute(msg) {
  Event(
    kind: event_kind,
    name:,
    handler:,
    include:,
    prevent_default:,
    stop_propagation:,
    immediate:,
    limit:,
  )
}

pub const debounce_kind: Int = 1

pub const throttle_kind: Int = 2

pub fn debounce(delay delay: Int) -> Limit {
  Debounce(kind: debounce_kind, delay:)
}

pub fn throttle(delay delay: Int) -> Limit {
  Throttle(kind: throttle_kind, delay:)
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
      Attribute(kind:, name: "class", value: class1),
      Attribute(kind: _, name: "class", value: class2),
      ..rest
    ] -> {
      let value = class1 <> " " <> class2
      let attribute = Attribute(kind:, name: "class", value: value)

      merge([attribute, ..rest], merged)
    }

    [
      Attribute(kind:, name: "style", value: style1),
      Attribute(kind: _, name: "style", value: style2),
      ..rest
    ] -> {
      let value = style1 <> ";" <> style2
      let attribute = Attribute(kind:, name: "style", value: value)

      merge([attribute, ..rest], merged)
    }

    [attribute, ..rest] -> merge(rest, [attribute, ..merged])
  }
}

@external(javascript, "./attribute.ffi.mjs", "compare")
pub fn compare(a: Attribute(msg), b: Attribute(msg)) -> Order {
  string.compare(a.name, b.name)
}

// ENCODING --------------------------------------------------------------------

pub fn to_json(attribute: Attribute(msg)) -> Json {
  case attribute {
    Attribute(kind:, name:, value:) -> attribute_to_json(kind, name, value)
    Property(kind:, name:, value:) -> property_to_json(kind, name, value)
    Event(
      kind:,
      name:,
      include:,
      prevent_default:,
      stop_propagation:,
      immediate:,
      limit:,
      ..,
    ) ->
      event_to_json(
        kind,
        name,
        include,
        prevent_default,
        stop_propagation,
        immediate,
        limit,
      )
  }
}

fn attribute_to_json(kind, name, value) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("name", name)
  |> json_object_builder.string("value", value)
  |> json_object_builder.build
}

fn property_to_json(kind, name, value) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("name", name)
  |> json_object_builder.json("value", value)
  |> json_object_builder.build
}

fn event_to_json(
  kind,
  name,
  include,
  prevent_default,
  stop_propagation,
  immediate,
  limit,
) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("name", name)
  |> json_object_builder.list("include", include, json.string)
  |> json_object_builder.bool("prevent_default", prevent_default)
  |> json_object_builder.bool("stop_propagation", stop_propagation)
  |> json_object_builder.bool("immediate", immediate)
  |> json_object_builder.object("limit", limit_to_json(limit))
  |> json_object_builder.build
}

fn limit_to_json(limit) {
  case limit {
    NoLimit(..) -> []

    Debounce(delay:, ..) ->
      json_object_builder.tagged(debounce_kind)
      |> json_object_builder.int("delay", delay)

    Throttle(delay:, ..) ->
      json_object_builder.tagged(throttle_kind)
      |> json_object_builder.int("delay", delay)
  }
}

// STRING RENDERING ------------------------------------------------------------

pub fn to_string_tree(
  key: String,
  namespace: String,
  attributes: List(Attribute(msg)),
) -> StringTree {
  let attributes = case key != "" {
    True -> [attribute("data-lustre-key", key), ..attributes]
    False -> attributes
  }
  let attributes = case namespace != "" {
    True -> [attribute("xmlns", namespace), ..attributes]
    False -> attributes
  }

  use html, attr <- list.fold(attributes, string_tree.new())
  case to_string_parts(attr) {
    Ok(#("class", "")) | Ok(#("style", "")) -> html
    Ok(#(key, "")) -> string_tree.append(html, " " <> key)
    Ok(#(key, val)) ->
      string_tree.append(html, " " <> key <> "=\"" <> escape(val) <> "\"")
    Error(_) -> html
  }
}

pub fn to_string_parts(attr: Attribute(msg)) -> Result(#(String, String), Nil) {
  case attr {
    Attribute(_, "", _) -> Error(Nil)
    Attribute(_, name, value) -> Ok(#(name, value))
    _ -> Error(Nil)
  }
}
