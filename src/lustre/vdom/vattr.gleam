// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/list
import gleam/order.{type Order}
import gleam/string
import gleam/string_tree.{type StringTree}
import houdini
import lustre/internals/constants
import lustre/internals/json_object_builder

// TYPES -----------------------------------------------------------------------

pub type Attribute(msg) {
  Attribute(kind: Int, name: String, value: String)
  Property(kind: Int, name: String, value: Json)
  Event(
    kind: Int,
    name: String,
    handler: Decoder(Handler(msg)),
    include: List(String),
    prevent_default: EventBehaviour,
    stop_propagation: EventBehaviour,
    debounce: Int,
    throttle: Int,
  )
}

pub type Handler(msg) {
  Handler(prevent_default: Bool, stop_propagation: Bool, message: msg)
}

pub type EventBehaviour {
  Never(kind: Int)
  Possible(kind: Int)
  Always(kind: Int)
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
  handler handler: Decoder(Handler(msg)),
  include include: List(String),
  prevent_default prevent_default: EventBehaviour,
  stop_propagation stop_propagation: EventBehaviour,
  debounce debounce: Int,
  throttle throttle: Int,
) -> Attribute(msg) {
  Event(
    kind: event_kind,
    name:,
    handler:,
    include:,
    prevent_default:,
    stop_propagation:,
    debounce:,
    throttle:,
  )
}

pub const never_kind: Int = 0

pub const never: EventBehaviour = Never(kind: never_kind)

pub const possible_kind: Int = 1

pub const possible: EventBehaviour = Possible(kind: possible_kind)

pub const always_kind: Int = 2

pub const always: EventBehaviour = Always(kind: always_kind)

//

pub fn prepare(attributes: List(Attribute(msg))) -> List(Attribute(msg)) {
  case attributes {
    // empty attribute lists or attribute lists with only a single attribute are
    // always sorted and merged by definition, so we don't have to touch them.
    [] | [_] -> attributes

    _ ->
      attributes
      // Sort in reverse because `merge` will build the list in reverse anyway.
      |> list.sort(by: fn(a, b) { compare(b, a) })
      |> merge(constants.empty_list)
  }
}

pub fn merge(
  attributes: List(Attribute(msg)),
  merged: List(Attribute(msg)),
) -> List(Attribute(msg)) {
  case attributes {
    [] -> merged

    [Attribute(name: "", ..), ..rest]
    | [Attribute(kind: _, name: "class", value: ""), ..rest]
    | [Attribute(kind: _, name: "style", value: ""), ..rest] ->
      merge(rest, merged)

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

@external(javascript, "./vattr.ffi.mjs", "compare")
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
      debounce:,
      throttle:,
      ..,
    ) ->
      event_to_json(
        kind,
        name,
        include,
        prevent_default,
        stop_propagation,
        debounce,
        throttle,
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
  kind: Int,
  name: String,
  include: List(String),
  prevent_default: EventBehaviour,
  stop_propagation: EventBehaviour,
  debounce: Int,
  throttle: Int,
) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("name", name)
  |> json_object_builder.list("include", include, json.string)
  |> json_object_builder.object("prevent_default", {
    event_behaviour_to_json_builder(prevent_default)
  })
  |> json_object_builder.object("stop_propagation", {
    event_behaviour_to_json_builder(stop_propagation)
  })
  |> json_object_builder.int("debounce", debounce)
  |> json_object_builder.int("throttle", throttle)
  |> json_object_builder.build
}

fn event_behaviour_to_json_builder(
  behaviour: EventBehaviour,
) -> json_object_builder.Builder {
  case behaviour {
    Never(kind:) -> json_object_builder.tagged(kind)
    // If we are serialising an event behaviour, that must mean we're working with
    // server components. In that case it is impossible to conditionally trigger
    // an event behaviour so we can safely encode this as `never`.
    //
    // Doing so has benefits because the client runtime can recognise this event
    // handler as passive.
    Possible(..) -> json_object_builder.tagged(never_kind)
    Always(kind:) -> json_object_builder.tagged(kind)
  }
}

// STRING RENDERING ------------------------------------------------------------

pub fn to_string_tree(
  key: String,
  namespace: String,
  parent_namespace: String,
  attributes: List(Attribute(msg)),
) -> StringTree {
  let attributes = case key != "" {
    True -> [attribute("data-lustre-key", key), ..attributes]
    False -> attributes
  }

  let attributes = case namespace != parent_namespace {
    True if namespace == "" -> [
      attribute("xmlns", "http://www.w3.org/1999/xhtml"),
      ..attributes
    ]

    True -> [attribute("xmlns", namespace), ..attributes]

    False -> attributes
  }

  use html, attr <- list.fold(attributes, string_tree.new())

  case attr {
    // We special-case this "virtual" attribute to stringify as a regular `"value"`
    // attribute. In HTML, the default value of an input is set by this value
    // attribute, but in Lustre users would use the `attribute.value` function
    // for inputs that should be controlled by their model.
    Attribute(name: "virtual:defaultValue", value:, ..) ->
      string_tree.append(html, " value=\"" <> houdini.escape(value) <> "\"")

    Attribute(name: "virtual:defaultChecked", ..) ->
      string_tree.append(html, " checked")

    Attribute(name: "virtual:defaultSelected", ..) ->
      string_tree.append(html, " selected")

    Attribute(name: "", ..) -> html
    Attribute(name:, value: "", ..) -> string_tree.append(html, " " <> name)
    Attribute(name:, value:, ..) ->
      string_tree.append(html, {
        " " <> name <> "=\"" <> houdini.escape(value) <> "\""
      })
    _ -> html
  }
}
