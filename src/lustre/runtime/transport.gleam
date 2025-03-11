// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import lustre/vdom/attribute.{type Attribute, Attribute, Event, Property}
import lustre/vdom/diff.{
  type Change, type Patch, Insert, Move, Remove, RemoveKey, Replace,
  ReplaceInnerHtml, ReplaceText, Update,
}
import lustre/vdom/node.{type Node, Element, Fragment, Text, UnsafeInnerHtml}

// TYPES -----------------------------------------------------------------------

pub type ClientMessage(msg) {
  Mount(vdom: Node(msg))
  Reconcile(patch: Patch(msg))
  Emit(name: String, data: Json)
}

pub type ServerMessage {
  AttributesChanged(attributes: List(#(String, Dynamic)))
  EventFired(path: List(String), name: String, event: Dynamic)
}

// DECODERS --------------------------------------------------------------------
// SERVER MESSAGE DECODERS -----------------------------------------------------

pub fn server_message_decoder() -> Decoder(ServerMessage) {
  use tag <- decode.field(0, decode.int)

  case tag {
    _ if tag == attributes_changed_variant -> changed_attributes_decoder()
    _ if tag == event_fired_variant -> event_fired_decoder()
    _ -> decode.failure(AttributesChanged([]), "")
  }
}

fn changed_attributes_decoder() -> Decoder(ServerMessage) {
  use attributes <- decode.field(
    attributes_changed_attributes,
    decode.list({
      use name <- decode.field(0, decode.string)
      use value <- decode.field(1, decode.dynamic)

      decode.success(#(name, value))
    }),
  )

  decode.success(AttributesChanged(attributes))
}

fn event_fired_decoder() -> Decoder(ServerMessage) {
  use path <- decode.field(event_fired_name, decode.list(decode.string))
  use name <- decode.field(event_fired_path, decode.string)
  use event <- decode.field(event_fired_event, decode.dynamic)

  decode.success(EventFired(path, name, event))
}

// ENCODERS --------------------------------------------------------------------
// CLIENT MESSAGE ENCODERS -----------------------------------------------------

pub fn client_message_to_json(message: ClientMessage(msg)) -> Json {
  case message {
    Mount(vdom:) -> mount_to_json(vdom)
    Reconcile(patch:) -> reconcile_to_json(patch)
    Emit(name:, data:) -> emit_to_json(name, data)
  }
}

pub const mount_variant: Int = 0

pub const mount_vdom: Int = 1

fn mount_to_json(vdom: Node(msg)) -> Json {
  json.preprocessed_array([json.int(mount_variant), node_to_json(vdom)])
}

pub const reconcile_variant: Int = 1

pub const reconcile_patch: Int = 1

fn reconcile_to_json(patch: Patch(msg)) -> Json {
  json.preprocessed_array([json.int(reconcile_variant), patch_to_json(patch)])
}

pub const emit_variant: Int = 2

pub const emit_name: Int = 1

pub const emit_data: Int = 2

fn emit_to_json(name: String, data: Json) -> Json {
  json.preprocessed_array([json.int(emit_variant), json.string(name), data])
}

// SERVER MESSAGE ENCODERS -----------------------------------------------------

pub const attributes_changed_variant: Int = 0

pub const attributes_changed_attributes: Int = 1

pub const event_fired_variant: Int = 1

pub const event_fired_path: Int = 1

pub const event_fired_name: Int = 2

pub const event_fired_event: Int = 3

// NODE ENCODERS ---------------------------------------------------------------

pub fn node_to_json(node: Node(msg)) -> Json {
  case node {
    Fragment(key:, children:, ..) -> fragment_to_json(key, children)
    Element(key:, namespace:, tag:, attributes:, children:, ..) ->
      element_to_json(key, namespace, tag, attributes, children)
    UnsafeInnerHtml(key:, namespace:, tag:, attributes:, inner_html:, ..) ->
      unsafe_inner_html_to_json(key, namespace, tag, attributes, inner_html)
    Text(key:, content:, ..) -> text_to_json(key, content)
  }
}

pub const fragment_variant: Int = 0

pub const fragment_key: Int = 1

pub const fragment_children: Int = 2

fn fragment_to_json(key: String, children: List(Node(msg))) -> Json {
  json.preprocessed_array([
    json.int(fragment_variant),
    json.string(key),
    json.array(children, node_to_json),
  ])
}

pub const element_variant: Int = 1

pub const element_key: Int = 1

pub const element_namespace: Int = 2

pub const element_tag: Int = 3

pub const element_attributes: Int = 4

pub const element_children: Int = 5

fn element_to_json(
  key: String,
  namespace: String,
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(Node(msg)),
) -> Json {
  json.preprocessed_array([
    json.int(element_variant),
    json.string(key),
    json.string(namespace),
    json.string(tag),
    json.array(attributes, attribute_to_json),
    json.array(children, node_to_json),
  ])
}

pub const unsafe_inner_html_variant: Int = 2

pub const unsafe_inner_html_key: Int = 1

pub const unsafe_inner_html_namespace: Int = 2

pub const unsafe_inner_html_tag: Int = 3

pub const unsafe_inner_html_attributes: Int = 4

pub const unsafe_inner_html_inner_html: Int = 5

fn unsafe_inner_html_to_json(key, namespace, tag, attributes, inner_html) {
  json.preprocessed_array([
    json.int(unsafe_inner_html_variant),
    json.string(key),
    json.string(namespace),
    json.string(tag),
    json.array(attributes, attribute_to_json),
    json.string(inner_html),
  ])
}

pub const text_variant: Int = 3

pub const text_key: Int = 1

pub const text_content: Int = 2

fn text_to_json(key: String, content: String) -> Json {
  json.preprocessed_array([
    json.int(text_variant),
    json.string(key),
    json.string(content),
  ])
}

// ATTRIBUTE ENCODERS ----------------------------------------------------------

pub fn attribute_to_json(attribute: Attribute(msg)) -> Json {
  case attribute {
    Attribute(name:, value:) -> attribute_variant_to_json(name, value)
    Property(name:, value:) -> property_to_json(name, value)
    Event(name:, include:, prevent_default:, stop_propagation:, immediate:, ..) ->
      event_to_json(name, include, prevent_default, stop_propagation, immediate)
  }
}

pub const attribute_variant: Int = 0

pub const attribute_name: Int = 1

pub const attribute_value: Int = 2

fn attribute_variant_to_json(name: String, value: String) -> Json {
  json.preprocessed_array([
    json.int(attribute_variant),
    json.string(name),
    json.string(value),
  ])
}

pub const property_variant: Int = 1

pub const property_name: Int = 1

pub const property_value: Int = 2

fn property_to_json(name: String, value: Json) -> Json {
  json.preprocessed_array([json.int(property_variant), json.string(name), value])
}

pub const event_variant: Int = 2

pub const event_name: Int = 1

pub const event_include: Int = 2

pub const event_prevent_default: Int = 3

pub const event_stop_propagation: Int = 4

pub const event_immediate: Int = 5

fn event_to_json(
  name: String,
  include: List(String),
  prevent_default: Bool,
  stop_propagation: Bool,
  immediate: Bool,
) -> Json {
  json.preprocessed_array([
    json.int(event_variant),
    json.string(name),
    json.array(include, json.string),
    json.bool(prevent_default),
    json.bool(stop_propagation),
    json.bool(immediate),
  ])
}

// PATCH ENCODERS --------------------------------------------------------------

pub const patch_index: Int = 0

pub const patch_removed: Int = 1

pub const patch_changes: Int = 2

pub const patch_children: Int = 3

pub fn patch_to_json(patch: Patch(msg)) -> Json {
  json.preprocessed_array([
    json.int(patch.index),
    json.int(patch.removed),
    json.array(patch.changes, change_to_json),
    json.array(patch.children, patch_to_json),
  ])
}

// CHANGE ENCODERS -------------------------------------------------------------

fn change_to_json(change: Change(msg)) -> Json {
  case change {
    // node updates
    Replace(from:, count:, with:) -> replace_to_json(from, count, with)
    ReplaceText(content:) -> replace_text_to_json(content)
    ReplaceInnerHtml(inner_html:) -> replace_inner_html_to_json(inner_html)
    Update(added:, removed:) -> update_to_json(added, removed)
    // keyed changes
    Move(key:, before:, count:) -> move_to_json(key, before, count)
    RemoveKey(key:, count:) -> remove_key_to_json(key, count)
    // unkeyed changes
    Insert(children:, before:) -> insert_to_json(children, before)
    Remove(from:, count:) -> remove_to_json(from, count)
  }
}

pub const replace_variant: Int = 0

pub const replace_from: Int = 1

pub const replace_count: Int = 2

pub const replace_with: Int = 3

fn replace_to_json(from: Int, count: Int, with: Node(msg)) -> Json {
  json.preprocessed_array([
    json.int(replace_variant),
    json.int(from),
    json.int(count),
    node_to_json(with),
  ])
}

pub const replace_text_variant: Int = 1

pub const replace_text_content: Int = 1

fn replace_text_to_json(content: String) -> Json {
  json.preprocessed_array([json.int(replace_text_variant), json.string(content)])
}

pub const replace_inner_html_variant: Int = 2

pub const replace_inner_html_inner_html: Int = 1

fn replace_inner_html_to_json(inner_html: String) -> Json {
  json.preprocessed_array([
    json.int(replace_inner_html_variant),
    json.string(inner_html),
  ])
}

pub const update_variant: Int = 3

pub const update_added: Int = 1

pub const update_removed: Int = 2

fn update_to_json(
  added: List(Attribute(msg)),
  removed: List(Attribute(msg)),
) -> Json {
  json.preprocessed_array([
    json.int(update_variant),
    json.array(added, attribute_to_json),
    json.array(removed, attribute_to_json),
  ])
}

pub const move_variant: Int = 4

pub const move_key: Int = 1

pub const move_before: Int = 2

pub const move_count: Int = 3

fn move_to_json(key: String, before: Int, count: Int) -> Json {
  json.preprocessed_array([
    json.int(move_variant),
    json.string(key),
    json.int(before),
    json.int(count),
  ])
}

pub const remove_key_variant: Int = 5

pub const remove_key_key: Int = 1

pub const remove_key_count: Int = 2

fn remove_key_to_json(key: String, count: Int) -> Json {
  json.preprocessed_array([
    json.int(remove_key_variant),
    json.string(key),
    json.int(count),
  ])
}

pub const insert_variant: Int = 6

pub const insert_children: Int = 1

pub const insert_before: Int = 2

fn insert_to_json(children: List(Node(msg)), before: Int) -> Json {
  json.preprocessed_array([
    json.int(insert_variant),
    json.array(children, node_to_json),
    json.int(before),
  ])
}

pub const remove_variant: Int = 7

pub const remove_from: Int = 1

pub const remove_count: Int = 2

fn remove_to_json(from: Int, count: Int) -> Json {
  json.preprocessed_array([
    json.int(remove_variant),
    json.int(from),
    json.int(count),
  ])
}
