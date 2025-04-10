// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import lustre/vdom/patch.{type Patch}
import lustre/vdom/vnode.{type Node}

// TYPES -----------------------------------------------------------------------

pub type ClientMessage(msg) {
  Mount(
    kind: Int,
    open_shadow_root: Bool,
    will_adopt_styles: Bool,
    observed_attributes: List(String),
    observed_properties: List(String),
    vdom: Node(msg),
  )
  Reconcile(kind: Int, patch: Patch(msg))
  Emit(kind: Int, name: String, data: Json)
}

pub type ServerMessage {
  Batch(kind: Int, messages: List(ServerMessage))
  AttributeChanged(kind: Int, name: String, value: String)
  PropertyChanged(kind: Int, name: String, value: Dynamic)
  EventFired(kind: Int, path: String, name: String, event: Dynamic)
}

// CONSTRUCTORS ----------------------------------------------------------------

pub const mount_kind: Int = 0

pub fn mount(
  open_shadow_root: Bool,
  will_adopt_styles: Bool,
  observed_attributes: List(String),
  observed_properties: List(String),
  vdom: Node(msg),
) -> ClientMessage(msg) {
  Mount(
    kind: mount_kind,
    open_shadow_root:,
    will_adopt_styles:,
    observed_attributes:,
    observed_properties:,
    vdom:,
  )
}

pub const reconcile_kind: Int = 1

pub fn reconcile(patch patch: Patch(msg)) -> ClientMessage(msg) {
  Reconcile(kind: reconcile_kind, patch:)
}

pub const emit_kind: Int = 2

pub fn emit(name name: String, data data: Json) -> ClientMessage(msg) {
  Emit(kind: emit_kind, name:, data:)
}

pub const attribute_changed_kind: Int = 0

pub fn attribute_changed(
  name name: String,
  value value: String,
) -> ServerMessage {
  AttributeChanged(kind: attribute_changed_kind, name:, value:)
}

pub const event_fired_kind: Int = 1

pub fn event_fired(
  path path: String,
  name name: String,
  event event: Dynamic,
) -> ServerMessage {
  EventFired(kind: event_fired_kind, path:, name:, event:)
}

pub const property_changed_kind: Int = 2

pub fn property_changed(
  name name: String,
  value value: Dynamic,
) -> ServerMessage {
  PropertyChanged(kind: property_changed_kind, name:, value:)
}

pub const batch_kind: Int = 3

pub fn batch(messages messages: List(ServerMessage)) -> ServerMessage {
  Batch(kind: batch_kind, messages:)
}

// ENCODING --------------------------------------------------------------------

pub fn client_message_to_json(message: ClientMessage(msg)) -> Json {
  case message {
    Mount(
      kind:,
      open_shadow_root:,
      will_adopt_styles:,
      observed_attributes:,
      observed_properties:,
      vdom:,
    ) ->
      mount_to_json(
        kind,
        open_shadow_root,
        will_adopt_styles,
        observed_attributes,
        observed_properties,
        vdom,
      )
    Reconcile(kind:, patch:) -> reconcile_to_json(kind, patch)
    Emit(kind:, name:, data:) -> emit_to_json(kind, name, data)
  }
}

fn mount_to_json(
  kind: Int,
  open_shadow_root: Bool,
  will_adopt_styles: Bool,
  observed_attributes: List(String),
  observed_properties: List(String),
  vdom: Node(msg),
) -> Json {
  json.object([
    #("kind", json.int(kind)),
    #("open_shadow_root", json.bool(open_shadow_root)),
    #("will_adopt_styles", json.bool(will_adopt_styles)),
    #("observed_attributes", json.array(observed_attributes, json.string)),
    #("observed_properties", json.array(observed_properties, json.string)),
    #("vdom", vnode.to_json(vdom)),
  ])
}

fn reconcile_to_json(kind: Int, patch: Patch(msg)) -> Json {
  json.object([#("kind", json.int(kind)), #("patch", patch.to_json(patch))])
}

fn emit_to_json(kind: Int, name: String, data: Json) -> Json {
  json.object([
    #("kind", json.int(kind)),
    #("name", json.string(name)),
    #("data", data),
  ])
}

// DECODERS --------------------------------------------------------------------

pub fn server_message_decoder() -> Decoder(ServerMessage) {
  use kind <- decode.field("kind", decode.int)

  case kind {
    _ if kind == attribute_changed_kind -> attribute_changed_decoder()
    _ if kind == property_changed_kind -> property_changed_decoder()
    _ if kind == event_fired_kind -> event_fired_decoder()
    _ if kind == batch_kind -> batch_decoder()
    _ -> decode.failure(batch([]), "")
  }
}

fn attribute_changed_decoder() -> Decoder(ServerMessage) {
  use name <- decode.field("name", decode.string)
  use value <- decode.field("value", decode.string)

  decode.success(attribute_changed(name, value))
}

fn property_changed_decoder() -> Decoder(ServerMessage) {
  use name <- decode.field("name", decode.string)
  use value <- decode.field("value", decode.dynamic)

  decode.success(property_changed(name, value))
}

fn event_fired_decoder() -> Decoder(ServerMessage) {
  use path <- decode.field("path", decode.string)
  use name <- decode.field("name", decode.string)
  use event <- decode.field("event", decode.dynamic)

  decode.success(event_fired(path, name, event))
}

fn batch_decoder() -> Decoder(ServerMessage) {
  use messages <- decode.field(
    "messages",
    decode.list(server_message_decoder()),
  )

  decode.success(batch(messages))
}
