// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import lustre/vdom/node.{type Node}
import lustre/vdom/patch.{type Patch}

// TYPES -----------------------------------------------------------------------

pub type ClientMessage(msg) {
  Mount(kind: Int, vdom: Node(msg))
  Reconcile(kind: Int, patch: Patch(msg))
  Emit(kind: Int, name: String, data: Json)
}

pub type ServerMessage {
  AttributesChanged(kind: Int, attributes: List(#(String, Dynamic)))
  EventFired(kind: Int, path: List(String), name: String, event: Dynamic)
}

// CONSTRUCTORS ----------------------------------------------------------------

pub const mount_kind: Int = 0

pub fn mount(vdom vdom: Node(msg)) -> ClientMessage(msg) {
  Mount(kind: mount_kind, vdom:)
}

pub const reconcile_kind: Int = 1

pub fn reconcile(patch patch: Patch(msg)) -> ClientMessage(msg) {
  Reconcile(kind: reconcile_kind, patch:)
}

pub const emit_kind: Int = 2

pub fn emit(name name: String, data data: Json) -> ClientMessage(msg) {
  Emit(kind: emit_kind, name:, data:)
}

pub const attributes_changed_kind: Int = 0

pub fn attributes_changed(
  attributes attributes: List(#(String, Dynamic)),
) -> ServerMessage {
  AttributesChanged(kind: attributes_changed_kind, attributes:)
}

pub const event_fired_kind: Int = 1

pub fn event_fired(
  path path: List(String),
  name name: String,
  event event: Dynamic,
) -> ServerMessage {
  EventFired(kind: event_fired_kind, path:, name:, event:)
}

// ENCODING --------------------------------------------------------------------

pub fn client_message_to_json(message: ClientMessage(msg)) -> Json {
  case message {
    Mount(kind:, vdom:) -> mount_to_json(kind, vdom)
    Reconcile(kind:, patch:) -> reconcile_to_json(kind, patch)
    Emit(kind:, name:, data:) -> emit_to_json(kind, name, data)
  }
}

fn mount_to_json(kind: Int, vdom: Node(msg)) -> Json {
  json.object([#("kind", json.int(kind)), #("vdom", node.to_json(vdom))])
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
    _ if kind == attributes_changed_kind -> attributes_changed_decoder()
    _ if kind == event_fired_kind -> event_fired_decoder()
    _ -> decode.failure(attributes_changed([]), "")
  }
}

fn attributes_changed_decoder() -> Decoder(ServerMessage) {
  use attributes <- decode.field(
    "attributes",
    decode.list({
      use name <- decode.field(0, decode.string)
      use value <- decode.field(1, decode.dynamic)

      decode.success(#(name, value))
    }),
  )

  decode.success(attributes_changed(attributes))
}

fn event_fired_decoder() -> Decoder(ServerMessage) {
  use path <- decode.field("path", decode.list(decode.string))
  use name <- decode.field("name", decode.string)
  use event <- decode.field("event", decode.dynamic)

  decode.success(event_fired(path, name, event))
}
