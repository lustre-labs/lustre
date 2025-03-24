// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type DecodeError, type Decoder}
import gleam/function
import gleam/int
import gleam/list
import gleam/string
import lustre/internals/constants
import lustre/internals/mutable_map.{type MutableMap}
import lustre/vdom/attribute.{type Attribute, Event}
import lustre/vdom/node.{type Node, Element, Fragment, Text, UnsafeInnerHtml}

// TYPES -----------------------------------------------------------------------

pub type Events(msg) {
  Events(
    handlers: MutableMap(String, Decoder(msg)),
    dispatched_paths: List(String),
    next_dispatched_paths: List(String),
  )
}

pub type Mapper =
  fn(Dynamic) -> Dynamic

// MAPPERS ---------------------------------------------------------------------

pub fn compose_mapper(mapper: Mapper, child_mapper: Mapper) -> Mapper {
  case mapper == function.identity, child_mapper == function.identity {
    _, True -> mapper
    True, False -> child_mapper
    False, False -> fn(msg) { mapper(child_mapper(msg)) }
  }
}

fn apply_mapper(mapper: Mapper, handler: Decoder(msg)) -> Decoder(msg) {
  decode.map(handler, coerce(mapper))
}

// PATH ------------------------------------------------------------------------

pub fn child_path(path: String, index: Int, key: String) -> String {
  let child_part = case key {
    "" -> int.to_string(index)
    _ -> key
  }

  case path {
    "" -> child_part
    _ -> path <> "\f" <> child_part
  }
}

fn event_path(path: String, event_name: String) -> String {
  path <> "\f\f" <> event_name
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn new() -> Events(msg) {
  Events(
    handlers: mutable_map.new(),
    dispatched_paths: constants.empty_list,
    next_dispatched_paths: constants.empty_list,
  )
}

pub fn from_node(root: Node(msg)) -> Events(msg) {
  add_child(new(), function.identity, "", 0, root)
}

pub fn tick(events: Events(msg)) -> Events(msg) {
  Events(
    handlers: events.handlers,
    dispatched_paths: events.next_dispatched_paths,
    next_dispatched_paths: constants.empty_list,
  )
}

// MANIPULATIONS ---------------------------------------------------------------

pub fn add_event(
  events: Events(msg),
  mapper: Mapper,
  path: String,
  name: String,
  handler: Decoder(msg),
) -> Events(msg) {
  let handlers = do_add_event(events.handlers, mapper, path, name, handler)
  Events(..events, handlers:)
}

fn do_add_event(
  handlers: MutableMap(String, Decoder(msg)),
  mapper: fn(Dynamic) -> Dynamic,
  path: String,
  name: String,
  handler: Decoder(msg),
) -> MutableMap(String, Decoder(msg)) {
  mutable_map.insert(
    handlers,
    event_path(path, name),
    apply_mapper(mapper, handler),
  )
}

pub fn remove_event(
  events: Events(msg),
  path: String,
  name: String,
) -> Events(msg) {
  let handlers = do_remove_event(events.handlers, path, name)
  Events(..events, handlers:)
}

fn do_remove_event(
  handlers: MutableMap(String, msg),
  path: String,
  name: String,
) -> MutableMap(String, msg) {
  mutable_map.delete(handlers, event_path(path, name))
}

pub fn add_child(
  events: Events(msg),
  mapper: Mapper,
  parent: String,
  index: Int,
  child: Node(msg),
) -> Events(msg) {
  let handlers = do_add_child(events.handlers, mapper, parent, index, child)
  Events(..events, handlers:)
}

fn do_add_child(
  handlers: MutableMap(String, Decoder(msg)),
  mapper: Mapper,
  parent: String,
  child_index: Int,
  child: Node(msg),
) -> MutableMap(String, Decoder(msg)) {
  case child {
    Element(attributes:, children:, ..) -> {
      let path = child_path(parent, child_index, child.key)
      let composed_mapper = compose_mapper(mapper, child.mapper)

      handlers
      |> add_attributes(composed_mapper, path, attributes)
      |> do_add_children(composed_mapper, path, 0, children)
    }

    Fragment(children:, ..) -> {
      let composed_mapper = compose_mapper(mapper, child.mapper)
      // skip the fragment text node
      let child_index = child_index + 1
      do_add_children(handlers, composed_mapper, parent, child_index, children)
    }

    UnsafeInnerHtml(attributes:, ..) -> {
      let path = child_path(parent, child_index, child.key)
      let composed_mapper = compose_mapper(mapper, child.mapper)

      add_attributes(handlers, composed_mapper, path, attributes)
    }

    Text(..) -> handlers
  }
}

fn add_attributes(
  handlers: MutableMap(String, Decoder(msg)),
  mapper: fn(Dynamic) -> Dynamic,
  path: String,
  attributes: List(Attribute(msg)),
) -> MutableMap(String, Decoder(msg)) {
  use events, attribute <- list.fold(attributes, handlers)
  case attribute {
    Event(name:, handler:, ..) ->
      do_add_event(events, mapper, path, name, handler)
    _ -> events
  }
}

///
///
pub fn add_children(
  events: Events(msg),
  mapper: Mapper,
  path: String,
  child_index: Int,
  children: List(Node(msg)),
) -> Events(msg) {
  let handlers =
    do_add_children(events.handlers, mapper, path, child_index, children)
  Events(..events, handlers:)
}

fn do_add_children(
  handlers: MutableMap(String, Decoder(msg)),
  mapper: Mapper,
  path: String,
  child_index: Int,
  children: List(Node(msg)),
) -> MutableMap(String, Decoder(msg)) {
  case children {
    [] -> handlers

    [child, ..rest] ->
      handlers
      |> do_add_child(mapper, path, child_index, child)
      |> do_add_children(mapper, path, child_index + node.advance(child), rest)
  }
}

pub fn remove_child(
  events: Events(msg),
  parent: String,
  child_index: Int,
  child: Node(msg),
) -> Events(msg) {
  let handlers = do_remove_child(events.handlers, parent, child_index, child)
  Events(..events, handlers:)
}

fn do_remove_child(
  handlers: MutableMap(String, Decoder(msg)),
  parent: String,
  child_index: Int,
  child: Node(msg),
) -> MutableMap(String, Decoder(msg)) {
  case child {
    Element(attributes:, children:, ..) -> {
      let path = child_path(parent, child_index, child.key)

      handlers
      |> remove_attributes(path, attributes)
      |> do_remove_children(path, 0, children)
    }

    Fragment(children:, ..) -> {
      do_remove_children(handlers, parent, child_index + 1, children)
    }

    UnsafeInnerHtml(attributes:, ..) -> {
      let path = child_path(parent, child_index, child.key)
      remove_attributes(handlers, path, attributes)
    }

    Text(..) -> handlers
  }
}

fn remove_attributes(
  handlers: MutableMap(String, Decoder(msg)),
  path: String,
  attributes: List(Attribute(msg)),
) -> MutableMap(String, Decoder(msg)) {
  use events, attribute <- list.fold(attributes, handlers)
  case attribute {
    Event(name:, ..) -> do_remove_event(events, path, name)
    _ -> events
  }
}

fn do_remove_children(
  handlers: MutableMap(String, Decoder(msg)),
  path: String,
  child_index: Int,
  children: List(Node(msg)),
) -> MutableMap(String, Decoder(msg)) {
  case children {
    [] -> handlers

    [child, ..rest] ->
      handlers
      |> do_remove_child(path, child_index, child)
      |> do_remove_children(path, child_index + node.advance(child), rest)
  }
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn handle(
  events: Events(msg),
  path: String,
  name: String,
  event: Dynamic,
) -> #(Events(msg), Result(msg, List(DecodeError))) {
  let next_dispatched_paths = [path, ..events.next_dispatched_paths]
  let events = Events(..events, next_dispatched_paths:)

  case mutable_map.get(events.handlers, event_path(path, name)) {
    Ok(handler) -> #(events, decode.run(event, handler))
    Error(_) -> #(events, Error([]))
  }
}

pub fn has_dispatched_events(events: Events(msg), path: String) {
  use dispatch_path <- list.any(events.dispatched_paths)
  string.starts_with(path, dispatch_path)
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b
