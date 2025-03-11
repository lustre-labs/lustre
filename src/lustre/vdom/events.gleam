// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type DecodeError, type Decoder}
import gleam/int
import gleam/option.{None, Some}
import gleam/result
import lustre/internals/mutable_map.{type MutableMap}
import lustre/vdom/attribute.{type Attribute, Event}
import lustre/vdom/node.{type Node, Element, Fragment, Text, UnsafeInnerHtml}

// TYPES -----------------------------------------------------------------------

/// A tree of event handlers.
///
pub type Events(msg) {
  Events(
    // It's possible for all events in a sub-tree to have the messages they
    // produced be mapped by some function. A naive implementation would require
    // a walk of the vdom to apply the mapping function to each decoder.
    //
    // Instead, we store the mapping function in the events tree itself and only
    // apply it when the event is handled.
    mapper: fn(Dynamic) -> Dynamic,
    // 🚨 Because of the `mapper` shenanigans above, the `msg`type parameter is
    // a lie until we actually handle an event!
    handlers: MutableMap(String, Decoder(msg)),
    children: MutableMap(String, Events(msg)),
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn new(mapper: fn(Dynamic) -> Dynamic) -> Events(msg) {
  Events(handlers: mutable_map.new(), mapper:, children: mutable_map.new())
}

pub fn from_attributes(
  attributes: List(Attribute(msg)),
  mapper: fn(Dynamic) -> Dynamic,
) {
  add_events(attributes, mapper, mutable_map.new())
}

fn add_events(
  attributes: List(Attribute(msg)),
  mapper: fn(Dynamic) -> Dynamic,
  handlers: MutableMap(String, Decoder(msg)),
) -> Events(msg) {
  case attributes {
    [] -> Events(handlers:, mapper:, children: mutable_map.new())

    [Event(..) as event, ..rest] ->
      handlers
      |> mutable_map.insert(event.name, event.handler)
      |> add_events(rest, mapper, _)

    [_, ..rest] -> add_events(rest, mapper, handlers)
  }
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn add_child(
  parent: Events(msg),
  mapper: fn(Dynamic) -> Dynamic,
  index: Int,
  child: Node(msg),
) -> Events(msg) {
  case child {
    Element(key:, attributes:, children:, ..) -> {
      let composed_mapper = case child.mapper {
        None -> mapper
        Some(child_mapper) -> fn(msg) { msg |> child_mapper |> mapper }
      }

      from_attributes(attributes, composed_mapper)
      |> add_children(composed_mapper, 0, children)
      |> add_child_events(parent, index, key, _)
    }

    Fragment(children:, ..) -> {
      let composed_mapper = case child.mapper {
        None -> mapper
        Some(child_mapper) -> fn(msg) { msg |> child_mapper |> mapper }
      }

      add_children(parent, composed_mapper, index + 1, children)
    }

    UnsafeInnerHtml(key:, attributes:, ..) -> {
      let composed_mapper = case child.mapper {
        None -> mapper
        Some(child_mapper) -> fn(msg) { msg |> child_mapper |> mapper }
      }

      from_attributes(attributes, composed_mapper)
      |> add_child_events(parent, index, key, _)
    }

    Text(..) -> parent
  }
}

///
///
pub fn add_child_events(
  events: Events(msg),
  index: Int,
  key: String,
  child_events: Events(msg),
) -> Events(msg) {
  case is_empty(child_events) {
    True -> events
    False if key != "" ->
      Events(
        ..events,
        children: mutable_map.insert(events.children, key, child_events),
      )

    False -> {
      let key = int.to_string(index)
      Events(
        ..events,
        children: mutable_map.insert(events.children, key, child_events),
      )
    }
  }
}

///
///
pub fn add_children(
  parent: Events(msg),
  mapper: fn(Dynamic) -> Dynamic,
  index: Int,
  children: List(Node(msg)),
) -> Events(msg) {
  case children {
    [] -> parent

    [child, ..rest] ->
      parent
      |> add_child(mapper, index, child)
      |> add_children(mapper, index + node.advance(child), rest)
  }
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn handle(
  events: Events(msg),
  path: List(String),
  name: String,
  event: Dynamic,
) -> Result(msg, List(DecodeError)) {
  case path {
    [] ->
      case mutable_map.get(events.handlers, name) {
        Ok(handler) ->
          event
          |> decode.run(handler)
          |> result.map(coerce(events.mapper))
        Error(_) -> Error([])
      }

    [key, ..path] ->
      case mutable_map.get(events.children, key) {
        Ok(child) -> handle(child, path, name, event)
        Error(_) -> Error([])
      }
  }
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b

///
///
pub fn is_empty(events: Events(msg)) -> Bool {
  mutable_map.is_empty(events.handlers) && mutable_map.is_empty(events.children)
}
