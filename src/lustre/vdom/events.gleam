// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type DecodeError, type Decoder}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import lustre/internals/mutable_map.{type MutableMap}
import lustre/vdom/attribute.{type Attribute, Event}
import lustre/vdom/node.{type Node, Element, Fragment, Text}

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

///
///
pub fn from_handlers(
  mapper: fn(Dynamic) -> Dynamic,
  handlers: MutableMap(String, Decoder(msg)),
) -> Events(msg) {
  Events(handlers:, mapper:, children: mutable_map.new())
}

fn attributes_to_handlers(
  attributes: List(Attribute(msg)),
  mapper: fn(Dynamic) -> Dynamic,
  handlers: MutableMap(String, Decoder(msg)),
) -> Events(msg) {
  case attributes {
    [] -> Events(handlers:, mapper:, children: mutable_map.new())

    [Event(..) as event, ..rest] ->
      handlers
      |> mutable_map.insert(event.name, event.handler)
      |> attributes_to_handlers(rest, mapper, _)

    [_, ..rest] -> attributes_to_handlers(rest, mapper, handlers)
  }
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn add_event_listener(
  events: Events(msg),
  name: String,
  handler: Decoder(msg),
) -> Events(msg) {
  Events(..events, handlers: mutable_map.insert(events.handlers, name, handler))
}

///
///
pub fn add_child_events(
  parent: Events(msg),
  index: Int,
  child: Events(msg),
) -> Events(msg) {
  Events(
    ..parent,
    children: mutable_map.insert(parent.children, int.to_string(index), child),
  )
}

pub fn add_keyed_child_events(
  parent: Events(msg),
  key: String,
  child: Events(msg),
) -> Events(msg) {
  Events(..parent, children: mutable_map.insert(parent.children, key, child))
}

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

      let child_events =
        attributes
        |> attributes_to_handlers(composed_mapper, parent.handlers)
        |> add_children(composed_mapper, index, children)

      Events(
        ..parent,
        children: mutable_map.insert(
          parent.children,
          case key {
            "" -> int.to_string(index)
            key -> key
          },
          child_events,
        ),
      )
    }

    Fragment(children:, ..) -> {
      let composed_mapper = case child.mapper {
        None -> mapper
        Some(child_mapper) -> fn(msg) { msg |> child_mapper |> mapper }
      }

      list.index_fold(children, parent, fn(parent, child, i) {
        add_child(parent, composed_mapper, index + i, child)
      })
    }

    Text(..) -> parent
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

    [Fragment(..) as fragment, ..rest] ->
      parent
      |> add_child(mapper, index, fragment)
      |> add_children(mapper, index + fragment.children_count, rest)

    [child, ..rest] ->
      parent
      |> add_child(mapper, index, child)
      |> add_children(mapper, index + 1, rest)
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
) -> Result(msg, List(DecodeError)) {
  path
  |> string.split(".")
  |> do_handle(events, _, name, event)
}

fn do_handle(
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
        Ok(child) -> do_handle(child, path, name, event)
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
