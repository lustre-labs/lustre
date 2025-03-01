// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type DecodeError, type Decoder}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import lustre/internals/mutable_map.{type MutableMap}
import lustre/vdom/attribute.{type Attribute, Event}
import lustre/vdom/node.{type Node, Element, Fragment, Text}

// TYPES -----------------------------------------------------------------------

///
///
pub type Events(msg) {
  Events(
    handlers: MutableMap(String, Decoder(msg)),
    mapper: fn(Dynamic) -> Dynamic,
    children: MutableMap(Int, Events(msg)),
    keyed_children: MutableMap(String, Events(msg)),
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn new(mapper: fn(Dynamic) -> Dynamic) -> Events(msg) {
  Events(
    handlers: mutable_map.new(),
    mapper:,
    children: mutable_map.new(),
    keyed_children: mutable_map.new(),
  )
}

///
///
pub fn from_handlers(
  mapper: fn(Dynamic) -> Dynamic,
  handlers: MutableMap(String, Decoder(msg)),
) -> Events(msg) {
  Events(
    handlers:,
    mapper:,
    children: mutable_map.new(),
    keyed_children: mutable_map.new(),
  )
}

fn attributes_to_handlers(
  attributes: List(Attribute(msg)),
  mapper: fn(Dynamic) -> Dynamic,
  handlers: MutableMap(String, Decoder(msg)),
) -> Events(msg) {
  case attributes {
    [] ->
      Events(
        handlers:,
        mapper:,
        children: mutable_map.new(),
        keyed_children: mutable_map.new(),
      )

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
  Events(..parent, children: mutable_map.insert(parent.children, index, child))
}

pub fn add_keyed_child_events(
  parent: Events(msg),
  key: String,
  child: Events(msg),
) -> Events(msg) {
  Events(
    ..parent,
    keyed_children: mutable_map.insert(parent.keyed_children, key, child),
  )
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

      case key {
        "" ->
          Events(
            ..parent,
            children: mutable_map.insert(parent.children, index, child_events),
          )

        key ->
          Events(
            ..parent,
            keyed_children: mutable_map.insert(
              parent.keyed_children,
              key,
              child_events,
            ),
          )
      }
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
  path: List(Dynamic),
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

    [index_or_key, ..path] ->
      case decode.run(index_or_key, decode.int) {
        Ok(index) ->
          case mutable_map.get(events.children, index) {
            Ok(child) -> handle(child, path, name, event)
            Error(_) -> Error([])
          }

        Error(_) ->
          case decode.run(index_or_key, decode.string) {
            Ok(key) ->
              case mutable_map.get(events.keyed_children, key) {
                Ok(child) -> handle(child, path, name, event)
                Error(_) -> Error([])
              }
            Error(_) -> Error([])
          }
      }
  }
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b

///
///
pub fn is_empty(events: Events(msg)) -> Bool {
  mutable_map.is_empty(events.handlers)
  && mutable_map.is_empty(events.children)
  && mutable_map.is_empty(events.keyed_children)
}
