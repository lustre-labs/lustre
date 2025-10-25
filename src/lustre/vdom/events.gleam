// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/function
import gleam/list
import lustre/internals/constants
import lustre/internals/mutable_map.{type MutableMap}
import lustre/vdom/path.{type Path}
import lustre/vdom/vattr.{type Attribute, type Handler, Event, Handler}
import lustre/vdom/vnode.{type Element, Element, Fragment, Text, UnsafeInnerHtml}

// TYPES -----------------------------------------------------------------------

pub opaque type Events(msg) {
  Events(
    handlers: MutableMap(String, Decoder(Handler(msg))),
    dispatched_paths: List(String),
    next_dispatched_paths: List(String),
  )
}

pub type Mapper =
  fn(Dynamic) -> Dynamic

// MAPPERS ---------------------------------------------------------------------

pub fn compose_mapper(mapper: Mapper, child_mapper: Mapper) -> Mapper {
  case
    is_reference_equal(mapper, function.identity),
    is_reference_equal(child_mapper, function.identity)
  {
    _, True -> mapper
    True, False -> child_mapper
    False, False -> fn(msg) { mapper(child_mapper(msg)) }
  }
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

pub fn from_node(root: Element(msg)) -> Events(msg) {
  add_child(new(), function.identity, path.root, 0, root)
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
  path: Path,
  name: String,
  handler: Decoder(Handler(msg)),
) -> Events(msg) {
  let handlers = do_add_event(events.handlers, mapper, path, name, handler)
  Events(..events, handlers:)
}

fn do_add_event(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  mapper: fn(Dynamic) -> Dynamic,
  path: Path,
  name: String,
  handler: Decoder(Handler(msg)),
) -> MutableMap(String, Decoder(Handler(msg))) {
  mutable_map.insert(
    handlers,
    path.event(path, name),
    decode.map(handler, fn(handler) {
      Handler(..handler, message: coerce(mapper)(handler.message))
    }),
  )
}

pub fn remove_event(
  events: Events(msg),
  path: Path,
  name: String,
) -> Events(msg) {
  let handlers = do_remove_event(events.handlers, path, name)
  Events(..events, handlers:)
}

fn do_remove_event(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  path: Path,
  name: String,
) -> MutableMap(String, Decoder(Handler(msg))) {
  mutable_map.delete(handlers, path.event(path, name))
}

pub fn add_child(
  events: Events(msg),
  mapper: Mapper,
  parent: Path,
  index: Int,
  child: Element(msg),
) -> Events(msg) {
  let handlers = do_add_child(events.handlers, mapper, parent, index, child)
  Events(..events, handlers:)
}

fn do_add_child(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  mapper: Mapper,
  parent: Path,
  child_index: Int,
  child: Element(msg),
) -> MutableMap(String, Decoder(Handler(msg))) {
  case child {
    Element(attributes:, children:, ..) -> {
      let path = path.add(parent, child_index, child.key)
      let composed_mapper = compose_mapper(mapper, child.mapper)

      handlers
      |> add_attributes(composed_mapper, path, attributes)
      |> do_add_children(composed_mapper, path, 0, children)
    }

    Fragment(children:, ..) -> {
      let path = path.add(parent, child_index, child.key)
      let composed_mapper = compose_mapper(mapper, child.mapper)
      do_add_children(handlers, composed_mapper, path, 0, children)
    }

    UnsafeInnerHtml(attributes:, ..) -> {
      let path = path.add(parent, child_index, child.key)
      let composed_mapper = compose_mapper(mapper, child.mapper)

      add_attributes(handlers, composed_mapper, path, attributes)
    }

    Text(..) -> handlers
  }
}

fn add_attributes(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  mapper: fn(Dynamic) -> Dynamic,
  path: Path,
  attributes: List(Attribute(msg)),
) -> MutableMap(String, Decoder(Handler(msg))) {
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
  path: Path,
  child_index: Int,
  children: List(Element(msg)),
) -> Events(msg) {
  let handlers =
    do_add_children(events.handlers, mapper, path, child_index, children)
  Events(..events, handlers:)
}

fn do_add_children(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  mapper: Mapper,
  path: Path,
  child_index: Int,
  children: List(Element(msg)),
) -> MutableMap(String, Decoder(Handler(msg))) {
  case children {
    [] -> handlers

    [child, ..rest] ->
      handlers
      |> do_add_child(mapper, path, child_index, child)
      |> do_add_children(mapper, path, child_index + 1, rest)
  }
}

pub fn remove_child(
  events: Events(msg),
  parent: Path,
  child_index: Int,
  child: Element(msg),
) -> Events(msg) {
  let handlers = do_remove_child(events.handlers, parent, child_index, child)
  Events(..events, handlers:)
}

fn do_remove_child(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  parent: Path,
  child_index: Int,
  child: Element(msg),
) -> MutableMap(String, Decoder(Handler(msg))) {
  case child {
    Element(attributes:, children:, ..) -> {
      let path = path.add(parent, child_index, child.key)

      handlers
      |> remove_attributes(path, attributes)
      |> do_remove_children(path, 0, children)
    }

    Fragment(children:, ..) -> {
      let path = path.add(parent, child_index, child.key)
      do_remove_children(handlers, path, 0, children)
    }

    UnsafeInnerHtml(attributes:, ..) -> {
      let path = path.add(parent, child_index, child.key)
      remove_attributes(handlers, path, attributes)
    }

    Text(..) -> handlers
  }
}

fn remove_attributes(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  path: Path,
  attributes: List(Attribute(msg)),
) -> MutableMap(String, Decoder(Handler(msg))) {
  use events, attribute <- list.fold(attributes, handlers)
  case attribute {
    Event(name:, ..) -> do_remove_event(events, path, name)
    _ -> events
  }
}

fn do_remove_children(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  path: Path,
  child_index: Int,
  children: List(Element(msg)),
) -> MutableMap(String, Decoder(Handler(msg))) {
  case children {
    [] -> handlers

    [child, ..rest] ->
      handlers
      |> do_remove_child(path, child_index, child)
      |> do_remove_children(path, child_index + 1, rest)
  }
}

// QUERIES ---------------------------------------------------------------------

pub opaque type DecodedEvent(msg) {
  DecodedEvent(path: String, handler: Handler(msg))
  DispatchedEvent(path: String)
}

pub fn decode(events: Events(msg), path: String, name: String, event: Dynamic) {
  case mutable_map.get(events.handlers, path <> path.separator_event <> name) {
    Ok(handler) ->
      case decode.run(event, handler) {
        Ok(handler) -> DecodedEvent(handler:, path:)
        Error(_) -> DispatchedEvent(path:)
      }
    Error(_) -> DispatchedEvent(path:)
  }
}

pub fn dispatch(events: Events(msg), event: DecodedEvent(msg)) {
  let next_dispatched_paths = [event.path, ..events.next_dispatched_paths]
  let events = Events(..events, next_dispatched_paths:)

  case event {
    DecodedEvent(handler:, path: _) -> #(events, Ok(handler))
    DispatchedEvent(_) -> #(events, Error(Nil))
  }
}

///
///
pub fn handle(
  events: Events(msg),
  path: String,
  name: String,
  event: Dynamic,
) -> #(Events(msg), Result(Handler(msg), Nil)) {
  decode(events, path, name, event)
  |> dispatch(events, _)
}

pub fn has_dispatched_events(events: Events(msg), path: Path) {
  path.matches(path, any: events.dispatched_paths)
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b

@external(javascript, "../internals/equals.ffi.mjs", "isReferenceEqual")
fn is_reference_equal(a: a, b: a) -> Bool {
  a == b
}
