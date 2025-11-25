// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/function
import gleam/list
import lustre/internals/constants
import lustre/internals/mutable_map.{type MutableMap}
import lustre/vdom/path.{type Path}
import lustre/vdom/vattr.{type Attribute, type Handler, Event, Handler}
import lustre/vdom/vnode.{
  type Element, type Memos, type View, Element, Fragment, Map, Memo, Text,
  UnsafeInnerHtml,
}

// TYPES -----------------------------------------------------------------------

pub opaque type ConcreteTree(msg) {
  ConcreteTree(
    root: Events(msg),
    // 🚨 Because of the `mapper` shenanigans, the `msg` type parameter is
    // a lie until we actually handle an event!
    memos: MutableMap(View(msg), Events(msg)),
    vdoms: Memos(msg),
    old_vdoms: Memos(msg),
    //
    dispatched_paths: List(String),
    next_dispatched_paths: List(String),
  )
}

pub opaque type Events(msg) {
  Events(
    handlers: MutableMap(String, Decoder(Handler(msg))),
    children: MutableMap(String, Child(msg)),
  )
}

type Child(msg) {
  Child(view: View(msg), mapper: Mapper)
}

pub type Mapper =
  fn(Dynamic) -> Dynamic

// MAPPERS ---------------------------------------------------------------------

pub fn compose_mapper(mapper: Mapper, child_mapper: Mapper) -> Mapper {
  fn(msg) { mapper(child_mapper(msg)) }
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn new() -> ConcreteTree(msg) {
  ConcreteTree(
    root: Events(handlers: mutable_map.new(), children: mutable_map.new()),
    memos: mutable_map.new(),
    vdoms: mutable_map.new(),
    old_vdoms: mutable_map.new(),
    dispatched_paths: constants.empty_list,
    next_dispatched_paths: constants.empty_list,
  )
}

pub fn from_node(root: Element(msg)) -> ConcreteTree(msg) {
  let events = Events(handlers: mutable_map.new(), children: mutable_map.new())
  let events = add_child(events, function.identity, path.root, 0, root)
  ConcreteTree(..new(), root: events)
}

pub fn tick(events: ConcreteTree(msg)) -> ConcreteTree(msg) {
  ConcreteTree(
    ..events,
    vdoms: mutable_map.new(),
    old_vdoms: events.vdoms,
    dispatched_paths: events.next_dispatched_paths,
    next_dispatched_paths: constants.empty_list,
  )
}

// MEMO MANIPULATIONS ---------------------------------------------------------

pub fn root(tree: ConcreteTree(msg)) -> Events(msg) {
  tree.root
}

pub fn with_root(
  tree: ConcreteTree(msg),
  root: Events(msg),
) -> ConcreteTree(msg) {
  ConcreteTree(..tree, root:)
}

pub fn memos(tree: ConcreteTree(msg)) -> Memos(msg) {
  tree.vdoms
}

// EVENTS MANIPULATIONS -------------------------------------------------------

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

      handlers
      |> add_attributes(mapper, path, attributes)
      |> do_add_children(mapper, path, 0, children)
    }

    Fragment(children:, ..) -> {
      let path = path.add(parent, child_index, child.key)
      do_add_children(handlers, mapper, path, 0, children)
    }

    UnsafeInnerHtml(attributes:, ..) -> {
      let path = path.add(parent, child_index, child.key)
      add_attributes(handlers, mapper, path, attributes)
    }

    Map(element:, ..) -> {
      let composed_mapper = compose_mapper(mapper, child.mapper)
      do_add_child(handlers, composed_mapper, parent, child_index, element)
    }

    Memo(view:, ..) ->
      todo as "do_add_child(Memo) should query and store the materialised tree"

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

    Map(element:, ..) -> do_remove_child(handlers, parent, child_index, element)

    Memo(..) ->
      todo as "I don't think removing a memo node does anything since it delegates to another tree"

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

pub fn decode(
  tree: ConcreteTree(msg),
  path: String,
  name: String,
  event: Dynamic,
) {
  let parts = path.split_memo(path <> path.separator_event <> name)

  case get_handler(tree, tree.root, parts, function.identity) {
    Ok(handler) ->
      case decode.run(event, handler) {
        Ok(handler) -> DecodedEvent(handler:, path:)
        Error(_) -> DispatchedEvent(path:)
      }

    Error(_) -> DispatchedEvent(path:)
  }
}

fn get_handler(
  tree: ConcreteTree(msg),
  events: Events(msg),
  path: List(String),
  mapper: Mapper,
) {
  case path {
    [] -> constants.error_nil

    [key] ->
      case mutable_map.has_key(events.handlers, key) {
        False -> constants.error_nil

        True -> {
          let handler = mutable_map.unsafe_get(events.handlers, key)
          Ok(decode.map(handler, coerce(mapper)))
        }
      }

    [key, ..path] ->
      case mutable_map.has_key(events.children, key) {
        False -> constants.error_nil

        True -> {
          let child = mutable_map.unsafe_get(events.children, key)

          case mutable_map.has_key(tree.memos, child.view) {
            False -> constants.error_nil

            True -> {
              let events = mutable_map.unsafe_get(tree.memos, child.view)
              let mapper = compose_mapper(mapper, child.mapper)
              get_handler(tree, events, path, mapper)
            }
          }
        }
      }
  }
}

pub fn dispatch(events: ConcreteTree(msg), event: DecodedEvent(msg)) {
  let next_dispatched_paths = [event.path, ..events.next_dispatched_paths]
  let events = ConcreteTree(..events, next_dispatched_paths:)

  case event {
    DecodedEvent(handler:, path: _) -> #(events, Ok(handler))
    DispatchedEvent(_) -> #(events, constants.error_nil)
  }
}

///
///
pub fn handle(
  events: ConcreteTree(msg),
  path: String,
  name: String,
  event: Dynamic,
) -> #(ConcreteTree(msg), Result(Handler(msg), Nil)) {
  decode(events, path, name, event)
  |> dispatch(events, _)
}

pub fn has_dispatched_events(events: ConcreteTree(msg), path: Path) {
  path.matches(path, any: events.dispatched_paths)
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b
