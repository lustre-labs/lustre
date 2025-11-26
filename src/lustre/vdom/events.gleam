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
  type Element, type Memos, Element, Fragment, Map, Memo, Text, UnsafeInnerHtml,
}

// TYPES -----------------------------------------------------------------------

pub type ConcreteTree(msg) {
  ConcreteTree(
    events: Events(msg),
    // 🚨 Because of the `mapper` shenanigans, the `msg` type parameter is
    // a lie until we actually handle an event!
    vdoms: Memos(msg),
    old_vdoms: Memos(msg),
    //
    dispatched_paths: List(String),
    next_dispatched_paths: List(String),
  )
}

pub type Events(msg) {
  Events(
    handlers: MutableMap(String, Decoder(Handler(msg))),
    children: MutableMap(String, Child(msg)),
  )
}

pub type Child(msg) {
  Child(
    mapper: Mapper,
    // 🚨 Because of the `mapper` shenanigans, the `msg` type parameter is
    // a lie until we actually handle an event!
    events: Events(msg),
  )
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
    events: new_events(),
    vdoms: mutable_map.new(),
    old_vdoms: mutable_map.new(),
    dispatched_paths: constants.empty_list,
    next_dispatched_paths: constants.empty_list,
  )
}

pub fn new_events() -> Events(msg) {
  Events(handlers: mutable_map.new(), children: mutable_map.new())
}

pub fn from_node(root: Element(msg)) -> ConcreteTree(msg) {
  let tree = new()
  let #(tree, events) =
    add_child(tree, tree.events, function.identity, path.root, 0, root)
  ConcreteTree(..tree, events:)
}

pub fn tick(tree: ConcreteTree(msg)) -> ConcreteTree(msg) {
  ConcreteTree(
    events: tree.events,
    vdoms: mutable_map.new(),
    old_vdoms: tree.vdoms,
    dispatched_paths: tree.next_dispatched_paths,
    next_dispatched_paths: constants.empty_list,
  )
}

// MEMO MANIPULATIONS ---------------------------------------------------------

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
  tree: ConcreteTree(msg),
  events: Events(msg),
  mapper: Mapper,
  parent: Path,
  index: Int,
  child: Element(msg),
) -> #(ConcreteTree(msg), Events(msg)) {
  let children = [child, ..constants.empty_list]
  add_children(tree, events, mapper, parent, index, children)
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

type AddedChildren(msg) {
  AddedChildren(
    handlers: MutableMap(String, Decoder(Handler(msg))),
    children: MutableMap(String, Child(msg)),
    vdoms: Memos(msg),
  )
}

///
///
pub fn add_children(
  tree: ConcreteTree(msg),
  events: Events(msg),
  mapper: Mapper,
  path: Path,
  child_index: Int,
  nodes: List(Element(msg)),
) -> #(ConcreteTree(msg), Events(msg)) {
  let vdoms = tree.vdoms
  let Events(handlers:, children:) = events
  let AddedChildren(handlers:, children:, vdoms:) =
    do_add_children(handlers, children, vdoms, mapper, path, child_index, nodes)
  #(ConcreteTree(..tree, vdoms:), Events(handlers:, children:))
}

fn do_add_children(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  children: MutableMap(String, Child(msg)),
  vdoms: Memos(msg),
  mapper: Mapper,
  parent: Path,
  child_index: Int,
  nodes: List(Element(msg)),
) -> AddedChildren(msg) {
  let next = child_index + 1

  case nodes {
    [] -> AddedChildren(handlers:, children:, vdoms:)

    [Element(key:, attributes:, children: nodes, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let handlers = add_attributes(handlers, mapper, path, attributes)

      let AddedChildren(handlers:, children:, vdoms:) =
        do_add_children(handlers, children, vdoms, mapper, path, 0, nodes)

      do_add_children(handlers, children, vdoms, mapper, parent, next, rest)
    }

    [Fragment(key:, children: nodes, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let AddedChildren(handlers:, children:, vdoms:) =
        do_add_children(handlers, children, vdoms, mapper, path, 0, nodes)

      do_add_children(handlers, children, vdoms, mapper, parent, next, rest)
    }

    [UnsafeInnerHtml(key:, attributes:, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let handlers = add_attributes(handlers, mapper, path, attributes)

      do_add_children(handlers, children, vdoms, mapper, parent, next, rest)
    }

    [Map(element:, mapper: child_mapper, ..), ..rest] -> {
      let mapper = compose_mapper(mapper, child_mapper)
      let rest = [element, ..rest]
      let next = child_index
      do_add_children(handlers, children, vdoms, mapper, parent, next, rest)
    }

    [Memo(key:, view:, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let child_node = view()
      let vdoms = mutable_map.insert(vdoms, view, child_node)

      let added =
        do_add_children(
          mutable_map.new(),
          mutable_map.new(),
          vdoms,
          function.identity,
          path.root,
          0,
          [child_node, ..constants.empty_list],
        )

      let vdoms = added.vdoms
      let child_events =
        Events(handlers: added.handlers, children: added.children)
      let child = Child(mapper:, events: child_events)
      let children = mutable_map.insert(children, path.child(path), child)

      do_add_children(handlers, children, vdoms, mapper, parent, next, rest)
    }

    [Text(..), ..rest] ->
      do_add_children(handlers, children, vdoms, mapper, parent, next, rest)
  }
}

pub fn remove_child(
  events: Events(msg),
  parent: Path,
  child_index: Int,
  child: Element(msg),
) -> Events(msg) {
  do_remove_children(events.handlers, events.children, parent, child_index, [
    child,
    ..constants.empty_list
  ])
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
  children: MutableMap(String, Child(msg)),
  parent: Path,
  child_index: Int,
  nodes: List(Element(msg)),
) -> Events(msg) {
  let next_index = child_index + 1

  case nodes {
    [] -> Events(handlers:, children:)

    [Element(key:, attributes:, children: nodes, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let handlers = remove_attributes(handlers, path, attributes)
      let Events(handlers:, children:) =
        do_remove_children(handlers, children, path, 0, nodes)

      do_remove_children(handlers, children, parent, next_index, rest)
    }

    [Fragment(key:, children: nodes, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let Events(handlers:, children:) =
        do_remove_children(handlers, children, path, 0, nodes)

      do_remove_children(handlers, children, parent, next_index, rest)
    }

    [UnsafeInnerHtml(key:, attributes:, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let handlers = remove_attributes(handlers, path, attributes)

      do_remove_children(handlers, children, parent, next_index, rest)
    }

    [Map(element:, ..), ..rest] -> {
      let nodes = [element, ..rest]
      // since we push a node here, we want to use child_index instead of next_index!
      do_remove_children(handlers, children, parent, child_index, nodes)
    }

    [Memo(key:, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let children = mutable_map.delete(children, path.child(path))

      do_remove_children(handlers, children, parent, next_index, rest)
    }

    [Text(..), ..rest] ->
      do_remove_children(handlers, children, parent, next_index, rest)
  }
}

pub fn replace_child(
  tree: ConcreteTree(msg),
  events: Events(msg),
  mapper: Mapper,
  parent: Path,
  child_index: Int,
  prev: Element(msg),
  next: Element(msg),
) -> #(ConcreteTree(msg), Events(msg)) {
  let events = remove_child(events, parent, child_index, prev)
  add_child(tree, events, mapper, parent, child_index, next)
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

  case get_handler(tree.events, parts, function.identity) {
    Ok(handler) ->
      case decode.run(event, handler) {
        Ok(handler) -> DecodedEvent(handler:, path:)
        Error(_) -> DispatchedEvent(path:)
      }

    Error(_) -> DispatchedEvent(path:)
  }
}

fn get_handler(events: Events(msg), path: List(String), mapper: Mapper) {
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
          let mapper = compose_mapper(mapper, child.mapper)
          get_handler(child.events, path, mapper)
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
