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

pub opaque type Cache(msg) {
  Cache(
    events: Events(msg),
    //
    vdoms: Memos(msg),
    old_vdoms: Memos(msg),
    //
    dispatched_paths: List(String),
    next_dispatched_paths: List(String),
  )
}

/// Event handlers form a tree.
///
/// Whenever we encounter a `Map` node, the transformation on the final
/// message type changes. Instead of transforming the handlers directly,
/// we generate isolated event subtrees. This keeps subtrees stable even
/// when a parent `Map` node updates.
///
/// This is necessary for Memo to function, since Memo does not update the old
/// events when its dependencies don't change.
///
/// 🚨 This means that the `msg` type in `handlers` is a lie until we actually
/// handle an evnet!
///
pub opaque type Events(msg) {
  Events(
    handlers: MutableMap(String, Decoder(Handler(msg))),
    children: MutableMap(String, Child(msg)),
  )
}

type Child(msg) {
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
pub fn new() -> Cache(msg) {
  Cache(
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

pub fn from_node(root: Element(msg)) -> Cache(msg) {
  let cache = new()
  let #(cache, events) = add_child(cache, cache.events, path.root, 0, root)
  Cache(..cache, events:)
}

pub fn tick(cache: Cache(msg)) -> Cache(msg) {
  Cache(
    events: cache.events,
    vdoms: mutable_map.new(),
    old_vdoms: cache.vdoms,
    dispatched_paths: cache.next_dispatched_paths,
    next_dispatched_paths: constants.empty_list,
  )
}

pub fn events(cache: Cache(msg)) -> Events(msg) {
  cache.events
}

pub fn update_events(cache: Cache(msg), events: Events(msg)) -> Cache(msg) {
  Cache(..cache, events:)
}

// MEMO MANIPULATIONS ---------------------------------------------------------

/// Get a dictionary of all materialised Memo views.
///
pub fn memos(cache: Cache(msg)) -> Memos(msg) {
  cache.vdoms
}

///
///
pub fn get_old_memo(
  cache: Cache(msg),
  old old: View(msg),
  new new: View(msg),
) -> Element(msg) {
  mutable_map.get_or_compute(cache.old_vdoms, old, new)
}

/// Reuses the cached element when dependencies are unchanged.
pub fn keep_memo(cache: Cache(msg), old old: View(msg), new new: View(msg)) {
  let node = mutable_map.get_or_compute(cache.old_vdoms, old, new)
  let vdoms = mutable_map.insert(cache.vdoms, new, node)
  Cache(..cache, vdoms:)
}

/// Caches a newly computed element when dependencies changed.
pub fn add_memo(
  cache: Cache(msg),
  new new: View(msg),
  node node: Element(msg),
) -> Cache(msg) {
  let vdoms = mutable_map.insert(cache.vdoms, new, node)
  Cache(..cache, vdoms:)
}

/// Gets the isolated event subtree for a Map node.
pub fn get_subtree(
  events: Events(msg),
  path: String,
  old_mapper old_mapper: Mapper,
) -> Events(msg) {
  let child =
    mutable_map.get_or_compute(events.children, path, fn() {
      Child(old_mapper, new_events())
    })

  child.events
}

/// Updates the Map node's isolated event subtree after diffing its child.
pub fn update_subtree(
  parent: Events(msg),
  path: String,
  mapper mapper: Mapper,
  events events: Events(msg),
) -> Events(msg) {
  let new_child = Child(mapper:, events:)
  let children = mutable_map.insert(parent.children, path, new_child)
  Events(..parent, children:)
}

// EVENTS MANIPULATIONS -------------------------------------------------------

pub fn add_event(
  events: Events(msg),
  path: Path,
  name: String,
  handler: Decoder(Handler(msg)),
) -> Events(msg) {
  let handlers = do_add_event(events.handlers, path, name, handler)
  Events(..events, handlers:)
}

fn do_add_event(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  path: Path,
  name: String,
  handler: Decoder(Handler(msg)),
) -> MutableMap(String, Decoder(Handler(msg))) {
  mutable_map.insert(handlers, path.event(path, name), handler)
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
  cache: Cache(msg),
  events: Events(msg),
  parent: Path,
  index: Int,
  child: Element(msg),
) -> #(Cache(msg), Events(msg)) {
  let children = [child, ..constants.empty_list]
  add_children(cache, events, parent, index, children)
}

fn add_attributes(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  path: Path,
  attributes: List(Attribute(msg)),
) -> MutableMap(String, Decoder(Handler(msg))) {
  use events, attribute <- list.fold(attributes, handlers)
  case attribute {
    Event(name:, handler:, ..) -> do_add_event(events, path, name, handler)
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
  cache: Cache(msg),
  events: Events(msg),
  path: Path,
  child_index: Int,
  nodes: List(Element(msg)),
) -> #(Cache(msg), Events(msg)) {
  let vdoms = cache.vdoms
  let Events(handlers:, children:) = events
  let AddedChildren(handlers:, children:, vdoms:) =
    do_add_children(handlers, children, vdoms, path, child_index, nodes)
  #(Cache(..cache, vdoms:), Events(handlers:, children:))
}

fn do_add_children(
  handlers: MutableMap(String, Decoder(Handler(msg))),
  children: MutableMap(String, Child(msg)),
  vdoms: Memos(msg),
  parent: Path,
  child_index: Int,
  nodes: List(Element(msg)),
) -> AddedChildren(msg) {
  let next = child_index + 1

  case nodes {
    [] -> AddedChildren(handlers:, children:, vdoms:)

    [Element(key:, attributes:, children: nodes, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let handlers = add_attributes(handlers, path, attributes)

      let AddedChildren(handlers:, children:, vdoms:) =
        do_add_children(handlers, children, vdoms, path, 0, nodes)

      do_add_children(handlers, children, vdoms, parent, next, rest)
    }

    [Fragment(key:, children: nodes, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let AddedChildren(handlers:, children:, vdoms:) =
        do_add_children(handlers, children, vdoms, path, 0, nodes)

      do_add_children(handlers, children, vdoms, parent, next, rest)
    }

    [UnsafeInnerHtml(key:, attributes:, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let handlers = add_attributes(handlers, path, attributes)

      do_add_children(handlers, children, vdoms, parent, next, rest)
    }

    [Map(key:, child:, mapper:, ..), ..rest] -> {
      let path = path.add(parent, child_index, key)

      let added =
        do_add_children(
          mutable_map.new(),
          mutable_map.new(),
          vdoms,
          path.subtree(path),
          0,
          [child, ..constants.empty_list],
        )

      let vdoms = added.vdoms
      let child_events =
        Events(handlers: added.handlers, children: added.children)
      let child = Child(mapper:, events: child_events)

      let children = mutable_map.insert(children, path.child(path), child)

      do_add_children(handlers, children, vdoms, parent, next, rest)
    }

    [Memo(view:, ..), ..rest] -> {
      let child_node = view()
      let vdoms = mutable_map.insert(vdoms, view, child_node)

      let next = child_index
      let rest = [child_node, ..rest]
      do_add_children(handlers, children, vdoms, parent, next, rest)
    }

    [Text(..), ..rest] ->
      do_add_children(handlers, children, vdoms, parent, next, rest)
  }
}

pub fn remove_child(
  cache: Cache(msg),
  events: Events(msg),
  parent: Path,
  child_index: Int,
  child: Element(msg),
) -> Events(msg) {
  do_remove_children(
    events.handlers,
    events.children,
    cache.old_vdoms,
    parent,
    child_index,
    [child, ..constants.empty_list],
  )
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
  vdoms: Memos(msg),
  parent: Path,
  index: Int,
  nodes: List(Element(msg)),
) -> Events(msg) {
  let next = index + 1

  case nodes {
    [] -> Events(handlers:, children:)

    [Element(key:, attributes:, children: nodes, ..), ..rest] -> {
      let path = path.add(parent, index, key)

      let handlers = remove_attributes(handlers, path, attributes)
      let Events(handlers:, children:) =
        do_remove_children(handlers, children, vdoms, path, 0, nodes)

      do_remove_children(handlers, children, vdoms, parent, next, rest)
    }

    [Fragment(key:, children: nodes, ..), ..rest] -> {
      let path = path.add(parent, index, key)

      let Events(handlers:, children:) =
        do_remove_children(handlers, children, vdoms, path, 0, nodes)

      do_remove_children(handlers, children, vdoms, parent, next, rest)
    }

    [UnsafeInnerHtml(key:, attributes:, ..), ..rest] -> {
      let path = path.add(parent, index, key)

      let handlers = remove_attributes(handlers, path, attributes)

      do_remove_children(handlers, children, vdoms, parent, next, rest)
    }

    [Map(key:, ..), ..rest] -> {
      let path = path.add(parent, index, key)

      let children = mutable_map.delete(children, path.child(path))

      do_remove_children(handlers, children, vdoms, parent, next, rest)
    }

    [Memo(view:, ..), ..rest] ->
      case mutable_map.has_key(vdoms, view) {
        True -> {
          let child = mutable_map.unsafe_get(vdoms, view)
          let nodes = [child, ..rest]
          // since we push a node here, we want to use index instead of next!
          do_remove_children(handlers, children, vdoms, parent, index, nodes)
        }
        False ->
          do_remove_children(handlers, children, vdoms, parent, next, rest)
      }

    [Text(..), ..rest] ->
      do_remove_children(handlers, children, vdoms, parent, next, rest)
  }
}

pub fn replace_child(
  cache: Cache(msg),
  events: Events(msg),
  parent: Path,
  child_index: Int,
  prev: Element(msg),
  next: Element(msg),
) -> #(Cache(msg), Events(msg)) {
  let events = remove_child(cache, events, parent, child_index, prev)
  add_child(cache, events, parent, child_index, next)
}

// QUERIES ---------------------------------------------------------------------

pub opaque type DecodedEvent(msg) {
  DecodedEvent(path: String, handler: Handler(msg))
  DispatchedEvent(path: String)
}

pub fn decode(cache: Cache(msg), path: String, name: String, event: Dynamic) {
  let parts = path.split_subtree_path(path <> path.separator_event <> name)

  case get_handler(cache.events, parts, function.identity) {
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
          Ok(
            decode.map(handler, fn(handler) {
              Handler(..handler, message: coerce(mapper)(handler.message))
            }),
          )
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

pub fn dispatch(cache: Cache(msg), event: DecodedEvent(msg)) {
  let next_dispatched_paths = [event.path, ..cache.next_dispatched_paths]
  let cache = Cache(..cache, next_dispatched_paths:)

  case event {
    DecodedEvent(handler:, path: _) -> #(cache, Ok(handler))
    DispatchedEvent(_) -> #(cache, constants.error_nil)
  }
}

///
///
pub fn handle(
  cache: Cache(msg),
  path: String,
  name: String,
  event: Dynamic,
) -> #(Cache(msg), Result(Handler(msg), Nil)) {
  decode(cache, path, name, event)
  |> dispatch(cache, _)
}

pub fn has_dispatched_events(cache: Cache(msg), path: Path) {
  path.matches(path, any: cache.dispatched_paths)
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b
