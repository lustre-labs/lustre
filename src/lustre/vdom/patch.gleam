// IMPORTS ---------------------------------------------------------------------

import gleam/json.{type Json}
import lustre/vdom/attribute.{type Attribute}
import lustre/vdom/events.{type Events}
import lustre/vdom/node.{type Node}

// TYPES -----------------------------------------------------------------------

///
///
pub type Diff(msg) {
  Diff(patch: Patch(msg), events: Events(msg))
}

/// A patch represents a set of precise changes needed to update the real DOM
/// to match a diffed VDOM. It is made up of four parts:
///
/// - An `index` which is the index of the node in the real DOM relative to its
///   parent's `childNodes` list.
///
/// - A `removed` count for the number of child nodes to remove *after* the list
///   of `changes` has been applied.
///
/// - A list of `changes` that modify the DOM node or the order of its children
///   in some way.
///
/// - A list of `children` patches that represents a traversal into the node's
///   children for further patching.
///
pub type Patch(msg) {
  Patch(
    index: Int,
    removed: Int,
    changes: List(Change(msg)),
    children: List(Patch(msg)),
  )
}

/// A `Change` represents a single modification to a DOM node (including re-ordering
/// or removing its children).
///
///
/// **Note**: when constructing a `Change` you should **always** use the provided
/// constructors to ensure that the `kind` field is set correctly, never construct
/// a variant directly.
///
pub type Change(msg) {
  // Self upadates:
  //
  ReplaceText(kind: Int, content: String)
  ReplaceInnerHtml(kind: Int, inner_html: String)
  Update(kind: Int, added: List(Attribute(msg)), removed: List(Attribute(msg)))

  // Keyed children changes:
  //
  /// Move a keyed child so that it is before the child at the given index. The
  /// `count` field is used to indicate how many children to move in the case of
  /// fragments.
  ///
  Move(kind: Int, key: String, before: Int, count: Int)
  /// Remove a keyed child with the given key. The `count` field is used to
  /// indicate how many children to remove in the case of fragments.
  ///
  RemoveKey(kind: Int, key: String, count: Int)

  // Indexed children changes:
  //
  /// Replace a node at the given index with a new node. The `count` field is
  /// used in cases where we're actually replacing a fragment: we need to know
  /// how many siblings to remove in the process.
  ///
  Replace(kind: Int, from: Int, count: Int, with: Node(msg))
  Insert(kind: Int, children: List(Node(msg)), before: Int)
  /// Remove `count` number of children starting at the given index. In most cases
  /// this is equivlaent to removing *all* children from the given index, but for
  /// fragments we need to know exactly how many children to remove so we don't
  /// trample on siblings outside the fragment.
  ///
  Remove(kind: Int, from: Int, count: Int)
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn new(
  index index: Int,
  removed removed: Int,
  changes changes: List(Change(msg)),
  children children: List(Patch(msg)),
) -> Patch(msg) {
  Patch(index:, removed:, changes:, children:)
}

pub const replace_text_kind: Int = 0

pub fn replace_text(content content: String) -> Change(msg) {
  ReplaceText(kind: replace_text_kind, content:)
}

pub const replace_inner_html_kind: Int = 1

pub fn replace_inner_html(inner_html inner_html: String) -> Change(msg) {
  ReplaceInnerHtml(kind: replace_inner_html_kind, inner_html:)
}

pub const update_kind: Int = 2

pub fn update(
  added added: List(Attribute(msg)),
  removed removed: List(Attribute(msg)),
) -> Change(msg) {
  Update(kind: update_kind, added:, removed:)
}

pub const move_kind: Int = 3

pub fn move(
  key key: String,
  before before: Int,
  count count: Int,
) -> Change(msg) {
  Move(kind: move_kind, key:, before:, count:)
}

pub const remove_key_kind: Int = 4

pub fn remove_key(key key: String, count count: Int) -> Change(msg) {
  RemoveKey(kind: remove_key_kind, key:, count:)
}

pub const replace_kind: Int = 5

pub fn replace(
  from from: Int,
  count count: Int,
  with with: Node(msg),
) -> Change(msg) {
  Replace(kind: replace_kind, from:, count:, with:)
}

pub const insert_kind: Int = 6

pub fn insert(
  children children: List(Node(msg)),
  before before: Int,
) -> Change(msg) {
  Insert(kind: insert_kind, children:, before:)
}

pub const remove_kind: Int = 7

pub fn remove(from from: Int, count count: Int) -> Change(msg) {
  Remove(kind: remove_kind, from:, count:)
}

// QUERIES ---------------------------------------------------------------------

pub fn is_empty(patch: Patch(msg)) -> Bool {
  case patch {
    Patch(removed: 0, changes: [], children: [], ..) -> True
    _ -> False
  }
}

// MANIPULATIONS ---------------------------------------------------------------

pub fn add_child(parent: Patch(msg), child: Patch(msg)) -> Patch(msg) {
  case is_empty(child) {
    True -> parent
    False -> Patch(..parent, children: [child, ..parent.children])
  }
}

// ENCODING --------------------------------------------------------------------

pub fn to_json(patch: Patch(msg)) -> Json {
  json.object([
    #("index", json.int(patch.index)),
    #("removed", json.int(patch.removed)),
    #("changes", json.array(patch.changes, change_to_json)),
    #("children", json.array(patch.children, to_json)),
  ])
}

fn change_to_json(change: Change(msg)) -> Json {
  case change {
    ReplaceText(kind, content) -> replace_text_to_json(kind, content)
    ReplaceInnerHtml(kind, inner_html) ->
      replace_inner_html_to_json(kind, inner_html)
    Update(kind, added, removed) -> update_to_json(kind, added, removed)
    Move(kind, key, before, count) -> move_to_json(kind, key, before, count)
    RemoveKey(kind, key, count) -> remove_key_to_json(kind, key, count)
    Replace(kind, from, count, with) -> replace_to_json(kind, from, count, with)
    Insert(kind, children, before) -> insert_to_json(kind, children, before)
    Remove(kind, from, count) -> remove_to_json(kind, from, count)
  }
}

fn replace_text_to_json(kind, content) {
  json.object([#("kind", json.int(kind)), #("content", json.string(content))])
}

fn replace_inner_html_to_json(kind, inner_html) {
  json.object([
    #("kind", json.int(kind)),
    #("inner_html", json.string(inner_html)),
  ])
}

fn update_to_json(kind, added, removed) {
  json.object([
    #("kind", json.int(kind)),
    #("added", json.array(added, attribute.to_json)),
    #("removed", json.array(removed, attribute.to_json)),
  ])
}

fn move_to_json(kind, key, before, count) {
  json.object([
    #("kind", json.int(kind)),
    #("key", json.string(key)),
    #("before", json.int(before)),
    #("count", json.int(count)),
  ])
}

fn remove_key_to_json(kind, key, count) {
  json.object([
    #("kind", json.int(kind)),
    #("key", json.string(key)),
    #("count", json.int(count)),
  ])
}

fn replace_to_json(kind, from, count, with) {
  json.object([
    #("kind", json.int(kind)),
    #("from", json.int(from)),
    #("count", json.int(count)),
    #("with", node.to_json(with)),
  ])
}

fn insert_to_json(kind, children, before) {
  json.object([
    #("kind", json.int(kind)),
    #("children", json.array(children, node.to_json)),
    #("before", json.int(before)),
  ])
}

fn remove_to_json(kind, from, count) {
  json.object([
    #("kind", json.int(kind)),
    #("from", json.int(from)),
    #("count", json.int(count)),
  ])
}
