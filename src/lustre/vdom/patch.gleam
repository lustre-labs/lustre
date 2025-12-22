// IMPORTS ---------------------------------------------------------------------

import gleam/json.{type Json}
import lustre/internals/json_object_builder
import lustre/vdom/vattr.{type Attribute}
import lustre/vdom/vnode.{type Element, type Memos}

// TYPES -----------------------------------------------------------------------

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
/// > **Note**: when constructing a `Change` you should **always** use the provided
/// > constructors to ensure that the `kind` field is set correctly, never construct
/// > a variant directly.
///
pub type Change(msg) {
  // Self upadates:
  //
  ReplaceText(kind: Int, content: String)
  ReplaceInnerHtml(kind: Int, inner_html: String)
  Update(kind: Int, added: List(Attribute(msg)), removed: List(Attribute(msg)))

  // Keyed children changes:
  //
  /// Move a keyed child so that it is before the child at the given index.
  Move(kind: Int, key: String, before: Int)

  // Indexed children changes:
  //
  /// Replace a node at the given index with a new vnode.
  Replace(kind: Int, index: Int, with: Element(msg))
  /// Remove a child at the given index.
  Remove(kind: Int, index: Int)
  /// Insert one or multiple children before the child with the given index.
  Insert(kind: Int, children: List(Element(msg)), before: Int)
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

pub fn move(key key: String, before before: Int) -> Change(msg) {
  Move(kind: move_kind, key:, before:)
}

pub const remove_kind: Int = 4

pub fn remove(index index: Int) -> Change(msg) {
  Remove(kind: remove_kind, index:)
}

pub const replace_kind: Int = 5

pub fn replace(index index: Int, with with: Element(msg)) -> Change(msg) {
  Replace(kind: replace_kind, index:, with:)
}

pub const insert_kind: Int = 6

pub fn insert(
  children children: List(Element(msg)),
  before before: Int,
) -> Change(msg) {
  Insert(kind: insert_kind, children:, before:)
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

pub fn to_json(patch: Patch(msg), memos: Memos(msg)) -> Json {
  json_object_builder.new()
  |> json_object_builder.int("index", patch.index)
  |> json_object_builder.int("removed", patch.removed)
  |> json_object_builder.list("changes", patch.changes, fn(change) {
    change_to_json(change, memos)
  })
  |> json_object_builder.list("children", patch.children, fn(child) {
    to_json(child, memos)
  })
  |> json_object_builder.build
}

fn change_to_json(change: Change(msg), memos: Memos(msg)) -> Json {
  case change {
    ReplaceText(kind, content) -> replace_text_to_json(kind, content)
    ReplaceInnerHtml(kind, inner_html) ->
      replace_inner_html_to_json(kind, inner_html)
    Update(kind, added, removed) -> update_to_json(kind, added, removed)
    Move(kind, key, before) -> move_to_json(kind, key, before)
    Remove(kind, index) -> remove_to_json(kind, index)
    Replace(kind, index, with) -> replace_to_json(kind, index, with, memos)
    Insert(kind, children, before) ->
      insert_to_json(kind, children, before, memos)
  }
}

fn replace_text_to_json(kind, content) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("content", content)
  |> json_object_builder.build
}

fn replace_inner_html_to_json(kind, inner_html) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("inner_html", inner_html)
  |> json_object_builder.build
}

fn update_to_json(kind, added, removed) {
  json_object_builder.tagged(kind)
  |> json_object_builder.list("added", added, vattr.to_json)
  |> json_object_builder.list("removed", removed, vattr.to_json)
  |> json_object_builder.build
}

fn move_to_json(kind, key, before) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.int("before", before)
  |> json_object_builder.build
}

fn remove_to_json(kind, index) {
  json_object_builder.tagged(kind)
  |> json_object_builder.int("index", index)
  |> json_object_builder.build
}

fn replace_to_json(kind, index, with, memos) {
  json_object_builder.tagged(kind)
  |> json_object_builder.int("index", index)
  |> json_object_builder.json("with", vnode.to_json(with, memos))
  |> json_object_builder.build
}

fn insert_to_json(kind, children, before, memos) {
  json_object_builder.tagged(kind)
  |> json_object_builder.int("before", before)
  |> json_object_builder.list("children", children, vnode.to_json(_, memos))
  |> json_object_builder.build
}
