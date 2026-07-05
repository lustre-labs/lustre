// IMPORTS ---------------------------------------------------------------------

import agnostic/internals/json_object_builder
import agnostic/internals/mutable_map.{type MutableMap}
import agnostic/internals/ref.{type Ref}
import agnostic/vdom/vattr.{type Attribute}
import gleam/dynamic.{type Dynamic}
import gleam/json.{type Json}
import gleam/option.{type Option}

// TYPES -----------------------------------------------------------------------

/// An external type for storing platform-specific raw content. For DOM platforms,
/// this holds HTML strings. For other platforms (like OpenTUI), this can hold
/// raw platform nodes. It's defined as external to avoid polluting the Element
/// type with another generic
pub type RawContent

/// Convert any value to a RawContent. This is the hack that make this work
@external(erlang, "gleam@function", "identity")
@external(javascript, "./vnode.ffi.mjs", "identity")
pub fn to_raw_content(value: a) -> RawContent

/// Convert RawContent back to a String. This should only be used when you know
/// the content is a String (e.g., in server-side HTML rendering).
@external(erlang, "gleam@function", "identity")
@external(javascript, "./vnode.ffi.mjs", "identity")
pub fn raw_content_to_string(content: RawContent) -> String

/// A comparator function for raw content. Used by the diff algorithm to
/// determine if raw content has changed when reference equality isn't sufficient.
pub type RawContentComparator =
  fn(RawContent, RawContent) -> Bool

pub type RawContentSerializer =
  fn(RawContent) -> String

pub type Element(message) {
  Fragment(
    kind: Int,
    key: String,
    children: List(Element(message)),
    keyed_children: MutableMap(String, Element(message)),
  )

  Element(
    kind: Int,
    key: String,
    namespace: String,
    tag: String,
    // To efficiently compare attributes during the diff, attribute are always
    // stored sorted. We do this while constructing the tree to not have to sort
    // the attribute in the previous tree again. The order does not matter, as
    // long as the new and old tree agree on the same order relation.
    //
    // When constructing a Node with attributes provided by a user, attributes
    // have to be sorted with the `attribute.prepare` function.
    //
    attributes: List(Attribute(message)),
    children: List(Element(message)),
    keyed_children: MutableMap(String, Element(message)),
  )

  Text(kind: Int, key: String, content: String)

  RawContainer(
    kind: Int,
    key: String,
    namespace: String,
    tag: String,
    //
    attributes: List(Attribute(message)),
    content: RawContent,
    compare: Option(RawContentComparator),
  )

  RawNode(
    kind: Int,
    key: String,
    content: RawContent,
    compare: Option(RawContentComparator),
  )

  Map(
    kind: Int,
    key: String,
    mapper: fn(Dynamic) -> Dynamic,
    child: Element(message),
  )

  Memo(kind: Int, key: String, dependencies: List(Ref), view: View(message))
}

pub type Memos(message) =
  mutable_map.MutableMap(View(message), Element(message))

pub type View(message) =
  fn() -> Element(message)

// CONSTRUCTORS ----------------------------------------------------------------

/// Create an empty keyed children map. Used internally when constructing
/// elements without keyed children.
///
pub fn empty_keyed_children() -> MutableMap(String, Element(message)) {
  mutable_map.new()
}

pub const fragment_kind: Int = 0

pub fn fragment(
  key key: String,
  children children: List(Element(message)),
  keyed_children keyed_children: MutableMap(String, Element(message)),
) -> Element(message) {
  Fragment(kind: fragment_kind, key:, children:, keyed_children:)
}

pub const element_kind: Int = 1

pub fn element(
  key key: String,
  namespace namespace: String,
  tag tag: String,
  attributes attributes: List(Attribute(message)),
  children children: List(Element(message)),
  keyed_children keyed_children: MutableMap(String, Element(message)),
) -> Element(message) {
  Element(
    kind: element_kind,
    key:,
    namespace:,
    tag:,
    attributes: vattr.prepare(attributes),
    children:,
    keyed_children:,
  )
}

pub const text_kind: Int = 2

pub fn text(key key: String, content content: String) -> Element(message) {
  Text(kind: text_kind, key: key, content: content)
}

pub const raw_container_kind: Int = 3

pub fn raw_container(
  key key: String,
  namespace namespace: String,
  tag tag: String,
  attributes attributes: List(Attribute(message)),
  content content: a,
  compare compare: Option(fn(a, a) -> Bool),
) -> Element(message) {
  // Convert generic comparator to RawContent comparator via coercion.
  // This is a hacky bypass
  let raw_compare =
    option.map(compare, fn(cmp) {
      fn(a: RawContent, b: RawContent) -> Bool { cmp(coerce(a), coerce(b)) }
    })

  RawContainer(
    kind: raw_container_kind,
    key:,
    namespace:,
    tag:,
    attributes: vattr.prepare(attributes),
    content: coerce(content),
    compare: raw_compare,
  )
}

pub const raw_node_kind: Int = 6

pub fn raw_node(
  key key: String,
  content content: a,
  compare compare: Option(fn(a, a) -> Bool),
) -> Element(message) {
  let raw_compare =
    option.map(compare, fn(cmp) {
      fn(a: RawContent, b: RawContent) -> Bool { cmp(coerce(a), coerce(b)) }
    })

  RawNode(
    kind: raw_node_kind,
    key:,
    content: coerce(content),
    compare: raw_compare,
  )
}

pub const map_kind: Int = 4

pub fn map(element: Element(a), mapper: fn(a) -> b) -> Element(b) {
  case element {
    Map(mapper: child_mapper, ..) ->
      Map(
        kind: map_kind,
        key: element.key,
        child: coerce(element.child),
        mapper: fn(handler) { coerce(mapper)(child_mapper(handler)) },
      )
    _ ->
      Map(
        kind: map_kind,
        key: element.key,
        mapper: coerce(mapper),
        child: coerce(element),
      )
  }
}

pub const memo_kind: Int = 5

pub fn memo(
  key key: String,
  dependencies dependencies: List(Ref),
  view view: fn() -> Element(message),
) -> Element(message) {
  Memo(kind: memo_kind, key:, dependencies:, view:)
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b

// MANIPULATION ----------------------------------------------------------------

pub fn to_keyed(key: String, node: Element(message)) -> Element(message) {
  case node {
    Element(..) -> Element(..node, key:)
    Text(..) -> Text(..node, key:)
    RawContainer(..) -> RawContainer(..node, key:)
    RawNode(..) -> RawNode(..node, key:)
    Fragment(..) -> Fragment(..node, key:)
    // because we skip Memo nodes when encoding and reconciling, we have
    // to set the key on the memo (for the diff) as well as the inner node!
    Memo(view:, ..) -> Memo(..node, key:, view: fn() { to_keyed(key, view()) })
    // we don't skip Map nodes but I feel safer doing it anyways :-)
    Map(child:, ..) -> Map(..node, key:, child: to_keyed(key, child))
  }
}

// ENCODERS --------------------------------------------------------------------

pub fn to_json(
  node: Element(message),
  memos: Memos(message),
  serialize_raw_content: RawContentSerializer,
) -> Json {
  case node {
    Fragment(kind:, key:, children:, ..) ->
      fragment_to_json(kind, key, children, memos, serialize_raw_content)
    Element(kind:, key:, namespace:, tag:, attributes:, children:, ..) ->
      element_to_json(
        kind,
        key,
        namespace,
        tag,
        attributes,
        children,
        memos,
        serialize_raw_content,
      )
    Text(kind:, key:, content:) -> text_to_json(kind, key, content)
    RawContainer(kind:, key:, namespace:, tag:, attributes:, content:, ..) ->
      raw_container_to_json(
        kind,
        key,
        namespace,
        tag,
        attributes,
        content,
        serialize_raw_content,
      )
    RawNode(kind:, key:, content:, ..) ->
      raw_node_to_json(kind, key, content, serialize_raw_content)
    Map(kind:, key:, child:, ..) ->
      map_to_json(kind, key, child, memos, serialize_raw_content)
    Memo(view:, ..) -> memo_to_json(view, memos, serialize_raw_content)
  }
}

fn fragment_to_json(kind, key, children, memos, serialize_raw_content) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.list("children", children, {
    to_json(_, memos, serialize_raw_content)
  })
  |> json_object_builder.build
}

fn element_to_json(
  kind,
  key,
  namespace,
  tag,
  attributes,
  children,
  memos,
  serialize_raw_content,
) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.string("namespace", namespace)
  |> json_object_builder.string("tag", tag)
  |> json_object_builder.list("attributes", attributes, vattr.to_json)
  |> json_object_builder.list("children", children, {
    to_json(_, memos, serialize_raw_content)
  })
  |> json_object_builder.build
}

fn text_to_json(kind, key, content) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.string("content", content)
  |> json_object_builder.build
}

fn raw_container_to_json(
  kind,
  key,
  namespace,
  tag,
  attributes,
  content,
  serialize_raw_content,
) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.string("namespace", namespace)
  |> json_object_builder.string("tag", tag)
  |> json_object_builder.list("attributes", attributes, vattr.to_json)
  |> json_object_builder.string("content", serialize_raw_content(content))
  |> json_object_builder.build
}

fn raw_node_to_json(kind, key, content, serialize_raw_content) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.string("content", serialize_raw_content(content))
  |> json_object_builder.build
}

fn memo_to_json(view, memos, serialize_raw_content) {
  // Memo nodes are transparent during encoding - we encode their cached child.
  let child = mutable_map.get_or_compute(memos, view, view)
  to_json(child, memos, serialize_raw_content)
}

// Map nodes are encoded with their child.
fn map_to_json(kind, key, child, memos, serialize_raw_content) {
  // They mark the boundary of an isolated event subtree, so we need to tell the runtime
  // about them to make sure it can construct the correct event paths!
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.json("child", {
    to_json(child, memos, serialize_raw_content)
  })
  |> json_object_builder.build
}
