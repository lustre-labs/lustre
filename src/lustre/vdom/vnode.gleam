// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/json.{type Json}
import gleam/list
import gleam/string
import gleam/string_tree.{type StringTree}
import houdini
import lustre/internals/json_object_builder
import lustre/internals/mutable_map.{type MutableMap}
import lustre/internals/ref.{type Ref}
import lustre/vdom/vattr.{type Attribute}

// TYPES -----------------------------------------------------------------------

pub type Element(msg) {
  Fragment(
    kind: Int,
    key: String,
    children: List(Element(msg)),
    keyed_children: MutableMap(String, Element(msg)),
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
    attributes: List(Attribute(msg)),
    children: List(Element(msg)),
    keyed_children: MutableMap(String, Element(msg)),
    // These two properties are only useful when rendering Elements to strings.
    // Certain HTML tags like <img> and <input> are called "void" elements,
    // which means they cannot have children and should not have a closing tag.
    // On the other hand, XML and SVG documents support self-closing tags like
    // <path /> and can *not* be void...
    //
    self_closing: Bool,
    void: Bool,
  )

  Text(kind: Int, key: String, content: String)

  UnsafeInnerHtml(
    kind: Int,
    key: String,
    namespace: String,
    tag: String,
    //
    attributes: List(Attribute(msg)),
    inner_html: String,
  )

  Map(
    kind: Int,
    key: String,
    mapper: fn(Dynamic) -> Dynamic,
    child: Element(msg),
  )

  Memo(kind: Int, key: String, dependencies: List(Ref), view: View(msg))
}

pub type Memos(msg) =
  mutable_map.MutableMap(View(msg), Element(msg))

pub type View(msg) =
  fn() -> Element(msg)

// CONSTRUCTORS ----------------------------------------------------------------

pub const fragment_kind: Int = 0

pub fn fragment(
  key key: String,
  children children: List(Element(msg)),
  keyed_children keyed_children: MutableMap(String, Element(msg)),
) -> Element(msg) {
  Fragment(kind: fragment_kind, key:, children:, keyed_children:)
}

pub const element_kind: Int = 1

pub fn element(
  key key: String,
  namespace namespace: String,
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(Element(msg)),
  keyed_children keyed_children: MutableMap(String, Element(msg)),
  self_closing self_closing: Bool,
  void void: Bool,
) -> Element(msg) {
  Element(
    kind: element_kind,
    key:,
    namespace:,
    tag:,
    attributes: vattr.prepare(attributes),
    children:,
    keyed_children:,
    self_closing:,
    void:,
  )
}

pub fn is_void_html_element(tag: String, namespace: String) -> Bool {
  case namespace {
    "" ->
      case tag {
        "area"
        | "base"
        | "br"
        | "col"
        | "embed"
        | "hr"
        | "img"
        | "input"
        | "link"
        | "meta"
        | "param"
        | "source"
        | "track"
        | "wbr" -> True
        _ -> False
      }
    _ -> False
  }
}

pub const text_kind: Int = 2

pub fn text(key key: String, content content: String) -> Element(msg) {
  Text(kind: text_kind, key: key, content: content)
}

pub const unsafe_inner_html_kind: Int = 3

pub fn unsafe_inner_html(
  key key: String,
  namespace namespace: String,
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  inner_html inner_html: String,
) -> Element(msg) {
  UnsafeInnerHtml(
    kind: unsafe_inner_html_kind,
    key:,
    namespace:,
    tag:,
    attributes: vattr.prepare(attributes),
    inner_html:,
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
  view view: fn() -> Element(msg),
) -> Element(msg) {
  Memo(kind: memo_kind, key:, dependencies:, view:)
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn coerce(a: a) -> b

// MANIPULATION ----------------------------------------------------------------

pub fn to_keyed(key: String, node: Element(msg)) -> Element(msg) {
  case node {
    Element(..) -> Element(..node, key:)
    Text(..) -> Text(..node, key:)
    UnsafeInnerHtml(..) -> UnsafeInnerHtml(..node, key:)
    Fragment(..) -> Fragment(..node, key:)
    // because we skip Memo nodes when encoding and reconciling, we have
    // to set the key on the memo (for the diff) as well as the inner node!
    Memo(view:, ..) -> Memo(..node, key:, view: fn() { to_keyed(key, view()) })
    // we don't skip Map nodes but I feel safer doing it anyways :-)
    Map(child:, ..) -> Map(..node, key:, child: to_keyed(key, child))
  }
}

// ENCODERS --------------------------------------------------------------------

pub fn to_json(node: Element(msg), memos: Memos(msg)) -> Json {
  case node {
    Fragment(kind:, key:, children:, ..) ->
      fragment_to_json(kind, key, children, memos)
    Element(kind:, key:, namespace:, tag:, attributes:, children:, ..) ->
      element_to_json(kind, key, namespace, tag, attributes, children, memos)
    Text(kind:, key:, content:) -> text_to_json(kind, key, content)
    UnsafeInnerHtml(kind:, key:, namespace:, tag:, attributes:, inner_html:) ->
      unsafe_inner_html_to_json(
        kind,
        key,
        namespace,
        tag,
        attributes,
        inner_html,
      )
    Map(kind:, key:, child:, ..) -> map_to_json(kind, key, child, memos)
    Memo(view:, ..) -> memo_to_json(view, memos)
  }
}

fn fragment_to_json(kind, key, children, memos) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.list("children", children, to_json(_, memos))
  |> json_object_builder.build
}

fn element_to_json(kind, key, namespace, tag, attributes, children, memos) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.string("namespace", namespace)
  |> json_object_builder.string("tag", tag)
  |> json_object_builder.list("attributes", attributes, vattr.to_json)
  |> json_object_builder.list("children", children, to_json(_, memos))
  |> json_object_builder.build
}

fn text_to_json(kind, key, content) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.string("content", content)
  |> json_object_builder.build
}

fn unsafe_inner_html_to_json(kind, key, namespace, tag, attributes, inner_html) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.string("namespace", namespace)
  |> json_object_builder.string("tag", tag)
  |> json_object_builder.list("attributes", attributes, vattr.to_json)
  |> json_object_builder.string("inner_html", inner_html)
  |> json_object_builder.build
}

fn memo_to_json(view, memos) {
  // Memo nodes are transparent during encoding - we encode their cached child.
  let child = mutable_map.get_or_compute(memos, view, view)
  to_json(child, memos)
}

// Map nodes are encoded with their child.
fn map_to_json(kind, key, child, memos) {
  // They mark the boundary of an isolated event subtree, so we need to tell the runtime
  // about them to make sure it can construct the correct event paths!
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.json("child", to_json(child, memos))
  |> json_object_builder.build
}

// STRING RENDERING ------------------------------------------------------------

pub fn to_string(node: Element(msg)) -> String {
  node
  |> to_string_tree("")
  |> string_tree.to_string
}

pub fn to_string_tree(
  node: Element(msg),
  parent_namespace: String,
) -> StringTree {
  case node {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) -> string_tree.from_string(houdini.escape(content))

    Element(key:, namespace:, tag:, attributes:, self_closing:, ..)
      if self_closing
    -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        vattr.to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append("/>")
    }

    Element(key:, namespace:, tag:, attributes:, void:, ..) if void -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        vattr.to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
    }

    Element(key:, namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        vattr.to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> children_to_string_tree(children, namespace)
      |> string_tree.append("</" <> tag <> ">")
    }

    UnsafeInnerHtml(key:, namespace:, tag:, attributes:, inner_html:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        vattr.to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append(inner_html)
      |> string_tree.append("</" <> tag <> ">")
    }

    Fragment(key:, children:, ..) -> {
      marker_comment("lustre:fragment", key)
      |> children_to_string_tree(children, parent_namespace)
      |> string_tree.append_tree(marker_comment("/lustre:fragment", ""))
    }

    Map(key:, child:, ..) -> {
      marker_comment("lustre:map", key)
      |> string_tree.append_tree(to_string_tree(child, parent_namespace))
    }

    Memo(key:, view:, ..) -> {
      marker_comment("lustre:memo", key)
      |> string_tree.append_tree(to_string_tree(view(), parent_namespace))
    }
  }
}

fn children_to_string_tree(
  html: StringTree,
  children: List(Element(msg)),
  namespace: String,
) -> StringTree {
  use html, child <- list.fold(children, html)
  string_tree.append_tree(html, to_string_tree(child, namespace))
}

pub fn to_snapshot(node: Element(msg), debug: Bool) -> String {
  do_to_snapshot_builder(node:, raw: False, debug:, namespace: "", indent: 0)
  |> string_tree.to_string
}

fn do_to_snapshot_builder(
  node node: Element(msg),
  raw raw: Bool,
  debug debug: Bool,
  namespace parent_namespace: String,
  indent indent: Int,
) -> StringTree {
  let spaces = string.repeat("  ", indent)

  case node {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) if raw -> string_tree.from_strings([spaces, content])
    Text(content:, ..) ->
      string_tree.from_strings([spaces, houdini.escape(content)])

    Element(key:, namespace:, tag:, attributes:, self_closing: True, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        vattr.to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append("/>")
    }

    Element(key:, namespace:, tag:, attributes:, void: True, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        vattr.to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
    }

    Element(key:, namespace:, tag:, attributes:, children: [], ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        vattr.to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append("</" <> tag <> ">")
    }

    Element(key:, namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        vattr.to_string_tree(key, namespace, parent_namespace, attributes)
      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">\n")
      |> children_to_snapshot_builder(
        children:,
        raw:,
        debug:,
        namespace:,
        indent: indent + 1,
      )
      |> string_tree.append(spaces)
      |> string_tree.append("</" <> tag <> ">")
    }

    UnsafeInnerHtml(key:, namespace:, tag:, attributes:, inner_html:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes =
        vattr.to_string_tree(key, namespace, parent_namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append(inner_html)
      |> string_tree.append("</" <> tag <> ">")
    }

    Fragment(key:, children:, ..) if debug -> {
      marker_comment("lustre:fragment", key)
      |> string_tree.prepend(spaces)
      |> string_tree.append("\n")
      |> children_to_snapshot_builder(
        children:,
        raw:,
        debug:,
        namespace: parent_namespace,
        indent: indent + 1,
      )
      |> string_tree.append(spaces)
      |> string_tree.append_tree(marker_comment("/lustre:fragment", ""))
    }

    Fragment(children:, ..) ->
      children_to_snapshot_builder(
        html: string_tree.new(),
        children: children,
        raw: raw,
        debug: debug,
        namespace: parent_namespace,
        indent: indent,
      )

    Map(key:, child:, ..) if debug -> {
      marker_comment("lustre:map", key)
      |> string_tree.prepend(spaces)
      |> string_tree.append("\n")
      |> string_tree.append_tree(do_to_snapshot_builder(
        node: child,
        raw:,
        debug:,
        namespace: parent_namespace,
        indent: indent + 1,
      ))
    }

    Map(child:, ..) ->
      do_to_snapshot_builder(
        node: child,
        raw:,
        debug:,
        namespace: parent_namespace,
        indent:,
      )

    Memo(key:, view:, ..) if debug -> {
      marker_comment("lustre:memo", key)
      |> string_tree.prepend(spaces)
      |> string_tree.append("\n")
      |> string_tree.append_tree(do_to_snapshot_builder(
        node: view(),
        raw:,
        debug:,
        namespace: parent_namespace,
        indent: indent + 1,
      ))
    }

    Memo(view:, ..) ->
      do_to_snapshot_builder(
        node: view(),
        raw:,
        debug:,
        namespace: parent_namespace,
        indent:,
      )
  }
}

fn children_to_snapshot_builder(
  html html: StringTree,
  children children: List(Element(msg)),
  raw raw: Bool,
  debug debug: Bool,
  namespace namespace: String,
  indent indent: Int,
) -> StringTree {
  case children {
    [Text(content: a, ..), Text(content: b, ..), ..rest] ->
      children_to_snapshot_builder(
        html:,
        children: [Text(kind: text_kind, key: "", content: a <> b), ..rest],
        raw:,
        debug:,
        namespace:,
        indent:,
      )

    [child, ..rest] ->
      child
      |> do_to_snapshot_builder(raw:, debug:, namespace:, indent:)
      |> string_tree.append("\n")
      |> string_tree.prepend_tree(html)
      |> children_to_snapshot_builder(
        children: rest,
        raw:,
        debug:,
        namespace:,
        indent:,
      )

    [] -> html
  }
}

fn marker_comment(label: String, key: String) {
  case key {
    "" -> string_tree.from_string("<!-- " <> label <> " -->")
    _ ->
      string_tree.from_string("<!-- " <> label <> " key=\"")
      |> string_tree.append(houdini.escape(key))
      |> string_tree.append("\" -->")
  }
}
