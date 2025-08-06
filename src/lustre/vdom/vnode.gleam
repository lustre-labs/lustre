// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/function
import gleam/json.{type Json}
import gleam/list
import gleam/string
import gleam/string_tree.{type StringTree}
import houdini
import lustre/internals/json_object_builder
import lustre/internals/mutable_map.{type MutableMap}
import lustre/vdom/vattr.{type Attribute}

// TYPES -----------------------------------------------------------------------

pub type Element(msg) {
  Fragment(
    kind: Int,
    key: String,
    mapper: fn(Dynamic) -> Dynamic,
    children: List(Element(msg)),
    keyed_children: MutableMap(String, Element(msg)),
  )

  Element(
    kind: Int,
    key: String,
    mapper: fn(Dynamic) -> Dynamic,
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

  Text(kind: Int, key: String, mapper: fn(Dynamic) -> Dynamic, content: String)

  UnsafeInnerHtml(
    kind: Int,
    key: String,
    mapper: fn(Dynamic) -> Dynamic,
    namespace: String,
    tag: String,
    //
    attributes: List(Attribute(msg)),
    inner_html: String,
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

pub const fragment_kind: Int = 0

pub fn fragment(
  key key: String,
  mapper mapper: fn(Dynamic) -> Dynamic,
  children children: List(Element(msg)),
  keyed_children keyed_children: MutableMap(String, Element(msg)),
) -> Element(msg) {
  Fragment(kind: fragment_kind, key:, mapper:, children:, keyed_children:)
}

pub const element_kind: Int = 1

pub fn element(
  key key: String,
  mapper mapper: fn(Dynamic) -> Dynamic,
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
    mapper:,
    namespace:,
    tag:,
    attributes: vattr.prepare(attributes),
    children:,
    keyed_children:,
    self_closing:,
    void: void || is_void_element(tag, namespace),
  )
}

fn is_void_element(tag: String, namespace: String) -> Bool {
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

pub fn text(
  key key: String,
  mapper mapper: fn(Dynamic) -> Dynamic,
  content content: String,
) -> Element(msg) {
  Text(kind: text_kind, key: key, mapper: mapper, content: content)
}

pub const unsafe_inner_html_kind: Int = 3

pub fn unsafe_inner_html(
  key key: String,
  mapper mapper: fn(Dynamic) -> Dynamic,
  namespace namespace: String,
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  inner_html inner_html: String,
) -> Element(msg) {
  UnsafeInnerHtml(
    kind: unsafe_inner_html_kind,
    key:,
    mapper:,
    namespace:,
    tag:,
    attributes: vattr.prepare(attributes),
    inner_html:,
  )
}

// MANIPULATION ----------------------------------------------------------------

pub fn to_keyed(key: String, node: Element(msg)) -> Element(msg) {
  case node {
    Element(..) -> Element(..node, key:)
    Text(..) -> Text(..node, key:)
    UnsafeInnerHtml(..) -> UnsafeInnerHtml(..node, key:)
    Fragment(..) -> Fragment(..node, key:)
  }
}

// ENCODERS --------------------------------------------------------------------

pub fn to_json(node: Element(msg)) -> Json {
  case node {
    Fragment(kind:, key:, children:, ..) ->
      fragment_to_json(kind, key, children)
    Element(kind:, key:, namespace:, tag:, attributes:, children:, ..) ->
      element_to_json(kind, key, namespace, tag, attributes, children)
    Text(kind:, key:, content:, ..) -> text_to_json(kind, key, content)
    UnsafeInnerHtml(kind:, key:, namespace:, tag:, attributes:, inner_html:, ..) ->
      unsafe_inner_html_to_json(
        kind,
        key,
        namespace,
        tag,
        attributes,
        inner_html,
      )
  }
}

fn fragment_to_json(kind, key, children) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.list("children", children, to_json)
  |> json_object_builder.build
}

fn element_to_json(kind, key, namespace, tag, attributes, children) {
  json_object_builder.tagged(kind)
  |> json_object_builder.string("key", key)
  |> json_object_builder.string("namespace", namespace)
  |> json_object_builder.string("tag", tag)
  |> json_object_builder.list("attributes", attributes, vattr.to_json)
  |> json_object_builder.list("children", children, to_json)
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

// STRING RENDERING ------------------------------------------------------------

pub fn to_string(node: Element(msg)) -> String {
  node
  |> to_string_tree
  |> string_tree.to_string
}

pub fn to_string_tree(node: Element(msg)) -> StringTree {
  case node {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) -> string_tree.from_string(houdini.escape(content))

    Fragment(children:, ..) ->
      children_to_string_tree(string_tree.new(), children)

    Element(key:, namespace:, tag:, attributes:, self_closing:, ..)
      if self_closing
    -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = vattr.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append("/>")
    }

    Element(key:, namespace:, tag:, attributes:, void:, ..) if void -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = vattr.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
    }

    Element(key:, namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = vattr.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> children_to_string_tree(children)
      |> string_tree.append("</" <> tag <> ">")
    }

    UnsafeInnerHtml(key:, namespace:, tag:, attributes:, inner_html:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = vattr.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append(inner_html)
      |> string_tree.append("</" <> tag <> ">")
    }
  }
}

fn children_to_string_tree(
  html: StringTree,
  children: List(Element(msg)),
) -> StringTree {
  use html, child <- list.fold(children, html)

  child
  |> to_string_tree
  |> string_tree.append_tree(html, _)
}

pub fn to_snapshot(node: Element(msg)) -> String {
  node
  |> do_to_snapshot_builder(False, 0)
  |> string_tree.to_string
}

fn do_to_snapshot_builder(
  node: Element(msg),
  raw_text: Bool,
  indent: Int,
) -> StringTree {
  let spaces = string.repeat("  ", indent)

  case node {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) if raw_text ->
      string_tree.from_strings([spaces, content])
    Text(content:, ..) ->
      string_tree.from_strings([spaces, houdini.escape(content)])

    Fragment(children: [], ..) -> string_tree.new()

    Fragment(children:, ..) ->
      string_tree.new()
      |> children_to_snapshot_builder(children, raw_text, indent)

    Element(key:, namespace:, tag:, attributes:, self_closing:, ..)
      if self_closing
    -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = vattr.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append("/>")
    }

    Element(key:, namespace:, tag:, attributes:, void:, ..) if void -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = vattr.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
    }

    Element(key:, namespace:, tag:, attributes:, children: [], ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = vattr.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append("</" <> tag <> ">")
    }

    Element(key:, namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = vattr.to_string_tree(key, namespace, attributes)
      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">\n")
      |> children_to_snapshot_builder(children, raw_text, indent + 1)
      |> string_tree.append(spaces)
      |> string_tree.append("</" <> tag <> ">")
    }

    UnsafeInnerHtml(key:, namespace:, tag:, attributes:, inner_html:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = vattr.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append(inner_html)
      |> string_tree.append("</" <> tag <> ">")
    }
  }
}

fn children_to_snapshot_builder(
  html: StringTree,
  children: List(Element(msg)),
  raw_text: Bool,
  indent: Int,
) -> StringTree {
  case children {
    [Text(content: a, ..), Text(content: b, ..), ..rest] ->
      children_to_snapshot_builder(
        html,
        [
          Text(
            kind: text_kind,
            key: "",
            mapper: function.identity,
            content: a <> b,
          ),
          ..rest
        ],
        raw_text,
        indent,
      )

    [Fragment(..) as child, ..rest] ->
      child
      |> do_to_snapshot_builder(raw_text, indent)
      |> string_tree.append_tree(html, _)
      |> children_to_snapshot_builder(rest, raw_text, indent)

    [child, ..rest] ->
      child
      |> do_to_snapshot_builder(raw_text, indent)
      |> string_tree.append("\n")
      |> string_tree.append_tree(html, _)
      |> children_to_snapshot_builder(rest, raw_text, indent)

    [] -> html
  }
}
