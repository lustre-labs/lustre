// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/string
import gleam/string_tree.{type StringTree}
import lustre/internals/constants
import lustre/internals/escape.{escape}
import lustre/vdom/attribute.{type Attribute, Attribute}

// TYPES -----------------------------------------------------------------------

pub type Node(msg) {
  Fragment(
    key: String,
    mapper: Option(fn(Dynamic) -> Dynamic),
    children: List(Node(msg)),
    // When encountering keyed children, we need to be able to differentiate
    // between these cases:
    //
    // - A child moved, so we need to access to the old child tree using its key
    // - A child got inserted, which means the key doesn't exist in the old tree
    // - A child got removed, which means the key doesn't exist in the new tree
    //
    // This requires us to have build a lookup table for every pair of trees we
    // diff. We therefore keep the lookup table on the node directly, meaning
    // we can re-use the old tree every tick.
    //
    // The table can constructed using the `vdom.to_keyed_children` function.
    keyed_children: Dict(String, Node(msg)),
    // When diffing Fragments, we need to know how many elements this fragment
    // spans when moving/deleting/updating it.
    children_count: Int,
  )

  Element(
    key: String,
    mapper: Option(fn(Dynamic) -> Dynamic),
    namespace: String,
    tag: String,
    //
    // To efficiently compare attributes during the diff, attribute are always
    // stored sorted. We do this while constructing the tree to not have to sort
    // the attribute in the previous tree again. The order does not matter, as
    // long as the new and old tree agree on the same order relation.
    //
    // When constructing a Node with attributes provided by a user, attributes
    // have to be sorted with the `vdom.prepare_attributes` function.
    attributes: List(Attribute(msg)),
    children: List(Node(msg)),
    // When encountering keyed children, we need to be able to differentiate
    // between these cases:
    //
    // - A child moved, so we need to access to the old child tree using its key
    // - A child got inserted, which means the key doesn't exist in the old tree
    // - A child got removed, which means the key doesn't exist in the new tree
    //
    // This requires us to have build a lookup table for every pair of trees we
    // diff. We therefore keep the lookup table on the node directly, meaning
    // we can re-use the old tree every tick.
    //
    // The table can constructed using the `vdom.to_keyed_children` function.
    keyed_children: Dict(String, Node(msg)),
    // These two properties are only useful when rendering Elements to strings.
    // Certain HTML tags like <img> and <input> are called "void" elements,
    // which means they cannot have children and should not have a closing tag.
    // On the other hand, XML and SVG documents support self-closing tags like
    // <path /> and can *not* be void...
    self_closing: Bool,
    void: Bool,
  )

  Text(key: String, mapper: Option(fn(Dynamic) -> Dynamic), content: String)

  UnsafeInnerHtml(
    key: String,
    mapper: Option(fn(Dynamic) -> Dynamic),
    namespace: String,
    tag: String,
    //
    attributes: List(Attribute(msg)),
    inner_html: String,
  )
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn advance(node: Node(msg)) {
  case node {
    Fragment(children_count:, ..) -> 1 + children_count
    _ -> 1
  }
}

// MANIPULATION ----------------------------------------------------------------

pub fn to_keyed(key: String, node: Node(msg)) -> Node(msg) {
  case node {
    Element(..) -> Element(..node, key:)
    Text(..) -> Text(..node, key:)
    UnsafeInnerHtml(..) -> UnsafeInnerHtml(..node, key:)
    Fragment(children:, ..) -> {
      let #(children, keyed_children) =
        set_fragment_key(
          key,
          children,
          0,
          constants.empty_list,
          constants.empty_dict(),
        )
      Fragment(..node, key:, children:, keyed_children:)
    }
  }
}

fn set_fragment_key(key, children, index, new_children, keyed_children) {
  case children {
    [] -> #(list.reverse(new_children), keyed_children)

    [Fragment(..) as node, ..children] if node.key == "" -> {
      // if the child of this fragment is an unkeyed fragment, we do not want to
      // give it a key itself, but make sure all the children have prefixed keys.
      let child_key = key <> "::" <> int.to_string(index)

      let #(node_children, node_keyed_children) =
        set_fragment_key(
          child_key,
          node.children,
          0,
          constants.empty_list,
          constants.empty_dict(),
        )

      let new_node =
        Fragment(
          ..node,
          children: node_children,
          keyed_children: node_keyed_children,
        )

      let new_children = [new_node, ..new_children]
      let index = index + 1

      set_fragment_key(key, children, index, new_children, keyed_children)
    }

    [node, ..children] if node.key != "" -> {
      let child_key = key <> "::" <> node.key
      let keyed_node = to_keyed(child_key, node)
      let new_children = [keyed_node, ..new_children]
      let keyed_children = dict.insert(keyed_children, child_key, keyed_node)
      let index = index + 1
      set_fragment_key(key, children, index, new_children, keyed_children)
    }

    [node, ..children] -> {
      let new_children = [node, ..new_children]
      let index = index + 1
      set_fragment_key(key, children, index, new_children, keyed_children)
    }
  }
}

// STRING RENDERING ------------------------------------------------------------

pub fn to_string(node: Node(msg)) -> String {
  node
  |> to_string_tree
  |> string_tree.to_string
}

pub fn to_string_tree(node: Node(msg)) -> StringTree {
  case node {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) -> string_tree.from_string(escape(content))

    Fragment(children:, ..) ->
      children_to_string_tree(string_tree.new(), children)

    Element(key:, namespace:, tag:, attributes:, self_closing:, ..)
      if self_closing
    -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = attribute.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append("/>")
    }

    Element(key:, namespace:, tag:, attributes:, void:, ..) if void -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = attribute.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
    }

    Element(key:, namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = attribute.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> children_to_string_tree(children)
      |> string_tree.append("</" <> tag <> ">")
    }

    UnsafeInnerHtml(key:, namespace:, tag:, attributes:, inner_html:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = attribute.to_string_tree(key, namespace, attributes)

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
  children: List(Node(msg)),
) -> StringTree {
  use html, child <- list.fold(children, html)

  child
  |> to_string_tree
  |> string_tree.append_tree(html, _)
}

pub fn to_snapshot(node: Node(msg)) -> String {
  node
  |> do_to_snapshot_builder(False, 0)
  |> string_tree.to_string
}

fn do_to_snapshot_builder(
  node: Node(msg),
  raw_text: Bool,
  indent: Int,
) -> StringTree {
  let spaces = string.repeat("  ", indent)

  case node {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) if raw_text ->
      string_tree.from_strings([spaces, content])
    Text(content:, ..) -> string_tree.from_strings([spaces, escape(content)])

    Fragment(children: [], ..) -> string_tree.new()

    Fragment(children:, ..) ->
      string_tree.new()
      |> children_to_snapshot_builder(children, raw_text, indent)

    Element(key, namespace:, tag:, attributes:, self_closing:, ..)
      if self_closing
    -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = attribute.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append("/>")
    }

    Element(key, namespace:, tag:, attributes:, void:, ..) if void -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = attribute.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
    }

    Element(key, namespace:, tag:, attributes:, children: [], ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = attribute.to_string_tree(key, namespace, attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append("</" <> tag <> ">")
    }

    Element(key, namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = attribute.to_string_tree(key, namespace, attributes)
      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">\n")
      |> children_to_snapshot_builder(children, raw_text, indent + 1)
      |> string_tree.append(spaces)
      |> string_tree.append("</" <> tag <> ">")
    }

    UnsafeInnerHtml(key, namespace:, tag:, attributes:, inner_html:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let attributes = attribute.to_string_tree(key, namespace, attributes)

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
  children: List(Node(msg)),
  raw_text: Bool,
  indent: Int,
) -> StringTree {
  case children {
    [Text(content: a, ..), Text(content: b, ..), ..rest] ->
      children_to_snapshot_builder(
        html,
        [Text(key: "", mapper: constants.option_none, content: a <> b), ..rest],
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
