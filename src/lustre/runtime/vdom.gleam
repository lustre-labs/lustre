// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option}
import gleam/string
import gleam/string_tree.{type StringTree}
import lustre/internals/escape.{escape}
import lustre/internals/keyed_lookup.{type KeyedLookup}

// ELEMENTS --------------------------------------------------------------------

pub type Element(msg) {
  Fragment(key: String, children: List(Element(msg)))
  Node(
    key: String,
    namespace: String,
    tag: String,
    attributes: List(Attribute(msg)),
    // We want the ability to map events produced by a node to other messages
    // before they are sent to the runtime. The most-obviously implementation
    // would involve wrapping every event handler in the mapping function but
    // this would produce a new function every render, which is wasteful.
    //
    // Instead we store this unsafe mapping function on the element itself which
    // we are free to call when an event is triggered. We're erasing the type of
    // the mapper here because otherwise the element type would have to be
    // `Element(to, from)` and would be confusing and unhelpful. Instead, we know
    // that mapped elements can only be produced by a type-safe api.
    //
    // Anyone reaching into a node and mucking with this function should expect
    // things to break!
    mapper: Option(fn(Dynamic) -> Dynamic),
    children: List(Element(msg)),
    // These two properties are only useful when rendering Elements to strings.
    // Certain HTML tags like <img> and <input> are called "void" elements,
    // which means they cannot have children and should not have a closing tag.
    // On the other hand, XML and SVG documents support self-closing tags like
    // <path /> and can *not* be void...
    self_closing: Bool,
    void: Bool,
  )
  Text(key: String, content: String)
}

pub type Attribute(msg) {
  Attribute(name: String, value: String)
  Property(name: String, value: Json)
  Event(
    name: String,
    handler: Decoder(msg),
    prevent_default: Bool,
    stop_propagation: Bool,
    immediate: Bool,
  )
}

// DIFFS -----------------------------------------------------------------------

pub type Diff(msg) {
  Diff(patch: Patch(msg), handlers: Dict(List(Int), Dict(String, Decoder(msg))))
}

pub type Patch(msg) {
  Patch(index: Int, changes: List(Change(msg)), children: List(Patch(msg)))
}

pub type Change(msg) {
  Append(children: List(Element(msg)))
  Insert(child: Element(msg), before: String)
  Map(fn(Dynamic) -> Dynamic)
  Move(key: String, before: String)
  RemoveAll(from: Int)
  RemoveKey(key: String)
  Remove(from: Int, count: Int)
  Replace(element: Element(msg))
  ReplaceText(content: String)
  Update(added: List(Attribute(msg)), removed: List(String))
}

pub fn diff(
  prev: Element(msg),
  next: Element(msg),
  handlers: Dict(List(Int), Dict(String, Decoder(msg))),
) -> Diff(msg) {
  do_diff(handlers, [
    Cursor(
      meta: Metadata(
        fragment: False,
        keyed: False,
        keyed_children: keyed_lookup.new(),
      ),
      node: Patch(0, changes: [], children: []),
      idx: 0,
      old: [prev],
      new: [next],
    ),
  ])
}

type Cursor(msg) {
  Cursor(
    meta: Metadata(msg),
    node: Patch(msg),
    idx: Int,
    old: List(Element(msg)),
    new: List(Element(msg)),
  )
}

type Metadata(msg) {
  Metadata(
    fragment: Bool,
    keyed: Bool,
    keyed_children: KeyedLookup(String, Element(msg)),
  )
}

fn do_diff(
  handlers: Dict(List(Int), Dict(String, Decoder(msg))),
  stack: List(Cursor(msg)),
) -> Diff(msg) {
  case stack {
    //
    [] -> Diff(patch: Patch(0, changes: [], children: []), handlers:)

    [Cursor(node:, old: [], new: [], ..)] -> Diff(patch: node, handlers:)

    [Cursor(node:, meta:, idx:, old: [], new: []), next, ..stack] ->
      case meta.fragment, node.changes, node.children {
        False, [], [] -> do_diff(handlers, [next, ..stack])

        False, _, _ -> {
          let children = [node, ..next.node.children]
          let parent = Patch(..next.node, children:)
          let next = Cursor(..next, node: parent)

          do_diff(handlers, [next, ..stack])
        }

        True, [], [] -> {
          let meta = next.meta
          let node = Patch(..node, index: next.node.index)
          let next = Cursor(..next, meta:, node:, idx:)

          do_diff(handlers, [next, ..stack])
        }

        True, _, _ -> {
          let meta = next.meta
          let node = Patch(..node, index: next.node.index)
          let next = Cursor(..next, meta:, node:, idx:)

          do_diff(handlers, [next, ..stack])
        }
      }

    [Cursor(meta:, node:, idx:, old:, new:) as cursor, ..stack] -> {
      case old, new {
        [], _ -> {
          let node = Patch(..node, changes: [Append(new), ..node.changes])
          let cursor = Cursor(..cursor, node:, new: [])

          do_diff(handlers, [cursor, ..stack])
        }

        _, [] if meta.keyed -> {
          let changes =
            keyed_lookup.remaining_keys(meta.keyed_children)
            |> list.fold(node.changes, fn(changes, key) {
              [RemoveKey(key), ..changes]
            })
          let node = Patch(..node, changes:)
          let cursor = Cursor(..cursor, node:, old: [])

          do_diff(handlers, [cursor, ..stack])
        }

        _, [] if meta.fragment -> {
          let changes = [
            Remove(from: idx, count: list.length(old)),
            ..node.changes
          ]
          let node = Patch(..node, changes:)
          let cursor = Cursor(..cursor, node:, old: [])

          do_diff(handlers, [cursor, ..stack])
        }

        _, [] -> {
          let changes = [RemoveAll(from: idx), ..node.changes]
          let node = Patch(..node, changes:)
          let cursor = Cursor(..cursor, node:, old: [])

          do_diff(handlers, [cursor, ..stack])
        }

        [prev, ..old], [next, ..new] if meta.keyed && prev.key != next.key -> {
          case keyed_lookup.pop(meta.keyed_children, next.key) {
            Ok(#(match, keyed_children)) -> {
              let old = [match, prev, ..old]
              // TODO: If the `next` node is a fragment this will throw everything
              // off.
              let changes = [
                Move(key: next.key, before: prev.key),
                ..node.changes
              ]
              let node = Patch(..node, changes:)
              let meta = Metadata(..meta, keyed_children:)
              let cursor =
                Cursor(..cursor, meta:, node:, old:, new: [next, ..new])

              do_diff(handlers, [cursor, ..stack])
            }

            Error(_) -> {
              let changes = [
                Insert(child: next, before: prev.key),
                ..node.changes
              ]
              let node = Patch(..node, changes: changes)
              let cursor =
                Cursor(meta:, node:, idx: idx + 1, old: [prev, ..old], new:)

              do_diff(handlers, [cursor, ..stack])
            }
          }
        }

        [Fragment(..) as prev, ..old], [Fragment(..) as next, ..new] -> {
          let child_meta = case prev.children {
            [head, ..] if head.key != "" ->
              Metadata(
                fragment: True,
                keyed: True,
                keyed_children: add_keyed_children(
                  keyed_lookup.new(),
                  prev.children,
                ),
              )

            _ ->
              Metadata(
                fragment: True,
                keyed: False,
                keyed_children: keyed_lookup.new(),
              )
          }
          let child_cursor =
            Cursor(
              node: Patch(..node, index: idx),
              meta: child_meta,
              idx: idx,
              old: prev.children,
              new: next.children,
            )
          let cursor = Cursor(..cursor, node:, old:, new:)

          do_diff(handlers, [child_cursor, cursor, ..stack])
        }

        [Node(..) as prev, ..old], [Node(..) as next, ..new]
          if prev.namespace == next.namespace && prev.tag == next.tag
        -> {
          let child_meta = case prev.children {
            [head, ..] if head.key != "" ->
              Metadata(
                fragment: False,
                keyed: True,
                keyed_children: add_keyed_children(
                  keyed_lookup.new(),
                  prev.children,
                ),
              )

            _ ->
              Metadata(
                fragment: False,
                keyed: False,
                keyed_children: keyed_lookup.new(),
              )
          }

          let child_cursor =
            Cursor(
              meta: child_meta,
              node: case diff_attributes(prev.attributes, next.attributes) {
                AttributeChange(added: [], removed: []) ->
                  Patch(idx, changes: [], children: [])

                AttributeChange(added:, removed:) ->
                  Patch(idx, changes: [Update(added:, removed:)], children: [])
              },
              idx: 0,
              old: prev.children,
              new: next.children,
            )
          let keyed_children =
            keyed_lookup.delete(meta.keyed_children, next.key)
          let meta = Metadata(..meta, keyed_children:)
          let cursor = Cursor(meta:, node:, idx: idx + 1, old:, new:)

          do_diff(handlers, [child_cursor, cursor, ..stack])
        }

        [Text(..) as prev, ..old], [Text(..) as next, ..new] ->
          case prev.content == next.content {
            True -> {
              let keyed_children =
                keyed_lookup.delete(meta.keyed_children, next.key)
              let meta = Metadata(..meta, keyed_children:)
              let cursor = Cursor(..cursor, meta:, idx: idx + 1, old:, new:)

              do_diff(handlers, [cursor, ..stack])
            }

            False -> {
              let changes = [ReplaceText(next.content)]
              let child = Patch(idx, changes:, children: [])
              let node = Patch(..node, children: [child, ..node.children])
              let keyed_children =
                keyed_lookup.delete(meta.keyed_children, next.key)
              let meta = Metadata(..meta, keyed_children:)
              let cursor = Cursor(meta:, node:, idx: idx + 1, old:, new:)

              do_diff(handlers, [cursor, ..stack])
            }
          }

        [_, ..old], [next, ..new] -> {
          let child = Patch(idx, changes: [Replace(next)], children: [])
          let node = Patch(..node, children: [child, ..node.children])
          let keyed_children =
            keyed_lookup.delete(meta.keyed_children, next.key)
          let meta = Metadata(..meta, keyed_children:)
          let cursor = Cursor(meta:, node:, idx: idx + 1, old:, new:)

          do_diff(handlers, [cursor, ..stack])
        }
      }
    }
  }
}

type AttributeChange(msg) {
  AttributeChange(added: List(Attribute(msg)), removed: List(String))
}

fn diff_attributes(
  prev: List(Attribute(msg)),
  next: List(Attribute(msg)),
) -> AttributeChange(msg) {
  case prev, next {
    [], [] -> AttributeChange(added: [], removed: [])
    [], _ -> AttributeChange(added: next, removed: [])
    _, [] ->
      AttributeChange(
        added: [],
        removed: list.map(prev, fn(attribute) { attribute.name }),
      )
    _, _ -> {
      let prev =
        list.fold(prev, dict.new(), fn(acc, attr) {
          dict.insert(acc, attr.name, attr)
        })

      do_diff_attributes(prev, next, [])
    }
  }
}

fn do_diff_attributes(
  prev: Dict(String, Attribute(msg)),
  next: List(Attribute(msg)),
  added: List(Attribute(msg)),
) -> AttributeChange(msg) {
  case next {
    [] -> AttributeChange(added:, removed: dict.keys(prev))
    [attr, ..rest] -> {
      case attr, dict.get(prev, attr.name) {
        _, Error(_) -> do_diff_attributes(prev, rest, [attr, ..added])

        Attribute(..), Ok(Attribute(..) as old) ->
          case attr.name {
            "value" | "checked" | "selected" ->
              do_diff_attributes(prev, rest, [attr, ..added])

            _ if attr.value == old.value ->
              do_diff_attributes(dict.delete(prev, attr.name), rest, added)

            _ ->
              do_diff_attributes(dict.delete(prev, attr.name), rest, [
                attr,
                ..added
              ])
          }

        Property(..), Ok(Property(..) as old) ->
          case attr.name {
            "value" | "checked" | "selected" | "scrollLeft" | "scrollTop" ->
              do_diff_attributes(prev, rest, [attr, ..added])

            _ if attr.value == old.value ->
              do_diff_attributes(dict.delete(prev, attr.name), rest, added)

            _ ->
              do_diff_attributes(dict.delete(prev, attr.name), rest, [
                attr,
                ..added
              ])
          }

        Event(..), Ok(Event(..) as old) if attr == old ->
          do_diff_attributes(dict.delete(prev, attr.name), rest, added)

        Event(..), Ok(Event(..)) ->
          do_diff_attributes(dict.delete(prev, attr.name), rest, [attr, ..added])

        _, Ok(_) -> do_diff_attributes(prev, rest, [attr, ..added])
      }
    }
  }
}

fn add_keyed_children(
  keyed_children: KeyedLookup(String, Element(msg)),
  children: List(Element(msg)),
) -> KeyedLookup(String, Element(msg)) {
  case children {
    [] -> keyed_children
    [child, ..] if child.key == "" -> keyed_children
    _ -> do_add_keyed_children(keyed_children, 0, children)
  }
}

fn do_add_keyed_children(
  keyed_children: KeyedLookup(String, Element(msg)),
  index: Int,
  children: List(Element(msg)),
) -> KeyedLookup(String, Element(msg)) {
  case children {
    [] -> keyed_children
    [child, ..rest] ->
      keyed_children
      |> keyed_lookup.set(child.key, child)
      |> do_add_keyed_children(index, rest)
  }
}

pub fn element_to_string(element: Element(msg)) -> String {
  element
  |> do_element_to_string_builder(False)
  |> string_tree.to_string
}

pub fn element_to_string_builder(element: Element(msg)) -> StringTree {
  do_element_to_string_builder(element, False)
}

fn do_element_to_string_builder(
  element: Element(msg),
  raw_text: Bool,
) -> StringTree {
  case element {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) if raw_text -> string_tree.from_string(content)
    Text(content:, ..) -> string_tree.from_string(escape(content))

    Fragment(children:, ..) ->
      children_to_string_builder(string_tree.new(), children, raw_text)

    Node(namespace:, tag:, attributes:, self_closing:, ..) if self_closing -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attributes, _) =
        attributes_to_string_builder(case namespace {
          "" -> attributes
          _ -> [Attribute("xmlns", namespace), ..attributes]
        })

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append("/>")
    }

    Node(namespace:, tag:, attributes:, void:, ..) if void -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attributes, _) =
        attributes_to_string_builder(case namespace {
          "" -> attributes
          _ -> [Attribute("xmlns", namespace), ..attributes]
        })

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
    }

    // Style and script tags are special beacuse they need to contain unescape
    // text content and not escaped HTML content.
    Node(
      namespace: "",
      tag: "style" as tag,
      attributes:,
      children:,
      self_closing: False,
      void: False,
      ..,
    )
    | Node(
        namespace: "",
        tag: "script" as tag,
        attributes:,
        children:,
        self_closing: False,
        void: False,
        ..,
      ) -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attributes, _) = attributes_to_string_builder(attributes)

      html
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> children_to_string_builder(children, True)
      |> string_tree.append("</" <> tag <> ">")
    }

    Node(namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attributes, inner_html) =
        attributes_to_string_builder(case namespace {
          "" -> attributes
          _ -> [Attribute("xmlns", namespace), ..attributes]
        })

      case inner_html {
        "" ->
          html
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">")
          |> children_to_string_builder(children, raw_text)
          |> string_tree.append("</" <> tag <> ">")
        _ ->
          html
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">" <> inner_html <> "</" <> tag <> ">")
      }
    }
  }
}

fn children_to_string_builder(
  html: StringTree,
  children: List(Element(msg)),
  raw_text: Bool,
) -> StringTree {
  use html, child <- list.fold(children, html)

  child
  |> do_element_to_string_builder(raw_text)
  |> string_tree.append_tree(html, _)
}

pub fn element_to_snapshot(element: Element(msg)) -> String {
  element
  |> do_element_to_snapshot_builder(False, 0)
  |> string_tree.to_string
}

fn do_element_to_snapshot_builder(
  element: Element(msg),
  raw_text: Bool,
  indent: Int,
) -> StringTree {
  let spaces = string.repeat("  ", indent)

  case element {
    Text(content: "", ..) -> string_tree.new()
    Text(content:, ..) if raw_text ->
      string_tree.from_strings([spaces, content])
    Text(content:, ..) -> string_tree.from_strings([spaces, escape(content)])

    Fragment(children: [], ..) -> string_tree.new()
    Fragment(children:, ..) ->
      children_to_snapshot_builder(
        string_tree.new(),
        children,
        raw_text,
        indent,
      )

    Node(namespace:, tag:, attributes:, self_closing:, ..) if self_closing -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attributes, _) =
        attributes_to_string_builder(case namespace {
          "" -> attributes
          _ -> [Attribute("xmlns", namespace), ..attributes]
        })

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append("/>")
    }

    Node(namespace:, tag:, attributes:, void:, ..) if void -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attributes, _) =
        attributes_to_string_builder(case namespace {
          "" -> attributes
          _ -> [Attribute("xmlns", namespace), ..attributes]
        })

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
    }

    Node(namespace: "", tag:, attributes:, children: [], ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attributes, _) = attributes_to_string_builder(attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> string_tree.append("</" <> tag <> ">")
    }

    // Style and script tags are special beacuse they need to contain unescape
    // text content and not escaped HTML content.
    Node(
      namespace: "",
      tag: "style" as tag,
      attributes:,
      children:,
      self_closing: False,
      void: False,
      ..,
    )
    | Node(
        namespace: "",
        tag: "script" as tag,
        attributes:,
        children:,
        self_closing: False,
        void: False,
        ..,
      ) -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attributes, _) = attributes_to_string_builder(attributes)

      html
      |> string_tree.prepend(spaces)
      |> string_tree.append_tree(attributes)
      |> string_tree.append(">")
      |> children_to_snapshot_builder(children, True, indent + 1)
      |> string_tree.append(spaces)
      |> string_tree.append("</" <> tag <> ">")
    }

    Node(namespace:, tag:, attributes:, children:, ..) -> {
      let html = string_tree.from_string("<" <> tag)
      let #(attributes, inner_html) =
        attributes_to_string_builder(case namespace {
          "" -> attributes
          _ -> [Attribute("xmlns", namespace), ..attributes]
        })

      case inner_html {
        "" ->
          html
          |> string_tree.prepend(spaces)
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">\n")
          |> children_to_snapshot_builder(children, raw_text, indent + 1)
          |> string_tree.append(spaces)
          |> string_tree.append("</" <> tag <> ">")
        _ ->
          html
          |> string_tree.append_tree(attributes)
          |> string_tree.append(">" <> inner_html <> "</" <> tag <> ">")
      }
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
        [Text(key: "", content: a <> b), ..rest],
        raw_text,
        indent,
      )
    [child, ..rest] ->
      child
      |> do_element_to_snapshot_builder(raw_text, indent)
      |> string_tree.append("\n")
      |> string_tree.append_tree(html, _)
      |> children_to_snapshot_builder(rest, raw_text, indent)
    [] -> html
  }
}

fn attributes_to_string_builder(
  attributes: List(Attribute(msg)),
) -> #(StringTree, String) {
  let #(html, class, style, inner_html) = {
    let init = #(string_tree.new(), "", "", "")
    use #(html, class, style, inner_html), attr <- list.fold(attributes, init)

    case attribute_to_string_parts(attr) {
      Ok(#("dangerous-unescaped-html", val)) -> #(
        html,
        class,
        style,
        inner_html <> val,
      )
      Ok(#("class", val)) if class == "" -> #(
        html,
        escape(val),
        style,
        inner_html,
      )
      Ok(#("class", val)) -> #(
        html,
        class <> " " <> escape(val),
        style,
        inner_html,
      )
      Ok(#("style", val)) if style == "" -> #(
        html,
        class,
        escape(val),
        inner_html,
      )
      Ok(#("style", val)) -> #(
        html,
        class,
        style <> " " <> escape(val),
        inner_html,
      )
      Ok(#(key, "")) -> #(
        string_tree.append(html, " " <> key),
        class,
        style,
        inner_html,
      )
      Ok(#(key, val)) -> #(
        string_tree.append(html, " " <> key <> "=\"" <> escape(val) <> "\""),
        class,
        style,
        inner_html,
      )
      Error(_) -> #(html, class, style, inner_html)
    }
  }

  #(
    case class, style {
      "", "" -> html
      _, "" -> string_tree.append(html, " class=\"" <> class <> "\"")
      "", _ -> string_tree.append(html, " style=\"" <> style <> "\"")
      _, _ ->
        string_tree.append(
          html,
          " class=\"" <> class <> "\" style=\"" <> style <> "\"",
        )
    },
    inner_html,
  )
}

fn attribute_to_string_parts(
  attr: Attribute(msg),
) -> Result(#(String, String), Nil) {
  case attr {
    Attribute("", _) -> Error(Nil)
    Attribute(name, value) -> Ok(#(name, value))
    _ -> Error(Nil)
  }
}
