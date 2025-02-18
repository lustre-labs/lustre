// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/string
import gleam/string_tree.{type StringTree}
import lustre/internals/escape.{escape}
import lustre/internals/keyed_lookup.{type KeyedLookup}

// CONSTANTS -------------------------------------------------------------------

const empty_list = []

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
  Patch(
    index: Int,
    size: Int,
    changes: List(Change(msg)),
    children: List(Patch(msg)),
  )
}

pub type Change(msg) {
  Append(children: List(Element(msg)))
  Insert(child: Element(msg), before: String)
  Map(fn(Dynamic) -> Dynamic)
  Move(key: String, before: String)
  // RemoveAll(from: Int)
  // RemoveKey(key: String)
  Remove(from: Int, count: Int)
  Replace(element: Element(msg))
  ReplaceText(content: String)
  Update(added: List(Attribute(msg)), removed: List(Attribute(msg)))
}

pub fn diff(
  prev: Element(msg),
  next: Element(msg),
  handlers: Dict(List(Int), Dict(String, Decoder(msg))),
) -> Diff(msg) {
  let patch =
    do_diff(
      // handlers,
      // node: Patch(0, changes: [], children: []),
      idx: 0,
      old: [prev],
      new: [next],
      fragment: False,
      keyed: False,
      keyed_children: dict.new(),
      patch_index: 0,
      changes: empty_list,
      children: empty_list,
    )

  Diff(handlers:, patch:)
}

// type Cursor(msg) {
//   Cursor(
//     node: Patch(msg),
//     idx: Int,
//     old: List(Element(msg)),
//     new: List(Element(msg)),
//     // metadata
//     fragment: Bool,
//     keyed: Bool,
//     keyed_children: KeyedLookup(Element(msg)),
//   )
// }

fn do_diff(
  // handlers handlers: Dict(List(Int), Dict(String, Decoder(msg))),
  // stack: List(Cursor(msg)),
  // cursor
  // node node: Patch(msg),
  idx idx: Int,
  old old: List(Element(msg)),
  new new: List(Element(msg)),
  // metadata
  fragment fragment: Bool,
  keyed keyed: Bool,
  keyed_children keyed_children: Dict(String, Element(msg)),
  // patch data
  patch_index patch_index: Int,
  changes changes: List(Change(msg)),
  children children: List(Patch(msg)),
) -> Patch(msg) {
  // // if we have at least 2 things on the stack and we reached the end of the top thing
  // [Cursor(node:, fragment:, idx:, old: [], new: [], ..), next, ..stack] ->
  //   case fragment, node.changes, node.children {
  //     // the top thing was empty and not a fragment
  //     // --> we got empty back from a recursive call -> skip it
  //     False, [], [] -> do_diff(handlers, [next, ..stack])

  //     // the top thing is not empty and not a fragment
  //     // -->  push it, baby
  //     False, _, _ -> {
  //       let children = [node, ..next.node.children]
  //       let parent = Patch(..next.node, children:)
  //       let next = Cursor(..next, node: parent)

  //       do_diff(handlers, [next, ..stack])
  //     }

  //     // fragments - fixup indices
  //     True, _, _ -> {
  //       let node = Patch(..node, index: next.node.index)
  //       let next = Cursor(..next, node:, idx:)

  //       do_diff(handlers, [next, ..stack])
  //     }
  //   }
  case old, new {
    [], [] -> Patch(patch_index, size: 0, changes:, children:)
    _, [] -> {
      // let changes = [RemoveAll(from: idx), ..changes]
      Patch(patch_index, size: idx, changes:, children:)
    }
    [], _ -> {
      let changes = [Append(new), ..changes]
      Patch(patch_index, size: idx, changes:, children:)
    }

    // _, [] if keyed -> {
    //   let changes =
    //     keyed_lookup.fold_remaining_keys(
    //       keyed_children,
    //       changes,
    //       fn(changes, key) { [RemoveKey(key), ..changes] },
    //     )
    //   Patch(index: patch_index, changes:, children:)
    // }
    // _, [] if fragment -> {
    //   let changes = [Remove(from: idx, count: list.length(old)), ..changes]
    //   Patch(index: patch_index, changes:, children:)
    // }
    [prev, ..old_rest], [next, ..new_rest] if keyed && prev.key != next.key -> {
      case dict.get(keyed_children, next.key) {
        Ok(match) -> {
          // TODO: if the `next` node is a fragment this will throw everything off.
          let old = [match, ..old]
          let changes = [Move(key: next.key, before: prev.key), ..changes]
          do_diff(
            idx:,
            old:,
            new:,
            fragment:,
            keyed:,
            keyed_children:,
            patch_index:,
            changes:,
            children:,
          )
        }

        Error(_) -> {
          let changes = [Insert(child: next, before: prev.key), ..changes]
          let idx = idx + 1
          let new = new_rest
          do_diff(
            idx:,
            old:,
            new:,
            fragment:,
            keyed:,
            keyed_children:,
            patch_index:,
            changes:,
            children:,
          )
        }
      }
    }

    [prev, ..old], [next, ..new] -> {
      case prev, next {
        Fragment(..), Fragment(..) -> {
          let child_patch = case prev.children {
            [head, ..] if head.key != "" -> {
              let keyed = True
              let keyed_children = to_keyed_children(prev.children)
              let fragment = True
              let old = prev.children
              let new = next.children
              let patch_index = idx
              do_diff(
                idx:,
                old:,
                new:,
                fragment:,
                keyed:,
                keyed_children:,
                patch_index:,
                changes:,
                children:,
              )
            }
            _ -> {
              let keyed = False
              let fragment = True
              let old = prev.children
              let new = next.children
              let patch_index = idx
              do_diff(
                idx:,
                old:,
                new:,
                fragment:,
                keyed:,
                keyed_children: dict.new(),
                patch_index:,
                changes:,
                children:,
              )
            }
          }

          // let node = Patch(..node, index: next.node.index)
          // let next = Cursor(..next, node:, idx:)
          let changes = child_patch.changes
          let children = child_patch.children
          do_diff(
            idx:,
            old:,
            new:,
            fragment:,
            keyed:,
            keyed_children:,
            patch_index:,
            changes:,
            children:,
          )
        }

        Node(..), Node(..)
          if prev.namespace == next.namespace && prev.tag == next.tag
        -> {
          let child_changes = case
            diff_attributes(prev.attributes, next.attributes)
          {
            AttributeChange(added: [], removed: []) -> []
            AttributeChange(added:, removed:) -> [Update(added:, removed:)]
          }

          let child_patch = case prev.children {
            [head, ..] if head.key != "" -> {
              let patch_index = idx
              let idx = 0
              let old = prev.children
              let new = next.children
              let fragment = False
              let keyed = True
              let keyed_children = to_keyed_children(prev.children)
              do_diff(
                idx:,
                old:,
                new:,
                fragment:,
                keyed:,
                keyed_children:,
                patch_index:,
                changes: child_changes,
                children: empty_list,
              )
            }
            _ -> {
              let patch_index = idx
              let idx = 0
              let old = prev.children
              let new = next.children
              let fragment = False
              let keyed = False
              do_diff(
                idx:,
                old:,
                new:,
                fragment:,
                keyed:,
                keyed_children: dict.new(),
                patch_index:,
                changes: child_changes,
                children: empty_list,
              )
            }
          }

          let idx = idx + 1
          // let keyed_children = keyed_lookup.delete(keyed_children, next.key)
          let children = case child_patch {
            Patch(changes: [], children: [], ..) -> children
            _ -> [child_patch, ..children]
          }
          do_diff(
            idx:,
            old:,
            new:,
            fragment:,
            keyed:,
            keyed_children:,
            patch_index:,
            changes:,
            children:,
          )
        }

        Text(..), Text(..) if prev.content == next.content -> {
          let idx = idx + 1
          // let keyed_children = keyed_lookup.delete(keyed_children, next.key)
          do_diff(
            idx:,
            old:,
            new:,
            fragment:,
            keyed:,
            keyed_children:,
            patch_index:,
            changes:,
            children:,
          )
        }

        Text(..), Text(..) -> {
          let child =
            Patch(
              idx,
              size: 0,
              changes: [ReplaceText(next.content)],
              children: empty_list,
            )
          let children = [child, ..children]
          // let keyed_children = keyed_lookup.delete(keyed_children, next.key)
          let idx = idx + 1
          do_diff(
            idx:,
            old:,
            new:,
            fragment:,
            keyed:,
            keyed_children:,
            patch_index:,
            changes:,
            children:,
          )
        }

        _, _ -> {
          // TODO: wtf is this
          let child =
            Patch(idx, size: 9_999_999, changes: [Replace(next)], children: [])
          let children = [child, ..children]
          // let keyed_children = keyed_lookup.delete(keyed_children, next.key)
          let idx = idx + 1
          do_diff(
            idx:,
            old:,
            new:,
            fragment:,
            keyed:,
            keyed_children:,
            patch_index:,
            changes:,
            children:,
          )
        }
      }
    }
  }
}

type AttributeChange(msg) {
  AttributeChange(added: List(Attribute(msg)), removed: List(Attribute(msg)))
}

fn compare_attributes(a: Attribute(msg), b: Attribute(msg)) -> order.Order {
  string.compare(a.name, b.name)
}

@external(javascript, "../../runtime.ffi.mjs", "sort_attributes")
pub fn sort_attributes(attributes: List(Attribute(msg))) -> List(Attribute(msg)) {
  list.sort(attributes, by: compare_attributes)
}

fn diff_attributes(
  prev: List(Attribute(msg)),
  next: List(Attribute(msg)),
) -> AttributeChange(msg) {
  // case prev, next {
  // [], [] -> AttributeChange(added: [], removed: [])
  // [], _ -> AttributeChange(added: next, removed: [])
  // _, [] -> AttributeChange(added: [], removed: prev)
  // _, _ -> {
  do_diff_attributes(prev, next, empty_list, empty_list)
  // }
  // }
}

fn do_diff_attributes(
  prev: List(Attribute(msg)),
  next: List(Attribute(msg)),
  added: List(Attribute(msg)),
  removed: List(Attribute(msg)),
) -> AttributeChange(msg) {
  case prev, next {
    [], [] -> AttributeChange(added:, removed:)
    _, [] ->
      AttributeChange(added:, removed: list.fold(prev, removed, list.prepend))
    [], _ ->
      AttributeChange(added: list.fold(next, added, list.prepend), removed:)
    [prev_attr, ..prev_rest], [next_attr, ..next_rest] ->
      case compare_attributes(prev_attr, next_attr) {
        order.Eq ->
          case prev_attr, next_attr {
            Attribute(..), Attribute(..) ->
              case next_attr.name {
                "value" | "checked" | "selected" -> {
                  let added = [next_attr, ..added]
                  do_diff_attributes(prev_rest, next_rest, added, removed)
                }

                _ if prev_attr.value == next_attr.value ->
                  do_diff_attributes(prev_rest, next_rest, added, removed)

                _ -> {
                  let added = [next_attr, ..added]
                  do_diff_attributes(prev_rest, next_rest, added, removed)
                }
              }

            Property(..), Property(..) ->
              case next_attr.name {
                "value" | "checked" | "selected" | "scrollLeft" | "scrollRight" -> {
                  let added = [next_attr, ..added]
                  do_diff_attributes(prev_rest, next_rest, added, removed)
                }

                _ if prev_attr.value == next_attr.value ->
                  do_diff_attributes(prev_rest, next_rest, added, removed)

                _ -> {
                  let added = [next_attr, ..added]
                  do_diff_attributes(prev_rest, next_rest, added, removed)
                }
              }

            // Event(..), Event(..) as old if attr == old ->
            //   do_diff_attributes(dict.delete(prev, attr.name), rest, added)
            // Event(..), Event(..) ->
            //   do_diff_attributes(dict.delete(prev, attr.name), rest, [
            //     attr,
            //     ..added
            //   ])
            _, _ -> {
              let added = [next_attr, ..added]
              do_diff_attributes(prev_rest, next_rest, added, removed)
            }
          }
        order.Gt -> {
          let added = [next_attr, ..added]
          do_diff_attributes(prev, next_rest, added, removed)
        }
        order.Lt -> {
          let removed = [prev_attr, ..removed]
          do_diff_attributes(prev_rest, next, added, removed)
        }
      }
  }
}

fn to_keyed_children(children: List(Element(msg))) -> Dict(String, Element(msg)) {
  use dict, child <- list.fold(children, dict.new())
  case child.key {
    "" -> dict
    _ -> dict.insert(dict, child.key, child)
  }
}

fn add_keyed_children(
  keyed_children: KeyedLookup(Element(msg)),
  children: List(Element(msg)),
) -> KeyedLookup(Element(msg)) {
  case children {
    [] -> keyed_children
    [child, ..] if child.key == "" -> keyed_children
    _ ->
      keyed_lookup.set_from_values(keyed_children, children, fn(child, _) {
        #(child.key, child)
      })
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
