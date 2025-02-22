// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option}
import gleam/order
import gleam/set.{type Set}
import gleam/string
import gleam/string_tree.{type StringTree}
import lustre/internals/escape.{escape}

// ELEMENTS --------------------------------------------------------------------

pub type Element(msg) {
  Fragment(
    key: String,
    children: List(Element(msg)),
    // When diffing Fragments, we need to know how many elements this fragment
    // spans when moving/deleting/updating it.
    children_count: Int,
  )
  Node(
    key: String,
    namespace: String,
    tag: String,
    // To efficiently compare attributes during the diff, attribute are always
    // stored sorted. We do this while constructing the tree to not have to sort
    // the attribute in the previous tree again. The order does not matter, as
    // long as the new and old tree agree on the same order relation.
    // 
    // When constructing a Node with attributes provided by a user, attributes
    // have to be sorted with the `vdom.sort_attributes` function.
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
    keyed_children: Dict(String, Element(msg)),
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

pub fn to_keyed_children(
  children: List(Element(msg)),
) -> Dict(String, Element(msg)) {
  use dict, child <- list.fold(children, empty_dict())
  case child.key {
    "" -> dict
    key -> dict.insert(dict, key, child)
  }
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

pub fn sort_attributes(attributes: List(Attribute(msg))) -> List(Attribute(msg)) {
  list.sort(attributes, by: compare_attributes)
}

// DIFFS -----------------------------------------------------------------------

pub type Diff(msg) {
  Diff(patch: Patch(msg), handlers: Dict(List(Int), Dict(String, Decoder(msg))))
}

pub type Patch(msg) {
  Patch(
    index: Int,
    remove_count: Int,
    changes: List(Change(msg)),
    children: List(Patch(msg)),
  )
  // Replace(index: Int, replacement: Element(msg))
}

pub type Change(msg) {
  // node updates
  Replace(element: Element(msg))
  ReplaceText(content: String)
  Update(added: List(Attribute(msg)), removed: List(Attribute(msg)))
  // Map(fn(Dynamic) -> Dynamic)
  // keyed changes
  Insert(child: Element(msg), before: Int)
  Move(key: String, before: Int, count: Int)
  RemoveKey(key: String, count: Int)
  // unkeyed changes
  InsertMany(children: List(Element(msg)), before: Int)
  Remove(from: Int, count: Int)
}

pub fn diff(
  prev: Element(msg),
  next: Element(msg),
  handlers: Dict(List(Int), Dict(String, Decoder(msg))),
) -> Diff(msg) {
  let patch =
    do_diff(
      // handlers,
      idx: 0,
      old: [prev],
      new: [next],
      old_keyed: empty_dict(),
      new_keyed: empty_dict(),
      moved_children: empty_set(),
      moved_children_offset: 0,
      patch_index: 0,
      changes: empty_list,
      children: empty_list,
      remove_count: 0,
    )

  Diff(handlers:, patch:)
}

fn do_diff(
  // handlers handlers: Dict(List(Int), Dict(String, Decoder(msg))),
  // Cursor - where we are on the patch node child list.
  idx idx: Int,
  old old: List(Element(msg)),
  new new: List(Element(msg)),
  moved_children moved_children: Set(String),
  moved_children_offset moved_children_offset: Int,
  // Metadata - does not change during the diff of a single node.
  old_keyed old_keyed: Dict(String, Element(msg)),
  new_keyed new_keyed: Dict(String, Element(msg)),
  // Patch data - Accumulators to construct the final `Patch`.
  patch_index patch_index: Int,
  changes changes: List(Change(msg)),
  children children: List(Patch(msg)),
  remove_count remove_count: Int,
) -> Patch(msg) {
  case old, new {
    // we have no more new nodes left, we are done.
    [], [] -> Patch(patch_index, remove_count:, changes:, children:)
    [prev, ..old_rest], [] -> {
      // we got to the end of the new list, but we still need to check if we
      // need to remove children nodes. We might have children left that we
      // already moved!
      case prev.key == "" || !set.contains(moved_children, prev.key) {
        True ->
          do_diff(
            idx:,
            old: old_rest,
            new:,
            moved_children:,
            moved_children_offset:,
            old_keyed:,
            new_keyed:,
            patch_index:,
            changes:,
            children:,
            remove_count: remove_count + node_advancement(prev),
          )
        False ->
          do_diff(
            idx:,
            old: old_rest,
            new:,
            moved_children:,
            moved_children_offset:,
            old_keyed:,
            new_keyed:,
            patch_index:,
            changes:,
            children:,
            remove_count:,
          )
      }
    }
    [], _ -> {
      // we have no more old nodes left, but still some new ones -
      // we append them all and do not set children_count, since we don't want
      // to remove any nodes.
      let append = InsertMany(new, before: idx - moved_children_offset)
      let changes = [append, ..changes]
      Patch(patch_index, remove_count:, changes:, children:)
    }

    // In the following 2 cases, we got at least 2 nodes in both lists that we
    // can compare. We handle keyed nodes first before all other diffs.
    // This includes cases where one node is keyed and the other node isn't!
    [prev, ..old_rest], [next, ..new_rest] if prev.key != next.key -> {
      // does the old node still exist? did the new node exist previously?
      case dict.get(new_keyed, prev.key), dict.get(old_keyed, next.key) {
        Ok(_), Ok(match) ->
          // Both keys still exist, so the new node had to got moved, since the
          // the keys are different. However, we might also be in a situation
          // where the old node is a duplicate produced by a previous move.
          // We definitely need to skip those nodes, and doing so first might
          // reveal that the new node didn't move at all.
          case set.contains(moved_children, prev.key) {
            False -> {
              // Both exist and we did not handle the previous node yet -
              // Here, we move the target node _further up_ to the front and loop
              // again, diffing and updating the pair in the next iteration.
              // After that, the old previous node again has a chance to match
              // the next node naturally, minimising moves.
              //
              // Since we only ever move elements _up_, it means whenever we do
              // we have to take that extra element into account until we see
              // that same element again at the position it was moved from.
              // In the reconciler we apply changes in the reverse order, so
              // during patching ew first get to the index where the element was
              // moved _from_. Once reached, the element is still there, since
              // we wont move it immediately.
              // This means that all indices between these 2 positions need to
              // be offset to account for the element that will be moved _later_!
              // The `idx` always refers to the final position where an element
              // ends up at, `idx-moved_children_offset` refers to the position
              // where the element is at currently/temporarily during patching!
              //
              // [a b c d] -> [c a b d]    // new -> old
              // diff -> apply in reverse order -> update children in reverse order
              // 
              // ↓ diff                   ↓ old         new       idx offs   ↑    changes   ↑    children
              // ↓ 1. move c before 0-0=0 ↓ [a b c d]   [c b a d]   0    0   ↑ 2. [c b a d] ↑
              // ↓ 2. update c at idx=0   ↓ [c a b c d] [c b a d]   0    1   ↑              ↑ 6. [C B A D]
              // ↓ 3. move b before 1-1=0 ↓ [a b c d]   [b a d]     1    1   ↑ 1. [b a c d] ↑
              // ↓ 4. update b at idx=1   ↓ [b a b c d] [b a d]     1    2   ↑              ↑ 5. [c B A D]
              // ↓ 5. update a at idx=2   ↓ [a b c d]   [a d]       2    2   ↑              ↑ 4. [c b A D]
              // ↓ 6. fixup offset for b  ↓ [b c d]     [d]         3    2   ↑              ↑
              // ↓ 7. fixup offset for c  ↓ [c d]       [d]         3    1   ↑              ↑
              // ↓ 8. update d at idx=3   ↓ [d]         [d]         3    0   ↑ 0. [a b c d] ↑ 3. [c b a D]
              //
              let count = node_advancement(next)
              let before = idx - moved_children_offset
              do_diff(
                idx:,
                old: [match, ..old],
                new:,
                old_keyed:,
                new_keyed:,
                moved_children: set.insert(moved_children, next.key),
                moved_children_offset: moved_children_offset + count,
                patch_index:,
                changes: [Move(next.key, before:, count:), ..changes],
                children:,
                remove_count:,
              )
            }
            True -> {
              // previous child got moved -> skip
              // 
              // Once we've seen the old node our indices are correct again,
              // so we can decrement moved_children_offset.
              let count = node_advancement(prev)
              do_diff(
                idx:,
                old: old_rest,
                new:,
                old_keyed:,
                new_keyed:,
                moved_children:,
                moved_children_offset: moved_children_offset - count,
                patch_index:,
                changes:,
                children:,
                remove_count:,
              )
            }
          }

        Error(_), Ok(_) -> {
          // old node deleted or not keyed, new previously existed -> delete old
          let count = node_advancement(prev)
          do_diff(
            idx:,
            old: old_rest,
            new:,
            old_keyed:,
            new_keyed:,
            moved_children:,
            moved_children_offset:,
            patch_index:,
            changes: [RemoveKey(prev.key, count:), ..changes],
            children:,
            remove_count:,
          )
        }

        Error(_), Error(_) -> {
          // old node deleted, new node is new -> replace
          // TODO: We have to handle the node diff here to make sure replace can work
          // if we got 2 fragments, trigger the normal diff?

          // if the old node is a fragment, we need to first delete n-1 nodes
          // and then replace the remaining node later.
          let count = node_advancement(prev)
          let changes = case count > 1 {
            True -> [RemoveKey(prev.key, count - 1), ..changes]
            False -> changes
          }
          do_diff(
            idx: idx + 1,
            old: old_rest,
            new: new_rest,
            old_keyed:,
            new_keyed:,
            moved_children:,
            moved_children_offset:,
            patch_index:,
            changes:,
            children: [Patch(idx, -1, [Replace(next)], []), ..children],
            remove_count:,
          )
        }

        Ok(_), Error(_) -> {
          // old node still exists, new node is new or not keyed -> insert
          let before = idx - moved_children_offset
          do_diff(
            idx: idx + 1,
            old:,
            new: new_rest,
            old_keyed:,
            new_keyed:,
            moved_children:,
            moved_children_offset: moved_children_offset + 1,
            patch_index:,
            changes: [Insert(child: next, before:), ..changes],
            children:,
            remove_count:,
          )
        }
      }
    }

    [prev, ..old], [next, ..new] -> {
      case prev, next {
        Fragment(..), Fragment(..) -> {
          let next_count = next.children_count
          let prev_count = prev.children_count

          // once we updated the fragment, we have to remove additinional
          // elements afterwards, similarly to hwow `remove_count` works.
          // We _have_ to to this afterwards though, otherwise we could delete
          // a keyed item that we still needed here!
          let changes = case prev_count - next_count {
            remove_count if remove_count > 0 -> {
              let remove_from = idx + next_count - moved_children_offset
              [Remove(remove_from, remove_count), ..changes]
            }
            _ -> changes
          }

          let child_patch =
            do_diff(
              idx:,
              old: prev.children,
              new: next.children,
              old_keyed: empty_dict(),
              new_keyed: empty_dict(),
              moved_children: empty_set(),
              moved_children_offset:,
              patch_index: -1,
              changes:,
              children:,
              remove_count: 0,
            )

          // when we apply the changes, we will have next_count nodes in the tree,
          // so we want to advance our index by that.
          let idx = idx + next_count

          // changes after us have to be generated as if we still had prev_count
          // children here, so we need to offset moved_children to account for that!
          let moved_children_offset =
            moved_children_offset + next_count - prev_count

          do_diff(
            idx:,
            old:,
            new:,
            old_keyed:,
            new_keyed:,
            moved_children:,
            moved_children_offset:,
            patch_index:,
            changes: child_patch.changes,
            children: child_patch.children,
            remove_count:,
          )
        }

        Node(..), Node(..)
          if prev.namespace == next.namespace && prev.tag == next.tag
        -> {
          let child_changes = case
            diff_attributes(
              prev.attributes,
              next.attributes,
              empty_list,
              empty_list,
            )
          {
            AttributeChange(added: [], removed: []) -> empty_list
            AttributeChange(added:, removed:) -> [Update(added:, removed:)]
          }

          let child_patch =
            do_diff(
              idx: 0,
              old: prev.children,
              new: next.children,
              old_keyed: prev.keyed_children,
              new_keyed: next.keyed_children,
              moved_children: empty_set(),
              moved_children_offset:,
              patch_index: idx,
              changes: child_changes,
              children: empty_list,
              remove_count:,
            )

          // we do not have to keep empty patches
          let children = case child_patch {
            Patch(remove_count: 0, changes: [], children: [], ..) -> children
            _ -> [child_patch, ..children]
          }

          do_diff(
            idx: idx + 1,
            old:,
            new:,
            old_keyed:,
            new_keyed:,
            moved_children:,
            moved_children_offset:,
            patch_index:,
            changes:,
            children:,
            remove_count:,
          )
        }

        Text(..), Text(..) if prev.content == next.content ->
          do_diff(
            idx: idx + 1,
            old:,
            new:,
            old_keyed:,
            new_keyed:,
            moved_children:,
            moved_children_offset:,
            patch_index:,
            changes:,
            children:,
            remove_count:,
          )

        Text(..), Text(..) -> {
          let child =
            Patch(
              idx,
              remove_count: 0,
              changes: [ReplaceText(next.content)],
              children: empty_list,
            )
          do_diff(
            idx: idx + 1,
            old:,
            new:,
            old_keyed:,
            new_keyed:,
            moved_children:,
            moved_children_offset:,
            patch_index:,
            changes:,
            children: [child, ..children],
            remove_count:,
          )
        }

        _, _ -> {
          // the nodes have a different constructor, tag, or namespace -
          // we assume they changed enough to warrant a replace and not diff
          // them further.

          // if the old node is a fragment, we need to first delete n-1 nodes
          // and then replace the remaining node later.
          let prev_count = node_advancement(prev)

          // this remove makes sure that the node has size=1 after the changes
          // got applied, so we have to treat it as such for our index tracking!
          let changes = case prev_count > 1 {
            True -> [
              Remove(idx - moved_children_offset + 1, prev_count - 1),
              ..changes
            ]
            False -> changes
          }

          // for the changes that we generate later, we have to offset them by
          // this amount, similar to moves!
          // So if we delete 2 elements from a fragment, we have to increase
          // the index for changes by 2, meaning we'd have an offset of -2
          let moved_children_offset = moved_children_offset - prev_count + 1

          let child =
            Patch(idx, remove_count: 0, changes: [Replace(next)], children: [])
          do_diff(
            idx: idx + 1,
            old:,
            new:,
            old_keyed:,
            new_keyed:,
            moved_children:,
            moved_children_offset:,
            patch_index:,
            changes:,
            children: [child, ..children],
            remove_count:,
          )
        }
      }
    }
  }
}

fn node_advancement(element: Element(msg)) {
  case element {
    Fragment(children_count:, ..) -> children_count
    _ -> 1
  }
}

type AttributeChange(msg) {
  AttributeChange(added: List(Attribute(msg)), removed: List(Attribute(msg)))
}

fn diff_attributes(
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
      // We assume atttribute lists are sorted here. This means we can figure out
      // if something got added or removed by comparing each pair of attributes
      // we encounter:
      //
      // - If both names are equal, we can continue diffing the attributes.
      // - If the previous attribute is "less than", it means the previous
      //   attribute is no longer in the next attributee list, because otherwise
      //   the next attribute would have been sorted in this position.
      // - Using the same arguments, we can assume that of the previous attribute
      //   is "greater than", the next attribute got added.
      case compare_attributes(prev_attr, next_attr) {
        order.Eq ->
          case prev_attr, next_attr {
            Attribute(..), Attribute(..) ->
              case next_attr.name {
                "value" | "checked" | "selected" -> {
                  let added = [next_attr, ..added]
                  diff_attributes(prev_rest, next_rest, added, removed)
                }

                _ if prev_attr.value == next_attr.value ->
                  diff_attributes(prev_rest, next_rest, added, removed)

                _ -> {
                  let added = [next_attr, ..added]
                  diff_attributes(prev_rest, next_rest, added, removed)
                }
              }

            Property(..), Property(..) ->
              case next_attr.name {
                "value" | "checked" | "selected" | "scrollLeft" | "scrollRight" -> {
                  let added = [next_attr, ..added]
                  diff_attributes(prev_rest, next_rest, added, removed)
                }

                _ if prev_attr.value == next_attr.value ->
                  diff_attributes(prev_rest, next_rest, added, removed)

                _ -> {
                  let added = [next_attr, ..added]
                  diff_attributes(prev_rest, next_rest, added, removed)
                }
              }

            Event(..), Event(..) -> {
              // we skip diffing event handlers, since Decoders are constructed
              // dynamically at every call to view and constantly changing.
              let added = [next_attr, ..added]
              diff_attributes(prev_rest, next_rest, added, removed)
            }
            _, _ -> {
              // we have the same name, but the type has changed!
              let added = [next_attr, ..added]
              let removed = [prev_attr, ..removed]
              diff_attributes(prev_rest, next_rest, added, removed)
            }
          }
        order.Gt -> {
          let added = [next_attr, ..added]
          diff_attributes(prev, next_rest, added, removed)
        }
        order.Lt -> {
          let removed = [prev_attr, ..removed]
          diff_attributes(prev_rest, next, added, removed)
        }
      }
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

// -- PERFORMANCE CRIMES -------------------------------------------------------

const empty_list = []

@external(javascript, "../../perf_crimes.ffi.mjs", "empty_dict")
fn empty_dict() -> Dict(a, b) {
  dict.new()
}

@external(javascript, "../../perf_crimes.ffi.mjs", "empty_set")
fn empty_set() -> Set(a) {
  set.new()
}

@external(javascript, "../../perf_crimes.ffi.mjs", "compare_attributes")
fn compare_attributes(a: Attribute(msg), b: Attribute(msg)) -> order.Order {
  string.compare(a.name, b.name)
}
