// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/set.{type Set}
import gleam/string
import gleam/string_tree.{type StringTree}
import lustre/internals/constants
import lustre/internals/escape.{escape}
import lustre/internals/events.{type Events}

// ELEMENTS --------------------------------------------------------------------

pub type Element(msg) {
  Fragment(
    key: String,
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
    // When diffing Fragments, we need to know how many elements this fragment
    // spans when moving/deleting/updating it.
    children_count: Int,
  )
  Node(
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
  Text(key: String, mapper: Option(fn(Dynamic) -> Dynamic), content: String)
}

pub type Attribute(msg) {
  Attribute(name: String, value: String)
  Property(name: String, value: Json)
  Event(
    name: String,
    handler: Decoder(msg),
    include: List(String),
    prevent_default: Bool,
    stop_propagation: Bool,
    immediate: Bool,
  )
}

pub fn prepare_attributes(
  attributes: List(Attribute(msg)),
) -> List(Attribute(msg)) {
  // we sort with the arguments flipped because merge_attributes will reverse
  // the list again. This makes it conceptually easier to think about.
  attributes
  |> list.sort(by: fn(a, b) { compare_attributes(b, a) })
  |> merge_attributes(constants.empty_list)
}

fn merge_attributes(
  attributes: List(Attribute(msg)),
  merged: List(Attribute(msg)),
) -> List(Attribute(msg)) {
  case attributes {
    [] -> merged
    [
      Attribute(name: "class", value: class1),
      Attribute(name: "class", value: class2),
      ..rest
    ] -> {
      let attribute = Attribute(name: "class", value: class1 <> " " <> class2)
      merge_attributes([attribute, ..rest], merged)
    }

    [
      Attribute(name: "style", value: style1),
      Attribute(name: "style", value: style2),
      ..rest
    ] -> {
      let attribute = Attribute(name: "style", value: style1 <> ";" <> style2)
      merge_attributes([attribute, ..rest], merged)
    }

    [attribute, ..rest] -> merge_attributes(rest, [attribute, ..merged])
  }
}

// DIFFS -----------------------------------------------------------------------

pub type Diff(msg) {
  Diff(patch: Patch(msg), events: Events(msg))
}

pub type Patch(msg) {
  Patch(
    index: Int,
    remove_count: Int,
    mapper: List(fn(Dynamic) -> Dynamic),
    changes: List(Change(msg)),
    children: List(Patch(msg)),
  )
}

pub type Change(msg) {
  // node updates
  Replace(element: Element(msg))
  ReplaceText(content: String)
  Update(added: List(Attribute(msg)), removed: List(Attribute(msg)))
  // keyed changes
  Insert(child: Element(msg), before: Int)
  Move(key: String, before: Int, count: Int)
  RemoveKey(key: String, count: Int)
  // unkeyed changes
  InsertMany(children: List(Element(msg)), before: Int)
  Remove(from: Int, count: Int)
}

pub fn init(view: Element(msg)) -> Events(msg) {
  insert_events(events.new(), constants.empty_list, view)
}

pub fn diff(
  initial_element_offset: Int,
  prev: Element(msg),
  next: Element(msg),
  events: Events(msg),
) -> Diff(msg) {
  let diff =
    do_diff(
      events: events.reset(events),
      idx: 0,
      old: [prev],
      new: [next],
      old_keyed: constants.empty_dict(),
      new_keyed: constants.empty_dict(),
      old_mapper: constants.empty_list,
      new_mapper: constants.empty_list,
      moved_children: constants.empty_set(),
      moved_children_offset: 0,
      patch_index: 0,
      changes: constants.empty_list,
      children: constants.empty_list,
      remove_count: 0,
    )

  // there are cases where we need to skip elements at the front of the node we
  // control, for example when we insert style sheets at the front.
  case initial_element_offset > 0 {
    True -> {
      let patch = offset_indices(diff.patch, initial_element_offset)

      Diff(..diff, patch:)
    }

    False -> diff
  }
}

fn offset_indices(patch: Patch(msg), initial_element_offset: Int) {
  Patch(
    ..patch,
    changes: {
      use change <- list.map(patch.changes)
      case change {
        Insert(child:, before:) ->
          Insert(child:, before: before + initial_element_offset)
        InsertMany(children:, before:) ->
          InsertMany(children:, before: before + initial_element_offset)
        Move(key:, before:, count:) ->
          Move(key:, count:, before: before + initial_element_offset)
        Remove(from:, count:) ->
          Remove(count:, from: from + initial_element_offset)

        Replace(..) | ReplaceText(..) | Update(..) | RemoveKey(..) -> change
      }
    },
    children: {
      use child <- list.map(patch.children)
      Patch(..child, index: child.index + initial_element_offset)
    },
  )
}

fn do_diff(
  events events: Events(msg),
  // Cursor - where we are on the patch node child list.
  idx idx: Int,
  old old: List(Element(msg)),
  new new: List(Element(msg)),
  moved_children moved_children: Set(String),
  moved_children_offset moved_children_offset: Int,
  // Metadata - does not change during the diff of a single node.
  old_keyed old_keyed: Dict(String, Element(msg)),
  new_keyed new_keyed: Dict(String, Element(msg)),
  old_mapper old_mapper: List(fn(Dynamic) -> Dynamic),
  new_mapper new_mapper: List(fn(Dynamic) -> Dynamic),
  // Patch data - Accumulators to construct the final `Patch`.
  patch_index patch_index: Int,
  changes changes: List(Change(msg)),
  children children: List(Patch(msg)),
  remove_count remove_count: Int,
) -> Diff(msg) {
  case old, new {
    // we have no more new nodes left, we are done.
    [], [] ->
      Diff(
        patch: Patch(
          patch_index,
          remove_count:,
          mapper: new_mapper,
          changes:,
          children:,
        ),
        events:,
      )

    [prev, ..old_rest], [] -> {
      // we got to the end of the new list, but we still need to check if we
      // need to remove children nodes. We might have children left that we
      // already moved!
      case prev.key == "" || !set.contains(moved_children, prev.key) {
        True ->
          do_diff(
            events: forget_events(events, old_mapper, prev),
            idx:,
            old: old_rest,
            new:,
            moved_children:,
            moved_children_offset:,
            old_keyed:,
            new_keyed:,
            old_mapper:,
            new_mapper:,
            patch_index:,
            changes:,
            children:,
            remove_count: remove_count + node_advancement(prev),
          )

        False ->
          do_diff(
            events:,
            idx:,
            old: old_rest,
            new:,
            moved_children:,
            moved_children_offset:,
            old_keyed:,
            new_keyed:,
            old_mapper:,
            new_mapper:,
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
      let events =
        list.fold(new, events, fn(events, next) {
          insert_events(events, new_mapper, next)
        })
      let append = InsertMany(new, before: idx - moved_children_offset)
      let changes = [append, ..changes]

      Diff(
        patch: Patch(
          patch_index,
          remove_count:,
          mapper: new_mapper,
          changes:,
          children:,
        ),
        events:,
      )
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
                events:,
                idx:,
                old: [match, ..old],
                new:,
                old_keyed:,
                new_keyed:,
                old_mapper:,
                new_mapper:,
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
                events:,
                idx:,
                old: old_rest,
                new:,
                old_keyed:,
                new_keyed:,
                old_mapper:,
                new_mapper:,
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
            events: forget_events(events, old_mapper, prev),
            idx:,
            old: old_rest,
            new:,
            old_keyed:,
            new_keyed:,
            old_mapper:,
            new_mapper:,
            moved_children:,
            moved_children_offset: moved_children_offset - count,
            patch_index:,
            changes: [RemoveKey(prev.key, count:), ..changes],
            children:,
            remove_count:,
          )
        }

        Error(_), Error(_) -> {
          // old node deleted, new node is new -> replace
          // NOTE: we have to copy-paste this code from the general replace case,
          // to stay tail-recursive!

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
            Patch(
              idx,
              remove_count: 0,
              mapper: push_mapper(next, new_mapper),
              changes: [Replace(next)],
              children: constants.empty_list,
            )

          let events =
            events
            |> forget_events(old_mapper, prev)
            |> insert_events(new_mapper, next)

          do_diff(
            events:,
            idx: idx + 1,
            old: old_rest,
            new: new_rest,
            old_keyed:,
            new_keyed:,
            old_mapper:,
            new_mapper:,
            moved_children:,
            moved_children_offset:,
            patch_index:,
            changes:,
            children: [child, ..children],
            remove_count:,
          )
        }

        Ok(_), Error(_) -> {
          // old node still exists, new node is new or not keyed -> insert
          let before = idx - moved_children_offset
          let count = node_advancement(next)

          do_diff(
            events: insert_events(events, new_mapper, next),
            idx: idx + count,
            old:,
            new: new_rest,
            old_keyed:,
            new_keyed:,
            old_mapper:,
            new_mapper:,
            moved_children:,
            moved_children_offset: moved_children_offset + count,
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

          let child =
            do_diff(
              events:,
              idx:,
              old: prev.children,
              new: next.children,
              old_keyed: prev.keyed_children,
              new_keyed: next.keyed_children,
              old_mapper: push_mapper(prev, old_mapper),
              new_mapper: push_mapper(next, new_mapper),
              moved_children: constants.empty_set(),
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
            events: child.events,
            idx:,
            old:,
            new:,
            old_keyed:,
            new_keyed:,
            old_mapper:,
            new_mapper:,
            moved_children:,
            moved_children_offset:,
            patch_index:,
            changes: child.patch.changes,
            children: child.patch.children,
            remove_count:,
          )
        }

        Node(..), Node(..)
          if prev.namespace == next.namespace && prev.tag == next.tag
        -> {
          let old_child_mapper = push_mapper(prev, old_mapper)
          let new_child_mapper = push_mapper(next, new_mapper)

          let AttributeChange(added:, removed:, events:) =
            diff_attrs(
              events:,
              old: prev.attributes,
              new: next.attributes,
              map_old: old_child_mapper,
              map_new: new_child_mapper,
              added: constants.empty_list,
              removed: constants.empty_list,
            )

          let child_changes = case added, removed {
            [], [] -> constants.empty_list
            _, _ -> [Update(added:, removed:)]
          }

          let child =
            do_diff(
              events:,
              idx: 0,
              old: prev.children,
              new: next.children,
              old_keyed: prev.keyed_children,
              new_keyed: next.keyed_children,
              old_mapper: old_child_mapper,
              new_mapper: new_child_mapper,
              moved_children: constants.empty_set(),
              moved_children_offset:,
              patch_index: idx,
              changes: child_changes,
              children: constants.empty_list,
              remove_count:,
            )

          // we do not have to keep empty patches
          let children = case child.patch {
            Patch(remove_count: 0, changes: [], children: [], ..) -> children
            _ -> [child.patch, ..children]
          }

          do_diff(
            events: child.events,
            idx: idx + 1,
            old:,
            new:,
            old_keyed:,
            new_keyed:,
            old_mapper:,
            new_mapper:,
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
            events:,
            idx: idx + 1,
            old:,
            new:,
            old_keyed:,
            new_keyed:,
            old_mapper:,
            new_mapper:,
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
              mapper: new_mapper,
              changes: [ReplaceText(next.content)],
              children: constants.empty_list,
            )

          do_diff(
            events:,
            idx: idx + 1,
            old:,
            new:,
            old_keyed:,
            new_keyed:,
            old_mapper:,
            new_mapper:,
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
            Patch(
              idx,
              remove_count: 0,
              mapper: push_mapper(next, new_mapper),
              changes: [Replace(next)],
              children: constants.empty_list,
            )

          let events =
            events
            |> forget_events(old_mapper, prev)
            |> insert_events(new_mapper, next)

          do_diff(
            events:,
            idx: idx + 1,
            old:,
            new:,
            old_keyed:,
            new_keyed:,
            old_mapper:,
            new_mapper:,
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

// ATTRIBUTE DIFFS -------------------------------------------------------------

type AttributeChange(msg) {
  AttributeChange(
    added: List(Attribute(msg)),
    removed: List(Attribute(msg)),
    events: Events(msg),
  )
}

fn diff_attrs(
  // cursor
  old old: List(Attribute(msg)),
  new new: List(Attribute(msg)),
  // context
  map_old map_old: List(fn(Dynamic) -> Dynamic),
  map_new map_new: List(fn(Dynamic) -> Dynamic),
  // accumulator
  added added: List(Attribute(msg)),
  removed removed: List(Attribute(msg)),
  events events: Events(msg),
) -> AttributeChange(msg) {
  case old, new {
    [], [] -> AttributeChange(added, removed, events:)

    [old_attr, ..old], [] ->
      case old_attr {
        Event(handler:, ..) -> {
          let events = events.forget(events, events.Event(map_old, handler))
          let removed = [old_attr, ..removed]
          diff_attrs(old, new, map_old, map_new, added, removed, events)
        }
        _ -> {
          let unset = [old_attr, ..removed]
          diff_attrs(old, new, map_old, map_new, added, unset, events)
        }
      }

    [], [new_attr, ..new] ->
      case new_attr {
        Event(handler:, ..) -> {
          let events = events.insert(events, events.Event(map_new, handler))
          let added = [new_attr, ..added]
          diff_attrs(old, new, map_old, map_new, added, removed, events)
        }

        _ -> {
          let set = [new_attr, ..added]
          diff_attrs(old, new, map_old, map_new, set, removed, events)
        }
      }

    [old_attr, ..prev], [new_attr, ..next] ->
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
      case compare_attributes(old_attr, new_attr) {
        order.Eq -> {
          let old = prev
          let new = next

          case old_attr, new_attr {
            Attribute(..), Attribute(..) ->
              case new_attr.name {
                "value" | "checked" | "selected" -> {
                  let added = [new_attr, ..added]
                  diff_attrs(old, new, map_old, map_new, added, removed, events)
                }

                _ if old_attr.value == new_attr.value ->
                  diff_attrs(old, new, map_old, map_new, added, removed, events)

                _ -> {
                  let added = [new_attr, ..added]
                  diff_attrs(old, new, map_old, map_new, added, removed, events)
                }
              }

            Property(..), Property(..) ->
              case new_attr.name {
                "value" | "checked" | "selected" | "scrollLeft" | "scrollRight" -> {
                  let added = [new_attr, ..added]
                  diff_attrs(old, new, map_old, map_new, added, removed, events)
                }

                _ if old_attr.value == new_attr.value ->
                  diff_attrs(old, new, map_old, map_new, added, removed, events)

                _ -> {
                  let added = [new_attr, ..added]
                  diff_attrs(old, new, map_old, map_new, added, removed, events)
                }
              }

            // If any of the ways to handle the event change, we know we need to
            // get the reconciler to update the event handler.
            Event(..), Event(..)
              if old_attr.prevent_default != new_attr.prevent_default
              || old_attr.stop_propagation != new_attr.stop_propagation
              || old_attr.immediate != new_attr.immediate
            -> {
              let old_event = events.Event(map_old, old_attr.handler)
              let new_event = events.Event(map_new, new_attr.handler)
              let events = events.replace(events, old_event, new_event)
              let added = [new_attr, ..added]

              diff_attrs(old, new, map_old, map_new, added, removed, events)
            }

            Event(..), Event(..) -> {
              let old_event = events.Event(map_old, old_attr.handler)
              let new_event = events.Event(map_new, new_attr.handler)
              let events = events.replace(events, old_event, new_event)

              diff_attrs(old, new, map_old, map_new, added, removed, events)
            }

            Event(..), _ -> {
              let events =
                events.forget(events, events.Event(map_old, old_attr.handler))
              let added = [new_attr, ..added]
              let removed = [old_attr, ..removed]

              diff_attrs(old, new, map_old, map_new, added, removed, events)
            }

            _, Event(..) -> {
              let events =
                events.insert(events, events.Event(map_new, new_attr.handler))
              let added = [new_attr, ..added]
              let removed = [old_attr, ..removed]

              diff_attrs(old, new, map_old, map_new, added, removed, events)
            }

            _, _ -> {
              // we have the same name, but the type has changed!
              let added = [new_attr, ..added]
              let removed = [old_attr, ..removed]

              diff_attrs(old, new, map_old, map_new, added, removed, events)
            }
          }
        }

        order.Gt ->
          case new_attr {
            Event(handler:, ..) -> {
              let events = events.insert(events, events.Event(map_new, handler))
              let added = [new_attr, ..added]

              diff_attrs(old, next, map_old, map_new, added, removed, events)
            }
            _ -> {
              let added = [new_attr, ..added]
              diff_attrs(old, next, map_old, map_new, added, removed, events)
            }
          }

        order.Lt ->
          case old_attr {
            Event(handler:, ..) -> {
              let events = events.forget(events, events.Event(map_old, handler))
              let removed = [old_attr, ..removed]

              diff_attrs(prev, new, map_old, map_new, added, removed, events)
            }
            _ -> {
              let removed = [old_attr, ..removed]
              diff_attrs(prev, new, map_old, map_new, added, removed, events)
            }
          }
      }
  }
}

// EVENT HANDLERS --------------------------------------------------------------

fn forget_events(
  events: Events(msg),
  mapper: List(fn(Dynamic) -> Dynamic),
  element: Element(msg),
) -> Events(msg) {
  let mapper = push_mapper(element, mapper)
  case element {
    Fragment(children:, ..) -> {
      use events, child <- list.fold(children, events)
      forget_events(events, mapper, child)
    }

    Node(attributes:, children:, ..) -> {
      let events = do_forget_events(attributes, mapper, events)
      use events, child <- list.fold(children, events)
      forget_events(events, mapper, child)
    }
    Text(..) -> events
  }
}

fn do_forget_events(
  attributes: List(Attribute(msg)),
  mapper: List(fn(Dynamic) -> Dynamic),
  events: Events(msg),
) -> Events(msg) {
  case attributes {
    [] -> events
    [Event(handler:, ..), ..rest] -> {
      let events = events.forget(events, events.Event(mapper, handler))
      do_forget_events(rest, mapper, events)
    }
    [_, ..rest] -> do_forget_events(rest, mapper, events)
  }
}

fn insert_events(
  events: Events(msg),
  mapper: List(fn(Dynamic) -> Dynamic),
  element: Element(msg),
) -> Events(msg) {
  let mapper = push_mapper(element, mapper)

  case element {
    Fragment(children:, ..) -> {
      use events, child <- list.fold(children, events)
      insert_events(events, mapper, child)
    }

    Node(attributes:, children:, ..) -> {
      let events = do_insert_events(attributes, mapper, events)
      use events, child <- list.fold(children, events)
      insert_events(events, mapper, child)
    }

    _ -> events
  }
}

fn do_insert_events(
  attributes: List(Attribute(msg)),
  mapper: List(fn(Dynamic) -> Dynamic),
  events: Events(msg),
) -> Events(msg) {
  case attributes {
    [] -> events

    [Event(handler:, ..), ..rest] -> {
      let events = events.insert(events, events.Event(mapper, handler))
      do_insert_events(rest, mapper, events)
    }

    [_, ..rest] -> do_insert_events(rest, mapper, events)
  }
}

fn push_mapper(
  element: Element(msg),
  mappers: List(fn(Dynamic) -> Dynamic),
) -> List(fn(Dynamic) -> Dynamic) {
  case element.mapper {
    Some(mapper) -> [mapper, ..mappers]
    None -> mappers
  }
}

//

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
        [Text(key: "", mapper: constants.option_none, content: a <> b), ..rest],
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

@external(javascript, "../../perf_crimes.ffi.mjs", "compare_attributes")
fn compare_attributes(a: Attribute(msg), b: Attribute(msg)) -> order.Order {
  string.compare(a.name, b.name)
}
