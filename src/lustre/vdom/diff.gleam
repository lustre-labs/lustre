// IMPORTS ---------------------------------------------------------------------

import gleam/json
import gleam/order.{Eq, Gt, Lt}
import lustre/internals/constants
import lustre/internals/mutable_map.{type MutableMap}
import lustre/internals/ref
import lustre/vdom/events.{type ConcreteTree, type Events}
import lustre/vdom/patch.{type Change, type Patch, Patch}
import lustre/vdom/path.{type Path}
import lustre/vdom/vattr.{type Attribute, Attribute, Event, Property}
import lustre/vdom/vnode.{
  type Element, Element, Fragment, Map, Memo, Text, UnsafeInnerHtml,
}

// TYPES -----------------------------------------------------------------------

///
///
pub type Diff(msg) {
  Diff(patch: Patch(msg), tree: ConcreteTree(msg))
}

type PartialDiff(msg) {
  PartialDiff(patch: Patch(msg), tree: ConcreteTree(msg), events: Events(msg))
}

// DIFFING ---------------------------------------------------------------------

pub fn diff(
  tree: ConcreteTree(msg),
  old: Element(msg),
  new: Element(msg),
) -> Diff(msg) {
  let tree = events.tick(tree)

  let PartialDiff(patch:, tree:, events:) =
    do_diff(
      old: [old, ..constants.empty_list],
      old_keyed: mutable_map.new(),
      new: [new, ..constants.empty_list],
      new_keyed: mutable_map.new(),
      //
      moved: mutable_map.new(),
      moved_offset: 0,
      removed: 0,
      //
      node_index: 0,
      patch_index: 0,
      changes: constants.empty_list,
      children: constants.empty_list,
      //
      path: path.root,
      tree:,
      events: events.events(tree),
    )

  Diff(patch:, tree: events.update_events(tree, events))
}

fn do_diff(
  old old: List(Element(msg)),
  old_keyed old_keyed: MutableMap(String, Element(msg)),
  new new: List(Element(msg)),
  new_keyed new_keyed: MutableMap(String, Element(msg)),
  //
  moved moved: MutableMap(String, Nil),
  moved_offset moved_offset: Int,
  removed removed: Int,
  //
  node_index node_index: Int,
  patch_index patch_index: Int,
  changes changes: List(Change(msg)),
  children children: List(Patch(msg)),
  //
  path path: Path,
  tree tree: ConcreteTree(msg),
  events events: Events(msg),
) -> PartialDiff(msg) {
  case old, new {
    [], [] -> {
      let patch = Patch(index: patch_index, removed:, changes:, children:)
      PartialDiff(patch:, tree:, events:)
    }

    // We've run out of new children to diff. We now need to check whether the
    // each remaining child has been moved (and so can be ignored) here, or if it
    // needs to be removed.
    [prev, ..old], [] -> {
      let removed = case
        prev.key == "" || !mutable_map.has_key(moved, prev.key)
      {
        // This node wasn't keyed or it wasn't moved during a keyed diff. Either
        // way, we need to remove it!
        True -> removed + 1
        False -> removed
      }

      let events = events.remove_child(tree, events, path, node_index, prev)

      do_diff(
        old:,
        old_keyed:,
        new:,
        new_keyed:,
        moved:,
        moved_offset:,
        removed:,
        node_index:,
        patch_index:,
        changes:,
        children:,
        path:,
        tree:,
        events:,
      )
    }

    // We've run out of old children to diff. We can produce a single `Insert`
    // change to group the all the new children together, but we still need to
    // walk the list and extract their event handlers (and their children's
    // event handlers...)
    [], [_, ..] -> {
      let #(tree, events) =
        events.add_children(tree, events, path, node_index, new)

      let insert =
        patch.insert(children: new, before: node_index - moved_offset)
      let changes = [insert, ..changes]
      let patch = Patch(index: patch_index, removed:, changes:, children:)

      PartialDiff(patch:, tree:, events:)
    }

    // We might be able to diff these two nodes, but at least one of them has a
    // key so we need to treat this like a keyed diff. That involves working out
    // if there's a node somewhere else in the tree with the same key we can diff
    // against, or if we need to insert the incoming vnode.
    [prev, ..old_remaining], [next, ..new_remaining] if prev.key != next.key -> {
      let next_did_exist = mutable_map.has_key(old_keyed, next.key)
      let prev_does_exist = mutable_map.has_key(new_keyed, prev.key)

      case prev_does_exist, next_did_exist {
        True, True ->
          case mutable_map.has_key(moved, prev.key) {
            // The previous child was already visited and moved during this diff.
            // That means we'll skip over this diff iteration and instead decrement
            // the `moved_offset` to account for it.
            True ->
              do_diff(
                old: old_remaining,
                old_keyed:,
                new:,
                new_keyed:,
                moved:,
                moved_offset: moved_offset - 1,
                removed:,
                node_index:,
                patch_index:,
                changes:,
                children:,
                path:,
                tree:,
                events:,
              )

            // The previous child exists in the incoming tree and this is the first
            // time we're seeing it this diff. We do this by moving the incoming node
            // further up the tree and attempt the diff again, this time against the
            // previous matching keyed child.
            //
            // After that the `prev` node has a chance to match against the next node
            // in the diff and so on, minimising moves.
            //
            // Since nodes are only ever moved *up* in the tree we have to take this
            // `prev` node into account until we see it again later in the diff. The
            // reconciler applies changes in reverse order, so during patching we first
            // will visit the index the `prev` node was moved *from*. Between that
            // index and the index the node was moved to we need to offset any other
            // work to account for this change that will occur later.
            //
            // `node_index` is the final index a node would be moved to.
            // `node_index - moved_offset` is the current (temporary) index of the
            // node during patching.
            //
            // Consider the following example:
            //
            // old children: [a b c d]
            // new children: [c a b d]
            //
            // First we perform a diff to generate a `Patch` that contains both changes
            // to apply to the parent node and a list of _child patches_ to update
            // children.
            //
            //   perform diff -------------------------------------------> changes -----> children
            //                            old         new       idx offs
            // ↓ 1. move c before 0-0=0 ↓ [a b c d]   [c b a d] 0   0    ↑ 2. [c b a d] ↑
            // ↓ 2. update c at idx=0   ↓ [c a b c d] [c b a d] 0   1    ↑              ↑ 6. [C B A D]
            // ↓ 3. move b before 1-1=0 ↓ [a b c d]   [b a d]   1   1    ↑ 1. [b a c d] ↑
            // ↓ 4. update b at idx=1   ↓ [b a b c d] [b a d]   1   2    ↑              ↑ 5. [c B A D]
            // ↓ 5. update a at idx=2   ↓ [a b c d]   [a d]     2   2    ↑              ↑ 4. [c b A D]
            // ↓ 6. fixup offset for b  ↓ [b c d]     [d]       3   2    ↑              ↑
            // ↓ 7. fixup offset for c  ↓ [c d]       [d]       3   1    ↑              ↑
            // ↓ 8. update d at idx=3   ↓ [d]         [d]       3   0    ↑ 0. [a b c d] ↑ 3. [c b a D]
            //
            False -> {
              let match = mutable_map.unsafe_get(old_keyed, next.key)
              let before = node_index - moved_offset
              let changes = [patch.move(key: next.key, before:), ..changes]
              let moved = mutable_map.insert(moved, next.key, Nil)

              do_diff(
                old: [match, ..old],
                old_keyed:,
                new:,
                new_keyed:,
                moved:,
                moved_offset: moved_offset + 1,
                removed:,
                node_index:,
                patch_index:,
                changes:,
                children:,
                path:,
                tree:,
                events:,
              )
            }
          }

        // The previous child no longer exists in the incoming tree, and the new
        // child did exist in the old tree. That means we need to add a `RemoveKey`
        // change and continue diffing the remaining nodes.
        False, True -> {
          let index = node_index - moved_offset
          let changes = [patch.remove(index), ..changes]
          let events = events.remove_child(tree, events, path, node_index, prev)

          do_diff(
            old: old_remaining,
            old_keyed:,
            new:,
            new_keyed:,
            moved:,
            moved_offset: moved_offset - 1,
            removed:,
            node_index:,
            patch_index:,
            changes:,
            children:,
            path:,
            tree:,
            events:,
          )
        }

        // The previous child still exists in the incoming tree, but the new child
        // is not keyed or did not exist as a keyed child in the previous render.
        // That means we need to add an `Insert` change.
        True, False -> {
          let before = node_index - moved_offset
          let #(tree, events) =
            events.add_child(tree, events, path, node_index, next)
          let insert = patch.insert(children: [next], before:)
          let changes = [insert, ..changes]

          do_diff(
            old:,
            old_keyed:,
            new: new_remaining,
            new_keyed:,
            moved:,
            moved_offset: moved_offset + 1,
            removed:,
            node_index: node_index + 1,
            patch_index:,
            changes:,
            children:,
            path:,
            tree:,
            events:,
          )
        }

        // The previous child no longer exists in the incoming tree *and* the new
        // child is new for this render. That means we can do a straight `Replace`.
        False, False -> {
          let change =
            patch.replace(index: node_index - moved_offset, with: next)

          let #(tree, events) =
            events.replace_child(tree, events, path, node_index, prev, next)

          do_diff(
            old: old_remaining,
            old_keyed:,
            new: new_remaining,
            new_keyed:,
            moved:,
            moved_offset:,
            removed:,
            node_index: node_index + 1,
            patch_index:,
            changes: [change, ..changes],
            children:,
            path:,
            tree:,
            events:,
          )
        }
      }
    }

    // The remaining cases handle like-for-like comparisons between nodes. In most
    // cases these means we can morph the existing DOM node into the new one by
    // producing precise changes.
    [Fragment(..) as prev, ..old], [Fragment(..) as next, ..new] -> {
      // We diff fragments as if they are "real" nodes with children.
      let PartialDiff(patch:, tree:, events:) =
        do_diff(
          old: prev.children,
          old_keyed: prev.keyed_children,
          new: next.children,
          new_keyed: next.keyed_children,
          moved: mutable_map.new(),
          moved_offset: 0,
          removed: 0,
          node_index: 0,
          patch_index: node_index,
          changes: constants.empty_list,
          children: constants.empty_list,
          path: path.add(path, node_index, next.key),
          tree:,
          events:,
        )

      let children = case patch {
        Patch(removed: 0, changes: [], children: [], ..) -> children
        _ -> [patch, ..children]
      }

      do_diff(
        old:,
        old_keyed:,
        new:,
        new_keyed:,
        moved:,
        moved_offset:,
        removed:,
        node_index: node_index + 1,
        patch_index:,
        changes:,
        children:,
        path:,
        tree:,
        events:,
      )
    }

    // We can morph an existing element into a new one iff they have the same
    // namespace and tag. If these change changed we have to tear down the node
    // and construct a new one, and its *very* safe to assume that if the tag has
    // changed the children have changed entirely too.
    [Element(..) as prev, ..old], [Element(..) as next, ..new]
      if prev.namespace == next.namespace && prev.tag == next.tag
    -> {
      let child_path = path.add(path, node_index, next.key)

      let controlled = is_controlled(tree, next.namespace, next.tag, child_path)

      let AttributeChange(events:, added: added_attrs, removed: removed_attrs) =
        diff_attributes(
          controlled: controlled,
          path: child_path,
          events:,
          old: prev.attributes,
          new: next.attributes,
          added: constants.empty_list,
          removed: constants.empty_list,
        )

      let initial_child_changes = case added_attrs, removed_attrs {
        [], [] -> constants.empty_list
        _, _ -> [patch.update(added: added_attrs, removed: removed_attrs)]
      }

      let PartialDiff(patch:, tree:, events:) =
        do_diff(
          old: prev.children,
          old_keyed: prev.keyed_children,
          new: next.children,
          new_keyed: next.keyed_children,
          moved: mutable_map.new(),
          moved_offset: 0,
          removed: 0,
          node_index: 0,
          patch_index: node_index,
          changes: initial_child_changes,
          children: constants.empty_list,
          path: child_path,
          tree:,
          events:,
        )

      let children = case patch {
        Patch(removed: 0, changes: [], children: [], ..) -> children
        _ -> [patch, ..children]
      }

      do_diff(
        old:,
        old_keyed:,
        new:,
        new_keyed:,
        moved:,
        moved_offset:,
        removed:,
        node_index: node_index + 1,
        patch_index:,
        changes:,
        children:,
        path:,
        tree:,
        events:,
      )
    }

    // It is trivial to compare if a text node has changed. If it hasn't, we can
    // skip over it and continue diffing the next nodes.
    [Text(..) as prev, ..old], [Text(..) as next, ..new]
      if prev.content == next.content
    ->
      do_diff(
        old:,
        old_keyed:,
        new:,
        new_keyed:,
        moved:,
        moved_offset:,
        removed:,
        node_index: node_index + 1,
        patch_index:,
        changes:,
        children:,
        path:,
        tree:,
        events:,
      )

    // Text nodes have a special `ReplaceText` change that allows us to update
    // the text content of the node without replacing the node itself.
    [Text(..), ..old], [Text(..) as next, ..new] -> {
      let child =
        patch.new(
          index: node_index,
          removed: 0,
          changes: [patch.replace_text(next.content)],
          children: constants.empty_list,
        )

      do_diff(
        old:,
        old_keyed:,
        new:,
        new_keyed:,
        moved:,
        moved_offset:,
        removed:,
        node_index: node_index + 1,
        patch_index:,
        changes:,
        children: [child, ..children],
        path:,
        tree:,
        events:,
      )
    }

    [UnsafeInnerHtml(..) as prev, ..old], [UnsafeInnerHtml(..) as next, ..new] -> {
      let child_path = path.add(path, node_index, next.key)

      let AttributeChange(events:, added: added_attrs, removed: removed_attrs) =
        diff_attributes(
          controlled: False,
          path: child_path,
          events:,
          old: prev.attributes,
          new: next.attributes,
          added: constants.empty_list,
          removed: constants.empty_list,
        )

      let child_changes = case added_attrs, removed_attrs {
        [], [] -> constants.empty_list
        _, _ -> [patch.update(added: added_attrs, removed: removed_attrs)]
      }

      let child_changes = case prev.inner_html == next.inner_html {
        True -> child_changes
        False -> [patch.replace_inner_html(next.inner_html), ..child_changes]
      }

      let children = case child_changes {
        [] -> children
        _ -> [patch.new(node_index, 0, child_changes, []), ..children]
      }

      do_diff(
        old:,
        old_keyed:,
        new:,
        new_keyed:,
        moved:,
        moved_offset:,
        removed:,
        node_index: node_index + 1,
        patch_index:,
        changes:,
        children:,
        path:,
        tree:,
        events:,
      )
    }

    [Map(..) as prev, ..old], [Map(..) as next, ..new] -> {
      let child_path = path.add(path, node_index, next.key)
      let child_key = path.child(child_path)

      let PartialDiff(patch:, tree:, events: child_events) =
        do_diff(
          old: [prev.child, ..constants.empty_list],
          old_keyed: mutable_map.new(),
          new: [next.child, ..constants.empty_list],
          new_keyed: mutable_map.new(),
          moved: mutable_map.new(),
          moved_offset: 0,
          removed: 0,
          node_index: 0,
          patch_index: node_index,
          changes: constants.empty_list,
          children: constants.empty_list,
          path: path.subtree(child_path),
          tree:,
          events: events.get_subtree(events, child_key, old_mapper: prev.mapper),
        )

      let events =
        events.update_subtree(events, child_key, next.mapper, child_events)

      let children = case patch {
        Patch(removed: 0, changes: [], children: [], ..) -> children
        _ -> [patch, ..children]
      }

      do_diff(
        old:,
        old_keyed:,
        new:,
        new_keyed:,
        moved:,
        moved_offset:,
        node_index: node_index + 1,
        removed:,
        patch_index:,
        changes:,
        children:,
        path:,
        tree:,
        events:,
      )
    }

    [Memo(..) as prev, ..old], [Memo(..) as next, ..new] -> {
      case ref.equal_lists(prev.dependencies, next.dependencies) {
        True -> {
          let tree = events.keep_memo(tree, prev.view, next.view)

          do_diff(
            old:,
            old_keyed:,
            new:,
            new_keyed:,
            moved:,
            moved_offset:,
            removed:,
            node_index: node_index + 1,
            patch_index:,
            changes:,
            children:,
            path:,
            tree:,
            events:,
          )
        }

        False -> {
          let prev_node = events.get_old_memo(tree, prev.view, prev.view)

          let next_node = next.view()
          let tree = events.add_memo(tree, new: next.view, node: next_node)

          let PartialDiff(patch:, events:, tree:) =
            do_diff(
              old: [prev_node, ..constants.empty_list],
              old_keyed: mutable_map.new(),
              new: [next_node, ..constants.empty_list],
              new_keyed: mutable_map.new(),
              moved: mutable_map.new(),
              moved_offset:,
              removed:,
              node_index:,
              patch_index:,
              changes:,
              children:,
              path:,
              tree:,
              events:,
            )

          let Patch(index: patch_index, removed:, changes:, children:) = patch

          do_diff(
            old:,
            old_keyed:,
            new:,
            new_keyed:,
            moved:,
            moved_offset:,
            removed:,
            node_index: node_index + 1,
            patch_index:,
            changes:,
            children:,
            path:,
            tree:,
            events:,
          )
        }
      }
    }

    // If we land here we've exhausted any other possibility for a more focused
    // diff of these two nodes. In this case, we just replace the old node with
    // the new one.
    [prev, ..old_remaining], [next, ..new_remaining] -> {
      let change = patch.replace(index: node_index - moved_offset, with: next)

      let #(tree, events) =
        events.replace_child(tree, events, path, node_index, prev, next)

      do_diff(
        old: old_remaining,
        old_keyed:,
        new: new_remaining,
        new_keyed:,
        moved:,
        moved_offset:,
        removed:,
        node_index: node_index + 1,
        patch_index:,
        changes: [change, ..changes],
        children:,
        path:,
        tree:,
        events:,
      )
    }
  }
}

// ATTRIBUTE DIFFING -----------------------------------------------------------

fn is_controlled(
  tree: ConcreteTree(msg),
  namespace: String,
  tag: String,
  path: Path,
) {
  case tag {
    "input" | "select" | "textarea" if namespace == "" ->
      events.has_dispatched_events(tree, path)
    _ -> False
  }
}

type AttributeChange(msg) {
  AttributeChange(
    added: List(Attribute(msg)),
    removed: List(Attribute(msg)),
    events: Events(msg),
  )
}

fn diff_attributes(
  controlled controlled: Bool,
  path path: Path,
  events events: Events(msg),
  old old: List(Attribute(msg)),
  new new: List(Attribute(msg)),
  added added: List(Attribute(msg)),
  removed removed: List(Attribute(msg)),
) -> AttributeChange(msg) {
  case old, new {
    [], [] -> AttributeChange(added:, removed:, events:)

    [Event(name:, ..) as prev, ..old], [] -> {
      let events = events.remove_event(events, path, name)
      let removed = [prev, ..removed]
      diff_attributes(controlled:, path:, events:, old:, new:, added:, removed:)
    }
    [prev, ..old], [] -> {
      let removed = [prev, ..removed]
      diff_attributes(controlled:, path:, events:, old:, new:, added:, removed:)
    }

    [], [Event(name:, handler:, ..) as next, ..new] -> {
      let events = events.add_event(events, path, name, handler)
      let added = [next, ..added]
      diff_attributes(controlled:, path:, events:, old:, new:, added:, removed:)
    }

    [], [next, ..new] -> {
      let added = [next, ..added]
      diff_attributes(controlled:, path:, events:, old:, new:, added:, removed:)
    }

    [prev, ..remaining_old], [next, ..remaining_new] ->
      case prev, vattr.compare(prev, next), next {
        Attribute(..), Eq, Attribute(..) -> {
          let has_changes = case next.name {
            "value" | "checked" | "selected" ->
              controlled || prev.value != next.value
            _ -> prev.value != next.value
          }

          let added = case has_changes {
            True -> [next, ..added]
            False -> added
          }

          diff_attributes(
            controlled:,
            path:,
            events:,
            old: remaining_old,
            new: remaining_new,
            added:,
            removed:,
          )
        }

        Property(..), Eq, Property(..) -> {
          let has_changes = case next.name {
            "scrollLeft" | "scrollRight" -> True
            "value" | "checked" | "selected" ->
              controlled || !property_value_equal(prev.value, next.value)
            _ -> !property_value_equal(prev.value, next.value)
          }

          let added = case has_changes {
            True -> [next, ..added]
            False -> added
          }

          diff_attributes(
            controlled:,
            path:,
            events:,
            old: remaining_old,
            new: remaining_new,
            added:,
            removed:,
          )
        }

        Event(..), Eq, Event(name:, handler:, ..) -> {
          let has_changes =
            prev.prevent_default.kind != next.prevent_default.kind
            || prev.stop_propagation.kind != next.stop_propagation.kind
            || prev.debounce != next.debounce
            || prev.throttle != next.throttle

          let added = case has_changes {
            True -> [next, ..added]
            False -> added
          }

          diff_attributes(
            controlled:,
            path:,
            events: events.add_event(events, path, name, handler),
            old: remaining_old,
            new: remaining_new,
            added:,
            removed:,
          )
        }

        Event(name:, ..), Eq, _ -> {
          diff_attributes(
            controlled:,
            path:,
            events: events.remove_event(events, path, name),
            old: remaining_old,
            new: remaining_new,
            added: [next, ..added],
            removed: [prev, ..removed],
          )
        }

        _, Eq, Event(name:, handler:, ..) -> {
          diff_attributes(
            controlled:,
            path:,
            events: events.add_event(events, path, name, handler),
            old: remaining_old,
            new: remaining_new,
            added: [next, ..added],
            removed: [prev, ..removed],
          )
        }

        _, Eq, _ -> {
          diff_attributes(
            controlled:,
            path:,
            events:,
            old: remaining_old,
            new: remaining_new,
            added: [next, ..added],
            removed: [prev, ..removed],
          )
        }

        _, Gt, Event(name:, handler:, ..) -> {
          diff_attributes(
            controlled:,
            path:,
            events: events.add_event(events, path, name, handler),
            old:,
            new: remaining_new,
            added: [next, ..added],
            removed:,
          )
        }

        _, Gt, _ -> {
          diff_attributes(
            controlled:,
            path:,
            events:,
            old:,
            new: remaining_new,
            added: [next, ..added],
            removed:,
          )
        }

        Event(name:, ..), Lt, _ -> {
          diff_attributes(
            controlled:,
            path:,
            events: events.remove_event(events, path, name),
            old: remaining_old,
            new:,
            added:,
            removed: [prev, ..removed],
          )
        }

        _, Lt, _ -> {
          diff_attributes(
            controlled:,
            path:,
            events:,
            old: remaining_old,
            new:,
            added:,
            removed: [prev, ..removed],
          )
        }
      }
  }
}

@external(javascript, "../internals/equals.ffi.mjs", "isEqual")
fn property_value_equal(a: json.Json, b: json.Json) -> Bool {
  a == b
}
