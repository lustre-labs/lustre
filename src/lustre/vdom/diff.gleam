// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/function
import gleam/list
import gleam/option.{None, Some}
import gleam/order.{Eq, Gt, Lt}
import gleam/set.{type Set}
import lustre/internals/constants
import lustre/vdom/attribute.{type Attribute, Attribute, Event, Property}
import lustre/vdom/events.{type Events}
import lustre/vdom/node.{type Node, Element, Fragment, Text, UnsafeInnerHtml}

// TYPES -----------------------------------------------------------------------

pub type Diff(msg) {
  Diff(patch: Patch(msg), events: Events(msg))
}

pub type Patch(msg) {
  Patch(
    index: Int,
    removed: Int,
    changes: List(Change(msg)),
    children: List(Patch(msg)),
  )
}

pub type Change(msg) {
  // node updates
  Replace(element: Node(msg))
  ReplaceText(content: String)
  ReplaceInnerHtml(inner_html: String)
  Update(added: List(Attribute(msg)), removed: List(Attribute(msg)))
  // keyed changes
  SetKey(index: Int, key: String)
  Move(key: String, before: Int, count: Int)
  RemoveKey(key: String, count: Int)
  // unkeyed changes
  Insert(children: List(Node(msg)), before: Int)
  Remove(from: Int, count: Int)
}

type AttributeChange(msg) {
  AttributeChange(
    added: List(Attribute(msg)),
    removed: List(Attribute(msg)),
    events: Events(msg),
  )
}

// DIFFING ---------------------------------------------------------------------

pub fn diff(
  old: Node(msg),
  new: Node(msg),
  initial_element_offset: Int,
) -> Diff(msg) {
  let diff =
    do_diff(
      old: [old],
      old_keyed: constants.empty_dict(),
      new: [new],
      new_keyed: constants.empty_dict(),
      //
      moved: constants.empty_set(),
      moved_offset: 0,
      removed: 0,
      //
      node_index: 0,
      patch_index: 0,
      changes: constants.empty_list,
      children: constants.empty_list,
      events: events.new(function.identity),
      mapper: function.identity,
    )

  case initial_element_offset > 0 {
    False -> diff
    True ->
      diff.patch
      |> offset_root_patch(initial_element_offset)
      |> Diff(patch: _, events: diff.events)
  }
}

fn offset_root_patch(root: Patch(msg), offset: Int) -> Patch(msg) {
  let changes =
    list.map(root.changes, fn(change) {
      case change {
        Insert(before:, ..) -> Insert(..change, before: before + offset)
        Move(before:, ..) -> Move(..change, before: before + offset)
        Remove(from:, ..) -> Remove(..change, from: from + offset)
        SetKey(index:, ..) -> SetKey(..change, index: index + offset)
        Replace(..)
        | ReplaceText(..)
        | ReplaceInnerHtml(..)
        | Update(..)
        | RemoveKey(..) -> change
      }
    })

  let children =
    list.map(root.children, fn(child) {
      Patch(..child, index: child.index + offset)
    })

  Patch(..root, changes:, children:)
}

fn do_diff(
  //
  old old: List(Node(msg)),
  old_keyed old_keyed: Dict(String, Node(msg)),
  new new: List(Node(msg)),
  new_keyed new_keyed: Dict(String, Node(msg)),
  //
  moved moved: Set(String),
  moved_offset moved_offset: Int,
  removed removed: Int,
  //
  node_index node_index: Int,
  patch_index patch_index: Int,
  changes changes: List(Change(msg)),
  children children: List(Patch(msg)),
  events events: Events(msg),
  mapper mapper: fn(Dynamic) -> Dynamic,
) -> Diff(msg) {
  case old, new {
    [], [] ->
      Diff(
        patch: Patch(index: patch_index, removed:, changes:, children:),
        events:,
      )

    // We've run out of new children to diff. We now need to check whether the
    // each remaining child has been moved (and so can be ignored) here, or if it
    // needs to be removed.
    [prev, ..old], [] -> {
      do_diff(
        old:,
        old_keyed:,
        new:,
        new_keyed:,
        moved:,
        moved_offset:,
        removed: case prev.key == "" || !set.contains(moved, prev.key) {
          // Tthis node wasn't keyed or it wasn't moved during a keyed diff. Either
          // way, we need to remove it! We call `advance` because if this vdom
          // node is a fragment, we need to account for however many children it
          // contains.
          True -> removed + advance(prev)
          False -> removed
        },
        node_index:,
        patch_index:,
        changes:,
        children:,
        events:,
        mapper:,
      )
    }

    // We've run out of old children to diff. We can produce a single `Insert`
    // change to group the all the new children together, but we still need to
    // walk the list and extract their event handlers (and their children's
    // event handlers).
    [], [_, ..] -> {
      let events =
        events.add_children(events, mapper, node_index - moved_offset, new)
      let insert = Insert(children: new, before: node_index - moved_offset)
      let changes = [insert, ..changes]

      Diff(
        patch: Patch(index: patch_index, removed:, changes:, children:),
        events: events,
      )
    }

    // We might be able to diff these two nodes, but at least one of them has a
    // key so we need to treat this like a keyed diff. That involves working out
    // if there's a node somewhere else in the tree with the same key we can diff
    // against, or if we need to insert the incoming node.
    [prev, ..old_remaining], [next, ..new_remaining] if prev.key != next.key -> {
      let next_did_exist = dict.get(old_keyed, next.key)
      let prev_does_exist = dict.get(new_keyed, prev.key)
      let prev_has_moved = set.contains(moved, prev.key)

      case prev_does_exist, next_did_exist {
        // The previous child was already visited and moved during this diff. That
        // means we'll skip over this diff iteration and instead decrement the
        // `moved_offset` to account for how many elements the prev node spanned.
        Ok(_), Ok(_) if prev_has_moved ->
          do_diff(
            old: old_remaining,
            old_keyed:,
            new:,
            new_keyed:,
            moved:,
            moved_offset: moved_offset - advance(prev),
            removed:,
            node_index:,
            patch_index:,
            changes:,
            children:,
            events:,
            mapper:,
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
        Ok(_), Ok(match) -> {
          let count = advance(next)
          let before = node_index - moved_offset
          let move = Move(key: next.key, before:, count:)
          let changes = [move, ..changes]
          let moved = set.insert(moved, next.key)
          let moved_offset = moved_offset + count

          do_diff(
            old: [match, ..old],
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
            events:,
            mapper:,
          )
        }

        // The previous child no longer exists in the incoming tree, and the new
        // child did exist in the old tree. That means we need to add a `RemoveKey`
        // change and continue diffing the remaining nodes.
        Error(_), Ok(_) -> {
          let count = advance(prev)
          let moved_offset = moved_offset - count
          let remove = RemoveKey(key: prev.key, count:)
          let changes = [remove, ..changes]

          do_diff(
            old: old_remaining,
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
            events:,
            mapper:,
          )
        }

        // The previous child still exists in the incoming tree, but the new child
        // is not keyed or did not exist as a keyed child in the previous render.
        // That means we need to add an `Insert` change.
        Ok(_), Error(_) -> {
          let before = node_index - moved_offset
          let count = advance(next)
          let events = events.add_child(events, mapper, node_index, next)
          let insert = Insert(children: [next], before:)
          let changes = [insert, ..changes]

          do_diff(
            old:,
            old_keyed:,
            new: new_remaining,
            new_keyed:,
            moved:,
            moved_offset: moved_offset + count,
            removed:,
            node_index: node_index + count,
            patch_index:,
            changes:,
            children:,
            events:,
            mapper:,
          )
        }

        // The previous child no longer exists in the incoming tree *and* the new
        // child is new for this render. The new element can "steal" the previous
        // one to continue diffing like normal by setting its key!
        Error(_), Error(_) -> {
          let prev_with_key = node.to_keyed(next.key, prev)

          let set_key = SetKey(index: node_index - moved_offset, key: next.key)
          let changes = [set_key, ..changes]

          do_diff(
            old: [prev_with_key, ..old_remaining],
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
            events:,
            mapper:,
          )
        }
      }
    }

    // The remaining cases handle like-for-like comparisons between nodes. In most
    // cases these means we can morph the existing DOM node into the new one by
    // producing precise changes.
    [Fragment(..) as prev, ..old], [Fragment(..) as next, ..new] -> {
      let prev_count = prev.children_count
      let next_count = next.children_count
      let changes = case prev_count - next_count {
        remove_count if remove_count > 0 -> {
          let remove_from = node_index + next_count - moved_offset
          let remove = Remove(from: remove_from, count: remove_count)

          [remove, ..changes]
        }

        _ -> changes
      }

      let composed_mapper = case next.mapper {
        Some(child_mapper) -> fn(msg) { msg |> child_mapper |> mapper }
        None -> mapper
      }

      // We diff fragments as if they are "real" nodes with children, but we must
      // remember to account update our offsets and indices to account for the
      // fact these are really more children of the parent node.
      let child =
        do_diff(
          old: prev.children,
          old_keyed: prev.keyed_children,
          new: next.children,
          new_keyed: next.keyed_children,
          moved: constants.empty_set(),
          moved_offset:,
          removed: 0,
          node_index:,
          patch_index: -1,
          changes:,
          children:,
          events:,
          mapper: composed_mapper,
        )

      do_diff(
        old:,
        old_keyed:,
        new:,
        new_keyed:,
        moved:,
        moved_offset: moved_offset + next_count - prev_count,
        removed:,
        node_index: node_index + next_count,
        patch_index:,
        changes: child.patch.changes,
        children: child.patch.children,
        events: child.events,
        mapper:,
      )
    }

    // We can morph an existing element into a new one iff they have the same
    // namespace and tag. If these change changed we have to tear down the node
    // and construct a new one, and its *very* safe to assume that if the tag has
    // changed the children have changed entirely too.
    [Element(..) as prev, ..old], [Element(..) as next, ..new]
      if prev.namespace == next.namespace && prev.tag == next.tag
    -> {
      let composed_mapper = case next.mapper {
        Some(child_mapper) -> fn(msg) { msg |> child_mapper |> mapper }
        None -> mapper
      }

      let attribute_change =
        diff_attributes(
          old: prev.attributes,
          new: next.attributes,
          added: constants.empty_list,
          removed: constants.empty_list,
          events: events.new(composed_mapper),
        )

      let initial_child_changes = case
        attribute_change.added,
        attribute_change.removed
      {
        [], [] -> constants.empty_list
        added, removed -> [Update(added:, removed:)]
      }

      let child =
        do_diff(
          old: prev.children,
          old_keyed: prev.keyed_children,
          new: next.children,
          new_keyed: next.keyed_children,
          moved: constants.empty_set(),
          moved_offset: 0,
          removed: 0,
          node_index: 0,
          patch_index: node_index,
          changes: initial_child_changes,
          children: constants.empty_list,
          events: attribute_change.events,
          mapper: composed_mapper,
        )

      let children = case child.patch {
        Patch(removed: 0, changes: [], children: [], ..) -> children
        _ -> [child.patch, ..children]
      }

      let events = case events.is_empty(child.events) {
        True -> events
        False if next.key != "" ->
          events.add_keyed_child_events(events, next.key, child.events)
        False -> events.add_child_events(events, node_index, child.events)
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
        patch_index: patch_index,
        changes:,
        children:,
        events:,
        mapper:,
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
        events:,
        mapper:,
      )

    // Text nodes have a special `ReplaceText` change that allows us to update
    // the text content of the node without replacing the node itself.
    [Text(..), ..old], [Text(..) as next, ..new] -> {
      let child =
        Patch(
          index: node_index,
          removed: 0,
          changes: [ReplaceText(next.content)],
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
        events:,
        mapper:,
      )
    }

    [UnsafeInnerHtml(..) as prev, ..old], [UnsafeInnerHtml(..) as next, ..new] -> {
      let composed_mapper = case next.mapper {
        Some(child_mapper) -> fn(msg) { msg |> child_mapper |> mapper }
        None -> mapper
      }

      let AttributeChange(
        added: added_attrs,
        removed: removed_attrs,
        events: child_events,
      ) =
        diff_attributes(
          old: prev.attributes,
          new: next.attributes,
          added: constants.empty_list,
          removed: constants.empty_list,
          events: events.new(composed_mapper),
        )

      let child_changes = case added_attrs, removed_attrs {
        [], [] -> constants.empty_list
        _, _ -> [Update(added: added_attrs, removed: removed_attrs)]
      }

      let child_changes = case prev.inner_html == next.inner_html {
        True -> child_changes
        False -> [ReplaceInnerHtml(next.inner_html), ..child_changes]
      }

      let children = case child_changes {
        [] -> children
        _ -> [Patch(node_index, 0, child_changes, []), ..children]
      }

      let events = case events.is_empty(child_events) {
        True -> events
        False if next.key != "" ->
          events.add_keyed_child_events(events, next.key, child_events)
        False -> events.add_child_events(events, node_index, child_events)
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
        patch_index: patch_index,
        changes:,
        children:,
        events:,
        mapper:,
      )
    }

    // If we land here we've exhausted any other possibility for a more focused
    // diff of these two nodes. In this case, we just replace the old node with
    // the new one.
    [prev, ..old], [next, ..new] -> {
      let prev_count = advance(prev)
      let changes = case prev_count > 1 {
        False -> changes
        True -> {
          let from = node_index - moved_offset + 1
          let remove = Remove(from:, count: prev_count - 1)

          [remove, ..changes]
        }
      }

      let child =
        Patch(
          index: node_index,
          removed: 0,
          changes: [Replace(next)],
          children: constants.empty_list,
        )

      let events = events.add_child(events, mapper, node_index, next)

      do_diff(
        old:,
        old_keyed:,
        new:,
        new_keyed:,
        moved:,
        moved_offset: moved_offset - prev_count + 1,
        removed:,
        node_index: node_index + 1,
        patch_index:,
        changes:,
        children: [child, ..children],
        events:,
        mapper:,
      )
    }
  }
}

// ATTRIBUTE DIFFING -----------------------------------------------------------

fn diff_attributes(
  old old: List(Attribute(msg)),
  new new: List(Attribute(msg)),
  added added: List(Attribute(msg)),
  removed removed: List(Attribute(msg)),
  events events: Events(msg),
) -> AttributeChange(msg) {
  case old, new {
    [], [] -> AttributeChange(added:, removed:, events:)

    [prev, ..old], [] ->
      diff_attributes(old:, new:, added:, removed: [prev, ..removed], events:)

    [], [Event(..) as next, ..new] ->
      diff_attributes(
        old:,
        new:,
        added: [next, ..added],
        removed:,
        events: events.add_event_listener(events, next.name, next.handler),
      )

    [], [next, ..new] ->
      diff_attributes(old:, new:, added: [next, ..added], removed:, events:)

    [prev, ..remaining_old], [next, ..remaining_new] ->
      case prev, attribute.compare(prev, next), next {
        Attribute(..), Eq, Attribute(..) ->
          case next.name {
            "value" | "checked" | "selected" ->
              diff_attributes(
                old: remaining_old,
                new: remaining_new,
                added: [next, ..added],
                removed:,
                events:,
              )

            _ if prev.value == next.value ->
              diff_attributes(
                old: remaining_old,
                new: remaining_new,
                added:,
                removed:,
                events:,
              )

            _ ->
              diff_attributes(
                old: remaining_old,
                new: remaining_new,
                added: [next, ..added],
                removed:,
                events:,
              )
          }

        Property(..), Eq, Property(..) ->
          case next.name {
            "value" | "checked" | "selected" | "scrollLeft" | "scrollRight" ->
              diff_attributes(
                old: remaining_old,
                new: remaining_new,
                added: [next, ..added],
                removed:,
                events:,
              )

            _ if prev.value == next.value ->
              diff_attributes(
                old: remaining_old,
                new: remaining_new,
                added:,
                removed:,
                events:,
              )

            _ ->
              diff_attributes(
                old: remaining_old,
                new: remaining_new,
                added: [next, ..added],
                removed:,
                events:,
              )
          }

        Event(..), Eq, Event(..) ->
          diff_attributes(
            old: remaining_old,
            new: remaining_new,
            added: case
              prev.prevent_default != next.prevent_default
              || prev.stop_propagation != next.stop_propagation
              || prev.immediate != next.immediate
            {
              True -> [next, ..added]
              False -> added
            },
            removed:,
            events: events.add_event_listener(events, next.name, next.handler),
          )

        _, Eq, Event(..) ->
          diff_attributes(
            old: remaining_old,
            new: remaining_new,
            added: [next, ..added],
            removed:,
            events: events.add_event_listener(events, next.name, next.handler),
          )

        _, Eq, _ ->
          diff_attributes(
            old: remaining_old,
            new: remaining_new,
            added: [next, ..added],
            removed: [prev, ..removed],
            events:,
          )

        _, Gt, _ ->
          diff_attributes(
            old:,
            new: remaining_new,
            added: [next, ..added],
            removed:,
            events:,
          )

        _, Lt, _ ->
          diff_attributes(
            old: remaining_old,
            new:,
            added:,
            removed: [prev, ..removed],
            events:,
          )
      }
  }
}

///
///
fn advance(node: Node(msg)) {
  case node {
    Fragment(children_count:, ..) -> children_count
    _ -> 1
  }
}
