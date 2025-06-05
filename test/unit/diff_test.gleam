// IMPORTS ---------------------------------------------------------------------

import gleeunit/should
import lustre/attribute.{attribute}
import lustre/element
import lustre/element/html
import lustre/element/keyed
import lustre/vdom/diff
import lustre/vdom/events
import lustre/vdom/patch
import lustre/vdom/vnode
import lustre_test

//

pub fn empty_node_test() {
  use <- lustre_test.test_filter("empty_node_test")

  let prev = html.div([], [])
  let next = html.div([], [])
  let diff = patch.new(0, 0, [], [])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

// TEXT DIFFS ------------------------------------------------------------------

pub fn text_element_replaced_test() {
  use <- lustre_test.test_filter("text_element_replaced_test")

  let prev = html.text("Hello, World!")
  let next = html.text("Hello, Joe!")
  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.replace_text("Hello, Joe!")], []),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn text_to_element_replacement_test() {
  use <- lustre_test.test_filter("text_to_element_replacement_test")

  let prev = html.div([], [html.text("Hello")])
  let next = html.div([], [html.span([], [html.text("Hello")])])
  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [patch.replace(0, 1, html.span([], [html.text("Hello")]))],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

// NODE DIFFS ------------------------------------------------------------------

pub fn nested_attribute_changes_test() {
  use <- lustre_test.test_filter("nested_attribute_changes_test")

  let prev =
    html.div([], [
      html.span([attribute.class("old")], [
        html.p([attribute("data-test", "123")], []),
      ]),
    ])

  let next =
    html.div([], [
      html.span([attribute.class("new")], [
        html.p([attribute("data-test", "456")], []),
      ]),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [], [
        patch.new(0, 0, [patch.update([attribute.class("new")], [])], [
          patch.new(
            0,
            0,
            [patch.update([attribute("data-test", "456")], [])],
            [],
          ),
        ]),
      ]),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn node_attribute_added_test() {
  use <- lustre_test.test_filter("node_attribute_added_test")

  let prev = html.div([], [])
  let next = html.div([attribute.class("wibble")], [])
  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.update([attribute.class("wibble")], [])], []),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn node_attribute_removed_test() {
  use <- lustre_test.test_filter("node_attribute_removed_test")

  let prev = html.div([attribute.class("wibble")], [])
  let next = html.div([], [])
  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.update([], [attribute.class("wibble")])], []),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn node_many_attributes_changed_test() {
  use <- lustre_test.test_filter("node_many_attributes_changed_test")

  let prev =
    html.div([attribute("id", "cool-node"), attribute.class("wibble")], [])
  let next = html.div([attribute.class("wobble")], [])
  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [patch.update([attribute.class("wobble")], [attribute.id("cool-node")])],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn node_child_replaced_test() {
  use <- lustre_test.test_filter("node_child_replaced_test")

  let prev = html.div([], [html.p([], [])])
  let next = html.div([], [html.h1([], [])])
  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.replace(0, 1, html.h1([], []))], []),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn node_many_children_changed_test() {
  use <- lustre_test.test_filter("node_many_children_changed_test")

  let prev =
    html.div([], [
      html.h1([], [html.text("Welcome!")]),
      html.p([], [html.text("...")]),
    ])

  let next =
    html.div([], [
      html.h1([attribute.class("flash")], [html.text("Hello, Joe!")]),
      html.hr([]),
      html.p([], [html.text("...")]),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.insert([html.p([], [html.text("...")])], 2),
          patch.replace(1, 1, html.hr([])),
        ],
        [
          patch.new(0, 0, [patch.update([attribute.class("flash")], [])], [
            patch.new(0, 0, [patch.replace_text("Hello, Joe!")], []),
          ]),
        ],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn node_children_removed_test() {
  use <- lustre_test.test_filter("node_children_removed_test")

  let prev = html.div([], [html.h1([], []), html.p([], [])])
  let next = html.div([], [html.h1([], [])])
  let diff = patch.new(0, 0, [], [patch.new(0, 1, [], [])])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

// // FRAGMENT DIFFS --------------------------------------------------------------

pub fn fragment_many_children_changed_test() {
  use <- lustre_test.test_filter("fragment_many_children_changed_test")

  let prev =
    element.fragment([
      html.h1([], [html.text("Welcome!")]),
      html.p([], [html.text("...")]),
    ])
  let next =
    element.fragment([
      html.h1([attribute.class("flash")], [html.text("Hello, Joe!")]),
      html.hr([]),
      html.p([], [html.text("...")]),
    ])

  let diff =
    patch.new(
      0,
      0,
      [
        patch.insert([html.p([], [html.text("...")])], 3),
        patch.replace(2, 1, html.hr([])),
      ],
      [
        patch.new(1, 0, [patch.update([attribute.class("flash")], [])], [
          patch.new(0, 0, [patch.replace_text("Hello, Joe!")], []),
        ]),
      ],
    )

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn fragment_child_replaced_test() {
  use <- lustre_test.test_filter("fragment_child_replaced_test")

  let prev = element.fragment([html.p([], [])])
  let next = element.fragment([html.h1([], [])])
  let diff = patch.new(0, 0, [patch.replace(1, 1, html.h1([], []))], [])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn nested_fragment_child_replaced_test() {
  use <- lustre_test.test_filter("nested_fragment_child_replaced_test")

  let prev =
    element.fragment([element.fragment([html.p([], [])]), html.p([], [])])

  let next =
    element.fragment([element.fragment([html.h1([], [])]), html.p([], [])])

  let diff = patch.new(0, 0, [patch.replace(2, 1, html.h1([], []))], [])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn fragment_children_removed_test() {
  use <- lustre_test.test_filter("fragment_children_removed_test")

  let prev =
    html.div([], [
      element.fragment([html.h1([], []), html.p([], []), html.p([], [])]),
      html.p([], []),
    ])

  let next = html.div([], [element.fragment([html.h1([], [])]), html.p([], [])])

  let diff =
    patch.new(0, 0, [], [patch.new(0, 0, [patch.remove(from: 2, count: 2)], [])])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn nested_fragment_children_removed_test() {
  use <- lustre_test.test_filter("nested_fragment_children_removed_test")

  let prev =
    html.div([], [
      element.fragment([
        element.fragment([html.h1([], []), html.p([], [])]),
        html.p([], []),
        html.p([], []),
      ]),
      html.p([], []),
    ])

  let next =
    html.div([], [
      element.fragment([html.h1([], []), html.p([], [])]),
      html.div([], []),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.replace(from: 6, count: 1, with: html.div([], [])),
          patch.replace(from: 1, count: 3, with: html.h1([], [])),
          patch.remove(from: 3, count: 1),
        ],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn fragment_update_with_different_children_counts_test() {
  use <- lustre_test.test_filter(
    "fragment_update_with_different_children_counts_test",
  )

  let abc = element.fragment([html.text("a"), html.text("b"), html.text("c")])

  let x = html.text("x")

  let y = element.fragment([html.text("y")])

  let prev = html.div([], [x, y, abc])
  let next = html.div([], [y, abc, x])

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.replace(from: 3, count: 4, with: x),
          patch.insert([html.text("b"), html.text("c")], before: 3),
          patch.replace(from: 0, count: 1, with: y),
        ],
        [patch.new(3, 0, [patch.replace_text("a")], [])],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn fragment_prepend_and_replace_with_node_test() {
  use <- lustre_test.test_filter("fragment_prepend_and_replace_with_node_test")

  let ab = element.fragment([html.text("a"), html.text("b")])
  let x = html.text("x")

  let prev = html.div([], [ab])
  let next = html.div([], [x, ab])

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.insert([ab], before: 3),
          patch.replace(from: 0, count: 3, with: x),
        ],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn fragment_update_and_remove_test() {
  use <- lustre_test.test_filter("fragment_update_and_remove_test")

  let a = html.text("a")
  let bc = element.fragment([html.text("b"), html.text("c")])
  let de = element.fragment([html.text("d"), html.text("e")])

  let prev = html.div([], [a, bc, de])
  let next = html.div([], [a, de])

  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 3, [], [
        patch.new(3, 0, [patch.replace_text("e")], []),
        patch.new(2, 0, [patch.replace_text("d")], []),
      ]),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn multiple_nested_fragments_test() {
  use <- lustre_test.test_filter("multiple_nested_fragments_test")

  let prev =
    element.fragment([
      element.fragment([element.fragment([html.text("deep")]), html.p([], [])]),
      html.div([], []),
    ])

  let next =
    element.fragment([
      element.fragment([
        element.fragment([html.text("changed")]),
        html.p([], []),
      ]),
      html.div([], []),
    ])

  let diff =
    patch.new(0, 0, [], [patch.new(3, 0, [patch.replace_text("changed")], [])])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

// KEYED DIFFS -----------------------------------------------------------------

pub fn keyed_swap_test() {
  use <- lustre_test.test_filter("keyed_swap_test")

  let prev =
    keyed.div([], [#("a", html.text("wibble")), #("b", html.text("wobble"))])

  let next =
    keyed.div([], [#("b", html.text("wobble")), #("a", html.text("wibble"))])

  let diff = patch.new(0, 0, [], [patch.new(0, 0, [patch.move("b", 0, 1)], [])])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn keyed_reorder_test() {
  use <- lustre_test.test_filter("keyed_reorder_test")

  let prev =
    keyed.div([], [
      #("a", html.p([], [])),
      #("b", html.div([], [])),
      #("c", html.img([])),
    ])

  let next =
    keyed.div([], [
      #("a", html.p([], [])),
      #("c", html.img([])),
      #("b", html.div([], [])),
    ])

  let diff = patch.new(0, 0, [], [patch.new(0, 0, [patch.move("c", 1, 1)], [])])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn keyed_insert_test() {
  use <- lustre_test.test_filter("keyed_insert_test")

  let prev =
    keyed.div([], [
      #("a", html.p([], [])),
      #("b", html.div([], [])),
      #("c", html.img([])),
    ])

  let next =
    keyed.div([], [
      #("c", html.img([])),
      #("a", html.p([], [])),
      #("d", html.span([], [])),
      #("b", html.div([], [])),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.insert([vnode.to_keyed("d", html.span([], []))], 1),
          patch.move("c", 0, 1),
        ],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn keyed_list_with_updates_test() {
  use <- lustre_test.test_filter("keyed_list_with_updates_test")

  let prev =
    keyed.div([], [
      #("1", html.div([attribute.class("old")], [html.text("one")])),
      #("2", html.div([attribute.class("old")], [html.text("two")])),
    ])

  let next =
    keyed.div([], [
      #("2", html.div([attribute.class("new")], [html.text("two")])),
      #("1", html.div([attribute.class("new")], [html.text("one")])),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.move("2", 0, 1)], [
        patch.new(1, 0, [patch.update([attribute.class("new")], [])], []),
        patch.new(0, 0, [patch.update([attribute.class("new")], [])], []),
      ]),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn mixed_keyed_and_regular_nodes_test() {
  use <- lustre_test.test_filter("mixed_keyed_and_regular_nodes_test")

  let prev =
    html.div([], [
      keyed.ul([], [
        #("1", html.li([], [html.text("one")])),
        #("2", html.li([], [html.text("two")])),
      ]),
      html.p([], [html.text("regular")]),
    ])

  let next =
    html.div([], [
      keyed.ul([], [
        #("2", html.li([], [html.text("two")])),
        #("1", html.li([], [html.text("one")])),
      ]),
      html.p([], [html.text("changed")]),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [], [
        patch.new(1, 0, [], [
          patch.new(0, 0, [patch.replace_text("changed")], []),
        ]),
        patch.new(0, 0, [patch.move("2", 0, 1)], []),
      ]),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn complex_attribute_changes_test() {
  use <- lustre_test.test_filter("complex_attribute_changes_test")

  let prev =
    html.div(
      [
        attribute.class("one two"),
        attribute("data-test", "old"),
        attribute("style", "color: red"),
      ],
      [],
    )

  let next =
    html.div(
      [
        attribute.class("two three"),
        attribute("style", "color: blue"),
        attribute("aria-label", "new"),
      ],
      [],
    )

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.update(
            [
              attribute("style", "color: blue"),
              attribute.class("two three"),
              attribute("aria-label", "new"),
            ],
            [attribute("data-test", "old")],
          ),
        ],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn multiple_class_and_styles_test() {
  use <- lustre_test.test_filter("multiple_class_and_styles_test")

  let prev =
    html.div(
      [
        attribute.class("one"),
        attribute.class("two three"),
        attribute("style", "color: red"),
        attribute("style", "background: green"),
      ],
      [],
    )

  let next =
    html.div(
      [
        attribute.class("two three"),
        attribute.class("four"),
        attribute("style", "color: blue"),
        attribute("style", "font-size: 2em"),
      ],
      [],
    )

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.update(
            [
              attribute("style", "color: blue;font-size: 2em"),
              attribute.class("two three four"),
            ],
            [],
          ),
        ],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn empty_to_multiple_children_test() {
  use <- lustre_test.test_filter("empty_to_multiple_children_test")

  let prev = html.div([], [])
  let next =
    html.div([], [
      html.h1([], [html.text("Title")]),
      html.p([], [html.text("Paragraph")]),
      html.span([], [html.text("Span")]),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.insert(
            [
              html.h1([], [html.text("Title")]),
              html.p([], [html.text("Paragraph")]),
              html.span([], [html.text("Span")]),
            ],
            0,
          ),
        ],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn mixed_text_and_element_changes_test() {
  use <- lustre_test.test_filter("mixed_text_and_element_changes_test")

  let prev =
    html.div([], [
      html.text("start"),
      html.p([], [html.text("middle")]),
      html.text("end"),
    ])

  let next =
    html.div([], [
      html.text("new start"),
      html.p([], [html.text("new middle")]),
      html.text("new end"),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [], [
        patch.new(2, 0, [patch.replace_text("new end")], []),
        patch.new(1, 0, [], [
          patch.new(0, 0, [patch.replace_text("new middle")], []),
        ]),
        patch.new(0, 0, [patch.replace_text("new start")], []),
      ]),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

// KEYED DIFFS WITH FRAGMENTS --------------------------------------------------

pub fn keyed_move_fragment_with_replace_with_different_count_test() {
  use <- lustre_test.test_filter(
    "keyed_move_fragment_with_replace_with_different_count_test",
  )

  let x = element.fragment([html.text("x")])
  let prev =
    keyed.div([], [
      #("x", x),
      #("ab", element.fragment([html.text("a"), html.text("b")])),
    ])

  let cd = element.fragment([html.text("c"), html.text("d")])
  let next =
    keyed.div([], [
      #("ab", element.fragment([html.text("a"), html.text("b")])),
      #("cd", cd),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [patch.insert([vnode.to_keyed("cd", cd)], 5), patch.remove_key("x", 2)],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)

  // reverse

  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 3, [patch.insert([vnode.to_keyed("x", x)], 0)], []),
    ])

  diff.diff(events.new(), next, prev).patch
  |> should.equal(diff)
}

pub fn keyed_move_fragment_with_replace_to_simple_node_test() {
  use <- lustre_test.test_filter(
    "keyed_move_fragment_with_replace_to_simple_node_test",
  )

  let x = element.fragment([html.text("x")])
  let ab = element.fragment([html.text("a"), html.text("b")])

  let prev = keyed.div([], [#("x", x), #("a", ab)])

  let next =
    keyed.div([], [
      #("a", html.text("a")),
      #("b", html.text("b")),
      #("c", html.text("c")),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.insert(
            [
              vnode.to_keyed("b", html.text("b")),
              vnode.to_keyed("c", html.text("c")),
            ],
            5,
          ),
          patch.replace(2, 3, vnode.to_keyed("a", html.text("a"))),
          patch.remove_key("x", 2),
        ],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn keyed_replace_fragment_test() {
  use <- lustre_test.test_filter("keyed_replace_fragment_test")

  let x = element.fragment([html.text("x")])
  let ab = element.fragment([html.text("a"), html.text("b")])
  let prev = keyed.div([], [#("x", x), #("ab", ab)])

  let next =
    keyed.div([], [
      #("a", html.text("a")),
      #("b", html.text("b")),
      #("c", html.text("c")),
    ])

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.insert([vnode.to_keyed("c", html.text("c"))], 5),
          patch.replace(2, 3, vnode.to_keyed("b", html.text("b"))),
          patch.replace(0, 2, vnode.to_keyed("a", html.text("a"))),
        ],
        [],
      ),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn keyed_insert_fragment_test() {
  use <- lustre_test.test_filter("keyed_insert_fragment_test")

  let prev = keyed.div([], [#("a", html.text("a"))])
  let xyz = element.fragment([html.text("x"), html.text("y"), html.text("z")])
  let next = keyed.div([], [#("xyz", xyz), #("a", html.text("A"))])

  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.insert([vnode.to_keyed("xyz", xyz)], 0)], [
        patch.new(4, 0, [patch.replace_text("A")], []),
      ]),
    ])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

// KEYED FRAGMENTS -------------------------------------------------------------

pub fn keyed_fragment_swap_test() {
  use <- lustre_test.test_filter("keyed_fragment_swap_test")

  let prev =
    keyed.fragment([#("a", html.text("wibble")), #("b", html.text("wobble"))])

  let next =
    keyed.fragment([#("b", html.text("wobble")), #("a", html.text("wibble"))])

  let diff = patch.new(0, 0, [patch.move("b", before: 1, count: 1)], [])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn keyed_fragment_reorder_test() {
  use <- lustre_test.test_filter("keyed_fragment_reorder_test")

  let prev =
    keyed.fragment([
      #("a", html.p([], [])),
      #("b", html.div([], [])),
      #("c", html.img([])),
    ])

  let next =
    keyed.fragment([
      #("a", html.p([], [])),
      #("c", html.img([])),
      #("b", html.div([], [])),
    ])

  let diff = patch.new(0, 0, [patch.move("c", before: 2, count: 1)], [])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn keyed_fragment_insert_test() {
  use <- lustre_test.test_filter("keyed_fragment_insert_test")

  let prev =
    keyed.fragment([
      #("a", html.p([], [])),
      #("b", html.div([], [])),
      #("c", html.img([])),
    ])

  let next =
    keyed.fragment([
      #("c", html.img([])),
      #("a", html.p([], [])),
      #("d", html.span([], [])),
      #("b", html.div([], [])),
    ])

  let diff =
    patch.new(
      0,
      0,
      [
        patch.insert([vnode.to_keyed("d", html.span([], []))], 2),
        patch.move("c", 1, 1),
      ],
      [],
    )

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}

pub fn keyed_fragment_remove_test() {
  use <- lustre_test.test_filter("keyed_fragment_remove_test")

  let prev =
    keyed.fragment([
      #("a", html.p([], [])),
      #("b", html.div([], [])),
      #("c", html.img([])),
    ])

  let next = keyed.fragment([#("a", html.p([], [])), #("c", html.img([]))])

  let diff = patch.new(0, 0, [patch.remove_key(key: "b", count: 1)], [])

  diff.diff(events.new(), prev, next).patch
  |> should.equal(diff)
}
