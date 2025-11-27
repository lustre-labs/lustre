// IMPORTS ---------------------------------------------------------------------

import booklet
import gleam/dynamic
import gleam/json
import gleam/string
import lustre/attribute.{attribute}
import lustre/element
import lustre/element/html
import lustre/element/keyed
import lustre/event
import lustre/vdom/cache
import lustre/vdom/diff
import lustre/vdom/patch
import lustre/vdom/path
import lustre/vdom/vattr.{Handler}
import lustre/vdom/vnode
import lustre_test

//

pub fn empty_node_test() {
  use <- lustre_test.test_filter("empty_node_test")

  let prev = html.div([], [])
  let next = html.div([], [])
  let diff = patch.new(0, 0, [], [])

  assert diff.diff(cache.new(), prev, next).patch == diff
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

  assert diff.diff(cache.new(), prev, next).patch == diff
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
        [patch.replace(0, html.span([], [html.text("Hello")]))],
        [],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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

  assert diff.diff(cache.new(), prev, next).patch == diff
}

pub fn node_attribute_added_test() {
  use <- lustre_test.test_filter("node_attribute_added_test")

  let prev = html.div([], [])
  let next = html.div([attribute.class("wibble")], [])
  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.update([attribute.class("wibble")], [])], []),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
}

pub fn node_attribute_removed_test() {
  use <- lustre_test.test_filter("node_attribute_removed_test")

  let prev = html.div([attribute.class("wibble")], [])
  let next = html.div([], [])
  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.update([], [attribute.class("wibble")])], []),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
}

pub fn node_property_changed_test() {
  use <- lustre_test.test_filter("node_property_changed_test")

  let prev =
    html.div(
      [
        attribute.property(
          "data",
          json.object([#("a", json.int(1)), #("b", json.int(2))]),
        ),
      ],
      [],
    )
  let next = html.div([attribute.property("data", json.object([]))], [])

  let diff =
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [patch.update([attribute.property("data", json.object([]))], [])],
        [],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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

  assert diff.diff(cache.new(), prev, next).patch == diff
}

pub fn node_child_replaced_test() {
  use <- lustre_test.test_filter("node_child_replaced_test")

  let prev = html.div([], [html.p([], [])])
  let next = html.div([], [html.h1([], [])])
  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.replace(0, html.h1([], []))], []),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
          patch.replace(1, html.hr([])),
        ],
        [
          patch.new(0, 0, [patch.update([attribute.class("flash")], [])], [
            patch.new(0, 0, [patch.replace_text("Hello, Joe!")], []),
          ]),
        ],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
}

pub fn node_children_removed_test() {
  use <- lustre_test.test_filter("node_children_removed_test")

  let prev = html.div([], [html.h1([], []), html.p([], [])])
  let next = html.div([], [html.h1([], [])])
  let diff = patch.new(0, 0, [], [patch.new(0, 1, [], [])])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.insert([html.p([], [html.text("...")])], 2),
          patch.replace(1, html.hr([])),
        ],
        [
          patch.new(0, 0, [patch.update([attribute.class("flash")], [])], [
            patch.new(0, 0, [patch.replace_text("Hello, Joe!")], []),
          ]),
        ],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
}

pub fn fragment_child_replaced_test() {
  use <- lustre_test.test_filter("fragment_child_replaced_test")

  let prev = element.fragment([html.p([], [])])
  let next = element.fragment([html.h1([], [])])
  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.replace(0, html.h1([], []))], []),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
}

pub fn nested_fragment_child_replaced_test() {
  use <- lustre_test.test_filter("nested_fragment_child_replaced_test")

  let prev =
    element.fragment([element.fragment([html.p([], [])]), html.p([], [])])

  let next =
    element.fragment([element.fragment([html.h1([], [])]), html.p([], [])])

  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [], [
        patch.new(0, 0, [patch.replace(0, html.h1([], []))], []),
      ]),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
}

pub fn fragment_children_removed_test() {
  use <- lustre_test.test_filter("fragment_children_removed_test")

  let prev =
    html.div([], [
      element.fragment([html.h1([], []), html.p([], []), html.p([], [])]),
      html.p([], []),
    ])

  let next =
    html.div([], [
      element.fragment([html.h1([], [])]),
      html.p([], []),
      //
    ])

  let diff =
    patch.new(0, 0, [], [patch.new(0, 0, [], [patch.new(0, 2, [], [])])])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
      patch.new(0, 0, [patch.replace(index: 1, with: html.div([], []))], [
        patch.new(0, 1, [patch.replace(index: 0, with: html.h1([], []))], []),
      ]),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
        [patch.replace(index: 2, with: x), patch.replace(index: 0, with: y)],
        [
          patch.new(
            1,
            0,
            [
              patch.insert(before: 1, children: [html.text("b"), html.text("c")]),
            ],
            [patch.new(0, 0, [patch.replace_text("a")], [])],
          ),
        ],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
        [patch.insert([ab], before: 1), patch.replace(index: 0, with: x)],
        [],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
      patch.new(0, 1, [], [
        patch.new(1, 0, [], [
          patch.new(1, 0, [patch.replace_text("e")], []),
          patch.new(0, 0, [patch.replace_text("d")], []),
        ]),
      ]),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
    patch.new(0, 0, [], [
      patch.new(0, 0, [], [
        patch.new(0, 0, [], [
          patch.new(0, 0, [], [
            patch.new(0, 0, [patch.replace_text("changed")], []),
          ]),
        ]),
      ]),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
}

// KEYED DIFFS -----------------------------------------------------------------

pub fn keyed_swap_test() {
  use <- lustre_test.test_filter("keyed_swap_test")

  let prev =
    keyed.div([], [#("a", html.text("wibble")), #("b", html.text("wobble"))])

  let next =
    keyed.div([], [#("b", html.text("wobble")), #("a", html.text("wibble"))])

  let diff = patch.new(0, 0, [], [patch.new(0, 0, [patch.move("b", 0)], [])])

  assert diff.diff(cache.new(), prev, next).patch == diff
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

  let diff = patch.new(0, 0, [], [patch.new(0, 0, [patch.move("c", 1)], [])])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
          patch.move("c", 0),
        ],
        [],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
      patch.new(0, 0, [patch.move("2", 0)], [
        patch.new(1, 0, [patch.update([attribute.class("new")], [])], []),
        patch.new(0, 0, [patch.update([attribute.class("new")], [])], []),
      ]),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
        patch.new(0, 0, [patch.move("2", 0)], []),
      ]),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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

  assert diff.diff(cache.new(), prev, next).patch == diff
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

  assert diff.diff(cache.new(), prev, next).patch == diff
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

  assert diff.diff(cache.new(), prev, next).patch == diff
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

  assert diff.diff(cache.new(), prev, next).patch == diff
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
        [patch.insert([vnode.to_keyed("cd", cd)], before: 2), patch.remove(0)],
        [],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff

  // reverse

  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 1, [patch.insert([vnode.to_keyed("x", x)], 0)], []),
    ])

  assert diff.diff(cache.new(), next, prev).patch == diff
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
            before: 2,
          ),
          patch.replace(index: 1, with: vnode.to_keyed("a", html.text("a"))),
          patch.remove(0),
        ],
        [],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
          patch.insert([vnode.to_keyed("c", html.text("c"))], before: 2),
          patch.replace(index: 1, with: vnode.to_keyed("b", html.text("b"))),
          patch.replace(index: 0, with: vnode.to_keyed("a", html.text("a"))),
        ],
        [],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
}

pub fn keyed_insert_fragment_test() {
  use <- lustre_test.test_filter("keyed_insert_fragment_test")

  let prev = keyed.div([], [#("a", html.text("a"))])
  let xyz = element.fragment([html.text("x"), html.text("y"), html.text("z")])
  let next = keyed.div([], [#("xyz", xyz), #("a", html.text("A"))])

  let diff =
    patch.new(0, 0, [], [
      patch.new(0, 0, [patch.insert([vnode.to_keyed("xyz", xyz)], 0)], [
        patch.new(1, 0, [patch.replace_text("A")], []),
      ]),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
}

// KEYED FRAGMENTS -------------------------------------------------------------

pub fn keyed_fragment_swap_test() {
  use <- lustre_test.test_filter("keyed_fragment_swap_test")

  let prev =
    keyed.fragment([#("a", html.text("wibble")), #("b", html.text("wobble"))])

  let next =
    keyed.fragment([#("b", html.text("wobble")), #("a", html.text("wibble"))])

  let diff =
    patch.new(0, 0, [], [patch.new(0, 0, [patch.move("b", before: 0)], [])])

  assert diff.diff(cache.new(), prev, next).patch == diff
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

  let diff =
    patch.new(0, 0, [], [patch.new(0, 0, [patch.move("c", before: 1)], [])])

  assert diff.diff(cache.new(), prev, next).patch == diff
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
    patch.new(0, 0, [], [
      patch.new(
        0,
        0,
        [
          patch.insert([vnode.to_keyed("d", html.span([], []))], 1),
          patch.move("c", before: 0),
        ],
        [],
      ),
    ])

  assert diff.diff(cache.new(), prev, next).patch == diff
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

  let diff = patch.new(0, 0, [], [patch.new(0, 0, [patch.remove(1)], [])])

  assert diff.diff(cache.new(), prev, next).patch == diff
}

// MEMO TESTS ------------------------------------------------------------------

pub fn memo_not_recomputed_test() {
  use <- lustre_test.test_filter("memo_not_recomputed_test")

  let counter = booklet.new(0)
  let dep = element.ref(1)

  let view = fn() {
    booklet.update(counter, fn(n) { n + 1 })
    html.div([], [html.text("Hello")])
  }

  let vdom1 = element.memo([dep], view)
  let vdom2 = element.memo([dep], view)

  let cache = cache.new()
  let patch = diff.diff(cache, vdom1, vdom2).patch

  assert booklet.get(counter) == 1
  assert patch == patch.Patch(0, 0, [], [])
}

pub fn memo_recomputed_when_dependency_changes_test() {
  use <- lustre_test.test_filter("memo_recomputed_when_dependency_changes_test")

  let counter = booklet.new(0)

  let view = fn() {
    booklet.update(counter, fn(n) { n + 1 })
    html.div([], [html.text("Hello")])
  }

  let dep1 = element.ref(1)
  let dep2 = element.ref(2)

  let vdom1 = element.memo([dep1], view)
  let vdom2 = element.memo([dep2], view)

  let cache = cache.new()
  diff.diff(cache, vdom1, vdom2)

  assert booklet.get(counter) == 2
}

pub fn memo_with_multiple_dependencies_test() {
  use <- lustre_test.test_filter("memo_with_multiple_dependencies_test")

  let counter = booklet.new(0)

  let view = fn() {
    booklet.update(counter, fn(n) { n + 1 })
    html.div([], [html.text("Hello")])
  }

  let dep1 = element.ref(1)
  let dep2 = element.ref("a")

  let vdom1 = element.memo([dep1, dep2], view)
  let vdom2 = element.memo([dep1, dep2], view)

  let cache = cache.new()
  let patch = diff.diff(cache, vdom1, vdom2).patch

  assert booklet.get(counter) == 1
  assert patch == patch.Patch(0, 0, [], [])
}

pub fn memo_recomputed_when_one_dependency_changes_test() {
  use <- lustre_test.test_filter(
    "memo_recomputed_when_one_dependency_changes_test",
  )

  let counter = booklet.new(0)

  let view = fn() {
    booklet.update(counter, fn(n) { n + 1 })
    html.div([], [html.text("Hello")])
  }

  let dep1 = element.ref(1)
  let dep2a = element.ref("a")
  let dep2b = element.ref("b")

  let vdom1 = element.memo([dep1, dep2a], view)
  let vdom2 = element.memo([dep1, dep2b], view)

  let cache = cache.new()
  let patch = diff.diff(cache, vdom1, vdom2).patch

  assert booklet.get(counter) == 2
  assert patch == patch.Patch(0, 0, [], [])
}

pub fn memo_with_map_event_test() {
  use <- lustre_test.test_filter("memo_with_map_event_test")

  let counter = booklet.new(0)
  let dep = element.ref(1)

  let view = fn() {
    booklet.update(counter, fn(n) { n + 1 })
    html.button([event.on_click("hello!")], [html.text("Click me!")])
  }

  let vdom = element.map(element.memo([dep], view), string.uppercase)

  let events = cache.from_node(vdom)

  let path = path.root |> path.add(0, "") |> path.subtree |> path.add(0, "")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "HELLO!",
    ))

  let #(_, actual) =
    cache.handle(events, path.to_string(path), "click", dynamic.nil())

  assert actual == expected
  assert booklet.get(counter) == 1
}

pub fn memo_with_map_event_not_recomputed_test() {
  use <- lustre_test.test_filter("memo_with_map_event_not_recomputed_test")

  let counter = booklet.new(0)
  let dep = element.ref(1)

  let view = fn() {
    booklet.update(counter, fn(n) { n + 1 })
    html.button([event.on_click("hello!")], [html.text("Click me!")])
  }

  let vdom = element.map(element.memo([dep], view), string.uppercase)

  let initial_cache = cache.from_node(vdom)
  let diff = diff.diff(initial_cache, vdom, vdom)

  let path = path.root |> path.add(0, "") |> path.subtree |> path.add(0, "")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "HELLO!",
    ))

  let #(_, actual) =
    cache.handle(diff.cache, path.to_string(path), "click", dynamic.nil())

  assert actual == expected
  assert booklet.get(counter) == 1
}

pub fn memo_with_map_event_recomputed_test() {
  use <- lustre_test.test_filter("memo_with_map_event_recomputed_test")

  let counter = booklet.new(0)

  let view = fn() {
    booklet.update(counter, fn(n) { n + 1 })
    html.button([event.on_click("hello!")], [html.text("Click me!")])
  }

  let dep1 = element.ref(1)
  let dep2 = element.ref(2)

  let vdom1 = element.map(element.memo([dep1], view), string.uppercase)
  let vdom2 = element.map(element.memo([dep2], view), string.uppercase)

  let cache = cache.from_node(vdom1)
  let cache = diff.diff(cache, vdom1, vdom2).cache

  let path = path.root |> path.add(0, "") |> path.subtree |> path.add(0, "")

  let expected =
    Ok(Handler(
      prevent_default: False,
      stop_propagation: False,
      message: "HELLO!",
    ))

  let #(_, actual) =
    cache.handle(cache, path.to_string(path), "click", dynamic.nil())

  assert actual == expected
  assert booklet.get(counter) == 2
}

pub fn nested_memo_test() {
  use <- lustre_test.test_filter("nested_memo_test")

  let counter1 = booklet.new(0)
  let counter2 = booklet.new(0)

  let dep1 = element.ref(1)
  let dep2 = element.ref("a")

  let inner_view = fn() {
    booklet.update(counter1, fn(n) { n + 1 })
    html.text("Inner")
  }

  let outer_view = fn() {
    booklet.update(counter2, fn(n) { n + 1 })
    html.div([], [element.memo([dep2], inner_view)])
  }

  let vdom1 = element.memo([dep1], outer_view)

  let initial_cache = cache.from_node(vdom1)
  assert booklet.get(counter2) == 1
  assert booklet.get(counter1) == 1

  let vdom2 = element.memo([dep1], outer_view)
  let diff2 = diff.diff(initial_cache, vdom1, vdom2)

  // Outer memo reused, so counters unchanged
  assert booklet.get(counter2) == 1
  assert booklet.get(counter1) == 1

  assert diff2.patch == patch.Patch(0, 0, [], [])
}

pub fn nested_memo_outer_changes_test() {
  use <- lustre_test.test_filter("nested_memo_outer_changes_test")

  let counter1 = booklet.new(0)
  let counter2 = booklet.new(0)

  let dep1a = element.ref(1)
  let dep1b = element.ref(2)
  let dep2 = element.ref("a")

  let inner_view = fn() {
    booklet.update(counter1, fn(n) { n + 1 })
    html.text("Inner")
  }

  let outer_view = fn() {
    booklet.update(counter2, fn(n) { n + 1 })
    html.div([], [element.memo([dep2], inner_view)])
  }

  let vdom1 = element.memo([dep1a], outer_view)

  // First diff establishes outer memo
  let initial_cache = cache.from_node(vdom1)
  assert booklet.get(counter2) == 1
  assert booklet.get(counter1) == 1

  let vdom2 = element.memo([dep1b], outer_view)
  let diff2 = diff.diff(initial_cache, vdom1, vdom2)

  assert booklet.get(counter2) == 2
  assert booklet.get(counter1) == 1

  assert diff2.patch == patch.Patch(0, 0, [], [])
}
