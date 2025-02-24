// IMPORTS ---------------------------------------------------------------------

import gleam/dict
import gleeunit/should
import lustre/attribute.{attribute}
import lustre/element
import lustre/element/html
import lustre/element/keyed
import lustre/runtime/vdom.{
  Insert, InsertMany, Move, Patch, Remove, RemoveKey, Replace, ReplaceText,
  Update,
}

pub fn empty_node_test() {
  let prev = html.div([], [])
  let next = html.div([], [])
  let diff = Patch(0, 0, [], [])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

// TEXT DIFFS ------------------------------------------------------------------

pub fn text_element_replaced_test() {
  let prev = html.text("Hello, World!")
  let next = html.text("Hello, Joe!")
  let diff = Patch(0, 0, [], [Patch(0, 0, [ReplaceText("Hello, Joe!")], [])])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn text_to_element_replacement_test() {
  let prev = html.div([], [html.text("Hello")])
  let next = html.div([], [html.span([], [html.text("Hello")])])
  let diff =
    Patch(0, 0, [], [
      Patch(0, 0, [], [
        Patch(0, 0, [Replace(html.span([], [html.text("Hello")]))], []),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

// // NODE DIFFS ------------------------------------------------------------------

pub fn nested_attribute_changes_test() {
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
    Patch(0, 0, [], [
      Patch(0, 0, [], [
        Patch(0, 0, [Update([attribute.class("new")], [])], [
          Patch(0, 0, [Update([attribute("data-test", "456")], [])], []),
        ]),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_attribute_added_test() {
  let prev = html.div([], [])
  let next = html.div([attribute.class("wibble")], [])
  let diff =
    Patch(0, 0, [], [Patch(0, 0, [Update([attribute.class("wibble")], [])], [])])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_attribute_removed_test() {
  let prev = html.div([attribute.class("wibble")], [])
  let next = html.div([], [])
  let diff =
    Patch(0, 0, [], [Patch(0, 0, [Update([], [attribute.class("wibble")])], [])])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_many_attributes_changed_test() {
  let prev =
    html.div([attribute("id", "cool-node"), attribute.class("wibble")], [])
  let next = html.div([attribute.class("wobble")], [])
  let diff =
    Patch(0, 0, [], [
      Patch(
        0,
        0,
        [Update([attribute.class("wobble")], [attribute.id("cool-node")])],
        [],
      ),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_child_replaced_test() {
  let prev = html.div([], [html.p([], [])])
  let next = html.div([], [html.h1([], [])])
  let diff =
    Patch(0, 0, [], [
      Patch(0, 0, [], [Patch(0, 0, [Replace(html.h1([], []))], [])]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_many_children_changed_test() {
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
    Patch(0, 0, [], [
      Patch(0, 0, [InsertMany([html.p([], [html.text("...")])], 2)], [
        Patch(1, 0, [Replace(html.hr([]))], []),
        Patch(0, 0, [Update([attribute.class("flash")], [])], [
          Patch(0, 0, [ReplaceText("Hello, Joe!")], []),
        ]),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_children_removed_test() {
  let prev = html.div([], [html.h1([], []), html.p([], [])])
  let next = html.div([], [html.h1([], [])])
  let diff = Patch(0, 0, [], [Patch(0, 1, [], [])])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

// // FRAGMENT DIFFS --------------------------------------------------------------

pub fn fragment_many_children_changed_test() {
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
    Patch(0, 0, [InsertMany([html.p([], [html.text("...")])], 2)], [
      Patch(1, 0, [Replace(html.hr([]))], []),
      Patch(0, 0, [Update([attribute.class("flash")], [])], [
        Patch(0, 0, [ReplaceText("Hello, Joe!")], []),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn nested_fragment_child_replaced_test() {
  let prev =
    element.fragment([element.fragment([html.p([], [])]), html.p([], [])])

  let next =
    element.fragment([element.fragment([html.h1([], [])]), html.p([], [])])

  let diff = Patch(0, 0, [], [Patch(0, 0, [Replace(html.h1([], []))], [])])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn nested_fragment_children_removed_test() {
  let prev =
    html.div([], [
      element.fragment([html.h1([], []), html.p([], []), html.p([], [])]),
      html.p([], []),
    ])

  let next = html.div([], [element.fragment([html.h1([], [])]), html.p([], [])])

  let diff = Patch(0, 0, [], [Patch(0, 0, [Remove(from: 1, count: 2)], [])])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn fragment_update_with_different_children_counts_test() {
  let abc = element.fragment([html.text("a"), html.text("b"), html.text("c")])

  let x = html.text("x")

  let y = element.fragment([html.text("y")])

  let prev = html.div([], [x, y, abc])
  let next = html.div([], [y, abc, x])

  let diff =
    Patch(0, 0, [], [
      Patch(
        0,
        0,
        [
          Remove(from: 3, count: 2),
          InsertMany([html.text("b"), html.text("c")], before: 2),
        ],
        [
          Patch(4, 0, [Replace(x)], []),
          Patch(1, 0, [ReplaceText("a")], []),
          Patch(0, 0, [Replace(y)], []),
        ],
      ),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn fragment_prepend_and_replace_with_node_test() {
  let ab = element.fragment([html.text("a"), html.text("b")])
  let x = html.text("x")

  let prev = html.div([], [ab])
  let next = html.div([], [x, ab])

  let diff =
    Patch(0, 0, [], [
      Patch(0, 0, [InsertMany([ab], before: 2), Remove(from: 1, count: 1)], [
        Patch(0, 0, [Replace(x)], []),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn fragment_update_and_remove_test() {
  let a = html.text("a")
  let bc = element.fragment([html.text("b"), html.text("c")])
  let de = element.fragment([html.text("d"), html.text("e")])

  let prev = html.div([], [a, bc, de])
  let next = html.div([], [a, de])

  let diff =
    Patch(0, 0, [], [
      Patch(0, 2, [], [
        Patch(2, 0, [ReplaceText("e")], []),
        Patch(1, 0, [ReplaceText("d")], []),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn multiple_nested_fragments_test() {
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

  let diff = Patch(0, 0, [], [Patch(0, 0, [ReplaceText("changed")], [])])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

// KEYED DIFFS -----------------------------------------------------------------

pub fn keyed_swap_test() {
  let prev =
    keyed.div([], [#("a", html.text("wibble")), #("b", html.text("wobble"))])

  let next =
    keyed.div([], [#("b", html.text("wobble")), #("a", html.text("wibble"))])

  let diff = Patch(0, 0, [], [Patch(0, 0, [Move("b", 0, 1)], [])])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_reorder_test() {
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

  let diff = Patch(0, 0, [], [Patch(0, 0, [Move("c", 1, 1)], [])])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_insert_test() {
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
    Patch(0, 0, [], [
      Patch(
        0,
        0,
        [Insert(keyed("d", html.span([], [])), 1), Move("c", 0, 1)],
        [],
      ),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_list_with_updates_test() {
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
    Patch(0, 0, [], [
      Patch(0, 0, [Move("2", 0, 1)], [
        Patch(1, 0, [Update([attribute.class("new")], [])], []),
        Patch(0, 0, [Update([attribute.class("new")], [])], []),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn mixed_keyed_and_regular_nodes_test() {
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
    Patch(0, 0, [], [
      Patch(0, 0, [], [
        Patch(1, 0, [], [Patch(0, 0, [ReplaceText("changed")], [])]),
        Patch(0, 0, [Move("2", 0, 1)], []),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn complex_attribute_changes_test() {
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
    Patch(0, 0, [], [
      Patch(
        0,
        0,
        [
          Update(
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

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn empty_to_multiple_children_test() {
  let prev = html.div([], [])
  let next =
    html.div([], [
      html.h1([], [html.text("Title")]),
      html.p([], [html.text("Paragraph")]),
      html.span([], [html.text("Span")]),
    ])

  let diff =
    Patch(0, 0, [], [
      Patch(
        0,
        0,
        [
          InsertMany(
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

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn mixed_text_and_element_changes_test() {
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
    Patch(0, 0, [], [
      Patch(0, 0, [], [
        Patch(2, 0, [ReplaceText("new end")], []),
        Patch(1, 0, [], [Patch(0, 0, [ReplaceText("new middle")], [])]),
        Patch(0, 0, [ReplaceText("new start")], []),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

// KEYED DIFFS WITH FRAGMENTS --------------------------------------------------

pub fn keyed_move_fragment_with_replace_with_different_count_test() {
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
    Patch(0, 0, [], [
      Patch(0, 0, [InsertMany([keyed("cd", cd)], 3), RemoveKey("x", 1)], []),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)

  // reverse

  let diff = Patch(0, 0, [], [Patch(0, 2, [Insert(keyed("x", x), 0)], [])])
  vdom.diff(0, next, prev, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_move_fragment_with_replace_to_simple_node_test() {
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
    Patch(0, 0, [], [
      Patch(
        0,
        0,
        [
          InsertMany(
            [keyed("b", html.text("b")), keyed("c", html.text("c"))],
            3,
          ),
          Remove(2, 1),
          RemoveKey("x", 1),
        ],
        [Patch(0, 0, [Replace(keyed("a", html.text("a")))], [])],
      ),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_replace_fragment_test() {
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
    Patch(0, 0, [], [
      Patch(0, 0, [InsertMany([keyed("c", html.text("c"))], 3), Remove(2, 1)], [
        Patch(1, 0, [Replace(keyed("b", html.text("b")))], []),
        Patch(0, 0, [Replace(keyed("a", html.text("a")))], []),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_insert_fragment_test() {
  let prev = keyed.div([], [#("a", html.text("a"))])
  let xyz = element.fragment([html.text("x"), html.text("y"), html.text("z")])
  let next = keyed.div([], [#("xyz", xyz), #("a", html.text("A"))])

  let diff =
    Patch(0, 0, [], [
      Patch(0, 0, [Insert(keyed("xyz", xyz), 0)], [
        Patch(3, 0, [ReplaceText("A")], []),
      ]),
    ])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

// KEYED FRAGMENTS -------------------------------------------------------------

pub fn keyed_fragment_swap_test() {
  let prev =
    keyed.fragment([#("a", html.text("wibble")), #("b", html.text("wobble"))])

  let next =
    keyed.fragment([#("b", html.text("wobble")), #("a", html.text("wibble"))])

  let diff = Patch(0, 0, [Move("b", 0, 1)], [])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_fragment_reorder_test() {
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

  let diff = Patch(0, 0, [Move("c", 1, 1)], [])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_fragment_insert_test() {
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
    Patch(0, 0, [Insert(keyed("d", html.span([], [])), 1), Move("c", 0, 1)], [])

  vdom.diff(0, prev, next, dict.new()).patch |> should.equal(diff)
}

// INITIAL ELEMENT OFFSET ------------------------------------------------------

pub fn node_initial_offset_test() {
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

  let offset = 123

  let diff =
    Patch(0, 0, [], [
      Patch(offset, 0, [InsertMany([html.p([], [html.text("...")])], 2)], [
        Patch(1, 0, [Replace(html.hr([]))], []),
        Patch(0, 0, [Update([attribute.class("flash")], [])], [
          Patch(0, 0, [ReplaceText("Hello, Joe!")], []),
        ]),
      ]),
    ])

  vdom.diff(123, prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn fragment_initial_offset_test() {
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

  let offset = 42

  let diff =
    Patch(0, 0, [InsertMany([html.p([], [html.text("...")])], offset + 2)], [
      Patch(offset + 1, 0, [Replace(html.hr([]))], []),
      Patch(offset + 0, 0, [Update([attribute.class("flash")], [])], [
        Patch(0, 0, [ReplaceText("Hello, Joe!")], []),
      ]),
    ])

  vdom.diff(42, prev, next, dict.new()).patch |> should.equal(diff)
}

// UTILS -----------------------------------------------------------------------

fn keyed(key, el) {
  case el {
    vdom.Fragment(..) -> vdom.Fragment(..el, key:)
    vdom.Node(..) -> vdom.Node(..el, key:)
    vdom.Text(..) -> vdom.Text(..el, key:)
  }
}
