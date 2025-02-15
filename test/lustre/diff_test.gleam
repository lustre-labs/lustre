// IMPORTS ---------------------------------------------------------------------

import gleam/dict
import gleam/int
import gleam/list
import gleeunit/should
import lustre/attribute.{attribute}
import lustre/element
import lustre/element/html
import lustre/runtime/vdom.{
  Append, Insert, Move, Patch, Remove, RemoveAll, Replace, ReplaceText, Update,
}

pub fn empty_node_test() {
  let prev = html.div([], [])
  let next = html.div([], [])
  let diff = Patch(0, [], [])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

// TEXT DIFFS ------------------------------------------------------------------

pub fn text_element_replaced_test() {
  let prev = html.text("Hello, World!")
  let next = html.text("Hello, Joe!")
  let diff = Patch(0, [], [Patch(0, [ReplaceText("Hello, Joe!")], [])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn text_to_element_replacement_test() {
  let prev = html.div([], [html.text("Hello")])
  let next = html.div([], [html.span([], [html.text("Hello")])])
  let diff =
    Patch(0, [], [
      Patch(0, [], [
        Patch(0, [Replace(html.span([], [html.text("Hello")]))], []),
      ]),
    ])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
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
    Patch(0, [], [
      Patch(0, [], [
        Patch(0, [Update([attribute.class("new")], [])], [
          Patch(0, [Update([attribute("data-test", "456")], [])], []),
        ]),
      ]),
    ])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_attribute_added_test() {
  let prev = html.div([], [])
  let next = html.div([attribute.class("wibble")], [])
  let diff =
    Patch(0, [], [Patch(0, [Update([attribute.class("wibble")], [])], [])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_attribute_removed_test() {
  let prev = html.div([attribute.class("wibble")], [])
  let next = html.div([], [])
  let diff = Patch(0, [], [Patch(0, [Update([], ["class"])], [])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_many_attributes_changed_test() {
  let prev =
    html.div([attribute("id", "cool-node"), attribute.class("wibble")], [])
  let next = html.div([attribute.class("wobble")], [])
  let diff =
    Patch(0, [], [Patch(0, [Update([attribute.class("wobble")], ["id"])], [])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_child_replaced_test() {
  let prev = html.div([], [html.p([], [])])
  let next = html.div([], [html.h1([], [])])
  let diff =
    Patch(0, [], [Patch(0, [], [Patch(0, [Replace(html.h1([], []))], [])])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
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
    Patch(0, [], [
      Patch(0, [Append([html.p([], [html.text("...")])])], [
        Patch(1, [Replace(html.hr([]))], []),
        Patch(0, [Update([attribute.class("flash")], [])], [
          Patch(0, [ReplaceText("Hello, Joe!")], []),
        ]),
      ]),
    ])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn node_children_removed_test() {
  let prev = html.div([], [html.h1([], []), html.p([], [])])
  let next = html.div([], [html.h1([], [])])
  let diff = Patch(0, [], [Patch(0, [RemoveAll(from: 1)], [])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
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
    Patch(0, [Append([html.p([], [html.text("...")])])], [
      Patch(1, [Replace(html.hr([]))], []),
      Patch(0, [Update([attribute.class("flash")], [])], [
        Patch(0, [ReplaceText("Hello, Joe!")], []),
      ]),
    ])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn nested_fragment_child_replaced_test() {
  let prev =
    element.fragment([element.fragment([html.p([], [])]), html.p([], [])])

  let next =
    element.fragment([element.fragment([html.h1([], [])]), html.p([], [])])

  let diff = Patch(0, [], [Patch(0, [Replace(html.h1([], []))], [])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn nested_fragment_children_removed_test() {
  let prev =
    html.div([], [
      element.fragment([html.h1([], []), html.p([], []), html.p([], [])]),
      html.p([], []),
    ])

  let next = html.div([], [element.fragment([html.h1([], [])]), html.p([], [])])

  let diff = Patch(0, [], [Patch(0, [Remove(from: 1, count: 2)], [])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

// // KEYED DIFFS -----------------------------------------------------------------

pub fn keyed_swap_test() {
  let prev =
    element.keyed(html.div([], _), [
      #("a", html.text("wibble")),
      #("b", html.text("wobble")),
    ])

  let next =
    element.keyed(html.div([], _), [
      #("b", html.text("wobble")),
      #("a", html.text("wibble")),
    ])

  let diff = Patch(0, [], [Patch(0, [Move("b", 0)], [])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_reorder_test() {
  let prev =
    element.keyed(html.div([], _), [
      #("a", html.p([], [])),
      #("b", html.div([], [])),
      #("c", html.img([])),
    ])

  let next =
    element.keyed(html.div([], _), [
      #("a", html.p([], [])),
      #("c", html.img([])),
      #("b", html.div([], [])),
    ])

  let diff = Patch(0, [], [Patch(0, [Move("c", 1)], [])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_insert_test() {
  let prev =
    element.keyed(html.div([], _), [
      #("a", html.p([], [])),
      #("b", html.div([], [])),
      #("c", html.img([])),
    ])

  let next =
    element.keyed(html.div([], _), [
      #("c", html.img([])),
      #("a", html.p([], [])),
      #("d", html.span([], [])),
      #("b", html.div([], [])),
    ])

  let diff =
    Patch(0, [], [
      Patch(0, [Insert(keyed("d", html.span([], [])), 1), Move("c", 0)], []),
    ])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn keyed_list_with_updates_test() {
  let prev =
    element.keyed(html.div([], _), [
      #("1", html.div([attribute.class("old")], [html.text("one")])),
      #("2", html.div([attribute.class("old")], [html.text("two")])),
    ])

  let next =
    element.keyed(html.div([], _), [
      #("2", html.div([attribute.class("new")], [html.text("two")])),
      #("1", html.div([attribute.class("new")], [html.text("one")])),
    ])

  let diff =
    Patch(0, [], [
      Patch(0, [Move("2", 0)], [
        Patch(1, [Update([attribute.class("new")], [])], []),
        Patch(0, [Update([attribute.class("new")], [])], []),
      ]),
    ])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
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

  let diff = Patch(0, [], [Patch(0, [ReplaceText("changed")], [])])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

pub fn mixed_keyed_and_regular_nodes_test() {
  let prev =
    html.div([], [
      element.keyed(html.ul([], _), [
        #("1", html.li([], [html.text("one")])),
        #("2", html.li([], [html.text("two")])),
      ]),
      html.p([], [html.text("regular")]),
    ])

  let next =
    html.div([], [
      element.keyed(html.ul([], _), [
        #("2", html.li([], [html.text("two")])),
        #("1", html.li([], [html.text("one")])),
      ]),
      html.p([], [html.text("changed")]),
    ])

  let diff =
    Patch(0, [], [
      Patch(0, [], [
        Patch(1, [], [Patch(0, [ReplaceText("changed")], [])]),
        Patch(0, [Move("2", 0)], []),
      ]),
    ])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
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
    Patch(0, [], [
      Patch(
        0,
        [
          Update(
            [
              attribute("aria-label", "new"),
              attribute("style", "color: blue"),
              attribute.class("two three"),
            ],
            ["data-test"],
          ),
        ],
        [],
      ),
    ])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
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
    Patch(0, [], [
      Patch(
        0,
        [
          Append([
            html.h1([], [html.text("Title")]),
            html.p([], [html.text("Paragraph")]),
            html.span([], [html.text("Span")]),
          ]),
        ],
        [],
      ),
    ])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
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
    Patch(0, [], [
      Patch(0, [], [
        Patch(2, [ReplaceText("new end")], []),
        Patch(1, [], [Patch(0, [ReplaceText("new middle")], [])]),
        Patch(0, [ReplaceText("new start")], []),
      ]),
    ])

  vdom.diff(prev, next, dict.new()).patch |> should.equal(diff)
}

// UTILS -----------------------------------------------------------------------

fn keyed(key, el) {
  case el {
    vdom.Fragment(..) ->
      vdom.Fragment(
        key:,
        children: list.index_map(el.children, fn(child, index) {
          case child.key {
            "" -> keyed(key <> ":" <> int.to_string(index), child)
            _ -> keyed(key <> ":" <> child.key, child)
          }
        }),
      )
    vdom.Node(..) -> vdom.Node(..el, key:)
    vdom.Text(..) -> vdom.Text(..el, key:)
  }
}
