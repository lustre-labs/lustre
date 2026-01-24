// IMPORTS ---------------------------------------------------------------------
//
@target(javascript)
import gleam/json
@target(javascript)
import lustre/attribute.{attribute}
@target(javascript)
import lustre/element.{type Element}
@target(javascript)
import lustre/element/html
@target(javascript)
import lustre/element/keyed
@target(javascript)
import lustre/internals/mutable_map
@target(javascript)
import lustre/vdom/cache
@target(javascript)
import lustre/vdom/diff
@target(javascript)
import lustre/vdom/patch.{type Patch}
@target(javascript)
import lustre/vdom/vnode
@target(javascript)
import lustre_test

// MOUNT TESTS -----------------------------------------------------------------

@target(javascript)
pub fn reconciler_mount_none_test() {
  use <- lustre_test.test_filter("reconciler_mount_none_test")
  test_mount(element.none())
}

@target(javascript)
pub fn reconciler_mount_empty_div_test() {
  use <- lustre_test.test_filter("reconciler_mount_empty_div_test")
  test_mount(html.div([], []))
}

@target(javascript)
pub fn reconciler_mount_fragment_root_test() {
  use <- lustre_test.test_filter("reconciler_mount_fragment_root_test")
  test_mount(element.fragment([html.div([], []), html.div([], [])]))
}

@target(javascript)
pub fn reconciler_mount_text_test() {
  use <- lustre_test.test_filter("reconciler_mount_text_test")
  test_mount(html.text("Hello, Joe!"))
}

@target(javascript)
pub fn reconciler_mount_tree_test() {
  use <- lustre_test.test_filter("reconciler_mount_tree_test")
  let html =
    html.div([], [
      html.h1([], [html.text("Welcome!")]),
      html.p([], [html.text("...")]),
    ])

  test_mount(html)
}

@target(javascript)
pub fn reconciler_server_component_mount_input_test() {
  use <- lustre_test.test_filter("reconciler_mount_input_test")

  let html = html.input([attribute.value("")])

  // we cannot use test_mount here since your to_string function produces a
  // slightly different result that we don't handle (`value=""` vs `value`)
  use reconciler <- with_reconciler(True)

  mount_json(reconciler, vnode.to_json(html, mutable_map.new()))
  assert lustre_test.nodes_equal_ignoring_memo(get_vdom(), html)
}

@target(javascript)
fn test_mount(vdom: Element(msg)) {
  use reconciler <- with_reconciler(True)

  mount(reconciler, vdom)
  assert get_html() == element.to_string(vdom)

  mount_json(reconciler, vnode.to_json(vdom, mutable_map.new()))
  assert lustre_test.nodes_equal_ignoring_memo(get_vdom(), vdom)
}

// DIFF TESTS ------------------------------------------------------------------

@target(javascript)
pub fn reconciler_push_empty_node_test() {
  use <- lustre_test.test_filter("reconciler_push_empty_node_test")

  let prev = html.div([], [])
  let next = html.div([], [])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_text_element_replaced_test() {
  use <- lustre_test.test_filter("reconciler_push_text_element_replaced_test")

  let prev = html.text("Hello, World!")
  let next = html.text("Hello, Joe!")

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_text_to_element_replacement_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_text_to_element_replacement_test",
  )

  let prev = html.div([], [html.text("Hello")])
  let next = html.div([], [html.span([], [html.text("Hello")])])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_nested_attribute_changes_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_nested_attribute_changes_test",
  )

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_node_attribute_added_test() {
  use <- lustre_test.test_filter("reconciler_push_node_attribute_added_test")

  let prev = html.div([], [])
  let next = html.div([attribute.class("wibble")], [])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_node_attribute_removed_test() {
  use <- lustre_test.test_filter("reconciler_push_node_attribute_removed_test")

  let prev = html.div([attribute.class("wibble")], [])
  let next = html.div([], [])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_node_many_attributes_changed_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_node_many_attributes_changed_test",
  )

  let prev =
    html.div([attribute("id", "cool-node"), attribute.class("wibble")], [])
  let next = html.div([attribute.class("wobble")], [])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_node_child_replaced_test() {
  use <- lustre_test.test_filter("reconciler_push_node_child_replaced_test")

  let prev = html.div([], [html.p([], [])])
  let next = html.div([], [html.h1([], [])])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_node_many_children_changed_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_node_many_children_changed_test",
  )

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_node_children_removed_test() {
  use <- lustre_test.test_filter("reconciler_push_node_children_removed_test")

  let prev = html.div([], [html.h1([], []), html.p([], [])])
  let next = html.div([], [html.h1([], [])])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_fragment_many_children_changed_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_fragment_many_children_changed_test",
  )

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_fragment_child_replaced_test() {
  use <- lustre_test.test_filter("reconciler_push_fragment_child_replaced_test")

  let prev = element.fragment([html.p([], [])])
  let next = element.fragment([html.h1([], [])])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_nested_fragment_child_replaced_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_nested_fragment_child_replaced_test",
  )

  let prev =
    element.fragment([
      element.fragment([html.p([attribute.class("a")], [])]),
      html.p([attribute.class("b")], []),
    ])

  let next =
    element.fragment([
      element.fragment([html.h1([attribute.class("a")], [])]),
      html.p([attribute.class("b")], []),
    ])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_fragment_children_removed_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_fragment_children_removed_test",
  )

  let prev =
    html.div([], [
      element.fragment([html.h1([], []), html.p([], []), html.p([], [])]),
      html.p([], []),
    ])

  let next = html.div([], [element.fragment([html.h1([], [])]), html.p([], [])])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_nested_fragment_children_removed_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_nested_fragment_children_removed_test",
  )

  let prev =
    html.div([], [
      element.fragment([
        element.fragment([html.h1([], []), html.p([], [])]),
        html.p([attribute.class("a")], []),
        html.p([attribute.class("b")], []),
      ]),
      html.p([], []),
    ])

  let next =
    html.div([], [
      element.fragment([html.h1([], []), html.p([attribute.class("a")], [])]),
      html.div([], []),
    ])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_fragment_update_with_different_children_counts_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_fragment_update_with_different_children_counts_test",
  )

  let abc = element.fragment([html.text("a"), html.text("b"), html.text("c")])
  let x = html.text("x")
  let y = element.fragment([html.text("y")])

  let prev = html.div([], [x, y, abc])
  let next = html.div([], [y, abc, x])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_fragment_prepend_and_replace_with_node_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_fragment_prepend_and_replace_with_node_test",
  )

  let ab = element.fragment([html.text("a"), html.text("b")])
  let x = html.text("x")

  let prev = html.div([], [ab])
  let next = html.div([], [x, ab])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_fragment_update_and_remove_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_fragment_update_and_remove_test",
  )

  let a = html.text("a")
  let bc = element.fragment([html.text("b"), html.text("c")])
  let de = element.fragment([html.text("d"), html.text("e")])

  let prev = html.div([], [a, bc, de])
  let next = html.div([], [a, de])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_multiple_nested_fragments_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_multiple_nested_fragments_test",
  )

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_swap_test() {
  use <- lustre_test.test_filter("reconciler_push_keyed_swap_test")

  let prev =
    keyed.div([], [#("a", html.text("wibble")), #("b", html.text("wobble"))])

  let next =
    keyed.div([], [#("b", html.text("wobble")), #("a", html.text("wibble"))])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_reorder_test() {
  use <- lustre_test.test_filter("reconciler_push_keyed_reorder_test")

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_insert_test() {
  use <- lustre_test.test_filter("reconciler_push_keyed_insert_test")

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_list_with_updates_test() {
  use <- lustre_test.test_filter("reconciler_push_keyed_list_with_updates_test")

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_mixed_keyed_and_regular_nodes_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_mixed_keyed_and_regular_nodes_test",
  )

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_multiple_class_and_styles_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_multiple_class_and_styles_test",
  )

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_empty_to_multiple_children_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_empty_to_multiple_children_test",
  )

  let prev = html.div([], [])
  let next =
    html.div([], [
      html.h1([], [html.text("Title")]),
      html.p([], [html.text("Paragraph")]),
      html.span([], [html.text("Span")]),
    ])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_mixed_text_and_element_changes_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_mixed_text_and_element_changes_test",
  )

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_move_fragment_with_replace_with_different_count_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_keyed_move_fragment_with_replace_with_different_count_test",
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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_move_fragment_with_replace_to_simple_node_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_keyed_move_fragment_with_replace_to_simple_node_test",
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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_replace_fragment_test() {
  use <- lustre_test.test_filter("reconciler_push_keyed_replace_fragment_test")

  let x = element.fragment([html.text("x")])
  let ab = element.fragment([html.text("a"), html.text("b")])
  let prev = keyed.div([], [#("x", x), #("ab", ab)])

  let next =
    keyed.div([], [
      #("a", html.text("a")),
      #("b", html.text("b")),
      #("c", html.text("c")),
    ])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_insert_fragment_test() {
  use <- lustre_test.test_filter("reconciler_push_keyed_insert_fragment_test")

  let prev = keyed.div([], [#("a", html.text("a"))])
  let xyz = element.fragment([html.text("x"), html.text("y"), html.text("z")])
  let next = keyed.div([], [#("xyz", xyz), #("a", html.text("A"))])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_fragment_swap_test() {
  use <- lustre_test.test_filter("reconciler_push_keyed_fragment_swap_test")

  let prev =
    keyed.fragment([#("a", html.text("wibble")), #("b", html.text("wobble"))])

  let next =
    keyed.fragment([#("b", html.text("wobble")), #("a", html.text("wibble"))])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_fragment_reorder_test() {
  use <- lustre_test.test_filter("reconciler_push_keyed_fragment_reorder_test")

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_fragment_insert_test() {
  use <- lustre_test.test_filter("reconciler_push_keyed_fragment_insert_test")

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

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_keyed_fragment_remove_test() {
  use <- lustre_test.test_filter("reconciler_push_keyed_fragment_remove_test")

  let prev =
    keyed.fragment([
      #("a", html.p([], [])),
      #("b", html.div([], [])),
      #("c", html.img([])),
    ])

  let next = keyed.fragment([#("a", html.p([], [])), #("c", html.img([]))])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_memo_with_fragment_test() {
  use <- lustre_test.test_filter("reconciler_push_memo_with_fragment_test")

  let dep1 = element.ref(1)
  let dep2 = element.ref(2)

  let prev =
    html.div([], [
      element.memo([dep1], fn() {
        element.fragment([html.text("a"), html.text("b")])
      }),
    ])

  let next =
    html.div([], [
      element.memo([dep2], fn() {
        element.fragment([html.text("c"), html.text("d")])
      }),
    ])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_memo_with_nested_fragment_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_memo_with_nested_fragment_test",
  )

  let dep1 = element.ref(1)
  let dep2 = element.ref(2)

  let prev =
    html.div([], [
      element.memo([dep1], fn() {
        element.fragment([
          element.fragment([html.text("a"), html.text("b")]),
          html.text("c"),
        ])
      }),
    ])

  let next =
    html.div([], [
      element.memo([dep2], fn() {
        element.fragment([
          element.fragment([html.text("x"), html.text("y")]),
          html.text("z"),
        ])
      }),
    ])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_map_with_fragment_test() {
  use <- lustre_test.test_filter("reconciler_push_map_with_fragment_test")

  let prev =
    html.div([], [
      element.map(element.fragment([html.text("a"), html.text("b")]), fn(msg) {
        msg
      }),
    ])

  let next =
    html.div([], [
      element.map(element.fragment([html.text("c"), html.text("d")]), fn(msg) {
        msg
      }),
    ])

  test_diff(prev, next)
}

@target(javascript)
pub fn reconciler_push_map_with_nested_fragment_test() {
  use <- lustre_test.test_filter(
    "reconciler_push_map_with_nested_fragment_test",
  )

  let prev =
    html.div([], [
      element.map(
        element.fragment([
          element.fragment([html.text("a"), html.text("b")]),
          html.text("c"),
        ]),
        fn(msg) { msg },
      ),
    ])

  let next =
    html.div([], [
      element.map(
        element.fragment([
          element.fragment([html.text("x"), html.text("y")]),
          html.text("z"),
        ]),
        fn(msg) { msg },
      ),
    ])

  test_diff(prev, next)
}

@target(javascript)
pub fn recociler_push_nested_fragment_replace_test() {
  use <- lustre_test.test_filter("recociler_push_nested_fragment_replace_test")

  let prev =
    html.div([], [
      keyed.fragment([
        #("Child row", element.none()),
      ]),
      html.div([], [element.text("Other row - should stay BELOW")]),
    ])

  let next =
    html.div([], [
      keyed.fragment([
        #("Child row", {
          element.fragment([
            html.div([], [element.text("Bad logic child")]),
          ])
        }),
      ]),
      html.div([], [element.text("Other row - should stay BELOW")]),
    ])

  // We cannot use `test_diff` here since the error only exists with debug=false.
  use reconciler <- with_reconciler(False)

  let diff.Diff(patch:, ..) = diff.diff(cache.new(), prev, next)

  mount(reconciler, prev)
  push(reconciler, patch)

  let html = get_html()
  assert html
    == "<div><div>Bad logic child</div><div>Other row - should stay BELOW</div></div>"
}

@target(javascript)
pub fn reconciler_push_memo_map_with_fragment_test() {
  use <- lustre_test.test_filter("reconciler_push_memo_map_with_fragment_test")

  let dep1 = element.ref(1)
  let dep2 = element.ref(2)

  let prev =
    html.div([], [
      element.memo([dep1], fn() {
        element.map(element.fragment([html.text("a"), html.text("b")]), fn(msg) {
          msg
        })
      }),
    ])

  let next =
    html.div([], [
      element.memo([dep2], fn() {
        element.map(element.fragment([html.text("c"), html.text("d")]), fn(msg) {
          msg
        })
      }),
    ])

  test_diff(prev, next)
}

@target(javascript)
fn test_diff(prev: Element(msg), next: Element(msg)) {
  use reconciler <- with_reconciler(True)

  let diff.Diff(patch:, ..) = diff.diff(cache.new(), prev, next)

  mount(reconciler, prev)
  push(reconciler, patch)

  assert lustre_test.nodes_equal_ignoring_memo(get_vdom(), next)

  mount(reconciler, prev)
  push_json(reconciler, patch.to_json(patch, mutable_map.new()))
  assert lustre_test.nodes_equal_ignoring_memo(get_vdom(), next)
}

// FFI -------------------------------------------------------------------------

@target(javascript)
pub type Reconciler

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "with_reconciler")
pub fn with_reconciler(debug: Bool, f: fn(Reconciler) -> Nil) -> Nil

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "mount")
pub fn mount(reconciler: Reconciler, vdom: Element(msg)) -> Nil

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "mount")
fn mount_json(reconciler: Reconciler, vdom: json.Json) -> Nil

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "push")
fn push(reconciler: Reconciler, patch: Patch(msg)) -> Nil

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "push")
fn push_json(reconciler: Reconciler, patch: json.Json) -> Nil

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "get_html")
pub fn get_html() -> String

@target(javascript)
@external(javascript, "./client_test.ffi.mjs", "get_vdom")
fn get_vdom() -> Element(msg)
