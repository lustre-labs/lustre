// IMPORTS ---------------------------------------------------------------------

import birdie
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/keyed
import lustre/element/mathml
import lustre/element/svg
import lustre/platform/dom
import lustre/vdom/vnode
import lustre_test

// ELEMENT TESTS ---------------------------------------------------------------

pub fn element_empty_test() {
  use <- lustre_test.test_filter("element_empty_test")

  let input = html.div([], [])

  input |> snapshot("Empty element should have nothing between tags")
}

pub fn element_multiple_children_test() {
  use <- lustre_test.test_filter("element_multiple_children_test")

  let input = html.div([], [html.p([], []), html.p([], []), html.p([], [])])

  input |> snapshot("Element with multiple children should render in order")
}

pub fn void_elements_test() {
  use <- lustre_test.test_filter("void_elements_test")

  let input =
    html.div([], [
      html.area([]),
      html.base([]),
      html.br([]),
      html.col([]),
      html.embed([]),
      html.hr([]),
      html.img([]),
      html.input([]),
      html.link([]),
      html.meta([]),
      html.source([]),
      html.track([]),
      html.wbr([]),
    ])

  input |> snapshot("Void elements should render as such")
}

pub fn element_namespaced_void_test() {
  use <- lustre_test.test_filter("element_namespaced_void_test")

  let input =
    html.div([], [
      element.element("link", [], []),
      element.namespaced("", "link", [], []),
      element.namespaced("bitter-end", "link", [], []),
    ])

  input
  |> snapshot(
    "Semi-advanced tag functions should still have the HTML void checking",
  )
}

pub fn serializer_custom_void_test() {
  use <- lustre_test.test_filter("serializer_custom_void_test")

  // Test that a custom serializer can mark additional elements as void.
  let serializer =
    dom.serializer_config()
    |> dom.with_void("custom-void")

  let input =
    html.div([], [
      element.element("custom-void", [], []),
      html.span([], [element.text("normal")]),
    ])

  input
  |> dom.serialize(serializer, _)
  |> birdie.snap("[html] Serializer with custom void element")
}

pub fn serializer_self_closing_test() {
  use <- lustre_test.test_filter("serializer_self_closing_test")

  // Test that a serializer can render elements as self-closing (XML style).
  let serializer =
    dom.empty_serializer_config()
    |> dom.with_self_closing_tags(["icon", "path"])

  let input =
    html.div([], [
      element.element("icon", [], []),
      element.element("path", [], []),
      html.span([], []),
    ])

  input
  |> dom.serialize(serializer, _)
  |> birdie.snap("[html] Serializer with self-closing elements")
}

pub fn keyed_void_elements_test() {
  use <- lustre_test.test_filter("keyed_void_elements_test")

  let input =
    keyed.div([], [
      #("area", html.area([])),
      #("base", html.base([])),
      #("br", html.br([])),
      #("col", html.col([])),
      #("embed", html.embed([])),
      #("hr", html.hr([])),
      #("img", html.img([])),
      #("input", html.input([])),
      #("link", html.link([])),
      #("meta", html.meta([])),
      #("source", html.source([])),
      #("track", html.track([])),
      #("wbr", html.wbr([])),
    ])

  input |> snapshot("Keyed void elements should render as such")
}

pub fn keyed_element_namespaced_void_test() {
  use <- lustre_test.test_filter("keyed_element_namespaced_void_test")

  let input =
    keyed.div([], [
      #("wee", element.element("link", [], [])),
      #("oo", element.namespaced("", "link", [], [])),
      #("goes the ambulance", element.namespaced("rapture", "link", [], [])),
    ])

  input
  |> snapshot(
    "Semi-advanced keyed tag functions should still have the HTML void checking",
  )
}

pub fn serializer_keyed_custom_void_test() {
  use <- lustre_test.test_filter("serializer_keyed_custom_void_test")

  // Test that a custom serializer works correctly with keyed elements.
  let serializer =
    dom.serializer_config()
    |> dom.with_void("custom-void")

  let input =
    keyed.div([], [
      #("void", element.element("custom-void", [], [])),
      #("normal", html.span([], [element.text("content")])),
    ])

  input
  |> dom.serialize(serializer, _)
  |> birdie.snap("[html] Serializer with keyed custom void elements")
}

// FRAGMENT TESTS --------------------------------------------------------------

pub fn fragment_empty_test() {
  use <- lustre_test.test_filter("fragment_empty_test")

  let input = element.fragment([])

  input |> snapshot("Empty fragment should render nothing")
}

pub fn fragment_single_text_test() {
  use <- lustre_test.test_filter("fragment_single_text_test")

  let input = element.fragment([html.text("Hello, world!")])

  input |> snapshot("Fragment with single text node should render plain text")
}

pub fn fragment_single_element_test() {
  use <- lustre_test.test_filter("fragment_single_element_test")

  let input = element.fragment([html.div([], [])])

  input |> snapshot("Fragment with single element should render that element")
}

pub fn fragment_multiple_text_test() {
  use <- lustre_test.test_filter("fragment_multiple_text_test")

  let input = element.fragment([html.text("Hello, "), html.text("world!")])

  input
  |> snapshot(
    "Fragment with multiple text nodes should render concatenated text",
  )
}

pub fn fragment_adjacent_text_nodes_test() {
  use <- lustre_test.test_filter("fragment_adjacent_text_nodes_test")

  let input =
    element.fragment([
      html.text("A"),
      html.text("B"),
      html.text("C"),
      html.div([], []),
      html.text("D"),
      html.text("E"),
    ])

  input
  |> snapshot("Fragment with adjacent text nodes uses start/end markers")
}

pub fn fragment_nested_simple_test() {
  use <- lustre_test.test_filter("fragment_nested_simple_test")

  let input =
    element.fragment([
      html.div([], [html.text("outer 1")]),
      element.fragment([
        html.div([], [html.text("inner 1")]),
        html.div([], [html.text("inner 2")]),
      ]),
      html.div([], [html.text("outer 2")]),
    ])

  input
  |> snapshot("Nested fragment with start/end markers for each fragment")
}

pub fn fragment_nested_with_text_test() {
  use <- lustre_test.test_filter("fragment_nested_with_text_test")

  let input =
    element.fragment([
      html.text("A"),
      element.fragment([html.text("B"), html.text("C")]),
      html.text("D"),
    ])

  input
  |> snapshot("Nested fragment with text: each fragment has start/end markers")
}

pub fn fragment_deeply_nested_test() {
  use <- lustre_test.test_filter("fragment_deeply_nested_test")

  let input =
    element.fragment([
      html.div([], [html.text("1")]),
      element.fragment([
        html.div([], [html.text("2")]),
        element.fragment([
          html.div([], [html.text("3")]),
          html.div([], [html.text("4")]),
        ]),
        html.div([], [html.text("5")]),
      ]),
      html.div([], [html.text("6")]),
    ])

  input
  |> snapshot("Deeply nested: each fragment has start/end markers")
}

pub fn fragment_nested_mixed_content_test() {
  use <- lustre_test.test_filter("fragment_nested_mixed_content_test")

  let input =
    element.fragment([
      html.text("start"),
      html.div([], []),
      element.fragment([
        html.text("inner1"),
        html.span([], [html.text("span")]),
        html.text("inner2"),
      ]),
      html.text("end"),
      html.div([], []),
    ])

  input
  |> snapshot("Nested with mixed content: fragments use start/end markers")
}

pub fn fragment_multiple_elements_test() {
  use <- lustre_test.test_filter("fragment_multiple_elements_test")

  let input = element.fragment([html.div([], []), html.div([], [])])

  input
  |> snapshot("Fragment with multiple elements should render on multiple lines")
}

pub fn fragment_multiple_mixed_test() {
  use <- lustre_test.test_filter("fragment_multiple_mixed_test")

  let input =
    element.fragment([
      html.text("Hello, "),
      html.span([], [html.text("world")]),
      html.text("!"),
    ])

  input
  |> snapshot(
    "Fragment with multiple mix elements should render on multiple lines",
  )
}

pub fn keyed_fragment_test() {
  use <- lustre_test.test_filter("keyed_fragment_test")

  let input =
    keyed.fragment([
      #("a", html.div([], [html.text("a")])),
      #("b", html.div([], [html.text("b")])),
      #("c", html.div([], [html.text("c")])),
    ])

  input
  |> snapshot("Keyed fragment adds keys to its children")
}

// MAP TESTS -------------------------------------------------------------------

pub fn map_basic_test() {
  use <- lustre_test.test_filter("map_basic_test")

  let input =
    html.div([], [html.text("Hello")])
    |> element.map(fn(_) { Nil })

  input |> snapshot("Map node should produce comment")
}

pub fn map_with_key_test() {
  use <- lustre_test.test_filter("map_with_key_test")

  let input =
    html.div([], [html.text("Hello")])
    |> element.map(fn(_) { Nil })
    |> vnode.to_keyed("my-key", _)

  input |> snapshot("Map node with key should include key in comment")
}

pub fn map_with_special_chars_in_key_test() {
  use <- lustre_test.test_filter("map_with_special_chars_in_key_test")

  let input =
    html.div([], [html.text("Hello")])
    |> element.map(fn(_) { Nil })
    |> vnode.to_keyed("key-with-<>&\"", _)

  input
  |> snapshot("Map node with special chars in key should escape them properly")
}

// MEMO TESTS ------------------------------------------------------------------

pub fn memo_basic_test() {
  use <- lustre_test.test_filter("memo_basic_test")

  let input = element.memo([], fn() { html.div([], [html.text("Hello")]) })

  input |> snapshot("Memo node should produce comment")
}

pub fn memo_with_key_test() {
  use <- lustre_test.test_filter("memo_with_key_test")

  let input =
    element.memo([], fn() { html.div([], [html.text("Hello")]) })
    |> vnode.to_keyed("my-key", _)

  input |> snapshot("Memo node with key should include key in comment")
}

pub fn memo_with_special_chars_in_key_test() {
  use <- lustre_test.test_filter("memo_with_special_chars_in_key_test")

  let input =
    element.memo([], fn() { html.div([], [html.text("Hello")]) })
    |> vnode.to_keyed("key-with-<>&\"", _)

  input
  |> snapshot("Memo node with special chars in key should escape them properly")
}

// ATTRIBUTE TESTS -------------------------------------------------------------

pub fn default_value_attribute_test() {
  use <- lustre_test.test_filter("default_value_attribute_test")

  let input =
    html.input([
      attribute.type_("text"),
      attribute.default_value("Hello, world!"),
    ])

  input
  |> snapshot("Default value attribute should be rendered as value attribute")
}

pub fn default_checked_attribute_test() {
  use <- lustre_test.test_filter("default_checked_attribute_test")

  let input =
    html.input([
      attribute.type_("checkbox"),
      attribute.default_checked(True),
    ])

  input
  |> snapshot(
    "Default checked attribute should be rendered as checked attribute",
  )
}

pub fn default_selected_attribute_test() {
  use <- lustre_test.test_filter("default_selected_attribute_test")

  let input =
    html.select([], [html.option([attribute.default_selected(True)], "")])

  input
  |> snapshot(
    "Default selected attribute should be rendered as selected attribute",
  )
}

// NAMESPACE TESTS -------------------------------------------------------------

pub fn namespaced_nesting_same_test() {
  use <- lustre_test.test_filter("namespaced_nesting_same_test")

  let input = html.svg([], [svg.circle([])])

  input |> snapshot("Namespaced element nested in same namespace")
}

pub fn namespaced_nesting_different_test() {
  use <- lustre_test.test_filter("namespaced_nesting_different_test")

  let input =
    html.svg([], [
      svg.foreign_object([], [
        html.math([], [
          mathml.annotation([], []),
        ]),
      ]),
    ])

  input |> snapshot("Namespaced element nested in different namespace")
}

pub fn namespaced_nesting_default_html_test() {
  use <- lustre_test.test_filter("namespaced_nesting_default_html_test")

  let input =
    html.svg([], [
      svg.foreign_object([], [
        html.div([], [html.p([], [])]),
      ]),
    ])

  input
  |> snapshot("Namespaced element with nested html element")
}

// UTILS -----------------------------------------------------------------------

fn snapshot(el: Element(msg), title: String) -> Nil {
  el |> dom.to_snapshot(True) |> birdie.snap("[html] " <> title)
}
